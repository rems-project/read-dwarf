(** This module handles a Z3 server

    For high level usage, call {!start} then use any of
    - {!simplify}
    - {!check_sat}
    - {!check}

    For low-level details:

    The module keeps Z3 as a child process and communicates through pipes.
    As input and output may be ambiguous in this context
    (the input of Z3 is the output of read-dwarf), we use "request" for messages
    going from read-dwarf to Z3 and answer for messages going from Z3 to read-dwarf.

    On start it sends the introduction in [intro.smt2] to Z3 and then {!push} to
    start a specific request. See {!start}.

    To do a request look at {!request}

    After each request one should pop and push to delete the context of
    the special request and keep the intro context. Use {!soft_reset} for that.
    {!request_reset} so that all in one.*)

open Logs.Logger (struct
  let str = "Z3"
end)

let z3_trace = true

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Raw server management } *)

type server = Cmd.IOServer.t

let server : server option ref = ref None

type context_elem = { name : string; mutable num : int }

let context : context_elem list ref = ref [{ name = "top"; num = 0 }]

(** Assume the server is started and get it out of the reference *)
let get_server () =
  match !server with Some serv -> serv | None -> failwith "Z3 server was not started"

(** Assume the server is started and get the channel to send it requests *)
let get_request_channel () = (get_server ()).output

(** Start Z3 without any checks and without sending intro or pushing *)
let raw_start () =
  if !server != None then failwith "Z3 server starting when there is already a server online";
  server := Some (Cmd.IOServer.start [|!CommonOpt.z3_ref; "-in"|]);
  ()

(** Stop Z3 without asking politely *)
let raw_stop () =
  match !server with
  | Some serv ->
      Cmd.IOServer.stop serv;
      server := None
  | None -> ()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Raw Context management } *)

let context_elem_to_string ce = Printf.sprintf "(%s num %d)" ce.name ce.num

let get_context_string () =
  if z3_trace then String.concat "." (List.map context_elem_to_string !context)
  else "Z3 tracing disabled"

let incr_context () =
  let ce = List.hd !context in
  ce.num <- ce.num + 1

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Request and answer management }

    TODO Document this section *)

let send_string (serv : server) s =
  output_string serv.output s;
  output_char serv.output '\n';
  flush serv.output;
  if z3_trace then incr_context ()

let send_smt (serv : server) ?(ppv = PP.erase) smt =
  debug "In context %t, sent smt %t"
    (fun o -> output_string o (get_context_string ()))
    (PP.top (Ast.pp_smt ppv) smt);
  PP.fprintln serv.output @@ Ast.pp_smt ppv smt;
  if z3_trace then incr_context ()

let read_string (serv : server) =
  let sexp = Files.input_sexp serv.input in
  debug "In context %t, read %s" (fun o -> output_string o (get_context_string ())) sexp;
  sexp

let read_smt_ans (serv : server) : Ast.rsmt_ans =
  let filename = get_context_string () in
  Ast.parse_smt_ans_string ~filename (read_string serv)

let request ?(ppv = PP.erase) smt : Ast.rsmt_ans =
  let serv = get_server () in
  send_smt serv ~ppv smt;
  read_smt_ans serv

let command ?(ppv = PP.erase) smt =
  let serv = get_server () in
  send_smt serv ~ppv smt

(** Expect a version answer and fails if it is not the case *)
let expect_version : Ast.rsmt_ans -> string = function
  | Version s -> s
  | _ -> failwith "expected version from Z3"

(** Expect an expression answer and fails if it is not the case *)
let expect_exp : Ast.rsmt_ans -> Ast.rexp = function
  | Exp e -> e
  | _ -> failwith "expected expression from Z3"

(** Get the Z3 version string *)
let get_version () =
  match request Ast.GetVersion with Version s -> s | _ -> failwith "Z3 version, protocol error"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Context management }

    Z3 context can be managed in stack way. When pushing Z3 create a new context.
    Inside it, all declaration from parent contexts are available.
    When pop-ing the current context is closed and all the declaration are lost
*)

let open_context name =
  let serv = get_server () in
  if z3_trace then context := { name; num = 0 } :: !context;
  command Ast.Push

let close_context () =
  let serv = get_server () in
  if z3_trace then context := List.tl !context;
  command Ast.Pop

module ContextCounter (S : Logs.String) = struct
  let counter = Counter.make 0

  let openc () =
    if z3_trace then open_context (Printf.sprintf "%s %d" S.str (Counter.get counter))
    else open_context S.str

  let num () = Counter.read counter

  let closec = close_context
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Full startup and shutdown } *)

(** Start the Z3 Server, setup the intro and {!push} *)
let start () =
  raw_start ();
  let ver = get_version () in
  info "Z3 started with version %s" ver;
  send_string (get_server ()) SmtIntro.intro

(** Stop the Z3 server properly *)
let stop () =
  command Ast.Exit;
  raw_stop ();
  info "Closed connection with Z3"

(** Call {!start} if the server wasn't started *)
let ensure_started () = match !server with None -> start () | Some _ -> ()

(** Call {!stop} if the server wasn't stopped *)
let ensure_stopped () = match !server with None -> () | Some _ -> stop ()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Medium-level interaction } *)

type ('a, 'v) in_exp = ('a, 'v, Ast.no, Ast.Size.t) Ast.exp

type 'v out_exp = (Ast.lrng, 'v, Ast.no, Ast.Size.t) Ast.exp

(** Simplify an expression in the context. The context must already have been declared *)
let simplify_full ~ppv ~vofs (exp : ('a, 'v) in_exp) : 'v2 out_exp =
  exp |> AstManip.allow_lets |> Ast.Op.simplify |> request ~ppv |> expect_exp
  |> AstManip.unfold_lets |> AstManip.exp_conv_var vofs

(** Check a property in the current context. The context must already have been declared
    Do not modify the context (It creates a subcontext) *)
let check_full ~ppv (exp : ('a, 'v) in_exp) =
  open_context "check";
  command ~ppv (exp |> AstManip.allow_lets |> Ast.Op.not |> Ast.Op.assert_op);
  let r = request CheckSat in
  close_context ();
  match r with
  | Error s -> Raise.fail "Z3: error %s on check in %s" s (get_context_string ())
  | Sat -> Some false
  | Unsat -> Some true
  | Unknown -> None
  | _ -> Raise.fail "Z3 protocol error on check in %s" (get_context_string ())

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 High level interaction with state } *)

let exp_conv exp = exp |> AstManip.allow_mem |> AstManip.allow_lets

(** Declare all the variables in exp in the (declare-const ...) format on the
    [out_channel]

    If declared is given, use it to know which variable are already declared.
*)
let declare_vars ?(declared = Hashtbl.create 10) serv (exp : State.exp) : unit =
  let process_var (var : State.var) =
    if not @@ Hashtbl.mem declared @@ State.Var.to_string var then begin
      let decl = Ast.DeclareConst (var, State.var_type var |> AstManip.ty_allow_mem) in
      send_smt serv ~ppv:State.Var.pp decl;
      Hashtbl.add declared (State.Var.to_string var) ()
    end
  in
  AstManip.exp_iter_var process_var exp

module SimpContext = ContextCounter (struct
  let str = "Simplification"
end)

(** Simplify an expression using Z3 [simplify] command. This is a context-free simplification*)
let simplify (exp : State.exp) : State.exp =
  let serv = get_server () in
  SimpContext.openc ();
  declare_vars serv exp;
  let res_exp =
    exp |> AstManip.allow_mem
    |> simplify_full ~ppv:State.Var.pp ~vofs:State.Var.of_string
    |> AstManip.expect_no_mem
  in
  close_context ();
  res_exp

module SatContext = ContextCounter (struct
  let str = "Check sat"
end)

(** Check that a set of assertion is satisfiable. If the answer is [None] then Z3 didn't know*)
let check_sat asserts : bool option =
  let serv = get_server () in
  SatContext.openc ();
  let declared = Hashtbl.create 100 in
  List.iter (declare_vars ~declared serv) asserts;
  List.iter (fun e -> send_smt ~ppv:State.Var.pp serv (Assert (e |> exp_conv))) asserts;
  let a = request CheckSat in
  close_context ();
  match a with
  | Error s -> Raise.fail "Z3 encountered an error on check_sat %d : %s" (SatContext.num ()) s
  | Sat -> Some true
  | Unsat -> Some false
  | Unknown -> None
  | _ -> Raise.fail "Z3 protocol error on check_sat %d" (SatContext.num ())

module CheckContext = ContextCounter (struct
  let str = "Check"
end)

(** Check that a property always holds under a set of hypothesis ([hyps]).

    All unquantified variable in property are implicitly universally quantified.

    If the answer is [None] then Z3 didn't know *)
let check ?(hyps = []) property =
  let serv = get_server () in
  CheckContext.openc ();
  let declared = Hashtbl.create 100 in
  List.iter (declare_vars ~declared serv) hyps;
  declare_vars ~declared serv property;
  List.iter (fun e -> send_smt ~ppv:State.Var.pp serv (Assert (e |> exp_conv))) hyps;
  send_smt ~ppv:State.Var.pp serv (Assert (property |> exp_conv |> Ast.Op.not));
  let a = request CheckSat in
  close_context ();
  match a with
  | Error s -> Raise.fail "Z3 encountered an error on check %d : %s" (CheckContext.num ()) s
  | Sat -> Some false
  | Unsat -> Some true
  | Unknown -> None
  | _ -> Raise.fail "Z3 protocol error on check %d" (CheckContext.num ())

(*****************************************************************************)
(*        Tests                                                              *)
(*****************************************************************************)

let _ =
  Tests.add_test "Z3.direct" (fun () ->
      let output ochannel =
        Printf.fprintf ochannel "(display 42)\n";
        flush ochannel
      in
      let input ichannel = input_line ichannel in
      Cmd.io [|!CommonOpt.z3_ref; "-in"|] output input = "42")

let _ =
  Tests.add_test "Z3" (fun () ->
      start ();
      stop ();
      true)

let _ =
  Tests.add_test "Z3.simplify" (fun () ->
      start ();
      let exp =
        Ast.(
          Binop
            ( Bvarith Bvsub,
              Bits ("#x3", State.dummy_annot),
              Bits ("#x1", State.dummy_annot),
              State.dummy_annot ))
      in
      let exp = simplify exp in
      stop ();
      match exp with Bits ("#x2", _) -> true | _ -> false)

let _ =
  Tests.add_test "Z3.check_sat" (fun () ->
      start ();
      let exp =
        Ast.(
          Binop
            ( Eq,
              Bits ("#x1", State.dummy_annot),
              Bits ("#x1", State.dummy_annot),
              State.dummy_annot ))
      in
      let res = check_sat [exp] in
      stop ();
      Option.value res ~default:false)

let _ =
  Tests.add_test "Z3.check" (fun () ->
      start ();
      let exp =
        Ast.(
          Binop
            ( Eq,
              Bits ("#x1", State.dummy_annot),
              Bits ("#x1", State.dummy_annot),
              State.dummy_annot ))
      in
      let res = check exp in
      stop ();
      Option.value res ~default:false)
