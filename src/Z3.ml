(** This module handles a Z3 server

    For high level usage, call {!start} or {!ensure_started} then use any of
    - {!simplify}
    - {!check_sat}
    - {!check}

    For a more medium level usage, you may want to manage you context before making requests.
    Look at section {!section:context} for context management and TODO.

    In the idea, one would open a context, use {!send_smt} or {!command} to
    send declaration or commands to Z3 and then finish by the {!request}.
    Then one can close the context.

    For low-level details:

    The module keeps Z3 as a child process and communicates through pipes.

    {!start} sends the introduction in [intro.smt2] to Z3 so that it is available
    in any context.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Raw server management } *)

type server = Cmd.IOServer.t

let server : server option ref = ref None

(** Assume the server is started and returns it. *)
let get_server () =
  match !server with Some serv -> serv | None -> failwith "Z3 server was not started"

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
(** {1 Raw Context management }
    The {!context} represent a stack of {!context_elem} that represent the
    current opened contexts.
*)

(** A boolean enabling context tracing (For better error messages) *)
let z3_trace = true

(** An element of the context stack. It has a name and a declaration number.

    The declaration number is incremented at each declaration in the context.*)
type context_elem = { name : string; mutable num : int }

(** The current context stack *)
let context : context_elem list ref = ref [{ name = "top"; num = 0 }]

(** Give a string representation of a {!context_elem} *)
let context_elem_to_string ce = Printf.sprintf "(%s num %d)" ce.name ce.num

(** Give a string representation of the current context for error reporting *)
let get_context_string () =
  if z3_trace then String.concat "." (List.map context_elem_to_string !context)
  else "Z3 tracing disabled"

(** Increment the declaration number of the top {!context_elem}. *)
let incr_context () =
  let ce = List.hd !context in
  ce.num <- ce.num + 1

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Request and answer management }

    This section handle raw and less raw direct interaction with the Z3 server.

    The [send_*] function are for sending information to the server with a server handle.
    This avoided checking if the server is open at each declaration.

    The [read_*] functions are the same the other way around.

    The {!request} and {!command} are high level versions
*)

(** Send a string to the server and increment the declaration number in the context *)
let send_string (serv : server) s =
  output_string serv.output s;
  output_char serv.output '\n';
  flush serv.output;
  if z3_trace then incr_context ()

(** Send a smt statement to the server and increment the declaration number in the context *)
let send_smt (serv : server) ?(ppv = PP.erase) smt =
  debug "In context %t, sent smt %t"
    (fun o -> output_string o (get_context_string ()))
    (PP.top (Ast.pp_smt ppv) smt);
  PP.fprintln serv.output @@ Ast.pp_smt ppv smt;
  if z3_trace then incr_context ()

(** Read a string from the server (A Z3 answer is always a valid sexp) *)
let read_string (serv : server) =
  let sexp = Files.input_sexp serv.input in
  debug "In context %t, read %s" (fun o -> output_string o (get_context_string ())) sexp;
  sexp

(** Read a {!smt_ans} from the server *)
let read_smt_ans (serv : server) : Ast.rsmt_ans =
  let filename = get_context_string () in
  Ast.parse_smt_ans_string ~filename (read_string serv)

(** Make a request to the server and expect an answer. Will hang if there is no answer *)
let request ?(ppv = PP.erase) smt : Ast.rsmt_ans =
  let serv = get_server () in
  send_smt serv ~ppv smt;
  read_smt_ans serv

(** Send a command or a declaration to the server *)
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
(** {1 Full startup and shutdown } *)

(** Stop the Z3 server properly *)
let stop () =
  command Ast.Exit;
  raw_stop ();
  info "Closed connection with Z3"

(** Call {!stop} if the server wasn't stopped *)
let ensure_stopped () = match !server with None -> () | Some _ -> stop ()

(** Start the Z3 Server and setup the intro *)
let start () =
  raw_start ();
  let ver = get_version () in
  info "Z3 started with version %s" ver;
  send_string (get_server ()) SmtIntro.intro;
  (* Stop the server at program exit *)
  at_exit ensure_stopped

(** Call {!start} if the server wasn't started *)
let ensure_started () = match !server with None -> start () | Some _ -> ()

(** Call {!start} if the server wasn't started, then return the server *)
let ensure_started_get () =
  match !server with
  | None ->
      start ();
      get_server ()
  | Some serv -> serv

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:context Context management }

    Z3 contexts are managed in a stack way.

    There are implemented using [(push)] and [(pop)] on the Z3 side.

    {!open_context} can open a named context.
    However if it is expected to open a given context name multiple times,
    using {!ContextCounter} is advised.
*)

(** Open a new context with a name. Ensure the server is started *)
let open_context name =
  let serv = ensure_started_get () in
  if z3_trace then context := { name; num = 0 } :: !context;
  command Ast.Push

(** Closes current context *)
let close_context () =
  let serv = get_server () in
  if z3_trace then context := List.tl !context;
  command Ast.Pop

(** Module for handling a context numbering scheme automatically.

    To use it do:
    [module Whatever = ContextCounter(struct let str= "name here" end)]
    Then you can use it with
    [Whatever.openc ()] and [Whatever.closec ()].
    You can get the current instance number with [Whatever.num()] at any time.*)

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
(** {1 Medium-level interaction }

    This section allow medium level Z3 interaction.
    One can do calls to simplify an expression or check that a property is true or false
    in the current context. All requests do not affect the context.
    The context must contain declaration of all variable used and all hypothesis in the
    case of a {!check_gen} or {!check_sat_gen}
*)

(** The type of input expression. No let bindings *)
type ('a, 'v) in_exp = ('a, 'v, Ast.no, Ast.Size.t) Ast.exp

(** The type of input expression. No let bindings *)
type 'v out_exp = (Ast.lrng, 'v, Ast.no, Ast.Size.t) Ast.exp

(** Simplify an expression in the context. The context must already have been declared *)
let simplify_gen ~ppv ~vofs (exp : ('a, 'v) in_exp) : 'v2 out_exp =
  exp |> AstManip.allow_lets |> Ast.Op.simplify |> request ~ppv |> expect_exp
  |> AstManip.unfold_lets |> AstManip.exp_conv_var vofs

(** Check the correctness of a property in the current context.
    The context must already have been declared
    Do not modify the context (It creates a subcontext) *)
let check_gen ~ppv (exp : ('a, 'v) in_exp) =
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

(** Check the satisfiability of a property in the current context.
    The context must already have been declared
    Do not modify the context (It creates a subcontext) *)
let check_sat_gen ~ppv (exp : ('a, 'v) in_exp) =
  open_context "check_sat";
  command ~ppv (exp |> AstManip.allow_lets |> Ast.Op.assert_op);
  let r = request CheckSat in
  close_context ();
  match r with
  | Error s -> Raise.fail "Z3: error %s on check_sat in %s" s (get_context_string ())
  | Sat -> Some true
  | Unsat -> Some false
  | Unknown -> None
  | _ -> Raise.fail "Z3 protocol error on check_sat in %s" (get_context_string ())

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 High level interaction with state }

    Those requests can be made at any time and do not require any special context to be declared.
*)

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
  let serv = ensure_started_get () in
  SimpContext.openc ();
  declare_vars serv exp;
  let res_exp =
    exp |> AstManip.allow_mem
    |> simplify_gen ~ppv:State.Var.pp ~vofs:State.Var.of_string
    |> AstManip.expect_no_mem
  in
  close_context ();
  res_exp

module SatContext = ContextCounter (struct
  let str = "Check sat"
end)

(** Check that a set of assertion is satisfiable. If the answer is [None] then Z3 didn't know*)
let check_sat asserts : bool option =
  let serv = ensure_started_get () in
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
  let serv = ensure_started_get () in
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

let _ = Tests.add_reset "Z3 stop" ensure_stopped

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
      let exp = Ast.Op.(bits_smt "#x3" - bits_int ~size:4 1) in
      let exp = simplify exp in
      stop ();
      match exp with Bits (v, _) -> BitVec.to_int v = 2 | _ -> false)

let _ =
  Tests.add_test "Z3.check_sat" (fun () ->
      start ();
      let exp = Ast.Op.(bits_int ~size:4 1 = bits_smt "#x1") in
      let res = check_sat [exp] in
      stop ();
      Option.value res ~default:false)

let _ =
  Tests.add_test "Z3.check" (fun () ->
      start ();
      let exp = Ast.Op.(bits_int ~size:4 1 = bits_smt "#x1") in
      let res = check exp in
      stop ();
      Option.value res ~default:false)
