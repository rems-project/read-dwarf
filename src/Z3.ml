(** This module handles a Z3 server

    For high level usage, call {!start} then use any of
    - {!simplify}
    - {!check_sat}
    - {!check}

    For low-level details:

    The module keeps Z3 as a child process and communicates through pipes.
    As input and output may be ambiguous in this context
    (the input of Z3 is the output of read-dwarf), we use "request" for message
    going from read-dwarf to Z3 and answer for message going from Z3 to read-dwarf.

    On start it sends the introduction in intro.smt2 to Z3 and then {!push} to
    start a specific request. See {!start}.

    To do a request look at {!request}

    After each request one should pop and push to delete the context of
    the special request and keep the intro context. Use {!soft_reset} for that.
    {!request_reset} so that all in one.*)

open Logs.Logger (struct
  let str = "Z3"
end)

open Ast

(*****************************************************************************)
(*         Raw server management                                             *)
(*****************************************************************************)

type server = Cmd.IOServer.t

let server : server option ref = ref None

(** The request number for Z3 *)
let req_num = ref (-1)

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
(*         Request and answer management                                     *)
(*****************************************************************************)

(** Read one Z3 answer as a string. A Z3 answer is always a single S expression

    To get a parsed answer use {!read_answer}.
*)
let read_string_answer () : string =
  let serv = get_server () in
  Files.input_sexp serv.input

(** Read one Z3 answer as a structured smt answer *)
let read_answer () : rsmt_ans =
  let filename = "Z3 output " ^ string_of_int !req_num in
  parse_smt_ans_string ~filename (read_string_answer ())

(** Expect a version answer and fails if it is not the case *)
let expect_version = function Version s -> s | _ -> failwith "expected version from Z3"

(** Expect an expression answer and fails if it is not the case *)
let expect_exp = function Exp e -> e | _ -> failwith "expected expression from Z3"

(** Send the string as a request to Z3.

    If the request is too long to be concatenated efficiently in one string,
    output it directly on {!get_request_channel}[ ()] and then call {!finish_request}

    If you want to get the result call {!request} or {!string_request}
*)
let send_string_request (req : string) : unit =
  let serv = get_server () in
  req_num := !req_num + 1;
  output_string serv.output req;
  output_char serv.output '\n';
  flush serv.output

(** Call this after sending your request directly on {!get_request_channel}[ ()] *)
let finish_request () = send_string_request ""

(** Basically {!send_string_request} then {!read_string_answer}  *)
let string_request (req : string) : string =
  send_string_request req;
  read_string_answer ()

(** This function take a string request and give the parsed Z3 answer

    Basically {!send_string_request} then {!read_answer}

    - Use {!string_request} to not parse the answer.
    - Use {!send_string_request} to not read the answer.
    - Use {!read_string_answer} to only read the answer.
    - Use {!read_answer} to only read and parse the answer.

*)
let request (req : string) : rsmt_ans =
  send_string_request req;
  read_answer ()

(** Push Z3 context. All Z3 context added from now on will be deleted at next {!pop}*)
let push () = send_string_request "(push)"

(** Pop Z3 context. Delete all Z3 context since matching push*)
let pop () = send_string_request "(pop)"

(** This reset the context except the intro. {!pop} then {!push} *)
let soft_reset () =
  pop ();
  push ()

(** This does a {!request} and then a {!soft_reset} *)
let request_reset (req : string) =
  let a = request req in
  soft_reset ();
  a

(** Get the Z3 version string *)
let get_version () =
  match request "(get-info :version)" with
  | Version s -> s
  | _ -> failwith "Z3 version, protocol error"

(*****************************************************************************)
(*         Full startup and shutdowns                                        *)
(*****************************************************************************)

(** Start the Z3 Server, setup the intro and {!push} *)
let start () =
  raw_start ();
  let ver = get_version () in
  info "Z3 started with version %s" ver;
  flush stdout;
  send_string_request SmtIntro.intro;
  push ()

(** Stop the Z3 server properly *)
let stop () =
  send_string_request "(exit)";
  raw_stop ();
  info "Closed connection with Z3";
  flush stdout

(*****************************************************************************)
(*         High level interaction                                            *)
(*****************************************************************************)

(** Declare all the variables in exp in the (declare-const ...) format on the
    [out_channel]

    If declared is given, use it to know which variable are already declared.
*)
let declare_vars ?(declared = Hashtbl.create 10) ochannel (exp : State.exp) : unit =
  let process_var (var : State.var) =
    if not @@ Hashtbl.mem declared @@ State.Var.to_string var then begin
      let decl = DeclareConst (var, State.var_type var |> AstManip.ty_allow_mem) in
      PPI.(fprint ochannel @@ pp_smt State.Var.pp decl ^^ hardline);
      Hashtbl.add declared (State.Var.to_string var) ()
    end
  in
  AstManip.exp_iter_var process_var exp

(** Simplify an expression using Z3 [simplify] command. This is a context-free simplification*)
let simplify (exp : State.exp) : State.exp =
  let ochannel = get_request_channel () in
  declare_vars ochannel exp;
  PPI.(fprint ochannel @@ parens (!^"simplify " ^^ State.pp_exp exp));
  finish_request ();
  let rexp = read_answer () |> expect_exp in
  soft_reset ();
  rexp
  |> AstManip.exp_conv_var State.Var.of_string
  |> AstManip.unfold_lets |> AstManip.expect_no_mem

(** Check that a set of assertion is satisfiable. If the answer is [None] then Z3 didn't know*)
let check_sat asserts : bool option =
  let ochannel = get_request_channel () in
  let declared = Hashtbl.create 100 in
  List.iter (declare_vars ~declared ochannel) asserts;
  List.iter (fun e -> PP.fprintln ochannel @@ State.pp_smt (Assert e)) asserts;
  let a = request "(check-sat)" in
  soft_reset ();
  match a with
  | Error s -> failwith (Printf.sprintf "Z3 encountered an error on request %d : %s" !req_num s)
  | Sat -> Some true
  | Unsat -> Some false
  | Unknown -> None
  | _ -> failwith (Printf.sprintf "Z3 protocol error on request %d " !req_num)

(** Check that a property always holds under a set of hypothesis ([hyps]).

    All unquantified variable in property are implicitly universally quantified.

    If the answer is [None] then Z3 didn't know *)
let check ?(hyps = []) property =
  let ochannel = get_request_channel () in
  let declared = Hashtbl.create 100 in
  List.iter (declare_vars ~declared ochannel) hyps;
  declare_vars ~declared ochannel property;
  List.iter (fun e -> PP.fprintln ochannel @@ State.pp_smt (Assert e)) hyps;
  PP.fprintln ochannel @@ State.pp_smt (Assert (Op.not property));
  match request_reset "(check-sat)" with
  | Error s -> failwith (Printf.sprintf "Z3 encountered an error on request %d : %s" !req_num s)
  | Sat -> Some false
  | Unsat -> Some true
  | Unknown -> None
  | _ -> failwith (Printf.sprintf "Z3 protocol error on request %d " !req_num)

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
