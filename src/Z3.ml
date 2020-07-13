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

type server = { ioserver : Cmd.IOServer.t; config : ConfigFile.Z3.t }

let server : server option ref = ref None

(** Assume the server is started and returns it. *)
let get_server () =
  match !server with Some serv -> serv | None -> failwith "Z3 server was not started"

(** Start Z3 without any checks and without sending intro or pushing *)
let raw_start () =
  if !server != None then failwith "Z3 server starting when there is already a server online";
  let config = ConfigFile.get_z3_config () in
  let base_cmd = [!CommonOpt.z3_ref; "-in"] in
  let timeout_cmd = config.timeout |> Opt.map (Printf.sprintf "-t:%d") |> Opt.to_list in
  let memory_cmd = config.memory |> Opt.map (Printf.sprintf "-memory:%d") |> Opt.to_list in
  let cmd = Array.of_list (base_cmd @ timeout_cmd @ memory_cmd) in
  server := Some { ioserver = Cmd.IOServer.start cmd; config };
  ()

(** Stop Z3 without asking politely *)
let raw_stop () =
  match !server with
  | Some { ioserver; _ } ->
      Cmd.IOServer.stop ioserver;
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
  output_string serv.ioserver.output s;
  output_char serv.ioserver.output '\n';
  flush serv.ioserver.output;
  if z3_trace then incr_context ()

(** Send a smt statement to the server and increment the declaration number in the context *)
let send_smt (serv : server) ?(ppv = PP.erase) smt =
  debug "In context %t, sent smt %t"
    (fun o -> output_string o (get_context_string ()))
    (PP.top (Ast.pp_smt ppv) smt);
  PP.fprintln serv.ioserver.output @@ Ast.pp_smt ppv smt;
  if z3_trace then incr_context ()

(** Read a string from the server (A Z3 answer is always a valid sexp) *)
let read_string (serv : server) =
  let sexp = Files.input_sexp serv.ioserver.input in
  debug "In context %t, read \n%s" (fun o -> output_string o (get_context_string ())) sexp;
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
  | _ -> Raise.fail "expected version from Z3 in %s" (get_context_string ())

(** Expect an expression answer and fails if it is not the case *)
let expect_exp : Ast.rsmt_ans -> Ast.rexp = function
  | Exp e -> e
  | _ -> Raise.fail "expected expression from Z3 in %s" (get_context_string ())

(** Get the Z3 version string *)
let get_version () =
  match request Ast.GetVersion with Version s -> s | _ -> failwith "Z3 version, protocol error"

(** Check if the current context is sat *)
let is_context_sat serv =
  send_smt serv Ast.CheckSat;
  match read_smt_ans serv with
  | Error s -> Raise.fail "Z3: error %s on check_sat in %s" s (get_context_string ())
  | Sat -> Some true
  | Unsat -> Some false
  | Unknown -> None
  | _ -> Raise.fail "Z3 protocol error on check_sat in %s" (get_context_string ())

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

(** Reset the Z3 server, forgetting everything. Useful for resetting in a test failure context,
    but probably shouldn't be used in normal operations*)
let reset () =
  ensure_stopped ();
  start ()

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
  ensure_started ();
  if z3_trace then begin
    debug "Opening context %s" name;
    context := { name; num = 0 } :: !context
  end;
  command Ast.Push

(** Closes current context *)
let close_context () =
  if z3_trace then (
    let { name; num } = List.hd !context in
    debug "Closing context %s at %d" name num;
    context := List.tl !context
  );
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
    if z3_trace then open_context (Printf.sprintf "%s:%d" S.str (Counter.get counter))
    else open_context S.str

  let num () = Counter.read counter

  let closec () =
    begin
      if z3_trace then
        let { name; _ } = List.hd !context in
        Scanf.sscanf name "%s@:%d" (fun n _ -> assert (n = S.str))
    end;
    close_context ()
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 High level interaction }

    This section provide functors that can be instantiated to get easy to use SMT functionality *)

(** The functors in this section require a bit more support from variable than plain {!Exp.Var} *)
module type Var = sig
  include Exp.Var

  (** Hashing function to store variable in [Hashtbl]s. Must be compatible with {!equal} *)
  val hash : t -> int

  (** Parser from a string. Must be the inverse of {!pp} *)
  val of_string : string -> t
end

module type S = sig
  type var

  (** {1 Variable declarations }

      The goal of those operation is to declare variables with their types to the SMT solver.
      The {!Htbl} allow to declare every variable only once.
  *)
  type exp = (var, Ast.no) ExpTyped.t

  (*****************************************************************************)
  (*****************************************************************************)
  (*****************************************************************************)

  (** Hash tables over variables *)
  module Htbl : Hashtbl.S with type key = var

  (** Declare the variable regardless of whether it has already been declared *)
  val declare_var_always : server -> var -> unit

  (** Declare the variable if it's not in [declared]. In that case, it also
      add it to [declared] *)
  val declare_var : server -> declared:unit Htbl.t -> var -> unit

  (** Declare all not yet declared the variable in the expression,
      then add them to [declared] *)
  val declare_vars : server -> declared:unit Htbl.t -> exp -> unit

  (*****************************************************************************)
  (*****************************************************************************)
  (*****************************************************************************)
  (** {1 Simplification } *)

  (** Simplify the expression in the current context. All the variable
      must already have been declared. *)
  val simplify : server -> exp -> exp

  (** Literally just {!declare_vars} then {!simplify} *)
  val simplify_decl : server -> declared:unit Htbl.t -> exp -> exp

  (*****************************************************************************)
  (*****************************************************************************)
  (*****************************************************************************)
  (** {1 SMT checking } *)

  (** Assert that expression in the current context *)
  val send_assert : server -> exp -> unit

  (** Literally just {!declare_vars} then {!send_assert} *)
  val send_assert_decl : server -> declared:unit Htbl.t -> exp -> unit

  (** Check if this expression is always true in the current context.
      [None] is returned when the SMT solver didn't know *)
  val check : server -> exp -> bool option

  (** Check if this expression is true on at least one assignment of the variables.ioserver
      [None] is returned when the SMT solver didn't know *)
  val check_sat : server -> exp -> bool option

  (** Check if this expression is always true of always false.
      [None] is returned if neither can be proven.

      This results in two calls to the SMT solver. one with {!check} and one with {!check_sat} *)
  val check_both : server -> exp -> bool option

  (*****************************************************************************)
  (*****************************************************************************)
  (*****************************************************************************)
  (** {1 Context less operation }
      Those operations do not require a context to operate. They require not setup and
      tear-down. They are standalone and fully automated.

      On the other hand doing multiple one of those in sequence if much less efficient than
      using properly the functions of previous section.
  *)

  (** Do a standalone simplification in it's own context. Do not need anything
      to be already declared or any context to be opened.*)
  val simplify_full : exp -> exp

  (** Do a standalone check of whether the expression is implied by the list of hypothesis. *)
  val check_full : ?hyps:exp list -> exp -> bool option

  (** Do a standalone check of whether the set of assertion is sat *)
  val check_sat_full : exp list -> bool option
end

module SimpContext = ContextCounter (struct
  let str = "Simplification"
end)

module CheckContext = ContextCounter (struct
  let str = "Check"
end)

module Make (Var : Var) : S with type var = Var.t = struct
  type var = Var.t

  module Exp = Exp.Make (Var)

  type exp = Exp.t

  module Htbl = Hashtbl.Make (Var)

  let declare_var_always serv var =
    let decl = Ast.DeclareConst (var, Var.ty var |> AstManip.ty_allow_mem) in
    send_smt serv ~ppv:Var.pp decl

  let declare_var serv ~declared var =
    if not @@ Htbl.mem declared @@ var then begin
      declare_var_always serv var;
      Htbl.add declared var ()
    end

  let declare_vars serv ~declared exp = AstManip.exp_iter_var (declare_var serv ~declared) exp

  let simplify serv (e : Exp.t) : Exp.t =
    e |> AstManip.allow_mem |> AstManip.allow_lets
    |> Ast.simplify_smt ~flags:serv.config.simplify_opts
    |> request ~ppv:Var.pp |> expect_exp |> AstManip.unfold_lets
    |> AstManip.exp_conv_var Var.of_string
    |> AstManip.expect_no_mem
    |> ExpTyped.add_type ~ty_of_var:(Fun.const Var.ty)

  let simplify_decl serv ~declared (e : Exp.t) : Exp.t =
    declare_vars serv ~declared e;
    simplify serv e

  let simplify_full (e : Exp.t) : Exp.t =
    let serv = ensure_started_get () in
    SimpContext.openc ();
    let declared = Htbl.create 10 in
    let res = simplify_decl serv ~declared e in
    SimpContext.closec ();
    res

  let send_assert serv (e : Exp.t) : unit =
    e |> AstManip.allow_mem |> AstManip.allow_lets |> Ast.assert_smt |> send_smt serv ~ppv:Var.pp

  let send_assert_decl serv ~declared (e : Exp.t) : unit =
    declare_vars serv ~declared e;
    send_assert serv e

  let check serv (e : Exp.t) : bool option =
    open_context "check";
    send_assert serv (ExpTyped.not e);
    let res = is_context_sat serv |> Opt.map not in
    close_context ();
    res

  let check_full ?(hyps : Exp.t list = []) (e : Exp.t) : bool option =
    let serv = ensure_started_get () in
    CheckContext.openc ();
    let declared = Htbl.create 100 in
    List.iter (send_assert_decl ~declared serv) hyps;
    send_assert_decl ~declared serv (ExpTyped.not e);
    let res = is_context_sat serv |> Opt.map not in
    CheckContext.closec ();
    res

  let check_sat serv (e : Exp.t) : bool option =
    open_context "check_sat";
    send_assert serv e;
    let res = is_context_sat serv in
    close_context ();
    res

  let check_sat_full (asserts : Exp.t list) : bool option =
    let serv = ensure_started_get () in
    CheckContext.openc ();
    let declared = Htbl.create 100 in
    List.iter (send_assert_decl ~declared serv) asserts;
    let res = is_context_sat serv in
    CheckContext.closec ();
    res

  let check_both serv (e : Exp.t) : bool option =
    match check serv e with
    | Some true as t -> t
    | _ -> (
        match check_sat serv e with Some false as f -> f | _ -> None
      )
end
