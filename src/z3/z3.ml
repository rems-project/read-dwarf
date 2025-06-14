(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module handles a Z3 server

    For high level usage, call {!start} or {!ensure_started} then
    instantiate the {!Make} functor and use operation in the section
    about {!S.nocontext}:

    For a more medium level usage, you may want to manage your context manually
    before making requests. The best way of doing that is by using a
    {!ContextCounter}.

    Once you are in the correct context, you can use your instantiated version of {!Make}
    to do operations such as: {!S.declare_var} or {!S.send_assert} to build your context,
    and then {!S.simplify}, {!S.check} of {!S.check_sat}.

    For low-level details (All function in the first two section of this module should
    probably be reserved to those who understand the implementation)

    The module keeps Z3 as a child process and communicates through pipes using
    {!Utils.Cmd.IOServer}.

    {!start} sends the introduction in [intro.smt2] to Z3 so that it is available
    in any context.
    SMT answer are parsed from the pipe with {!Utils.Files.input_sexp}. If the wrong
    number of answer is expected, the system will just deadlock.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Raw server management } *)

(** A boolean enabling context tracing (For better error messages) *)
let z3_trace = true

(** An element of the context stack. It has a name and a declaration number.

    The declaration number is incremented at each declaration in the context.*)
type context_elem = { name : string; mutable num : int }

(** The type of a SMT context stack *)
type context = context_elem list

(** The starting context *)
let start_context : context = [{ name = "top"; num = 0 }]

(** Give a string representation of a {!context_elem} *)
let context_elem_to_string ce = Printf.sprintf "(%s num %d)" ce.name ce.num

(** The type of a Z3 server *)
type server = { ioserver : Cmd.IOServer.t; config : Config.File.Z3.t; mutable context : context }

(** The global Z3 server *)
let server : server option ref = ref None

(** Assume the server is started and returns it. *)
let get_server () =
  match !server with Some serv -> serv | None -> failwith "Z3 server was not started"

(** Start Z3 without any checks and without sending intro or pushing *)
let raw_start () =
  if !server != None then failwith "Z3 server starting when there is already a server online";
  let config = Config.File.get_z3_config () in
  let base_cmd = [!Config.CommonOpt.z3_ref; "-in"] in
  let timeout_cmd = config.timeout |> Option.map (Printf.sprintf "-t:%d") |> Option.to_list in
  let memory_cmd = config.memory |> Option.map (Printf.sprintf "-memory:%d") |> Option.to_list in
  let cmd = Array.of_list (base_cmd @ timeout_cmd @ memory_cmd) in
  server := Some { ioserver = Cmd.IOServer.start cmd; config; context = start_context };
  ()

(** Stop Z3 without asking politely *)
let raw_stop () =
  match !server with
  | Some { ioserver; _ } ->
      Cmd.IOServer.stop ioserver;
      server := None
  | None -> ()

(** Give a string representation of the current context for error reporting *)
let get_context_string server =
  if z3_trace then String.concat "." (List.map context_elem_to_string server.context)
  else "Z3 tracing disabled"

(** Increment the declaration number of the top {!context_elem}. *)
let incr_context server =
  let ce = List.hd server.context in
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
  if z3_trace then incr_context serv

(** Send a smt statement to the server and increment the declaration number in the context *)
let send_smt (serv : server) ?(ppv = Pp.erase) smt =
  debug "In context %t, sent smt %t"
    (fun o -> output_string o (get_context_string serv))
    (Pp.top (Ast.pp_smt ppv) smt);
  Pp.fprintln serv.ioserver.output @@ Ast.pp_smt ppv smt;
  if z3_trace then incr_context serv

(** Read a string from the server (A Z3 answer is always a valid sexp) *)
let read_string (serv : server) =
  let sexp = Files.input_sexp serv.ioserver.input in
  debug "In context %t, read \n%s" (fun o -> output_string o (get_context_string serv)) sexp;
  sexp

(** Read a {!smt_ans} from the server *)
let read_smt_ans (serv : server) : Ast.rsmt_ans =
  let filename = get_context_string serv in
  Ast.parse_smt_ans_string ~filename (read_string serv)

(** Make a request to the server and expect an answer. Will hang if there is no answer *)
let request ?(ppv = Pp.erase) smt : Ast.rsmt_ans =
  let serv = get_server () in
  send_smt serv ~ppv smt;
  read_smt_ans serv

(** Send a command or a declaration to the server *)
let command ?(ppv = Pp.erase) smt =
  let serv = get_server () in
  send_smt serv ~ppv smt

(** Expect a version answer and fails if it is not the case *)
let expect_version (serv : server) : Ast.rsmt_ans -> string = function
  | Version s -> s
  | _ -> Raise.fail "expected version from Z3 in %s" (get_context_string serv)

(** Expect an expression answer and fails if it is not the case *)
let expect_exp (serv : server) : Ast.rsmt_ans -> Ast.rexp = function
  | Exp e -> e
  | _ -> Raise.fail "expected expression from Z3 in %s" (get_context_string serv)

(** Get the Z3 version string *)
let get_version () =
  match request Ast.GetVersion with Version s -> s | _ -> failwith "Z3 version, protocol error"

(** Check if the current context is sat *)
let is_context_sat serv =
  send_smt serv Ast.CheckSat;
  match read_smt_ans serv with
  | Error s -> Raise.fail "Z3: error %s on check_sat in %s" s (get_context_string serv)
  | Sat -> Some true
  | Unsat -> Some false
  | Unknown -> None
  | _ -> Raise.fail "Z3 protocol error on check_sat in %s" (get_context_string serv)

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

(** Open a new context with a name. *)
let open_context serv name =
  if z3_trace then begin
    debug "Opening context %s" name;
    serv.context <- { name; num = 0 } :: serv.context
  end;
  send_smt serv Ast.Push

(** Closes current context of the server *)
let close_context serv =
  if z3_trace then (
    let { name; num } = List.hd serv.context in
    debug "Closing context %s at %d" name num;
    serv.context <- List.tl serv.context
  );
  send_smt serv Ast.Pop

(** Module for handling a context numbering scheme automatically.

    To use it do:
    [module Whatever = ContextCounter(struct let str= "name here" end)]
    Then you can use it with
    [Whatever.openc ()] and [Whatever.closec ()].
    You can get the current instance number with [Whatever.num()] at any time.*)
module ContextCounter (S : Logs.String) = struct
  let counter = Counter.make 0

  (** Open a new context of that [ContextCounter]. Ensure the server is started *)
  let openc () =
    let serv = ensure_started_get () in
    if z3_trace then open_context serv (Printf.sprintf "%s:%d" S.str (Counter.get counter))
    else open_context serv S.str

  (** Get the current context number *)
  let num () = Counter.read counter

  (** Close a context opened with {!openc}. Assert that the current context
      was indeed opened by this module *)
  let closec () =
    let serv = get_server () in
    begin
      if z3_trace then
        let { name; _ } = List.hd serv.context in
        Scanf.sscanf name "%s@:%d" (fun n _ -> assert (n = S.str))
    end;
    close_context serv
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 High level interaction }

    This section provide functor that can be instantiated to get easy to use SMT functionality *)

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

  (*****************************************************************************)
  (*****************************************************************************)
  (*****************************************************************************)
  (** {1 Variable declarations }

      The goal of those operation is to declare variables with their types to the SMT solver.
      The {!Htbl} allow to declare every variable only once.
  *)

  (** The type of expression on which SMT operation are made *)
  type exp = (var, Ast.no) Exp.Typed.t

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

  val simplify_subterms : server -> exp -> exp

  val simplify_subterms_decl : server -> declared:unit Htbl.t -> exp -> exp

  (*****************************************************************************)
  (*****************************************************************************)
  (*****************************************************************************)
  (** {1:nocontext Context less operation }
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

  val simplify_subterms_full : ?hyps:exp list -> exp -> exp
end

module SimpContext = ContextCounter (struct
  let str = "Simplification"
end)

module CheckContext = ContextCounter (struct
  let str = "Check"
end)

module Make (Var : Var) : S with type var = Var.t = struct
  type var = Var.t

  module Typed = Exp.Typed
  module Exp = Exp.Make (Var)

  type exp = Exp.t

  module Htbl = Hashtbl.Make (Var)

  let declare_var_always serv var =
    let decl = Ast.DeclareConst (var, Var.ty var |> Ast.Manip.ty_allow_mem) in
    send_smt serv ~ppv:Var.pp decl

  let declare_var serv ~declared var =
    if not @@ Htbl.mem declared @@ var then begin
      declare_var_always serv var;
      Htbl.add declared var ()
    end

  let declare_vars serv ~declared exp =
    Ast.Manip.exp_iter_var (declare_var serv ~declared) exp;
    Htbl.iter (fun x _ -> debug "declared: %t" (Pp.top Var.pp x)) declared

  let simplify serv (e : Exp.t) : Exp.t =
    e |> Ast.Manip.allow_mem |> Ast.Manip.allow_lets
    |> Ast.simplify_smt ~flags:serv.config.simplify_opts
    |> request ~ppv:Var.pp |> expect_exp serv |> Ast.Manip.unfold_lets
    |> Ast.Manip.exp_conv_var Var.of_string
    |> Ast.Manip.expect_no_mem
    |> Typed.add_type ~ty_of_var:(Fun.const Var.ty)

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
    e |> Ast.Manip.allow_mem |> Ast.Manip.allow_lets |> Ast.assert_smt
    |> send_smt serv ~ppv:Var.pp

  let send_assert_decl serv ~declared (e : Exp.t) : unit =
    declare_vars serv ~declared e;
    send_assert serv e

  let check serv (e : Exp.t) : bool option =
    open_context serv "check";
    send_assert serv (Typed.not e);
    let res = is_context_sat serv |> Option.map not in
    close_context serv;
    res

  let check_full ?(hyps : Exp.t list = []) (e : Exp.t) : bool option =
    let serv = ensure_started_get () in
    CheckContext.openc ();
    let declared = Htbl.create 100 in
    List.iter (send_assert_decl ~declared serv) hyps;
    send_assert_decl ~declared serv (Typed.not e);
    let res = is_context_sat serv |> Option.map not in
    CheckContext.closec ();
    res

  let check_sat serv (e : Exp.t) : bool option =
    open_context serv "check_sat";
    send_assert serv e;
    let res = is_context_sat serv in
    close_context serv;
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

  
  let rec simplify_subterms serv (e : Exp.t) : Exp.t =
    e |> Ast.Manip.all_subterms
    |> List.find_opt (fun t ->
      Typed.get_type e = Typed.get_type t &&
      let result = check serv Typed.(e = t) in
      result = Some true
    )
    |> Option.map (simplify_subterms serv)
    |> Option.value_fun ~default:(fun () ->
      Ast.Manip.direct_exp_map_exp (simplify_subterms serv) e
    )
  
  let simplify_subterms_decl serv ~declared (e : Exp.t) : Exp.t =
    declare_vars serv ~declared e;
    simplify_subterms serv e

  let simplify_subterms_full ?(hyps = []) e =
    let serv = ensure_started_get () in
    SimpContext.openc ();
    let declared = Htbl.create 10 in
    List.iter (send_assert_decl ~declared serv) hyps;
    let res = simplify_subterms_decl ~declared serv e in
    SimpContext.closec ();
    res
end

module Test = struct
  module Var = struct
    include String
    
    let pp = Pp.string

    let ty _ = Ast.Ty_BitVec 64

    let of_string = Fun.id
  end

  module Typed = Exp.Typed
  module Exp = Exp.Make (Var)

  module Z3Test = Make (Var)

  let test () =
    let x = Typed.var ~typ:(Ast.Ty_BitVec 64) "x" in
    let bvint64 = Typed.bits_int ~size:64 in
    let constr = Typed.(binop (Ast.Bvcomp Ast.Bvult) x (bvint64 16)) in
    let exp = Typed.(concat [bits_int ~size:60 0; extract x ~first:0 ~last:3]) in
    let simplified = Z3Test.simplify_subterms_full ~hyps:[constr] exp in
    Printf.printf "original: %t\n" (Pp.top Exp.pp exp);
    Printf.printf "simplified: %t\n" (Pp.top Exp.pp simplified);

  
  open Cmdliner
  open Config.CommonOpt

  
  let term = Term.(CmdlinerHelper.func_options (config :: z3 :: comopts) test $ const ())

  let info =
    let doc = ""
    in
    Cmd.(info "z3-test" ~doc ~exits)
  
  let command = (term, info)
end
