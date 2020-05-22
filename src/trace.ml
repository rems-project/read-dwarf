(** This module defines a new simplified kind of trace to replace Isla traces in the later stages of the instruction processing.

    The traces are even simpler and more easily typable.
    The possible events are in the type {!event} and traces ({!t}) are just list of them.

    Compared to Isla, the concept of reading a register do not exist anymore.
    Instead, a register can be used as a variable in any expression.
    When a register appears in an expression, it represents the value of the register at
    the start of the trace even if that register was written to later.
    This is why trace are {i not} naively concatenable.

    Furthermore, branching do not exist either. Branching instruction are represented by a
    set of trace.

    The important functions are {!of_isla} to convert and Isla traces
    and {!simplify} for simplify traces.
*)

(** This module contains variable used in traces *)
module Var = struct
  (** A trace variable *)
  type t =
    | Register of Reg.path  (** The value of the register at the beginning of the trace *)
    | Read of int  (** The result of that memory reading operation *)

  (** Convert the variable to the string encoding. For parsing infractructure reason,
      the encoding must always contain at least one [:]. *)
  let to_string = function
    | Register r -> Printf.sprintf "reg:%s" (Reg.path_to_string r)
    | Read i -> Printf.sprintf "read:%d" i

  (** Inverse of {!to_string} *)
  let of_string s =
    match String.split_on_char ':' s with
    | ["reg"; reg] -> Register (Reg.path_of_string reg)
    | ["read"; num] -> Read (int_of_string num)
    | _ -> Raise.inv_arg "%s is not a Trace.Var.t" s

  (** Pretty prints the variable *)
  let pp v = v |> to_string |> PP.string
end

(** A trace expression. No let bindings, no memory operations *)
type exp = (Ast.lrng, Var.t, Ast.no, Ast.no) Ast.exp

type event =
  | WriteReg of { reg : Reg.path; value : exp }
  | ReadMem of { addr : exp; value : int; size : Ast.Size.t }
  | WriteMem of { addr : exp; value : exp; size : Ast.Size.t }
  | Assert of exp

type t = event list

(** Allow all feature in an expression *)
let exp_allow (e : exp) = e |> AstManip.allow_lets |> AstManip.allow_mem

(** Restrict the features of an expression to make it usable in this module again *)
let exp_restrict e = e |> AstManip.expect_no_mem

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

(** Pretty print an expression *)
let pp_exp e = Ast.pp_exp Var.pp (exp_allow e)

(** Pretty print an event *)
let pp_event =
  let open PP in
  function
  | WriteReg { reg; value } ->
      dprintf "Write |reg:%s| with " (Reg.path_to_string reg) ^^ nest 4 (pp_exp value)
  | ReadMem { addr; value; size } ->
      dprintf "Read |read:%d| of %dbits from " value (Ast.Size.to_bits size)
      ^^ nest 4 (pp_exp addr)
  | WriteMem { addr; value; size } ->
      dprintf "Write %dbits at " (Ast.Size.to_bits size)
      ^^ nest 4 (pp_exp addr ^^ !^" with " ^^ pp_exp value)
  | Assert exp -> !^"Assert " ^^ nest 4 (pp_exp exp)

(** Pretty print a trace *)
let pp events = PP.separate_map PP.hardline pp_event events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Isla to Trace conversion }

    This section perform the conversion from Isla trace to the
    traces of this module.

    The conversion is generrally obvious, however there is subtlety: If the Isla
    trace reads a register after having written it, then the read produce the written
    expression instead of just the symbolic value of that register. That why there is
    a [written_registers] parameter to some function of this section.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Throw an error in case of local conversion error.
    Normally a type-checked Isla trace should not fail in this section *)
exception OfIslaError

(** The context mapping Isla variable numbers to trace expression *)
type value_context = exp HashVector.t

(** Get the exression of the variable at the index.
    Throw {!OfIslaError} if the variable is not bound *)
let get_var vc i =
  match HashVector.get_opt vc i with
  | Some exp -> exp
  | None ->
      warn "Could not find the variable v%d" i;
      raise OfIslaError

(** Convert an Isla expression to a [Trace] expression by replacing all Isla variable
    by their value in the context. Throw {!OfIslaError} if the substitution fails *)
let exp_conv_subst (vc : value_context) (exp : Isla.rexp) : exp =
  let vconv i l = get_var vc i in
  IslaConv.exp_var_subst vconv exp

(** Convert an {!Isla.valu} in a expression *)
let exp_of_valu l vc : Isla.valu -> exp = function
  | Val_Symbolic i -> get_var vc i
  | Val_Bool b -> Bool (b, l)
  | Val_Bits bv -> Bits (BitVec.of_smt bv, l)
  | Val_I (int, size) -> Bits (BitVec.of_int ~size int, l)
  | Val_Enum (n, a) -> Enum ((n, a), l)
  | valu ->
      Raise.fail "%t Can't convert %t to a trace expression" (PP.tos PP.lrng l)
        (PP.tos Isla.pp_valu valu)

(** Write an expression to an {!Isla.valu} *)
let write_to_valu vc valu exp =
  match valu with Isla.Val_Symbolic i -> HashVector.set vc i exp | _ -> ()

let event_of_isla ~written_registers ~read_counter ~(vc : value_context) :
    Isla.revent -> event option = function
  | Smt (DeclareConst (i, e), l) -> None
  | Smt (DefineConst (i, e), l) ->
      (try HashVector.set vc i (exp_conv_subst vc e) with OfIslaError -> ());
      None
  | Smt (Assert e, l) -> Some (Assert (exp_conv_subst vc e))
  | ReadReg (name, al, valu, l) ->
      let string_path = IslaManip.string_of_accessor_list al in
      let valu = IslaManip.valu_get valu string_path in
      let path = Reg.path_of_string_list (name :: string_path) in
      ( match Hashtbl.find_opt written_registers path with
      | Some exp -> write_to_valu vc valu exp
      | None -> write_to_valu vc valu (Var (Register path, l))
      );
      None
  | WriteReg (name, al, valu, l) ->
      let string_path = IslaManip.string_of_accessor_list al in
      let valu = IslaManip.valu_get valu string_path in
      let path = Reg.path_of_string_list (name :: string_path) in
      let new_exp = exp_of_valu l vc valu in
      Hashtbl.add written_registers path new_exp;
      Some (WriteReg { reg = path; value = new_exp })
  | ReadMem (result, kind, addr, size, l) ->
      let addr = exp_of_valu l vc addr |> Pointer.to_ptr_size in
      let size = Ast.Size.of_bytes size in
      let value = Counter.get read_counter in
      write_to_valu vc result (Var (Read value, l));
      Some (ReadMem { addr; size; value })
  | WriteMem (success, kind, addr, data, size, l) ->
      let addr = exp_of_valu l vc addr |> Pointer.to_ptr_size in
      let size = Ast.Size.of_bytes size in
      let value = exp_of_valu l vc data in
      Some (WriteMem { addr; size; value })
  | Cycle _ -> None
  | Branch _ -> None
  | BranchAddress _ -> None
  | DefineEnum _ -> None

(** Top level function to convert an isla trace to one of this module *)
let of_isla (Trace events : Isla.rtrc) : t =
  let written_registers = Hashtbl.create 10 in
  let read_counter = Counter.make 0 in
  let vc = HashVector.empty () in
  List.filter_map (event_of_isla ~written_registers ~read_counter ~vc) events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Trace simplification } *)

(** A instance of {!Z3.ContextCounter}. *)
module TraceSimpContext = Z3.ContextCounter (struct
  let str = "Trace simplification"
end)

(** Simplify a trace by using Z3. Perform both local expression simplification and
    global assertion removal (when an assertion is always true) *)
let simplify events =
  let exp_simplify exp : exp =
    exp |> exp_allow |> Z3.simplify_gen ~ppv:Var.pp ~vofs:Var.of_string |> exp_restrict
  in
  let event_simplify = function
    | WriteReg wr -> Some (WriteReg { wr with value = exp_simplify wr.value })
    | ReadMem rm ->
        Z3.command ~ppv:Var.pp (Ast.DeclareConst (Read rm.value, Ast.Size.to_bv rm.size));
        Some (ReadMem { rm with addr = exp_simplify rm.addr })
    | WriteMem wm ->
        Some (WriteMem { wm with addr = exp_simplify wm.addr; value = exp_simplify wm.value })
    | Assert exp -> (
        let nexp = exp_simplify exp in
        match Z3.check_gen ~ppv:Var.pp (nexp |> exp_allow) with
        | Some true -> None
        | _ ->
            Z3.command ~ppv:Var.pp (Ast.Assert (nexp |> exp_allow));
            Some (Assert nexp)
      )
  in
  let declare_regs () =
    let serv = Z3.get_server () in
    Reg.iter_path (fun path ty ->
        Z3.send_smt serv ~ppv:Var.pp (DeclareConst (Register path, ty |> AstManip.ty_allow_mem)))
  in
  TraceSimpContext.openc ();
  declare_regs ();
  let events = List.filter_map event_simplify events in
  Z3.close_context ();
  events
