(* Semantic note:

   When a register variable is used in an expression, it means the value of this register
   at the beginning of the trace, even if there are write to that register at certain points.
*)

module Var = struct
  type t = Register of Reg.path | Read of int

  (** Convert the variable to the string encoding. For parsing infractructure reason,
      the encoding must always contain at least one [:]. *)
  let to_string = function
    | Register r -> Printf.sprintf "reg:%s" (Reg.path_to_string r)
    | Read i -> Printf.sprintf "read:%d" i

  let of_string s =
    match String.split_on_char ':' s with
    | ["reg"; reg] -> Register (Reg.path_of_string reg)
    | ["read"; num] -> Read (int_of_string num)
    | _ -> Raise.inv_arg "%s is not a Trace.Var.t" s

  let pp v = v |> to_string |> PP.string
end

type exp = (Ast.lrng, Var.t, Ast.no, Ast.no) Ast.exp

let exp_to_z3 (e : exp) = e |> AstManip.allow_lets |> AstManip.allow_mem

let exp_of_z3 e = e |> AstManip.expect_no_mem

let pp_exp e = Ast.pp_exp Var.pp (exp_to_z3 e)

(* Implicitely, ReadMem and WriteMem add an assertion about the address expression *)

type event =
  | WriteReg of { reg : Reg.path; value : exp }
  | ReadMem of { addr : exp; value : int; size : Ast.Size.t }
  | WriteMem of { addr : exp; value : exp; size : Ast.Size.t }
  | Assert of exp

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

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Isla to Trace conversion } *)

exception OfIslaError

type value_context = exp HashVector.t

type t = event list

let pp events = PP.separate_map PP.hardline pp_event events

let get_var vc i =
  match HashVector.get_opt vc i with Some exp -> exp | None -> raise OfIslaError

let exp_conv_subst (vc : value_context) (exp : Isla.rexp) : exp =
  let vconv i l = get_var vc i in
  IslaConv.exp_var_subst vconv exp

let exp_of_valu l vc : Isla.valu -> exp = function
  | Val_Symbolic i -> get_var vc i
  | Val_Bool b -> Bool (b, l)
  | Val_Bits bv -> Bits (bv, l)
  | Val_I (bvi, i) -> Bits (IslaManip.bvi_to_bv bvi i, l)
  | Val_Enum (n, a) -> Enum ((n, a), l)
  | valu ->
      Raise.fail "%t Can't convert %t to a trace expression" (PP.tos PP.lrng l)
        (PP.tos Isla.pp_valu valu)

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

let of_isla (Trace events : Isla.rtrc) =
  let written_registers = Hashtbl.create 10 in
  let read_counter = Counter.make 0 in
  let vc = HashVector.empty () in
  List.filter_map (event_of_isla ~written_registers ~read_counter ~vc) events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Trace simplification } *)

module TraceSimpContext = Z3.ContextCounter (struct
  let str = "Trace simplification"
end)

let simplify events =
  let exp_simplify exp : exp =
    exp |> exp_to_z3 |> Z3.simplify_full ~ppv:Var.pp ~vofs:Var.of_string |> exp_of_z3
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
        match Z3.check_full ~ppv:Var.pp (nexp |> exp_to_z3) with
        | Some true -> None
        | _ ->
            Z3.command ~ppv:Var.pp (Ast.Assert (nexp |> exp_to_z3));
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
