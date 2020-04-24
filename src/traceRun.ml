open Logs.Logger (struct
  let str = "TraceRun"
end)

type context = {
  reg_writes : (Reg.path * State.tval) Vector.t;
  mem_reads : State.tval HashVector.t;
  state : State.t;
}

type tval = { ctyp : Ctype.t option; exp : Trace.exp }

(* (\** Exception that represent an Isla typing error *\)
 * exception TypeError of Ast.lrng * string
 *
 * (\* Registering and pretty printer for that exception *\)
 * let _ =
 *   Printexc.register_printer (function
 *     | TypeError (l, s) ->
 *         Some PP.(sprint @@ hardline ^^ prefix 2 1 (lrng l ^^ !^": ") (!^"TypeError: " ^^ !^s))
 *     | _ -> None)
 *
 * let type_error l fmt = Printf.ksprintf (fun s -> raise (TypeError (l, s))) fmt *)

let machine_of_size ?(update = 0) (typ : Ctype.t) : Ctype.t =
  let size = Ctype.sizeof typ in
  let constexpr = typ.constexpr in
  Ctype.machine ~constexpr (size + update)

let type_unop (u : Ast.unop) tval : Ctype.t option =
  let open Opt in
  let* typ = tval.ctyp in
  match u with
  | Not | Bvredand | Bvredor -> None
  | Bvneg | Bvnot -> machine_of_size typ |> some
  | Extract (b, a) ->
      if a = 0 && b = Arch.address_size - 1 && Ctype.is_ptr typ then tval.ctyp
      else
        let bitsize = b - a + 1 in
        let constexpr = typ.constexpr in
        if bitsize mod 8 = 0 then Ctype.machine ~constexpr (bitsize / 8) |> some else None
  | ZeroExtend m | SignExtend m ->
      if m mod 8 = 0 then machine_of_size ~update:(m / 8) typ |> some else None

let type_binop (b : Ast.no Ast.binop) (tval : tval) (tval' : tval) : Ctype.t option =
  let open Opt in
  let* typ = tval.ctyp and* typ' = tval'.ctyp in
  match b with
  | Eq | Neq | Bvcomp _ -> None
  | Bvarith Bvsub when Ctype.is_ptr typ ->
      if typ'.constexpr then
        let v' = AstManip.exp_bv_to_int tval'.exp in
        Ctype.ptr_update typ (-v') |> some
      else Ctype.ptr_forget typ |> some
  | Bvarith _ ->
      let constexpr = typ.constexpr && typ'.constexpr in
      Ctype.machine ~constexpr (Ctype.sizeof typ) |> some
  | Binmem b -> Ast.destr_binmem b

let type_manyop (m : Ast.manyop) (tvals : tval list) : Ctype.t option =
  let open Opt in
  match m with
  | Concat | And | Or -> None
  | Bvmanyarith Bvadd -> (
      let+ typs = map_lift (fun tval -> tval.ctyp) tvals in
      let (ptrs, rest) = List.partition Ctype.is_ptr typs in
      match ptrs with
      | [] ->
          let constexpr = List.for_all Ctype.is_constexpr typs in
          let size = Ctype.sizeof @@ List.hd typs in
          Ctype.machine ~constexpr size
      | [ptr] ->
          let constexpr = List.for_all Ctype.is_constexpr rest in
          if constexpr then
            let v =
              List.fold_left
                (fun v tval ->
                  let c = Option.get tval.ctyp in
                  if c.constexpr then v + AstManip.exp_bv_to_int tval.exp else v)
                0 tvals
            in
            Ctype.ptr_update ptr v
          else Ctype.ptr_forget ptr
      | _ ->
          warn "Multiple pointer in addition, returnining Machine";
          Ctype.machine Ctype.ptr_size
    )
  | Bvmanyarith _ ->
      let+ typs = map_lift (fun tval -> tval.ctyp) tvals in
      let constexpr = List.for_all Ctype.is_constexpr typs in
      let size = Ctype.sizeof @@ List.hd typs in
      Ctype.machine ~constexpr size

(** Stage 1 expression typer *)
let rec type_exp ~ctxt (exp : Trace.exp) : Ctype.t option =
  match exp with
  | Var (Register reg, l) -> State.get_reg ctxt.state reg |> State.get_ctyp
  | Var (Read r, l) -> HashVector.get ctxt.mem_reads r |> State.get_ctyp
  | Bits (bv, l) ->
      let size = AstManip.bv_size bv in
      if size mod 8 = 0 || size = Arch.address_size then
        Ctype.machine ~constexpr:true (size / 8) |> Opt.some
      else None
  | Bool (_, l) -> None
  | Enum (_, l) -> None
  | Unop (u, e, l) -> type_exp_tval ~ctxt e |> type_unop u
  | Binop (b, e, e', l) ->
      let te = type_exp_tval ~ctxt e in
      let te' = type_exp_tval ~ctxt e' in
      type_binop b te te'
  | Manyop (m, el, l) -> List.map (type_exp_tval ~ctxt) el |> type_manyop m
  | Ite (c, e, e', l) -> None
  | Bound _ -> .
  | Let _ -> .

and type_exp_tval ~ctxt exp = { exp; ctyp = type_exp ~ctxt exp }

let make_context state =
  let reg_writes = Vector.empty () in
  let mem_reads = HashVector.empty () in
  { state; reg_writes; mem_reads }

let expand_var ~(ctxt : context) (v : Trace.Var.t) (a : Ast.lrng) =
  match v with
  | Register reg -> State.get_reg ctxt.state reg |> State.get_exp
  | Read i -> (HashVector.get ctxt.mem_reads i).exp

let expand ~(ctxt : context) (exp : Trace.exp) : State.exp =
  AstManip.exp_var_subst (expand_var ~ctxt) exp

let expand_tval ?env ~(ctxt : context) (exp : Trace.exp) : State.tval =
  let sexp = expand ~ctxt exp in
  if env = None then { ctyp = None; exp = sexp }
  else
    let ctyp = type_exp ~ctxt exp in
    { ctyp; exp = sexp }

(* This is where type-inference will happen (soon) *)
let event_mut ?env ~(ctxt : context) (event : Trace.event) =
  debug "Running: %t with env:%b" (PP.top Trace.pp_event event) (env <> None);
  match event with
  | WriteReg { reg; value } -> Vector.add_one ctxt.reg_writes (reg, expand_tval ?env ~ctxt value)
  | ReadMem { addr; value; size } ->
      let mb : State.Mem.block = { addr = expand ~ctxt addr; size } in
      let tval =
        match env with
        | Some env ->
            let ptrtype = type_exp ~ctxt addr in
            State.typed_read ~env ctxt.state ?ptrtype mb
        | None -> State.read ctxt.state mb |> State.make_tval
      in
      HashVector.set ctxt.mem_reads value tval
  | WriteMem { addr; value; size } -> (
      let mb : State.Mem.block = { addr = expand ~ctxt addr; size } in
      match env with
      | Some env ->
          let ptrtype = type_exp ~ctxt addr in
          debug "Typed write mem with ptr:%t" (PP.top (PP.opt Ctype.pp) ptrtype);
          let value = expand_tval ~env ~ctxt value in
          State.typed_write ~env ctxt.state ?ptrtype mb value
      | None ->
          let value = expand ~ctxt value in
          State.write ctxt.state mb value
    )
  | Assert exp -> State.push_assert ctxt.state (expand ~ctxt exp)

let trace_mut ?env (state : State.t) (events : Trace.t) : unit =
  debug "running trace with env:%b" (env <> None);
  let ctxt = make_context state in
  List.iter (event_mut ?env ~ctxt) events;
  Vector.iter (fun (reg, tval) -> Reg.Map.set state.regs reg tval) ctxt.reg_writes

let trace ?env (start : State.t) (events : Trace.t) : State.t =
  let state = State.copy start in
  trace_mut ?env state events;
  State.lock state;
  state
