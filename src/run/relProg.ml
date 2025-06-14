open Cmdliner
open Config.CommonOpt

open Logs.Logger (struct
  let str = __MODULE__
end)

let rec pp_array pp sz dims value =
  match dims with
  | [] -> pp value
  | None::_ -> pp value
  | Some d :: dims ->
      let sz = sz / d in
      Seq.iota d
      |> Seq.map (fun x -> Exp.Typed.extract ~first:(8*x*sz) ~last:(8*(x+1)*sz-1) value)
      |> List.of_seq
      |> Pp.list (pp_array pp sz dims) 

let pp_typed ~(tenv: Ctype.env) ~(ctype: Ctype.t) ~(pp : ?hex:bool -> _ -> _) (value: State.Exp.t) =
  match ctype.unqualified with
  | Machine _ -> pp value
  | Cint _ -> pp value
  | Cbool -> pp value
  | Ptr _ -> pp ~hex:true value
  | Struct { id; _ } ->
      let s = IdMap.geti tenv.structs id in
      Pp.(
        Ctype.FieldMap.to_seq s.layout
        |> Seq.map (fun (offset, (field:Ctype.field)) -> (
          Option.value ~default:"?" field.fname,
          pp (Exp.Typed.extract ~first:(8*offset) ~last:(8*(offset + field.size)-1) value)
        ))
        |> List.of_seq
        |> record s.name
      )
  | Array { dims; _ } ->
      let sz = Ctype.sizeof ctype in
      pp_array pp sz dims value
  | Enum _ -> pp value
  | FuncPtr -> pp value
  | Missing -> pp value
  | Bits -> pp value

let read_big ~prov st addr sz =
  let addr = Exp.Typed.extract ~last:(Arch.address_size-1) ~first:0 addr in
  Seq.iota_step_up ~step:16 ~endi:sz
  |> Seq.map (fun off ->
    let addr = Exp.Typed.(addr + bits_int ~size:Arch.address_size off) in
    let len = min 16 (sz - off) in
    match prov with
    | None -> State.read_noprov st ~addr ~size:(Ast.Size.of_bytes len)
    | Some p -> State.read ~provenance:p st ~addr:addr ~size:(Ast.Size.of_bytes len)
  )
  |> List.of_seq
  |> Exp.Typed.concat

let eval_loc ?frame_value sz st (loc: Dw.Loc.t) : State.Exp.t option =
  match loc with
  | Register reg -> Some (State.get_reg_exp st reg)
  | RegisterOffset (reg, off) ->
      let r = State.get_reg st reg in
      let open Ctype in
      let prov = Option.bind r.ctyp (fun ctype ->
        match ctype.unqualified with
        | Ptr { provenance; _ } -> Some provenance
        | _ -> None
      ) in
      Some (read_big ~prov st Exp.Typed.(r.exp + bits_int ~size:64 off) sz)
  | StackFrame off -> 
    (* This is a bit hacky, should instead extract the provenance from frame_value *)
    let stack_provenance = Option.bind (State.get_reg st (Arch.sp())).ctyp (fun ctype ->
      match ctype.unqualified with
      | Ptr { provenance; _ } -> Some provenance
      | _ -> None
    ) in

    let open Option in
    let+ frame_value = frame_value in
    debug "Reading from %t" Pp.(top State.Exp.pp Exp.Typed.(frame_value + bits_int ~size:64 off));
    read_big ~prov:stack_provenance st Exp.Typed.(frame_value + bits_int ~size:64 off) sz
  | Global symoff -> 
      let addr = Elf.SymTable.to_addr_offset symoff in
      let addr = State.Exp.of_address ~size:Arch.address_size addr in
      Some (read_big ~prov:None st addr sz)
  | Const x ->
      Some (match x with
      | Absolute x -> x |> BitVec.of_z ~size:(8*sz) |> Exp.Typed.bits
      | Offset (s, o) -> State.Exp.of_address ~size:(8*sz) Elf.Address.{section=s; offset=Z.to_int o}
      )
  | Dwarf _ops -> None

let eval_loc_from_list ?frame_value sz st pc locs=
  let open Option in
  let+ loc = List.find_map (fun ((lo,hi), loc) -> (
    let open Elf.Address in
    let* hi = hi in
    let* over = lo <= pc in
    let* under = pc < hi in
    if over && under then
      Some loc
    else
      None
  )) locs in
  eval_loc ?frame_value sz st loc

let pp_variable_value  ~(tenv: Ctype.env) ~(ctype: Ctype.t) value =
  let pp ?(hex=false) = fun value ->
    match Exp.ConcreteEval.eval_if_concrete value with
    | Some(Exp.Value.Bv bv) when not hex -> Pp.int @@ BitVec.to_int bv
    | Some(value) -> Exp.Value.pp value
    | None -> State.Exp.pp value
  in
  Pp.optional (pp_typed ~tenv ~ctype ~pp) value

let printvars ~st ~(dwarf: Dw.t) pc =
  let out = ref "" in

  let st = State.copy_if_locked st in
  let pv vars =
    Seq.iter (fun (v: Dw.Var.t) -> 
      let sz = Ctype.sizeof v.ctype in
      let frame_value = eval_loc_from_list sz st pc v.locs_frame_base |> Option.join in
      debug "Frame value %t" Pp.(top (optional State.Exp.pp) frame_value);
      let value = eval_loc_from_list ?frame_value sz st pc v.locs in
      match value with
      | None -> ()
      | Some var_val -> out := !out ^ Printf.sprintf "%s = %t\n" v.name Pp.(tos (pp_variable_value ~ctype:v.ctype ~tenv:dwarf.tenv) var_val);
    )
    vars
  in
  pv (Hashtbl.to_seq_values dwarf.vars);
  Hashtbl.iter (fun _ (fn:Dw.Func.t) ->
    let rec pscope (scope:Dw.Func.scope) =
      pv (List.to_seq scope.vars);
      List.iter pscope scope.scopes
    in
    pscope fn.func.scope
  ) dwarf.funcs;
  !out


let run_prog elfname name objdump_d branchtables =
  match Analyse.Utils.read_file_lines "src/analyse/html-preamble-insts.html" with
  | Error _ -> ()
  | Ok lines -> Array.iter (function s -> Printf.printf "%s\n" s) lines
  ;
  base "Running with rd %s in %s" name elfname;
  base "Loading %s" elfname;
  let dwarf = Dw.of_file elfname in
  base "Loading %s for Analyse" elfname;
  let analyse_test = Analyse.Elf.parse_elf_file elfname in
  base "Analysing %s for Analyse" elfname;
  let analyse_analysis = Analyse.Collected.mk_analysis analyse_test objdump_d branchtables in
  let print_analyse_instruction pc =
    let pc = Elf.Address.to_sym pc in
    let index = analyse_analysis.index_of_address pc in
    let instr = analyse_analysis.instructions.(index) in
    Analyse.Pp.pp_instruction Analyse.Types.Html (*Ascii*) analyse_test analyse_analysis 0 index
      instr
  in
  base "Start running";
  let tree = Func.get_state_tree ~elf:elfname ~name ~init:(State.init_sections ~sp:Arch.sp ~addr_size:Arch.address_size) ~every_instruction:true ()
    ~breakpoints:["UND.abort"; "UND.exit"]
  in
  base "Ended running, start pretty printing";
  (* This table will contain the state diff to print at each pc with a message *)
  (* let instr_data : (Elf.Address.t, string * State.t * State.Reg.t list) Hashtbl.t =
    Hashtbl.create 100
  in
  let get_footprint pc =
    Runner.get_normal_opt runner pc |> Option.fold ~none:[] ~some:Trace.Instr.footprint
  in *)
  let rec iter (f:Block_lib.label State.Tree.t) =
    let st = f.state in
    let last_pc = st.last_pc in
    (match f.data with
    | Block_lib.Start -> ()
    | Block_lib.BranchAt pc | Block_lib.NormalAt pc -> 
        if Elf.Address.(last_pc + 4 <> pc) then
          Printf.printf "\nJUMP from %t:\n\n" Pp.(top Elf.Address.pp last_pc);
        print_string @@ Analyse.Pp.css Analyse.Types.Html Render_vars @@ printvars ~st ~dwarf pc;
        print_string (print_analyse_instruction pc);
    | Block_lib.End _ -> 
        print_string "END";
      );
    let succ = List.filter (fun (s:Block_lib.label State.Tree.t) ->
      State.is_possible s.state
    ) f.rest in
    if List.length succ > 1 then
      print_string "BRANCH!";
    List.iter iter succ
  in
  iter tree;
  match Analyse.Utils.read_file_lines "src/analyse/html-postamble.html" with
  | Error _ -> ()
  | Ok lines -> Array.iter (function s -> Printf.printf "%s\n" s) lines


let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(value & pos 1 string "main" & info [] ~docv:"FUNCTION" ~doc)

let objdump_d =
  let doc = "File containing result of objdump -d" in
  Arg.(required & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_table =
  let doc = "File containing branch table base addresses and sizes" in
  Arg.(
    (* required *)
    value & opt (some non_dir_file) None & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let term =
  Term.(
    CmdlinerHelper.func_options comopts run_prog
    $ elf $ func $ objdump_d $ branch_table)

let info =
  let doc =
    "Run main of relocatable file"
  in
  Cmd.(info "run-rel-prog" ~doc ~exits)

let command = (term, info)
        