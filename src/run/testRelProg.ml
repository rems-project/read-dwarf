open Cmdliner
open Config.CommonOpt

open Logs.Logger (struct
  let str = __MODULE__
end)

type err = { msg: string; asserts: State.Exp.t list }
type node_result = { all_fail: bool; errors: err list }
let node_result_of_result = function
| Ok () -> { all_fail=false; errors=[] }
| Error e -> { all_fail=true; errors=[e] }

let rec process_tree ~pc ~ret ~ext (node:Block_lib.label State.Tree.t) =
  let l = node.data in
  let st = node.state in

  let found_symread = ref false in
  st.read_vars |> Vec.iter Fun.(State.Tval.exp %> Ast.Manip.exp_iter_var (fun v ->
    match v with
    | State.Var.ReadVar _ -> found_symread := true; warn "State contains symbolic read variable:\n %t" (Pp.top State.Var.pp v)
    | _ -> ()
  ));
  if !found_symread then
    warn "State:\n%t" (Pp.top State.pp st);

  if not (State.is_possible st) then
    { all_fail=true; errors=[] }
  else match l with
  | Block_lib.End _ -> let result = (
      let pc_exp = State.get_reg_exp st pc in
      let pc_addr = try
        Some (State.Exp.expect_sym_address pc_exp)
      with
        _ -> None
      in
      let ret_exp = match pc_addr with
      | Some pc_addr ->
        if pc_addr = Elf.Address.{ section="UND.abort"; offset=0 } then
          Result.error {
            msg=Printf.sprintf "abort called from %t" (Pp.tos Elf.Address.pp st.last_pc);
            asserts=st.asserts;
          }
        else if pc_addr <> Elf.Address.{ section="UND.exit"; offset=0 } then
          Result.error {
            msg=Printf.sprintf "finished at weird address %t" (Pp.tos Elf.Address.pp pc_addr);
            asserts=st.asserts;
          }
        else
          Result.ok (State.get_reg_exp st ext)
      | None ->
        Result.ok (State.get_reg_exp st ret) (* Symbolic pc = returned from main *)
      in
      Result.bind ret_exp @@ fun ret_exp ->
      let ret_val = ret_exp |> Exp.ConcreteEval.eval |> Exp.Value.expect_bv |> BitVec.to_int in
      if ret_val <> 0 then
        Result.error {
          msg=Printf.sprintf "non-zero return code %d" ret_val;
          asserts=st.asserts
        }
      else
        Result.ok ()
    ) in
    node_result_of_result result
  | _ ->
    let results = List.map (process_tree ~pc ~ret ~ext) node.rest in
    let all_errors = List.bind results (fun x -> x.errors) in
    if List.for_all (fun x -> x.all_fail) results then {
      all_fail=true;
      errors=List.map (fun x -> {x with asserts=st.asserts}) all_errors;
    }
    else {
      all_fail=false;
      errors=all_errors;
    }

let test return_register exit_register name =
  let tree = Func.get_state_tree ~elf:name ~name:"main" ~init:(State.init_sections ~sp:Arch.sp ~addr_size:Arch.address_size) ~every_instruction:false ()
    ~breakpoints:["UND.abort"; "UND.exit"]
  in
  debug "%t" (Pp.top (State.Tree.pp_all Block_lib.pp_label) tree);
  let pc = Arch.pc () in
  let ret = State.Reg.of_string return_register in
  let ext = State.Reg.of_string exit_register in
  let results = process_tree ~pc ~ret ~ext tree in
  if List.is_empty results.errors then
    base "Success"
  else
    fail "Some paths fail: %t" Pp.(
      top (list (fun (e:err) -> !^(e.msg) ^^ !^" when " ^^ list State.Exp.pp e.asserts)) results.errors
    )

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let return_register =
  let doc = "The name of the register containing the return value of main function" in
  Arg.(value & opt string "R0" & info ["r"] ~docv:"RETURN_REGISTER" ~doc)

let exit_register =
  let doc = "The name of the register containing the argument to exit function" in
  Arg.(value & opt string "R0" & info ["e"] ~docv:"EXIT_REGISTER" ~doc)
  

let term =
  Term.(
    CmdlinerHelper.func_options comopts test
    $ return_register $ exit_register $ elf)

let info =
  let doc =
    "Test run relocatable file\
    
    Test succeeds if all possble outcomes result in the program exiting with\
    with code 0"
  in
  Cmd.(info "test-rel-prog" ~doc ~exits)

let command = (term, info)
