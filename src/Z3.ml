(* TODO make that changable from CLI or environment *)

(** The Z3 executable *)
let isla = ref "z3"

(** The call number for Z3 *)
let call_num = ref 0

let declare_vars ochannel exp : unit =
  let declared = Hashtbl.create 10 in
  let process_var : State.var -> unit = function
    | Free _ -> failwith "Z3.declare_vars : free var"
    | State svar ->
      if not @@ Hashtbl.mem declared @@ State.Var.to_string svar then
      Isla.(
        let decl = DeclareConst (State svar, State.svar_type svar) in
        PPI.(fprint ochannel @@ pp_def PPI.svar decl ^^ hardline)
      );
      Hashtbl.add declared (State.Var.to_string svar) ()
  in
  IslaManip.exp_iter_var process_var exp

let simplify (exp : State.exp) : State.exp =
  let filename = "Z3 output " ^ string_of_int !call_num in
  call_num := !call_num + 1;
  let output ochannel =
    declare_vars ochannel exp;
    PPI.(fprint ochannel @@ parens @@ !^"simplify " ^^ pp_exp svar exp ^^ !^"\n");
    close_out ochannel
  in
  let input ichannel =
    let i = Files.read_all ichannel in
    let rexp = Isla.parse_exp_string filename i in
    let nexp = IslaManip.exp_conv_var State.Var.of_string rexp in
    nexp
  in
  Cmd.io [|"z3"; "-in"|] output input

let check asserts = failwith "Z3.check unimplemented"
