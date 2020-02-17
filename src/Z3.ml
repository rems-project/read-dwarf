(* TODO make that changable from CLI or environment *)

(** The Z3 executable *)
let z3 = ref "z3"

(** The call number for Z3 *)
let call_num = ref 0

(** Declare all the variables in exp in the (declare-const ...) format on ochannel *)
let declare_vars ochannel exp : unit =
  let declared = Hashtbl.create 10 in
  let process_var : State.var -> unit = function
    | Free _ -> failwith "Z3.declare_vars : free var"
    | State svar ->
        if not @@ Hashtbl.mem declared @@ State.Var.to_string svar then begin
          Isla.(
            let decl = DeclareConst (State svar, State.svar_type svar) in
            PPI.(fprint ochannel @@ pp_smt PPI.svar decl ^^ hardline));
          Hashtbl.add declared (State.Var.to_string svar) ()
        end
  in
  IslaManip.exp_iter_var process_var exp

(** Simplify exp using Z3 *)
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
    let rexp = Isla.parse_exp_string ~filename i in
    let nexp = IslaManip.exp_conv_svar State.Var.of_string rexp in
    nexp
  in
  Cmd.io [|!z3; "-in"|] output input

let check asserts = failwith "Z3.check unimplemented"

let _ =
  Tests.add_test "Z3" (fun () ->
      let output ochannel =
        Printf.fprintf ochannel "(display 42)\n";
        flush ochannel
      in
      let input ichannel = input_line ichannel in
      Cmd.io [|!z3; "-in"|] output input = "42")
