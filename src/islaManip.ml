(** This module provide generic manipulation function of isla ast *)

open Isla

(*****************************************************************************)
(*        Trace Properties                                                   *)
(*****************************************************************************)

(** Check if a trace is linear (has no branches) *)
let is_linear (Trace events : ('v, 'a) trc) =
  let rec no_branch = function
    | [] -> true
    | Branch (_, _, _) :: _ -> false
    | _ :: l -> no_branch l
  in
  no_branch events

(*****************************************************************************)
(*        Get annotations                                                    *)
(*****************************************************************************)

(** Get the annotation of an expression *)
let annot_exp : ('v, 'a) exp -> 'a = function
  | Var (_, a) -> a
  | Bits (_, a) -> a
  | Bool (_, a) -> a
  | Unop (_, _, a) -> a
  | Binop (_, _, _, a) -> a
  | Manyop (_, _, a) -> a
  | Ite (_, _, _, a) -> a

(** Get the annotation of an event *)
let annot_event : ('v, 'a) event -> 'a = function
  | Smt (_, a) -> a
  | Branch (_, _, a) -> a
  | ReadReg (_, _, _, a) -> a
  | WriteReg (_, _, _, a) -> a
  | Cycle a -> a

(*****************************************************************************)
(*        Generic conversion                                                 *)
(*****************************************************************************)

(* All of those function convert the underlying state variable type through the AST *)

let var_conv_svar (conv : 'a -> 'b) : 'a var -> 'b var = function
  | State a -> State (conv a)
  | Free i -> Free i

let rec exp_conv_svar (conv : 'a -> 'b) : ('a, 'c) exp -> ('b, 'c) exp = function
  | Var (v, a) -> Var (var_conv_svar conv v, a)
  | Bits (bv, a) -> Bits (bv, a)
  | Bool (b, a) -> Bool (b, a)
  | Unop (u, e, a) -> Unop (u, exp_conv_svar conv e, a)
  | Binop (u, e, e', a) -> Binop (u, exp_conv_svar conv e, exp_conv_svar conv e', a)
  | Manyop (m, el, a) -> Manyop (m, List.map (exp_conv_svar conv) el, a)
  | Ite (c, e, e', a) -> Ite (exp_conv_svar conv c, exp_conv_svar conv e, exp_conv_svar conv e', a)

let smt_conv_svar (conv : 'a -> 'b) : ('a, 'c) smt -> ('b, 'c) smt = function
  | DeclareConst (v, t) -> DeclareConst (var_conv_svar conv v, t)
  | DefineConst (v, e) -> DefineConst (var_conv_svar conv v, exp_conv_svar conv e)
  | Assert e -> Assert (exp_conv_svar conv e)

let event_conv_svar (conv : 'a -> 'b) : ('a, 'c) event -> ('b, 'c) event = function
  | Smt (d, a) -> Smt (smt_conv_svar conv d, a)
  | Branch (i, s, a) -> Branch (i, s, a)
  | ReadReg (n, al, v, a) -> ReadReg (n, al, v, a)
  | WriteReg (n, al, v, a) -> WriteReg (n, al, v, a)
  | Cycle a -> Cycle a

let trace_conv_svar (conv : 'a -> 'b) (Trace l) = Trace (List.map (event_conv_svar conv) l)

(*****************************************************************************)
(*        Exp substitution                                                   *)
(*****************************************************************************)

(** Substitute variable with expression according to subst function *)
let rec var_subst (subst : 'v var -> 'a -> ('v, 'a) exp) (exp : ('v, 'a) exp) : ('v, 'a) exp =
  (* PPI.(println @@ !^"Calling vc_subst_full " ^^ sexp exp); *)
  let s = var_subst subst in
  match exp with
  | Var (v, a) -> subst v a
  | Manyop (m, el, a) -> Manyop (m, List.map s el, a)
  | Binop (b, e, e', a) -> Binop (b, s e, s e', a)
  | Unop (u, e, a) -> Unop (u, s e, a)
  | Ite (c, e, e', a) -> Ite (s c, s e, s e', a)
  | e -> e

(** iterate a function on all the variable of an expression *)
let rec exp_iter_var (f : 'v var -> unit) (exp : ('v, 'a) exp) : unit =
  let i = exp_iter_var f in
  match exp with
  | Var (v, a) -> f v
  | Manyop (b, el, a) -> List.iter i el
  | Binop (b, e, e', a) ->
      i e;
      i e'
  | Unop (u, e, a) -> i e
  | Ite (c, e, e', a) ->
      i c;
      i e;
      i e'
  | Bool (_, _) -> ()
  | Bits (_, _) -> ()

(*****************************************************************************)
(*        Accessor list conversion                                           *)
(*****************************************************************************)

let accessor_of_string s = Field s

let string_of_accessor (Field s) = s

let accessor_of_string_list = function [] -> Nil | l -> Cons (List.map accessor_of_string l)

let string_of_accessor_list = function Nil -> [] | Cons l -> List.map string_of_accessor l

(*****************************************************************************)
(*        valu string path access                                            *)
(*****************************************************************************)

(** Follow the path in a value like A.B.C in (struct (|B| (struct (|C| ...)))) *)
let rec valu_get valu path =
  match (valu, path) with
  | (_, []) -> valu
  | (Val_Struct s, a :: l) -> valu_get (List.assoc a s) l
  | _ -> failwith "islaManip.valu_get: Invalid path in IslaManip.valu_get"

(*****************************************************************************)
(*         bvi conversion                                                    *)
(*****************************************************************************)

(** Convert the bvi constant style like (_ bv42 6) to explicit style #b101010 *)
let bvi_to_bv (bvi : bvi) (size : int) =
  if bvi > 0 then
    if size mod 4 = 0 then
      let s = Printf.sprintf "%x" bvi in
      "#x" ^ String.make ((size / 4) - String.length s) '0' ^ s
    else failwith "TODO in IslaManip.bvi_to_bv"
  else begin
    assert (bvi = -1);
    if size mod 4 = 0 then "#x" ^ String.make (size / 4) 'F' else "#b" ^ String.make size '1'
  end

(*****************************************************************************)
(*        Isla Filtering                                                     *)
(*****************************************************************************)

(* TODO Make a configuration file and read that from the config *)

(** List of ignored register for the purposes of the semantics. *)
let ignored_regs = ["SEE"; "__unconditional"; "__v85_implemented"]

(** Split the trace between before and after the "cycle" event *)
let split_cycle : ('a, 'v) trc -> ('a, 'v) trc * ('a, 'v) trc = function
  | Trace l ->
      let rec split_cycle_list aux = function
        | [] -> (List.rev aux, [])
        | Cycle _ :: l -> (List.rev aux, l)
        | a :: l -> split_cycle_list (a :: aux) l
      in
      let (l1, l2) = split_cycle_list [] l in
      (Trace l1, Trace l2)

(** Remove everything before the "cycle" event, should be equivalent to snd (split_cycle l) *)
let remove_init : ('a, 'v) trc -> ('a, 'v) trc = function
  | Trace l ->
      let rec pop_until_cycle = function
        | [] -> []
        | Cycle _ :: l -> l
        (* | Smt (v, a) :: l -> Smt(v,a) :: (pop_until_cycle l) *)
        | a :: l -> pop_until_cycle l
      in
      Trace (pop_until_cycle l)

(** Remove all the events related to ignored registers (ignored_regs) *)
let remove_ignored : ('a, 'v) trc -> ('a, 'v) trc = function
  | Trace l ->
      Trace
        (List.filter
           (function
             | ReadReg (name, _, _, _) | WriteReg (name, _, _, _) ->
                 not @@ List.mem name ignored_regs
             | _ -> true)
           l)

(** Do the global filtering to get the main usable trace *)
let filter t = t |> remove_init |> remove_ignored
