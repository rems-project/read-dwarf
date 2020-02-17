(** This module introduce a type to represent the state of the machine.
 *)

(*****************************************************************************)
(*        Support types                                                      *)
(*****************************************************************************)

type 'a vector = 'a Vector.t

(** This type is the standard annotation inside the state stucture.
    For now it is Isla.lrng, but it may change depending on our needs *)
type annot = Isla.lrng

let dummy_annot = Isla.Unknown

type ty = Isla.ty

(* TODO Think about that type *)
type memory = unit

let copy_mem () = ()

module Id = struct
  type t = int

  let to_string = string_of_int

  let of_string = int_of_string

  let pp id = id |> to_string |> PP.string
end

type id = Id.t

(*****************************************************************************)
(*        State types                                                        *)
(*****************************************************************************)

(** inner variable type *)
type ivar = Register of Reg.path  (** sid:REG.FIELD.INNER *) | Extra of int  (** sid:f42 *)

(** Represent the state of the machine.
    Should only be mutated for initialization purpose
    i.e you should only mutate it if you have just created it using make or copy*)
type t = {
  id : id;
  mutable regs : exp Reg.Map.t;
  extra_vars : (ty * exp) vector;
      (** Include extra variable that are not direct representation of registers or memory *)
  mutable asserts : exp list;
  memory : memory; (* We'll need to add C infered types later *)
}

and exp = (svar, annot) Isla.exp

and svar = { state : t; var : ivar }

(* Other isla aliases *)

type var = svar Isla.var

type event = (svar, annot) Isla.event

type trc = (svar, annot) Isla.trc

(*****************************************************************************)
(*        State to id management                                             *)
(*****************************************************************************)

(** Global map of states to associate them with identifiers *)
let id2state : (id, t) WeakMap.t = WeakMap.create 10

(** Next unused id *)
let next_id = ref 0

let of_id (id : id) = WeakMap.get id2state id

let to_id (st : t) = st.id

(*****************************************************************************)
(*        State variable management                                          *)
(*****************************************************************************)
module Var = struct
  type t = svar

  let to_string { state; var } =
    (state |> to_id |> Id.to_string)
    ^ ":"
    ^ match var with Register l -> Reg.path_to_string l | Extra i -> "f" ^ string_of_int i

  let of_string s =
    match String.split_on_char ':' s with
    | [state; var] ->
        {
          state = state |> Id.of_string |> of_id;
          var =
            ( match var.[0] with
            | 'f' -> Extra (int_of_string @@ String.sub var 1 (String.length var))
            | _ -> Register (Reg.path_of_string var)
            );
        }
    | _ -> raise (Invalid_argument ("Invalid state variable: " ^ s))

  let to_exp v = Isla.Var (Isla.State v, dummy_annot)

  let pp sv = sv |> to_string |> PP.string
end

(*****************************************************************************)
(*        State management                                                   *)
(*****************************************************************************)

(** Makes a fresh state with all variable fresh and new

    Remark: This create a state according to the current register layout in the Reg module
    ({!Reg.index}). If new registers are added afterwards, this state won't have them.
    Use {!copy_extend} to fix that.
*)
let make () =
  let id = !next_id in
  let state =
    { id; regs = Reg.Map.dummy (); extra_vars = Vector.empty (); asserts = []; memory = () }
  in
  (* Warning: here I'm creating a cyclic reference. Please be aware of that *)
  state.regs <- Reg.Map.init (fun p -> Var.to_exp { state; var = Register p });
  next_id := id + 1;
  WeakMap.add id2state id state;
  state

(** Make a state using the argument passed *)
let make_of regs extra_vars asserts memory =
  let id = !next_id in
  let state = { id; regs; extra_vars; asserts; memory } in
  next_id := id + 1;
  WeakMap.add id2state id state;
  state

(** Do a deep copy of all the mutable part of the state,
    so it can be mutated without retro-action.

    Remark: This create a new state with the same register layout as the input state
    It will not accommodate register discovered in between
    Use {!copy_extend} to add the new register that are missing
*)
let copy state =
  let id = !next_id in
  let nstate =
    {
      id;
      regs = Reg.Map.copy state.regs;
      extra_vars = Vector.empty ();
      asserts = state.asserts;
      memory = ();
    }
  in
  next_id := id + 1;
  WeakMap.add id2state id nstate;
  nstate

(** Do a deep copy of all the mutable part of the state,
    so it can be mutated without retro-action.
    All new register that are not present are added with fresh new variables
    as if created by {!make}
 *)
let copy_extend state =
  let id = !next_id in
  let nstate =
    {
      id;
      regs = Reg.Map.dummy ();
      extra_vars = Vector.empty ();
      asserts = state.asserts;
      memory = ();
    }
  in
  nstate.regs <-
    Reg.Map.copy_extend
      ~init:(fun p -> Var.to_exp { state = nstate; var = Register p })
      state.regs;
  next_id := id + 1;
  WeakMap.add id2state id nstate;
  nstate

(*****************************************************************************)
(*        State convenience manipulation                                     *)
(*****************************************************************************)

(** Add an assertion to a state *)
let push_assert (s : t) (e : exp) = s.asserts <- e :: s.asserts

(** Map a function on all the expression of a state and return a new state *)
let map_exp (f : exp -> exp) s =
  make_of (Reg.Map.map f s.regs)
    (Vector.map (fun (t, e) -> (t, f e)) s.extra_vars)
    (List.map f s.asserts) ()

(** Iter a function on all the expression of a state *)
let iter_exp (f : exp -> unit) s =
  Reg.Map.iter f s.regs;
  Vector.iter (fun (_, e) -> f e) s.extra_vars;
  List.iter f s.asserts

(** Iter a function on all the variables of a state *)
let iter_var (f : var -> unit) s = iter_exp (IslaManip.exp_iter_var f) s

(** Return the type of a state variable *)
let svar_type { state; var } =
  match var with
  | Register p -> p |> Reg.path_type |> Reg.assert_plain
  | Extra i -> Vector.get state.extra_vars i |> fst

(*****************************************************************************)
(*        Pretty printing                                                    *)
(*****************************************************************************)

let pp_sexp exp = Isla_lang.PP.pp_exp Var.pp exp

let pp_trc trc = Isla_lang.PP.pp_trc Var.pp trc

let pp s =
  PP.(
    !^"state"
    ^^ OCaml.record "state"
         [
           ("id", Id.pp s.id);
           ("regs", Reg.Map.pp pp_sexp s.regs);
           ("extra_vars", !^"todo");
           ( "asserts",
             s.asserts
             |> List.map (fun e -> prefix 2 1 !^"assert:" $ pp_sexp e)
             |> separate hardline );
           ("memory", !^"todo");
         ])
