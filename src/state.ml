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

  module PP = struct
    let sid id = id |> to_string |> PP.string
  end
end

type id = Id.t

(*****************************************************************************)
(*        State types                                                        *)
(*****************************************************************************)

(** inner variable type *)
type ivar = Register of Reg.path  (** sid:REG.FIELD.INNER *) | Extra of int  (** sid:f42 *)

(** Represent the state of the machine.
    Should only be mutated when running a trace on a newly instantiated state *)
type t = {
  id : id;
  mutable regs : exp Reg.Map.t;
  extra_vars : (ty * exp) vector;
      (** Include extra variable that are not direct representation of registers or memory *)
  mutable asserts : exp list;
  memory : memory; (* We'll need to add C infered types later *)
}

and exp = (var, annot) Isla.exp

and var = { state : t; var : ivar }

(* Other isla aliases *)

type svar = (var) Isla.var

type event = (var, annot) Isla.event

type trc = (var, annot) Isla.trc

type term = (var, annot) Isla.term

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
  type t = var

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

  module PP = struct
    let svar sv = sv |> to_string |> PP.string

    (* let svar sv = match sv.var with
     *   | Register l -> PP.list PP.int l
     *   | _ -> PP.empty *)
  end
end

(*****************************************************************************)
(*        State management                                                   *)
(*****************************************************************************)

(** Makes a fresh state with all variable fresh and new *)
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

(** Do a deep copy of all the mutable part of the state,
    so it can be mutated without retroaction *)
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

(*****************************************************************************)
(*        State convenience manipulation                                     *)
(*****************************************************************************)

(** The type of a mutable register cell *)
type reg_cell = exp Reg.Map.cell ArrayCell.t

(** Add an assertion to a state *)
let push_assert (s : t) (e : exp) = s.asserts <- e :: s.asserts

(*****************************************************************************)
(*        Pretty printing                                                    *)
(*****************************************************************************)

module PP = struct
  open PP

  let sexp exp = Isla_lang.PP.pp_exp Var.PP.svar exp

  let state s =
    !^"state"
    ^^ OCaml.record "state"
         [
           ("id", Id.PP.sid s.id);
           ("regs", Reg.Map.PP.rmap sexp s.regs);
           ("extra_vars", !^"todo");
           ("asserts", !^"todo");
           ("asserts_ref", !^"todo");
           ("memory", !^"todo");
         ]
end
