include Stdlib.Option

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Utility } *)

let take_first o o' = match o with Some _ -> o | None -> o'

let ( ||| ) = take_first

let take_first_list ol = List.fold_left ( ||| ) None ol

let take_all o o' = match (o, o') with (Some v, Some v') -> Some (v, v') | _ -> None

let ( &&& ) = take_all

let value_fail o fmt =
  match o with Some o -> Printf.ikfprintf (fun _ -> o) () fmt | None -> Raise.fail fmt

let value_fun o ~default:f = match o with Some o -> o | None -> f ()

let of_bool ~some b = if b then Some some else None

let of_bool_fun ~some b = if b then Some (some ()) else None

let for_all p = function None -> true | Some a -> p a

let exists p = function None -> false | Some a -> p a

let guard b v = if b then Some v else None

let guardn b v = if b then None else Some v

let ( let+ ) o f = map f o

let ( and+ ) = ( &&& )

let ( let+! ) o f = iter f o

let ( let* ) o b = bind o b

let ( and* ) = ( and+ )

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Lists } *)

let lift l =
  List.fold_right
    (fun e l ->
      let+ e = e and+ l = l in
      e :: l)
    l (Some [])

let map_lift f l =
  List.fold_right
    (fun e l ->
      let+ fe = f e and+ l = l in
      fe :: l)
    l (Some [])

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pairs } *)

(** Lift a pair of option to an option of pair *)
let lift_pair (a, b) =
  let+ a = a and+ b = b in
  (a, b)

(** Unlift an option of pair to a pair of option *)
let unlift_pair = function None -> (None, None) | Some (a, b) -> (Some a, Some b)
