include Stdlib.Option

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Utility } *)

let take_first o o' = match o with Some v -> o | None -> o'

let ( ||| ) = take_first

let take_first_list ol = List.fold_left ( ||| ) None ol

let take_all o o' = match (o, o') with (Some v, Some v') -> Some (v, v') | _ -> None

let ( &&& ) = take_all

let value_fail o fmt =
  match o with Some o -> Printf.ikfprintf (fun _ -> o) () fmt | None -> Raise.fail fmt

let value_fun o ~default:f = match o with Some o -> o | None -> f ()

let of_bool ~some b = if b then Some some else None

let of_bool_fun ~some b = if b then Some (some ()) else None

let ( let+ ) o f = Option.map f o

let ( and+ ) = ( &&& )

let ( let+! ) o f = Option.iter f o

let ( let* ) o b = Option.bind o b

let ( and* ) = ( and+ )

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 folding } *)

let list_fold_left f i l =
  List.fold_left
    (fun ao e ->
      let* a = ao in
      f a e)
    (Some i) l
