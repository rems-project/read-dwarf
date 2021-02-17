(* The documentation is in the mli file *)

type ('a, 'b) t = ('a, 'b WeakPtr.t) Hashtbl.t

let create size = Hashtbl.create size

exception Exists

let add tbl a b =
  if Hashtbl.mem tbl a then raise Exists
  else
    let ptr = WeakPtr.make b in
    Gc.finalise_last
      (fun () ->
        match (Hashtbl.find_opt tbl a : 'b WeakPtr.t option) with
        | Some p when p == ptr -> Hashtbl.remove tbl a
        | _ -> ())
      b;
    Hashtbl.replace tbl a ptr

let get tbl a =
  try a |> Hashtbl.find tbl |> WeakPtr.get
  with WeakPtr.Deleted ->
    assert (
      ignore "This should not happen";
      false
    );
    exit 1
