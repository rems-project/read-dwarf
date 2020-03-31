(** This module is just an alias for {!Config.Arch}: The architecture module.

    It also add some code that is related to the Architecture specific modules but
    is in it itself architecture independant.
*)

include ArchSig
include Config.Arch

(** Ensure that the right architecture type is loaded *)
let ensure_loaded (at : Type.t) =
  match initialized () with
  | Some at' ->
      if at' != at then
        Raise.fail "ensure_loaded: required architecture is %s but loaded architecture is %s"
          (Type.to_string at) (Type.to_string at')
  | None -> init at
