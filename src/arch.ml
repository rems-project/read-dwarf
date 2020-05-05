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

let pp_api (api : func_api) =
  let open PP in
  record "api"
    [
      ("args", api.args |> List.mapi (fun i t -> (int i, Ctype.pp t)) |> mapping "");
      ("ret", Option.fold ~none:!^"none" ~some:Ctype.pp api.ret);
    ]

(** Get the initialized architecture type. Fails ([Failure]) if not architecture was loaded *)
let get () = match initialized () with Some a -> a | None -> Raise.fail "No architecture loaded"

(** Get the configuration for the initialized architecture *)
let get_config () = ConfigFile.get_arch_config (get ())

(** Get the Isla configuration for the initialized architecture *)
let get_isla_config () = ConfigFile.get_isla_config (get ())

let load_elf_arch (elf : Elf.File.t) =
  match elf.machine with
  | Supp at -> ensure_loaded at
  | Other i -> Raise.fail "The ELF architecture %i is not supported by this program" i
