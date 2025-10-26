val get_state_tree :
  elf:string ->
  name:string ->
  ?dump:bool ->
  ?entry:bool ->
  ?len:int ->
  ?breakpoints:string list ->
  ?loop:int ->
  ?tree_to_file:string ->
  ?init:(State.t -> State.t) ->
  ?every_instruction:bool ->
  unit ->
  Block_lib.label State.Tree.t

val command : unit Cmdliner.Term.t * Cmdliner.Cmd.info
