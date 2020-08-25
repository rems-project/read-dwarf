let elf = ref (None : string option)

let branch_table_data_file = ref (None : string option)

let objdump_d = ref (None : string option)

let elf2 = ref (None : string option)

let branch_table_data_file2 = ref (None : string option)

let objdump_d2 = ref (None : string option)

let qemu_log = ref (None : string option)

let comp_dir = ref (None : string option)

let cfg_dot_file = ref (None : string option)

let cfg_source_nodes = ref (None : string option)

let cfg_source_nodes2 = ref (None : string option)

let out_file = ref (None : string option)

let clip_binary = ref false

let show_vars = ref true

let show_cfa = ref true

let show_source = ref true

(*let ppmode = ref Types.Ascii*)

let ppmode = ref AnalyseTypes.Html

let src_target_dir = ref (None : string option)

let copy_sources_dry_run = ref false
