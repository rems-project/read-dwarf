#!/usr/bin/env zsh

# not checked
./read-dwarf branch-tables ./hafnium-O2/hafnium.elf psci_handler
./read-dwarf branch-tables ./hafnium-O2/hafnium.elf spci_handler
./read-dwarf branch-tables ./hafnium-O2/hafnium.elf sync_lower_exception
./read-dwarf branch-tables ./hafnium-O2/hafnium.elf handle_system_register_access

# # correct 
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf vdlog

# set -euo pipefail
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf __stack_chk_fail
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf __stack_chk_guard
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf api_debug_log
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf api_page_pool
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf api_switch_to_primary
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf arch_mm_config
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf arch_mm_init.pa_bits_table
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf arch_regs_reset
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf begin_restoring_state
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf callstacks
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf complete_saving_state
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf cpio_get_file
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf cpu_count
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf cpu_main
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf cpu_message_buffer
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf cpu_off
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf cpus
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf deliver_msg
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf dlog
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf dlog_buffer_offset
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf dlog_lock_enabled
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf el3_psci_version
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf fdt_add_mem_reservation
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf fdt_find_child
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf fdt_next_sibling
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf fiq_current_exception_noreturn
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf fiq_lower
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf inject_el1_unknown_exception
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf irq_current_exception_noreturn
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf irq_lower
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf load_kernel
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf manifest_strerror
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf maybe_invalidate_tlb
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf memcpy
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf memcpy_s
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf memset_s
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_free_page_pte
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_map_level
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_ptable_defrag_entry
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_ptable_get_attrs_level
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_ptable_identity_map
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_s2_max_level
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_s2_root_table_count
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_stage2_invalidate
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mm_vm_get_mode
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mpool_fini
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf mpool_locks_enabled
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf one_time_init
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf one_time_init_mm
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf panic
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf plat_boot_flow_fdt_addr
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf ppool
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf print_string
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf ptable
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf ptable_buf
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf ptable_lock
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf serr_current_exception_noreturn
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf serr_lower
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf sl
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf smc32
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf spci_msg_check_transition.donate_transitions
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf spci_msg_check_transition.lend_transitions
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf spci_msg_check_transition.relinquish_transitions
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf spci_msg_check_transition.share_transitions
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf sync_current_exception_noreturn
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf tee_recv_buffer
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf tee_send_buffer
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf tee_vm
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf vcpu_handle_page_fault
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf vm_count
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf vm_init
# ./read-dwarf branch-tables ./hafnium-O2/hafnium.elf vms
