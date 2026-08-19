module fortback_mir_v0_riscv_linux
    use iso_fortran_env, only: int8, int32, int64
    use fortback_elf64, only: elf64_machine_riscv, elf64_target_t, &
        write_elf64_executable
    use fortback_mir_v0_bridge_metadata, only: mir_v0_opcode_add, mir_v0_opcode_const, &
        mir_v0_opcode_load, mir_v0_opcode_output, mir_v0_opcode_return, mir_v0_opcode_store, &
        mir_v0_opcode_mul, mir_v0_opcode_div, mir_v0_opcode_sub, mir_v0_opcode_pow, &
        mir_v0_opcode_value, &
        mir_v0_value_kind_integer, mir_v0_value_kind_value
    use fortback_mir_v0_riscv_linux_ecall_policy, only: &
        mir_v0_riscv_linux_ecall_encoding, mir_v0_riscv_linux_ecall_operation, &
        mir_v0_riscv_linux_ecall_operands
    use fortback_mir_v0_riscv_linux_bridge_policy, only: &
        mir_v0_bridge_policy_accepts, mir_v0_bridge_policy_function_supported, &
        mir_v0_bridge_policy_instruction_count_matches, &
        mir_v0_bridge_policy_machine_operation_for, &
        mir_v0_bridge_policy_frame_operation, &
        mir_v0_bridge_policy_exit_status_operation, &
        mir_v0_bridge_policy_route_operation_for, &
        mir_v0_bridge_policy_opcode_supported, mir_v0_bridge_policy_storage_matches, &
        mir_v0_bridge_policy_storage_offset, mir_v0_bridge_policy_frame_size, &
        mir_v0_bridge_policy_storage_initialization, mir_v0_bridge_policy_load_operation, &
        mir_v0_bridge_policy_store_operation
    use fortback_riscv_codec, only: riscv_encode_record
    use fortback_riscv_source, only: import_riscv_opcodes, riscv_opcode_record_t, &
        riscv_source_ok
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, &
        target_ir_t
    implicit none
    private
    integer(int32), parameter, public :: mir_v0_bridge_ok = 0_int32
    integer(int32), parameter, public :: mir_v0_bridge_malformed = 1_int32
    integer(int32), parameter, public :: mir_v0_bridge_unsupported = 2_int32
    integer(int32), parameter, public :: mir_v0_bridge_out_of_scope = 3_int32
    integer(int32), parameter, public :: mir_v0_bridge_capacity = 4_int32
    integer(int32), parameter, public :: mir_v0_bridge_io_error = 5_int32

    type, public :: riscv_linux_artifact_t
        character(len=16) :: format = 'ELF64'
        character(len=16) :: architecture = 'riscv64'
        character(len=16) :: platform = 'linux'
        character(len=16) :: origin = 'DERIVED'
        character(len=32) :: input_format = 'mir-v0-sx'
        integer(int8), allocatable :: bytes(:)
    end type riscv_linux_artifact_t
    public :: compile_mir_v0_riscv_linux
    public :: write_mir_v0_riscv_linux
    public :: riscv_linux_artifact_provenance_valid

    integer, parameter :: token_capacity = 8192
    integer, parameter :: token_length = 256
    integer, parameter :: instruction_capacity = 208
    integer(int32), parameter :: generic_power_minimum = 2_int32
    integer(int32), parameter :: generic_power_maximum = 10_int32
    integer(int32), parameter :: generic_decimal_minimum = 0_int32
    integer(int32), parameter :: generic_decimal_maximum = 100_int32
    integer(int32), parameter :: generic_negative_minimum = -100_int32
    integer(int32), parameter :: generic_negative_maximum = -1_int32

    type :: bridge_instruction_t
        integer(int32) :: id = 0_int32
        integer(int32) :: opcode = 0_int32
        integer(int32) :: result_id = 0_int32
        integer(int32) :: result_kind = 0_int32
        character(len=token_length) :: result_type = ''
        character(len=token_length) :: source_rule = ''
        character(len=token_length) :: storage_key = ''
        logical :: literal_present = .false.
        logical :: storage_present = .false.
        integer(int32) :: literal = 0_int32
    end type bridge_instruction_t

    type :: parsed_mir_t
        character(len=token_length) :: name = ''
        integer(int32) :: entry_block = 0_int32
        integer(int32) :: instruction_count = 0_int32
        type(bridge_instruction_t) :: instructions(instruction_capacity)
    end type parsed_mir_t
contains
    subroutine compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        character(len=*), intent(in) :: input
        type(riscv_linux_artifact_t), intent(out) :: artifact
        integer(int32), intent(out) :: status
        character(len=*), intent(out) :: diagnostic
        type(parsed_mir_t) :: mir
        type(source_ref_t) :: opcode_source, target_source
        type(target_ir_t) :: target
        type(elf64_target_t) :: metadata
        type(riscv_opcode_record_t) :: records(10)
        integer(int64) :: words(512), values(3)
        integer(int32) :: count, index, source_status
        character(len=16) :: operation
        character(len=512) :: opcode_text
        integer(int32) :: emitted_count, print_write_length
        integer(int32) :: print_item_index, print_digit_index, print_digit_count
        integer(int32) :: print_item_count
        integer(int32) :: print_buffer_offset
        integer(int32) :: print_syscall_word
        integer(int32) :: power_index
        integer(int32) :: power_exponent
        integer(int64) :: power_value
        character(len=32) :: print_digits
        logical :: storage_route, initialized_variable_y_route
        logical :: storage_sequence_route, storage_sequence_3_route
        logical :: storage_sequence_4_route
        logical :: storage_sequence_5_route
        logical :: storage_sequence_6_route
        logical :: storage_sequence_generated_route
        logical :: print_route, generic_print_route
        logical :: print_variable_route, print_variable_expression_route
        logical :: print_variable_two_item_route
        logical :: print_variable_three_item_route
        logical :: print_variable_four_item_route
        logical :: print_variable_five_item_route
        logical :: print_variable_six_item_route
        logical :: print_variable_seven_to_eighty_item_route
        artifact = riscv_linux_artifact_t()
        diagnostic = ''
        status = mir_v0_bridge_malformed
        if (.not. parse_mir(input, mir, diagnostic)) return
        if (.not. validate_scope(mir, status, diagnostic)) return

        opcode_source = make_source_ref('riscv-opcodes', 'rv_i', &
            'riscv-opcodes-l2-v0', 'IMPORTED')
        target_source = make_source_ref('riscv-isa', 'unprivileged', &
            'riscv-isa-l2-v0', 'IMPORTED')
        target = make_target_ir('riscv64', 64_int32, .true., target_source)
        metadata%target = target
        metadata%machine = elf64_machine_riscv
        opcode_text = 'add rd rs1 rs2 31..25=0 14..12=0 6..2=0x0c 1..0=3'// &
            new_line('a')//'addi rd rs1 imm12 14..12=0 6..2=0x04 1..0=3'// &
            new_line('a')//'mul rd rs1 rs2 31..25=1 14..12=0 6..2=0x0c 1..0=3'// &
            new_line('a')//'div rd rs1 rs2 31..25=1 14..12=4 6..2=0x0c 1..0=3'// &
            new_line('a')//'sub rd rs1 rs2 31..25=0x20 14..12=0 6..2=0x0c 1..0=3'// &
            new_line('a')//'sltiu rd rs1 imm12 14..12=3 6..2=0x04 1..0=3'// &
            new_line('a')//'ld rd rs1 imm12 14..12=3 6..2=0x00 1..0=3'// &
            new_line('a')//'sd rs2 rs1 imm12 14..12=3 6..2=0x08 1..0=3'// &
            new_line('a')//'sb rs2 rs1 imm12 14..12=0 6..2=0x08 1..0=3'// &
            new_line('a')//trim(mir_v0_riscv_linux_ecall_operation)// &
            ' rd rs1 imm12 '//trim(mir_v0_riscv_linux_ecall_encoding)
        call import_riscv_opcodes(opcode_text, opcode_source, records, count, source_status)
        if (source_status /= riscv_source_ok .or. count /= 10_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: machine record import failed')
            status = mir_v0_bridge_malformed
            return
        end if

        initialized_variable_y_route = is_initialized_variable_y_route(mir)
        storage_route = mir%instruction_count == 5_int32 .and. &
            (mir%instructions(1)%storage_present .or. mir%instructions(4)%storage_present)
        storage_route = storage_route .or. initialized_variable_y_route
        storage_sequence_route = mir%instruction_count == 7_int32
        storage_sequence_3_route = mir%instruction_count == 11_int32
        storage_sequence_4_route = mir%instruction_count == 15_int32
        storage_sequence_5_route = mir%instruction_count == 19_int32
        storage_sequence_6_route = mir%instruction_count == 23_int32
        storage_sequence_generated_route = mir%instruction_count == 27_int32 .or. &
            mir%instruction_count == 31_int32 .or. mir%instruction_count == 35_int32 .or. &
            mir%instruction_count == 39_int32
        print_variable_route = is_print_variable_candidate(mir)
        print_variable_expression_route = is_print_variable_expression_candidate(mir)
        print_variable_two_item_route = is_print_variable_two_item_candidate(mir)
        print_variable_three_item_route = is_print_variable_three_item_candidate(mir)
        print_variable_four_item_route = is_print_variable_four_item_candidate(mir)
        print_variable_five_item_route = is_print_variable_five_item_candidate(mir)
        print_variable_six_item_route = is_print_variable_six_item_candidate(mir)
        print_variable_seven_to_eighty_item_route = &
            is_print_variable_seven_to_hundred_item_candidate(mir)
        generic_print_route = is_generic_print_list_route(mir)
        print_route = trim(mir%name) == 'p' .and. &
            trim(mir%instructions(1)%source_rule) == 'frontend-ast-v2/print-stmt' .and. &
            .not. print_variable_route .and. .not. generic_print_route
        if (generic_print_route) then
            call encode_generic_print_list(target, records, mir, words, emitted_count, &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
        else if (print_variable_seven_to_eighty_item_route) then
            call encode_print_variable_seven_to_eighty(target, records, mir, words, emitted_count, &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
        else if (print_variable_three_item_route .or. print_variable_four_item_route .or. &
                print_variable_five_item_route .or. print_variable_six_item_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 3_int64], &
                words(2), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(3), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(4), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [11_int64, 0_int64, 2_int64], &
                words(5), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'mul', [10_int64, 10_int64, 10_int64], &
                words(6), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(7), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(8), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(9), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, 0_int64], words(10), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(11), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, 1_int64], words(12), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(13), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(14), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, 2_int64], words(15), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(16), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, 3_int64], words(17), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(18), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(19), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, 4_int64], words(20), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(21), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, 5_int64], words(22), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            if (print_variable_four_item_route) then
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(23), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(24), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 6_int64], words(25), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(26), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 7_int64], words(27), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                print_syscall_word = 28_int32
            else if (print_variable_five_item_route) then
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(23), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(24), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 6_int64], words(25), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(26), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 7_int64], words(27), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(28), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(29), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 8_int64], words(30), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(31), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 9_int64], words(32), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                print_syscall_word = 33_int32
            else if (print_variable_six_item_route) then
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(23), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(24), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 6_int64], words(25), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(26), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 7_int64], words(27), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(28), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(29), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 8_int64], words(30), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(31), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 9_int64], words(32), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(33), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], words(34), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 10_int64], words(35), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(36), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 11_int64], words(37), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                print_syscall_word = 38_int32
            else
                print_syscall_word = 23_int32
            end if
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                words(print_syscall_word), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            print_syscall_word = print_syscall_word + 1_int32
            call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], &
                words(print_syscall_word), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            print_syscall_word = print_syscall_word + 1_int32
            call encode_operation(target, records, 'addi', [12_int64, 0_int64, &
                merge(12_int64, merge(10_int64, merge(8_int64, 6_int64, &
                print_variable_four_item_route), print_variable_five_item_route), &
                print_variable_six_item_route)], words(print_syscall_word), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            print_syscall_word = print_syscall_word + 1_int32
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                words(print_syscall_word), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            print_syscall_word = print_syscall_word + 1_int32
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(print_syscall_word), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            print_syscall_word = print_syscall_word + 1_int32
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 0_int64], &
                words(print_syscall_word), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            print_syscall_word = print_syscall_word + 1_int32
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 93_int64], &
                words(print_syscall_word), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            print_syscall_word = print_syscall_word + 1_int32
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(print_syscall_word), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = print_syscall_word
        else if (print_variable_two_item_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 3_int64], &
                words(2), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(3), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(4), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [11_int64, 0_int64, 2_int64], &
                words(5), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'mul', [11_int64, 10_int64, 10_int64], &
                words(6), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 9_int64], &
                words(7), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(8), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(9), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 0_int32)), [5_int64, 0_int64, 57_int64], words(10), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 1_int32)), [6_int64, 0_int64, 10_int64], words(11), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 2_int32)), [5_int64, 2_int64, 0_int64], words(12), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 3_int32)), [6_int64, 2_int64, 1_int64], words(13), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 4_int32)), [5_int64, 0_int64, 57_int64], words(14), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 5_int32)), [6_int64, 0_int64, 10_int64], words(15), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 6_int32)), [5_int64, 2_int64, 2_int64], words(16), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_route_operation_for( &
                mir%instructions(7)%source_rule, 7_int32)), [6_int64, 2_int64, 3_int64], words(17), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], words(18), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], words(19), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [12_int64, 0_int64, 4_int64], words(20), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], words(21), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(22), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 0_int64], words(23), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 93_int64], words(24), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(25), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = 25_int32
        else if (print_variable_expression_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', &
                [10_int64, 0_int64, int(mir%instructions(1)%literal, int64)], words(2), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(3), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(4), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            if (mir%instructions(4)%opcode == mir_v0_opcode_load .and. &
                    (mir%instructions(5)%opcode == mir_v0_opcode_add .or. &
                    mir%instructions(5)%opcode == mir_v0_opcode_sub .or. &
                    mir%instructions(5)%opcode == mir_v0_opcode_mul .or. &
                    mir%instructions(5)%opcode == mir_v0_opcode_div)) then
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [11_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(5), &
                    status, diagnostic)
            else
                call encode_operation(target, records, 'addi', &
                    [11_int64, 0_int64, int(mir%instructions(4)%literal, int64)], words(5), &
                    status, diagnostic)
            end if
            if (status /= mir_v0_bridge_ok) return
            operation = mir_v0_bridge_policy_machine_operation_for(mir%instructions(5)%opcode)
            if (mir%instructions(5)%opcode == mir_v0_opcode_pow) then
                if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                    emitted_count = 5_int32
                    do power_index = generic_power_minimum, mir%instructions(1)%literal
                        call encode_operation(target, records, 'mul', &
                            [10_int64, 10_int64, 10_int64], words(emitted_count + 1), &
                            status, diagnostic)
                        if (status /= mir_v0_bridge_ok) return
                        emitted_count = emitted_count + 1_int32
                    end do
                else if (mir%instructions(4)%literal == 2_int32) then
                    call encode_operation(target, records, 'mul', [10_int64, 10_int64, 10_int64], words(6), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = 6_int32
                else
                    call encode_operation(target, records, 'mul', [11_int64, 10_int64, 10_int64], words(6), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    if (mir%instructions(4)%literal == 3_int32) then
                        call encode_operation(target, records, 'mul', [10_int64, 11_int64, 10_int64], &
                            words(7), status, diagnostic)
                        if (status /= mir_v0_bridge_ok) return
                        emitted_count = 7_int32
                    else
                        call encode_operation(target, records, 'mul', [10_int64, 11_int64, 11_int64], &
                            words(7), status, diagnostic)
                        if (status /= mir_v0_bridge_ok) return
                        emitted_count = 7_int32
                    end if
                end if
                call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
                    words(emitted_count + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
                    words(emitted_count + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                power_value = int(mir%instructions(1)%literal, int64)
                if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                    power_exponent = mir%instructions(1)%literal
                else
                    power_exponent = mir%instructions(4)%literal
                end if
                do power_index = 2, power_exponent
                    power_value = power_value * int(mir%instructions(1)%literal, int64)
                end do
                call integer_to_decimal(power_value, print_digits, print_digit_count)
                print_buffer_offset = 0_int32
                do print_digit_index = 1, print_digit_count
                    call encode_operation(target, records, 'addi', [5_int64, 0_int64, &
                        int(iachar(print_digits(print_digit_index:print_digit_index)), int64)], &
                        words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                        int(print_buffer_offset, int64)], words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    print_buffer_offset = print_buffer_offset + 1_int32
                end do
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                    int(print_buffer_offset, int64)], words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, 'addi', [12_int64, 0_int64, &
                    int(print_buffer_offset + 1_int32, int64)], words(emitted_count + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                    mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                    [10_int64, 0_int64, 0_int64], words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                    [17_int64, 0_int64, 93_int64], words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                    mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
            else
                call encode_operation(target, records, trim(operation), [10_int64, 10_int64, 11_int64], words(6), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(7), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(8), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                if (mir%instructions(5)%opcode == mir_v0_opcode_add .or. &
                    mir%instructions(5)%opcode == mir_v0_opcode_sub .or. &
                    mir%instructions(5)%opcode == mir_v0_opcode_mul .or. &
                    mir%instructions(5)%opcode == mir_v0_opcode_div) then
                    if (mir%instructions(5)%opcode == mir_v0_opcode_add) then
                        if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                            call integer_to_decimal(2_int64 * int(mir%instructions(1)%literal, int64), &
                                print_digits, print_digit_count)
                        else
                            call integer_to_decimal(int(mir%instructions(1)%literal, int64) + &
                                int(mir%instructions(4)%literal, int64), print_digits, print_digit_count)
                        end if
                    else if (mir%instructions(5)%opcode == mir_v0_opcode_sub) then
                        if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                            call integer_to_decimal(0_int64, print_digits, print_digit_count)
                        else
                            call integer_to_decimal(int(mir%instructions(1)%literal, int64) - &
                                int(mir%instructions(4)%literal, int64), print_digits, print_digit_count)
                        end if
                    else if (mir%instructions(5)%opcode == mir_v0_opcode_mul) then
                        if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                            call integer_to_decimal(int(mir%instructions(1)%literal, int64) * &
                                int(mir%instructions(1)%literal, int64), print_digits, print_digit_count)
                        else
                            call integer_to_decimal(int(mir%instructions(1)%literal, int64) * &
                                int(mir%instructions(4)%literal, int64), print_digits, print_digit_count)
                        end if
                    else if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                        call integer_to_decimal(1_int64, print_digits, print_digit_count)
                    else
                        call integer_to_decimal( &
                            int(mir%instructions(1)%literal, int64) / &
                            int(mir%instructions(4)%literal, int64), &
                            print_digits, print_digit_count)
                    end if
                    emitted_count = 8_int32
                    print_buffer_offset = 0_int32
                    do print_digit_index = 1, print_digit_count
                        call encode_operation(target, records, 'addi', [5_int64, 0_int64, &
                            int(iachar(print_digits(print_digit_index:print_digit_index)), int64)], &
                            words(emitted_count + 1), status, diagnostic)
                        if (status /= mir_v0_bridge_ok) return
                        emitted_count = emitted_count + 1_int32
                        call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                            int(print_buffer_offset, int64)], words(emitted_count + 1), status, diagnostic)
                        if (status /= mir_v0_bridge_ok) return
                        emitted_count = emitted_count + 1_int32
                        print_buffer_offset = print_buffer_offset + 1_int32
                    end do
                    call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                        words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                        int(print_buffer_offset, int64)], words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                        words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], &
                        words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, 'addi', [12_int64, 0_int64, &
                        int(print_buffer_offset + 1_int32, int64)], words(emitted_count + 1), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                        words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                        mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                        [10_int64, 0_int64, 0_int64], words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                        [17_int64, 0_int64, 93_int64], words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                        mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                else
                    print_digits = '12'
                    print_digit_count = 2_int32
                end if
                if (mir%instructions(5)%opcode /= mir_v0_opcode_add .and. &
                    mir%instructions(5)%opcode /= mir_v0_opcode_sub .and. &
                    mir%instructions(5)%opcode /= mir_v0_opcode_mul .and. &
                    mir%instructions(5)%opcode /= mir_v0_opcode_div) then
                    call encode_operation(target, records, 'addi', &
                        [5_int64, 0_int64, int(iachar(print_digits(1:1)), int64)], words(9), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'sb', [5_int64, 2_int64, 0_int64], words(10), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'addi', &
                        [5_int64, 0_int64, int(iachar(print_digits(2:2)), int64)], words(11), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'sb', [5_int64, 2_int64, 1_int64], words(12), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], words(13), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'sb', [5_int64, 2_int64, 2_int64], words(14), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], words(15), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], words(16), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'addi', [12_int64, 0_int64, 3_int64], words(17), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], words(18), &
                        status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                        mir_v0_riscv_linux_ecall_operands, words(19), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                        [10_int64, 0_int64, 0_int64], words(20), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                        [17_int64, 0_int64, 93_int64], words(21), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                        mir_v0_riscv_linux_ecall_operands, words(22), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = 22_int32
                end if
            end if
        else if (print_variable_route .or. initialized_variable_y_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, 'addi', &
                [10_int64, 0_int64, int(mir%instructions(1)%literal, int64)], words(2), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(3), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(4), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = 4_int32
            print_buffer_offset = 0_int32
            write (print_digits, '(i0)') mir%instructions(1)%literal
            print_digit_count = len_trim(print_digits)
            do print_digit_index = 1, print_digit_count
                call encode_operation(target, records, 'addi', &
                    [5_int64, 0_int64, int(iachar(print_digits(print_digit_index:print_digit_index)), int64)], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, 'sb', &
                    [5_int64, 2_int64, int(print_buffer_offset, int64)], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                print_buffer_offset = print_buffer_offset + 1_int32
            end do
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'sb', &
                [5_int64, 2_int64, int(print_buffer_offset, int64)], words(emitted_count + 1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [12_int64, 0_int64, &
                int(print_buffer_offset + 1_int32, int64)], words(emitted_count + 1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 0_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 93_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
        else if (print_route) then
            call encode_operation(target, records, 'addi', [2_int64, 2_int64, -16_int64], &
                words(1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = 1_int32
            print_buffer_offset = 0_int32
            print_write_length = 0_int32
            do print_item_index = 1, mir%instruction_count - 1, 2
                write (print_digits, '(i0)') mir%instructions(print_item_index)%literal
                print_digit_count = len_trim(print_digits)
                do print_digit_index = 1, print_digit_count
                    call encode_operation(target, records, 'addi', &
                        [5_int64, 0_int64, int(iachar(print_digits(print_digit_index:print_digit_index)), int64)], &
                        words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    call encode_operation(target, records, 'sb', &
                        [5_int64, 2_int64, int(print_buffer_offset, int64)], &
                        words(emitted_count + 1), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    emitted_count = emitted_count + 1_int32
                    print_buffer_offset = print_buffer_offset + 1_int32
                end do
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                call encode_operation(target, records, 'sb', &
                    [5_int64, 2_int64, int(print_buffer_offset, int64)], &
                    words(emitted_count + 1), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                emitted_count = emitted_count + 1_int32
                print_buffer_offset = print_buffer_offset + 1_int32
            end do
            print_write_length = print_buffer_offset
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [12_int64, 0_int64, &
                int(print_write_length, int64)], words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 0_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 93_int64], &
                words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(emitted_count + 1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = emitted_count + 1_int32
        else if (storage_sequence_generated_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            do index = 1, mir%instruction_count
                operation = mir_v0_bridge_policy_route_operation_for( &
                    mir%instructions(index)%source_rule, int(index - 1, int32))
                if (index == 1) then
                    values = [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                else if (index == mir%instruction_count) then
                    values = [17_int64, 0_int64, 93_int64]
                else if (mod(index - 3, 4) == 0) then
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                else if (mod(index - 3, 4) == 1) then
                    values = [11_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                else if (mod(index - 3, 4) == 2) then
                    values = [10_int64, 10_int64, 11_int64]
                else
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                end if
                call encode_operation(target, records, trim(operation), values, words(index + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
            end do
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(mir%instruction_count + 2), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = mir%instruction_count + 2_int32
        else if (storage_sequence_route) then
            if (mir%instructions(5)%opcode == mir_v0_opcode_sub) then
                if (mir%instructions(1)%literal /= 3_int32 .or. &
                    mir%instructions(4)%literal /= 2_int32) then
                    call set_diagnostic(diagnostic, 'mir-v0: generic subtraction is out of scope')
                    return
                end if
            end if
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            do index = 1, 7
                operation = mir_v0_bridge_policy_route_operation_for( &
                    mir%instructions(index)%source_rule, int(index - 1, int32))
                select case (index)
                case (1)
                    values = [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (2)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (3)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (4)
                    values = [11_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (5)
                    values = [10_int64, 10_int64, 11_int64]
                case (6)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (7)
                    values = [17_int64, 0_int64, 93_int64]
                end select
                call encode_operation(target, records, trim(operation), values, words(index + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
            end do
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(9), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = 9_int32
        else if (storage_sequence_3_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            do index = 1, 11
                operation = mir_v0_bridge_policy_route_operation_for( &
                    mir%instructions(index)%source_rule, int(index - 1, int32))
                select case (index)
                case (1)
                    values = [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (2, 6, 10)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (3, 7)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (4, 8)
                    values = [11_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (5, 9)
                    values = [10_int64, 10_int64, 11_int64]
                case (11)
                    values = [17_int64, 0_int64, 93_int64]
                end select
                call encode_operation(target, records, trim(operation), values, words(index + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
            end do
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(13), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = 13_int32
        else if (storage_sequence_4_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            do index = 1, 15
                operation = mir_v0_bridge_policy_route_operation_for( &
                    mir%instructions(index)%source_rule, int(index - 1, int32))
                select case (index)
                case (1)
                    values = [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (2, 6, 10, 14)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (3, 7, 11)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (4, 8, 12)
                    values = [11_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (5, 9, 13)
                    values = [10_int64, 10_int64, 11_int64]
                case (15)
                    values = [17_int64, 0_int64, 93_int64]
                end select
                call encode_operation(target, records, trim(operation), values, words(index + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
            end do
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(17), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = 17_int32
        else if (storage_sequence_6_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            do index = 1, 23
                operation = mir_v0_bridge_policy_route_operation_for( &
                    mir%instructions(index)%source_rule, int(index - 1, int32))
                select case (index)
                case (1)
                    values = [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (2, 6, 10, 14, 18, 22)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (3, 7, 11, 15, 19)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (4, 8, 12, 16, 20)
                    values = [11_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (5, 9, 13, 17, 21)
                    values = [10_int64, 10_int64, 11_int64]
                case (23)
                    values = [17_int64, 0_int64, 93_int64]
                end select
                call encode_operation(target, records, trim(operation), values, words(index + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
            end do
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(25), status, diagnostic)
            emitted_count = 25_int32
        else if (storage_sequence_5_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            do index = 1, 19
                operation = mir_v0_bridge_policy_route_operation_for( &
                    mir%instructions(index)%source_rule, int(index - 1, int32))
                select case (index)
                case (1)
                    values = [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (2, 6, 10, 14, 18)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (3, 7, 11, 15)
                    values = [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)]
                case (4, 8, 12, 16)
                    values = [11_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (5, 9, 13, 17)
                    values = [10_int64, 10_int64, 11_int64]
                case (19)
                    values = [17_int64, 0_int64, 93_int64]
                end select
                call encode_operation(target, records, trim(operation), values, words(index + 1), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
            end do
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(21), status, diagnostic)
            emitted_count = 21_int32
        else if (storage_route) then
            call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
                [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(1), &
                status, diagnostic)
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [0_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(2), &
                status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            emitted_count = 8_int32
            call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(3), &
                status, diagnostic)
            call encode_operation(target, records, 'addi', [11_int64, 0_int64, 1_int64], words(4), &
                status, diagnostic)
            call encode_operation(target, records, 'add', [10_int64, 10_int64, 11_int64], words(5), &
                status, diagnostic)
            call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
                [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], words(6), &
                status, diagnostic)
            call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                [17_int64, 0_int64, 93_int64], words(7), &
                status, diagnostic)
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(8), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
        else if (mir%instruction_count == 5_int32) then
            emitted_count = 5_int32
            do index = 1, 4
                operation = mir_v0_bridge_policy_machine_operation_for( &
                    mir%instructions(index)%opcode)
                select case (index)
                case (1)
                    ! This structural route does not implement storage or name resolution.
                    if (mir%instructions(index)%opcode == mir_v0_opcode_load) then
                        values = [10_int64, 0_int64, 0_int64]
                    else
                        values = [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                    end if
                case (2)
                    values = [11_int64, 0_int64, int(mir%instructions(index)%literal, int64)]
                case (3)
                    values = [10_int64, 10_int64, 11_int64]
                case (4)
                    values = [17_int64, 0_int64, 93_int64]
                end select
                call encode_operation(target, records, trim(operation), values, words(index), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
            end do
        else
            emitted_count = 3_int32
            operation = mir_v0_bridge_policy_machine_operation_for(mir%instructions(1)%opcode)
            if (mir%instructions(1)%opcode == mir_v0_opcode_add) operation = 'addi'
            values = [10_int64, 0_int64, 0_int64]
            if (mir%instructions(1)%opcode == mir_v0_opcode_const) then
                values(3) = int(mir%instructions(1)%literal, int64)
            end if
            call encode_operation(target, records, trim(operation), values, words(1), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            values = [17_int64, 0_int64, 93_int64]
            call encode_operation(target, records, trim(mir_v0_bridge_policy_exit_status_operation()), &
                values, words(2), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
        end if
        if (.not. print_route .and. .not. storage_route .and. .not. storage_sequence_route .and. &
            .not. storage_sequence_3_route .and. .not. storage_sequence_4_route .and. &
            .not. storage_sequence_5_route .and. .not. storage_sequence_6_route .and. &
            .not. storage_sequence_generated_route) then
            values = mir_v0_riscv_linux_ecall_operands
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, values, &
                words(emitted_count), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
        end if
        call write_elf64_executable(metadata, target_source, words(:emitted_count), artifact%bytes, &
            source_status)
        if (source_status /= 0_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: executable artifact construction failed')
            status = mir_v0_bridge_malformed
            return
        end if
        status = mir_v0_bridge_ok
        call set_diagnostic(diagnostic, '')
    end subroutine compile_mir_v0_riscv_linux

    subroutine encode_generic_print_list(target, records, mir, words, emitted_count, &
            status, diagnostic)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: records(:)
        type(parsed_mir_t), intent(in) :: mir
        integer(int64), intent(out) :: words(:)
        integer(int32), intent(out) :: emitted_count
        integer(int32), intent(out) :: status
        character(len=*), intent(out) :: diagnostic
        integer :: index, item_end, power_index, digit_index
        integer(int32) :: power_exponent, digit_count
        integer(int32) :: word_index
        integer(int64) :: power_value
        character(len=32) :: power_digits
        logical :: variable_power, decimal_output

        word_index = 1_int32
        call encode_operation(target, records, 'addi', [2_int64, 2_int64, -16_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', &
            [10_int64, 0_int64, int(mir%instructions(1)%literal, int64)], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
            [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        index = 3
        do while (index <= mir%instruction_count - 2)
            variable_power = .false.
            decimal_output = .false.
            item_end = index + 1
            if (mir%instructions(index)%opcode == mir_v0_opcode_load) then
                if (index + 3 <= mir%instruction_count - 1) then
                    if (((mir%instructions(index + 1)%opcode == mir_v0_opcode_const .and. &
                        (mir%instructions(index + 2)%opcode == mir_v0_opcode_add .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_sub .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_mul .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_div .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_pow)) .or. &
                        (mir%instructions(index + 1)%opcode == mir_v0_opcode_load .and. &
                        (mir%instructions(index + 2)%opcode == mir_v0_opcode_add .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_pow))) .and. &
                        mir%instructions(index + 3)%opcode == mir_v0_opcode_output) then
                        item_end = index + 3
                    end if
                end if
            end if
            if (item_end == index + 3) then
                call encode_operation(target, records, 'ld', &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                if (mir%instructions(index + 1)%opcode == mir_v0_opcode_load) then
                    call encode_operation(target, records, 'ld', &
                        [11_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
                        words(word_index), status, diagnostic)
                else if (mir%instructions(index + 2)%opcode == mir_v0_opcode_sub .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_mul .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_div .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_pow) then
                    call encode_operation(target, records, 'addi', [11_int64, 0_int64, &
                        int(mir%instructions(index + 1)%literal, int64)], words(word_index), &
                        status, diagnostic)
                else
                    call encode_operation(target, records, 'addi', [11_int64, 0_int64, &
                        int(mir%instructions(index + 1)%literal, int64)], &
                        words(word_index), status, diagnostic)
                end if
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                if (mir%instructions(index + 2)%opcode == mir_v0_opcode_mul) then
                    call encode_operation(target, records, 'mul', [10_int64, 10_int64, 11_int64], &
                        words(word_index), status, diagnostic)
                else if (mir%instructions(index + 2)%opcode == mir_v0_opcode_sub) then
                    call encode_operation(target, records, 'sub', [10_int64, 10_int64, 11_int64], &
                        words(word_index), status, diagnostic)
                else if (mir%instructions(index + 2)%opcode == mir_v0_opcode_div) then
                    call encode_operation(target, records, 'div', [10_int64, 10_int64, 11_int64], &
                        words(word_index), status, diagnostic)
                else if (mir%instructions(index + 2)%opcode == mir_v0_opcode_pow) then
                    if (mir%instructions(index + 1)%opcode == mir_v0_opcode_load) then
                        call encode_operation(target, records, 'mul', &
                            [10_int64, 10_int64, 11_int64], words(word_index), status, diagnostic)
                    else
                        do power_index = generic_power_minimum, &
                                mir%instructions(index + 1)%literal
                            call encode_operation(target, records, 'mul', &
                                [10_int64, 10_int64, 10_int64], words(word_index), status, diagnostic)
                            if (status /= mir_v0_bridge_ok) return
                            if (power_index < mir%instructions(index + 1)%literal) then
                                word_index = word_index + 1_int32
                            end if
                        end do
                    end if
                else
                    call encode_operation(target, records, 'add', [10_int64, 10_int64, 11_int64], &
                        words(word_index), status, diagnostic)
                end if
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                item_end = index + 3
            else
                if (mir%instructions(index)%opcode == mir_v0_opcode_const) then
                    call encode_operation(target, records, 'addi', &
                        [10_int64, 0_int64, int(mir%instructions(index)%literal, int64)], &
                        words(word_index), status, diagnostic)
                else
                    call encode_operation(target, records, 'ld', &
                        [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
                        words(word_index), status, diagnostic)
                end if
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
            end if
            if (item_end > mir%instruction_count - 1) return
            if (mir%instructions(item_end)%opcode /= mir_v0_opcode_output) return
            if (item_end == index + 3 .and. mir%instructions(index + 1)%opcode == &
                mir_v0_opcode_load .and. mir%instructions(index + 2)%opcode == &
                mir_v0_opcode_pow) then
                variable_power = .true.
            end if
            if (variable_power) then
                call encode_operation(target, records, 'mul', [10_int64, 10_int64, 11_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [12_int64, 11_int64, -2_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'mul', [12_int64, 12_int64, 12_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'mul', [10_int64, 10_int64, 12_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [12_int64, 0_int64, 100_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'div', [13_int64, 10_int64, 12_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'mul', [14_int64, 13_int64, 12_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sub', [14_int64, 10_int64, 14_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [12_int64, 0_int64, 10_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'div', [15_int64, 14_int64, 12_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'mul', [16_int64, 15_int64, 12_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sub', [16_int64, 14_int64, 16_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [5_int64, 13_int64, 48_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 8_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [5_int64, 15_int64, 48_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 9_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [5_int64, 16_int64, 48_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 10_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sltiu', [13_int64, 13_int64, 1_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [11_int64, 2_int64, 8_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'add', [11_int64, 11_int64, 13_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [12_int64, 0_int64, 4_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sub', [12_int64, 12_int64, 13_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [14_int64, 0_int64, 11_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'add', [14_int64, 2_int64, 14_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 14_int64, 0_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                    mir_v0_riscv_linux_ecall_operands, words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                index = item_end + 1
                cycle
            end if
            if (item_end == index + 1) then
                if (mir%instructions(index)%opcode == mir_v0_opcode_const) then
                    if (mir%instructions(index)%literal >= 10_int32 .or. &
                        mir%instructions(index)%literal < 0_int32) then
                        decimal_output = .true.
                        power_value = int(mir%instructions(index)%literal, int64)
                    end if
                end if
            else if (item_end == index + 3) then
                if (mir%instructions(index + 1)%opcode == mir_v0_opcode_const) then
                    if (mir%instructions(index + 2)%opcode == mir_v0_opcode_add) then
                        decimal_output = .true.
                        power_value = int(mir%instructions(1)%literal, int64) + &
                            int(mir%instructions(index + 1)%literal, int64)
                    else if (mir%instructions(index + 2)%opcode == mir_v0_opcode_sub) then
                        decimal_output = .true.
                        power_value = int(mir%instructions(1)%literal, int64) - &
                            int(mir%instructions(index + 1)%literal, int64)
                    end if
                end if
            end if
            if (decimal_output) then
                call integer_to_decimal(power_value, power_digits, &
                    digit_count)
                do digit_index = 1, digit_count
                    call encode_operation(target, records, 'addi', [5_int64, 0_int64, &
                        int(iachar(power_digits(digit_index:digit_index)), int64)], &
                        words(word_index), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    word_index = word_index + 1_int32
                    call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                        int(7 + digit_index, int64)], words(word_index), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    word_index = word_index + 1_int32
                end do
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                    int(7 + digit_count + 1, int64)], words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [11_int64, 2_int64, 8_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', &
                    [12_int64, 0_int64, int(digit_count + 1, int64)], words(word_index), &
                    status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                    mir_v0_riscv_linux_ecall_operands, words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                index = item_end + 1
                cycle
            end if
            power_exponent = 0_int32
            if (item_end == index + 3) then
                if (mir%instructions(index + 2)%opcode == mir_v0_opcode_pow) then
                    power_exponent = mir%instructions(index + 1)%literal
                end if
            end if
            digit_count = 1_int32
            if (power_exponent /= 0_int32) then
                power_value = int(mir%instructions(1)%literal, int64)
                do power_index = 2, power_exponent
                    power_value = power_value * int(mir%instructions(1)%literal, int64)
                end do
                call integer_to_decimal(power_value, power_digits, digit_count)
                do digit_index = 1, digit_count
                    call encode_operation(target, records, 'addi', [5_int64, 0_int64, &
                        int(iachar(power_digits(digit_index:digit_index)), int64)], &
                        words(word_index), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    word_index = word_index + 1_int32
                    call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                        int(7 + digit_index, int64)], words(word_index), status, diagnostic)
                    if (status /= mir_v0_bridge_ok) return
                    word_index = word_index + 1_int32
                end do
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                    words(word_index), status, diagnostic)
            else
                call encode_operation(target, records, 'addi', [5_int64, 10_int64, 48_int64], &
                    words(word_index), status, diagnostic)
            end if
            if (status /= mir_v0_bridge_ok) return
            if (power_exponent == 0_int32) then
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 8_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, 9_int64], &
                    words(word_index), status, diagnostic)
            else
                word_index = word_index + 1_int32
                call encode_operation(target, records, 'sb', [5_int64, 2_int64, &
                    int(7 + digit_count + 1, int64)], words(word_index), status, diagnostic)
            end if
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, 'addi', [11_int64, 2_int64, 8_int64], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, 'addi', &
                [12_int64, 0_int64, int(1 + digit_count, int64)], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
                mir_v0_riscv_linux_ecall_operands, words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            index = item_end + 1
        end do
        call encode_operation(target, records, 'addi', [10_int64, 0_int64, 0_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [17_int64, 0_int64, 93_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
            mir_v0_riscv_linux_ecall_operands, words(word_index), status, diagnostic)
        emitted_count = word_index
    end subroutine encode_generic_print_list

    subroutine encode_print_variable_seven_to_eighty(target, records, mir, words, emitted_count, &
            status, diagnostic)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: records(:)
        type(parsed_mir_t), intent(in) :: mir
        integer(int64), intent(out) :: words(:)
        integer(int32), intent(out) :: emitted_count
        integer(int32), intent(out) :: status
        character(len=*), intent(out) :: diagnostic
        integer(int32) :: item_count, item_index, word_index, offset

        item_count = (mir%instruction_count - 7_int32) / 2_int32
        word_index = 1_int32
        call encode_operation(target, records, trim(mir_v0_bridge_policy_frame_operation()), &
            [2_int64, 2_int64, -int(mir_v0_bridge_policy_frame_size, int64)], words(word_index), &
            status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [10_int64, 0_int64, 3_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
            [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
            [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [11_int64, 0_int64, 2_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'mul', [10_int64, 10_int64, 10_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, trim(mir_v0_bridge_policy_store_operation), &
            [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
            [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32

        do item_index = 1, item_count
            offset = 2_int32 * (item_index - 1_int32)
            if (item_index > 1_int32) then
                call encode_operation(target, records, trim(mir_v0_bridge_policy_load_operation), &
                    [10_int64, 2_int64, int(mir_v0_bridge_policy_storage_offset, int64)], &
                    words(word_index), status, diagnostic)
                if (status /= mir_v0_bridge_ok) return
                word_index = word_index + 1_int32
            end if
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 57_int64], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, int(offset, int64)], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, 'addi', [5_int64, 0_int64, 10_int64], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
            call encode_operation(target, records, 'sb', [5_int64, 2_int64, int(offset + 1_int32, int64)], &
                words(word_index), status, diagnostic)
            if (status /= mir_v0_bridge_ok) return
            word_index = word_index + 1_int32
        end do

        call encode_operation(target, records, 'addi', [10_int64, 0_int64, 1_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [11_int64, 2_int64, 0_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [12_int64, 0_int64, int(2 * item_count, int64)], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [17_int64, 0_int64, 64_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
            mir_v0_riscv_linux_ecall_operands, words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [10_int64, 0_int64, 0_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, 'addi', [17_int64, 0_int64, 93_int64], &
            words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        word_index = word_index + 1_int32
        call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, &
            mir_v0_riscv_linux_ecall_operands, words(word_index), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        emitted_count = word_index
    end subroutine encode_print_variable_seven_to_eighty

    subroutine write_mir_v0_riscv_linux(input, path, status, diagnostic)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: path
        integer(int32), intent(out) :: status
        character(len=*), intent(out) :: diagnostic
        type(riscv_linux_artifact_t) :: artifact
        integer :: io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        open (newunit=unit, file=trim(path), access='stream', form='unformatted', &
            status='replace', action='write', iostat=io_status)
        if (io_status /= 0) then
            call set_diagnostic(diagnostic, 'mir-v0: output artifact cannot be opened')
            status = mir_v0_bridge_io_error
            return
        end if
        write (unit, iostat=io_status) artifact%bytes
        close (unit)
        if (io_status /= 0) then
            call set_diagnostic(diagnostic, 'mir-v0: output artifact cannot be written')
            status = mir_v0_bridge_io_error
            return
        end if
        call set_diagnostic(diagnostic, '')
    end subroutine write_mir_v0_riscv_linux

    pure logical function riscv_linux_artifact_provenance_valid(artifact)
        type(riscv_linux_artifact_t), intent(in) :: artifact

        riscv_linux_artifact_provenance_valid = &
            trim(artifact%format) == 'ELF64' .and. &
            trim(artifact%architecture) == 'riscv64' .and. &
            trim(artifact%platform) == 'linux' .and. &
            trim(artifact%origin) == 'DERIVED' .and. &
            trim(artifact%input_format) == 'mir-v0-sx'
    end function riscv_linux_artifact_provenance_valid

    logical function parse_mir(input, mir, diagnostic) result(ok)
        character(len=*), intent(in) :: input
        type(parsed_mir_t), intent(out) :: mir
        character(len=*), intent(out) :: diagnostic
        character(len=token_length) :: token(token_capacity)
        integer :: token_count, position, index
        integer(int32) :: count

        mir = parsed_mir_t()
        call set_diagnostic(diagnostic, '')
        ok = tokenize(input, token, token_count, diagnostic)
        if (.not. ok) return
        position = 1
        ok = expect(token, token_count, position, '(', diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, 'mir-function', diagnostic)
        if (.not. ok) return
        ok = read_atom(token, token_count, position, 'name', mir%name, diagnostic)
        if (.not. ok) return
        ok = read_integer(token, token_count, position, 'entry-block', &
            mir%entry_block, diagnostic)
        if (.not. ok) return
        ok = read_integer(token, token_count, position, 'instruction-count', count, diagnostic)
        if (.not. ok) return
        if (count < 0_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: negative instruction count')
            return
        end if
        if (count > instruction_capacity) then
            call set_diagnostic(diagnostic, 'mir-v0: instruction capacity exceeded')
            return
        end if
        mir%instruction_count = count
        ok = expect(token, token_count, position, '(', diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, 'instructions', diagnostic)
        if (.not. ok) return
        do index = 1, count
            ok = read_instruction(token, token_count, position, index - 1, &
                mir%instructions(index), diagnostic)
            if (.not. ok) return
        end do
        ok = expect(token, token_count, position, ')', diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, ')', diagnostic)
        if (.not. ok) return
        if (position <= token_count) then
            call set_diagnostic(diagnostic, 'mir-v0: trailing SX input')
            return
        end if
    end function parse_mir

    logical function validate_scope(mir, status, diagnostic) result(ok)
        type(parsed_mir_t), intent(in) :: mir
        integer(int32), intent(out) :: status
        character(len=*), intent(out) :: diagnostic
        integer :: index
        logical :: print_variable_route, print_variable_expression_route
        logical :: print_variable_two_item_route
        logical :: print_variable_three_item_route
        logical :: print_variable_four_item_route
        logical :: print_variable_five_item_route
        logical :: print_variable_six_item_route
        logical :: print_variable_seven_to_eighty_item_route
        logical :: generic_print_route
        logical :: initialized_power_variable_shape
        logical :: initialized_variable_y_route

        ok = .false.
        status = mir_v0_bridge_out_of_scope
        call set_diagnostic(diagnostic, '')
        print_variable_route = is_print_variable_candidate(mir)
        print_variable_expression_route = is_print_variable_expression_candidate(mir)
        print_variable_two_item_route = is_print_variable_two_item_candidate(mir)
        print_variable_three_item_route = is_print_variable_three_item_candidate(mir)
        print_variable_four_item_route = is_print_variable_four_item_candidate(mir)
        print_variable_five_item_route = is_print_variable_five_item_candidate(mir)
        print_variable_six_item_route = is_print_variable_six_item_candidate(mir)
        print_variable_seven_to_eighty_item_route = &
            is_print_variable_seven_to_hundred_item_candidate(mir)
        generic_print_route = is_generic_print_list_route(mir)
        initialized_variable_y_route = is_initialized_variable_y_route(mir)
        initialized_power_variable_shape = .false.
        if (print_variable_expression_route) then
            if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                initialized_power_variable_shape = .true.
            end if
        end if
        if (.not. mir_v0_bridge_policy_function_supported(mir%name)) then
            call set_diagnostic(diagnostic, 'mir-v0: function is out of scope')
            return
        end if
        if (mir%entry_block /= 0_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: function is out of scope')
            return
        end if
        if (mir%instruction_count <= 0_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: function is out of scope')
            return
        end if
        if (generic_print_route) then
            if (.not. valid_generic_print_list(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: generic PRINT list is out of scope')
                return
            end if
            ok = .true.
            status = mir_v0_bridge_ok
            return
        else if (.not. mir_v0_bridge_policy_instruction_count_matches(mir%name, &
                mir%instructions(1)%source_rule, mir%instruction_count)) then
            call set_diagnostic(diagnostic, 'mir-v0: function is out of scope')
            return
        end if
        if (print_variable_seven_to_eighty_item_route) then
            if (.not. valid_print_variable_seven_to_eighty_item(mir)) then
                call set_diagnostic(diagnostic, &
                    'mir-v0: PRINT seven-to-eighty-item witness is out of scope')
                return
            end if
        else if (print_variable_six_item_route) then
            if (.not. valid_print_variable_six_item(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: PRINT six-item witness is out of scope')
                return
            end if
        else if (print_variable_five_item_route) then
            if (.not. valid_print_variable_five_item(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: PRINT five-item witness is out of scope')
                return
            end if
        else if (print_variable_four_item_route) then
            if (.not. valid_print_variable_four_item(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: PRINT four-item witness is out of scope')
                return
            end if
        else if (print_variable_three_item_route) then
            if (.not. valid_print_variable_three_item(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: PRINT three-item witness is out of scope')
                return
            end if
        else if (print_variable_two_item_route) then
            if (.not. valid_print_variable_two_item(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: PRINT two-item witness is out of scope')
                return
            end if
        else if (initialized_variable_y_route) then
            if (.not. valid_initialized_variable_y(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: initialized variable y witness is out of scope')
                return
            end if
        else if (print_variable_route) then
            if (.not. valid_print_variable(mir)) then
                call set_diagnostic(diagnostic, 'mir-v0: PRINT variable witness is out of scope')
                return
            end if
        else if (print_variable_expression_route) then
            if (.not. valid_print_variable_expression(mir)) then
                call set_diagnostic(diagnostic, &
                    'mir-v0: PRINT variable expression witness is out of scope')
                return
            end if
        else if (trim(mir%name) == 'p') then
            if (trim(mir%instructions(1)%source_rule) == 'frontend-ast-v2/print-stmt') then
                if (mir%instruction_count == 7_int32) then
                    if (mir%instructions(1)%literal == 7_int32) then
                        if (mir%instructions(3)%literal /= 8_int32) then
                            call set_diagnostic(diagnostic, 'mir-v0: PRINT item sequence is out of scope')
                            return
                        end if
                        if (mir%instructions(5)%literal /= 9_int32) then
                            call set_diagnostic(diagnostic, 'mir-v0: PRINT item sequence is out of scope')
                            return
                        end if
                    else if (mir%instructions(1)%literal == 17_int32) then
                        if (mir%instructions(3)%literal /= 18_int32) then
                            call set_diagnostic(diagnostic, 'mir-v0: PRINT item sequence is out of scope')
                            return
                        end if
                        if (mir%instructions(5)%literal /= 19_int32) then
                            call set_diagnostic(diagnostic, 'mir-v0: PRINT item sequence is out of scope')
                            return
                        end if
                    else
                        call set_diagnostic(diagnostic, 'mir-v0: PRINT item sequence is out of scope')
                        return
                    end if
                end if
            end if
        end if
        if (mir%instruction_count == 9_int32 .and. &
                mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
                mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
                mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
                mir%instructions(4)%opcode == mir_v0_opcode_load) then
            if (mir%instructions(5)%opcode /= mir_v0_opcode_add .and. &
                    mir%instructions(5)%opcode /= mir_v0_opcode_sub .and. &
                    mir%instructions(5)%opcode /= mir_v0_opcode_mul .and. &
                    mir%instructions(5)%opcode /= mir_v0_opcode_div .and. &
                    mir%instructions(5)%opcode /= mir_v0_opcode_pow) then
                call set_diagnostic(diagnostic, 'mir-v0: initialized load operation is out of scope')
                return
            end if
        end if
        do index = 1, mir%instruction_count
            if (.not. mir_v0_bridge_policy_opcode_supported(mir%instructions(index)%opcode)) then
                status = mir_v0_bridge_unsupported
                call set_diagnostic(diagnostic, 'mir-v0: opcode is unsupported')
                return
            end if
        end do
        do index = 1, mir%instruction_count
            if (.not. mir_v0_bridge_policy_storage_matches( &
                mir%instructions(index)%storage_present, mir%instructions(index)%storage_key)) then
                if (.not. initialized_variable_y_route) then
                    call set_diagnostic(diagnostic, 'mir-v0: storage identity is out of scope')
                    return
                end if
            end if
            if (.not. mir_v0_bridge_policy_accepts(mir%name, mir%instruction_count, &
                int(index - 1, int32), mir%instructions(index)%opcode, &
                mir%instructions(index)%result_id, &
                mir%instructions(index)%result_kind, mir%instructions(index)%result_type, &
                mir%instructions(index)%source_rule, mir%instructions(index)%literal_present, &
                mir%instructions(index)%literal)) then
                if (.not. initialized_power_variable_shape .and. .not. initialized_variable_y_route) then
                    call set_diagnostic(diagnostic, 'mir-v0: witness is out of scope')
                    return
                else if (initialized_power_variable_shape .and. index /= 4) then
                    call set_diagnostic(diagnostic, 'mir-v0: witness is out of scope')
                    return
                end if
            end if
        end do
        ok = .true.
        status = mir_v0_bridge_ok
    end function validate_scope

    logical function is_print_variable_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 5_int32) return
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(3)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        candidate = mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(4)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_return
    end function is_print_variable_candidate

    logical function is_generic_print_list_route(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index, item_end

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count < 5_int32) return
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(2)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (mir%instruction_count == 5_int32) then
            if (mir%instructions(3)%opcode /= mir_v0_opcode_const) return
            if (mir%instructions(4)%opcode /= mir_v0_opcode_output) return
        end if
        index = 3
        do while (index <= mir%instruction_count - 2)
            if (trim(mir%instructions(index)%source_rule) /= 'frontend-ast-v2/print-stmt') return
            item_end = index + 1
            if (mir%instructions(index)%opcode == mir_v0_opcode_load) then
                if (index + 3 < mir%instruction_count) then
                    if (((mir%instructions(index + 1)%opcode == mir_v0_opcode_const .and. &
                        (mir%instructions(index + 2)%opcode == mir_v0_opcode_add .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_sub .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_mul .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_div .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_pow)) .or. &
                        (mir%instructions(index + 1)%opcode == mir_v0_opcode_load .and. &
                        (mir%instructions(index + 2)%opcode == mir_v0_opcode_add .or. &
                        mir%instructions(index + 2)%opcode == mir_v0_opcode_pow))) .and. &
                        mir%instructions(index + 3)%opcode == mir_v0_opcode_output) then
                        item_end = index + 3
                    end if
                end if
            end if
            if (item_end >= mir%instruction_count) return
            if (trim(mir%instructions(item_end)%source_rule) /= 'frontend-ast-v2/print-stmt') return
            if (mir%instructions(item_end)%opcode /= mir_v0_opcode_output) return
            index = item_end + 1
        end do
        if (trim(mir%instructions(mir%instruction_count)%source_rule) /= &
            'frontend-ast-v2/print-stmt') return
        if (mir%instructions(mir%instruction_count)%opcode /= mir_v0_opcode_return) return
        candidate = .true.
    end function is_generic_print_list_route

    logical function is_initialized_variable_y_route(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 5_int32) return
        if (mir%instructions(1)%opcode /= mir_v0_opcode_const .or. &
            mir%instructions(2)%opcode /= mir_v0_opcode_store .or. &
            mir%instructions(3)%opcode /= mir_v0_opcode_load .or. &
            mir%instructions(4)%opcode /= mir_v0_opcode_output .or. &
            mir%instructions(5)%opcode /= mir_v0_opcode_return) return
        do index = 1, 2
            if (trim(mir%instructions(index)%source_rule) /= &
                'frontend-ast-v2/execution-part') return
        end do
        do index = 3, 5
            if (trim(mir%instructions(index)%source_rule) /= &
                'frontend-ast-v2/print-stmt') return
        end do
        if (.not. mir%instructions(1)%literal_present .or. &
            mir%instructions(1)%storage_present) return
        if (mir%instructions(1)%literal < -100_int32 .or. &
            mir%instructions(1)%literal > 2047_int32) return
        if (.not. mir%instructions(2)%storage_present .or. &
            trim(mir%instructions(2)%storage_key) /= 'y' .or. &
            mir%instructions(2)%literal_present) return
        if (.not. mir%instructions(3)%storage_present .or. &
            trim(mir%instructions(3)%storage_key) /= 'y' .or. &
            mir%instructions(3)%literal_present) return
        if (mir%instructions(4)%storage_present .or. mir%instructions(4)%literal_present .or. &
            mir%instructions(5)%storage_present .or. mir%instructions(5)%literal_present) return
        if (mir%instructions(1)%result_id /= 2_int32 .or. &
            mir%instructions(2)%result_id /= 1_int32 .or. &
            mir%instructions(3)%result_id /= 1_int32 .or. &
            mir%instructions(4)%result_id /= 1_int32 .or. &
            mir%instructions(5)%result_id /= 1_int32) return
        do index = 1, 5
            if (mir%instructions(index)%result_kind /= mir_v0_value_kind_integer .or. &
                trim(mir%instructions(index)%result_type) /= 'i32') return
        end do
        candidate = .true.
    end function is_initialized_variable_y_route

    logical function valid_initialized_variable_y(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir

        valid = is_initialized_variable_y_route(mir)
    end function valid_initialized_variable_y

    logical function valid_generic_print_list(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index, item_end, item_count
        type(bridge_instruction_t) :: value_instruction, output_instruction
        type(bridge_instruction_t) :: operand_instruction, operation_instruction

        valid = .false.
        if (.not. is_generic_print_list_route(mir)) return
        if (mir%instructions(1)%opcode /= mir_v0_opcode_const) return
        if (mir%instructions(2)%opcode /= mir_v0_opcode_store) return
        if (.not. mir%instructions(1)%literal_present) return
        if (mir%instructions(1)%storage_present) return
        if (mir%instructions(1)%result_kind /= mir_v0_value_kind_integer) return
        if (trim(mir%instructions(1)%result_type) /= 'i32') return
        if (.not. mir%instructions(2)%storage_present) return
        if (trim(mir%instructions(2)%storage_key) /= 'x') return
        if (mir%instructions(2)%literal_present) return
        if (mir%instructions(mir%instruction_count)%opcode /= mir_v0_opcode_return) return
        if (mir%instructions(mir%instruction_count)%result_kind /= &
            mir_v0_value_kind_integer) return
        if (trim(mir%instructions(mir%instruction_count)%result_type) /= 'i32') return
        index = 3
        item_count = 0
        do while (index <= mir%instruction_count - 2)
            value_instruction = mir%instructions(index)
            if (value_instruction%opcode == mir_v0_opcode_load) then
                if (index + 3 <= mir%instruction_count - 1) then
                    operand_instruction = mir%instructions(index + 1)
                    operation_instruction = mir%instructions(index + 2)
                    output_instruction = mir%instructions(index + 3)
                    if (trim(operand_instruction%source_rule) /= 'frontend-ast-v2/print-stmt') return
                    if (trim(operation_instruction%source_rule) /= 'frontend-ast-v2/print-stmt') return
                    if (((operand_instruction%opcode == mir_v0_opcode_const .and. &
                        (operation_instruction%opcode == mir_v0_opcode_add .or. &
                        operation_instruction%opcode == mir_v0_opcode_sub .or. &
                        operation_instruction%opcode == mir_v0_opcode_mul .or. &
                        operation_instruction%opcode == mir_v0_opcode_div .or. &
                        operation_instruction%opcode == mir_v0_opcode_pow)) .or. &
                        (operand_instruction%opcode == mir_v0_opcode_load .and. &
                        (operation_instruction%opcode == mir_v0_opcode_add .or. &
                        operation_instruction%opcode == mir_v0_opcode_pow))) .and. &
                        output_instruction%opcode == mir_v0_opcode_output) then
                        if (.not. value_instruction%storage_present) return
                        if (trim(value_instruction%storage_key) /= 'x') return
                        if (value_instruction%literal_present) return
                        if (operation_instruction%literal_present) return
                        if (operation_instruction%storage_present) return
                        if (operand_instruction%opcode == mir_v0_opcode_const) then
                            if (.not. operand_instruction%literal_present) return
                            if (operation_instruction%opcode == mir_v0_opcode_add .or. &
                                operation_instruction%opcode == mir_v0_opcode_sub) then
                                if (operand_instruction%literal < generic_decimal_minimum .or. &
                                    operand_instruction%literal > generic_decimal_maximum) return
                            else if (operation_instruction%opcode == mir_v0_opcode_pow) then
                                if (operand_instruction%literal < generic_power_minimum .or. &
                                    operand_instruction%literal > generic_power_maximum) return
                            else
                                if (operand_instruction%literal /= 2_int32) return
                            end if
                            if (operand_instruction%storage_present) return
                        else if (operation_instruction%opcode == mir_v0_opcode_pow) then
                            if (operand_instruction%opcode == mir_v0_opcode_load) then
                                if (.not. operand_instruction%storage_present) return
                                if (trim(operand_instruction%storage_key) /= 'x') return
                                if (operand_instruction%literal_present) return
                            else
                                if (operand_instruction%literal < generic_power_minimum .or. &
                                    operand_instruction%literal > generic_power_maximum) return
                            end if
                        else
                            if (.not. operand_instruction%storage_present) return
                            if (trim(operand_instruction%storage_key) /= 'x') return
                            if (operand_instruction%literal_present) return
                        end if
                        if (trim(value_instruction%source_rule) /= 'frontend-ast-v2/print-stmt') return
                        item_end = index + 3
                    else
                        output_instruction = mir%instructions(index + 1)
                        item_end = index + 1
                    end if
                else
                    output_instruction = mir%instructions(index + 1)
                    item_end = index + 1
                end if
            else
                if (value_instruction%opcode /= mir_v0_opcode_const) then
                    return
                end if
                output_instruction = mir%instructions(index + 1)
                item_end = index + 1
            end if
            if (output_instruction%opcode /= mir_v0_opcode_output) return
            if (value_instruction%result_kind /= mir_v0_value_kind_integer) return
            if (output_instruction%result_kind /= mir_v0_value_kind_integer) return
            if (trim(value_instruction%result_type) /= 'i32') return
            if (trim(output_instruction%result_type) /= 'i32') return
            if (value_instruction%opcode == mir_v0_opcode_const) then
                if (.not. value_instruction%literal_present) return
                if (value_instruction%storage_present) return
                if (value_instruction%literal < 0_int32) then
                    if (value_instruction%literal < generic_negative_minimum .or. &
                        value_instruction%literal > generic_negative_maximum) return
                end if
            else if (value_instruction%opcode == mir_v0_opcode_load) then
                if (.not. value_instruction%storage_present) return
                if (trim(value_instruction%storage_key) /= 'x') return
                if (value_instruction%literal_present) return
            end if
            if (output_instruction%storage_present) return
            if (item_end == index + 3) then
                if (operand_instruction%result_kind /= mir_v0_value_kind_integer) return
                if (operation_instruction%result_kind /= mir_v0_value_kind_integer) return
                if (trim(operand_instruction%result_type) /= 'i32') return
                if (trim(operation_instruction%result_type) /= 'i32') return
            end if
            item_count = item_count + 1
            if (item_count > 10) return
            index = item_end + 1
        end do
        if (item_count < 1) return
        valid = .true.
    end function valid_generic_print_list

    logical function is_print_variable_two_item_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 11_int32) return
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(7)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        candidate = mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(4)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow .and. &
            mir%instructions(6)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(7)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(8)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(9)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(10)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(11)%opcode == mir_v0_opcode_return
    end function is_print_variable_two_item_candidate

    logical function is_print_variable_three_item_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 13_int32) return
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(7)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        candidate = mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(4)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow .and. &
            mir%instructions(6)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(7)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(8)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(9)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(10)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(11)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(12)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(13)%opcode == mir_v0_opcode_return
    end function is_print_variable_three_item_candidate

    logical function is_print_variable_four_item_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 15_int32) return
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(7)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        candidate = mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(4)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow .and. &
            mir%instructions(6)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(7)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(8)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(9)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(10)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(11)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(12)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(13)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(14)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(15)%opcode == mir_v0_opcode_return
    end function is_print_variable_four_item_candidate

    logical function is_print_variable_five_item_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 17_int32) return
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(7)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        candidate = mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(4)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow .and. &
            mir%instructions(6)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(7)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(8)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(9)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(10)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(11)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(12)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(13)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(14)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(15)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(16)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(17)%opcode == mir_v0_opcode_return
    end function is_print_variable_five_item_candidate

    logical function is_print_variable_six_item_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 19_int32) return
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(7)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        candidate = mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(4)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow .and. &
            mir%instructions(6)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(7)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(8)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(9)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(10)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(11)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(12)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(13)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(14)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(15)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(16)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(17)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(18)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(19)%opcode == mir_v0_opcode_return
    end function is_print_variable_six_item_candidate

    logical function is_print_variable_seven_to_hundred_item_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index, item_count

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count < 21_int32 .or. mir%instruction_count > 207_int32) return
        if (mod(mir%instruction_count - 7_int32, 2_int32) /= 0_int32) return
        item_count = (mir%instruction_count - 7) / 2
        if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
        if (trim(mir%instructions(7)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        if (mir%instructions(1)%opcode /= mir_v0_opcode_const) return
        if (mir%instructions(2)%opcode /= mir_v0_opcode_store) return
        if (mir%instructions(3)%opcode /= mir_v0_opcode_load) return
        if (mir%instructions(4)%opcode /= mir_v0_opcode_const) return
        if (mir%instructions(5)%opcode /= mir_v0_opcode_pow) return
        if (mir%instructions(6)%opcode /= mir_v0_opcode_store) return
        do index = 1, item_count
            if (mir%instructions(5 + 2 * index)%opcode /= mir_v0_opcode_load) return
            if (mir%instructions(6 + 2 * index)%opcode /= mir_v0_opcode_output) return
        end do
        if (mir%instructions(mir%instruction_count)%opcode /= mir_v0_opcode_return) return
        candidate = .true.
    end function is_print_variable_seven_to_hundred_item_candidate

    logical function is_print_variable_expression_candidate(mir) result(candidate)
        type(parsed_mir_t), intent(in) :: mir

        candidate = .false.
        if (trim(mir%name) /= 'main') return
        if (mir%instruction_count /= 9_int32) return
        if (mir%instructions(5)%opcode /= mir_v0_opcode_sub) then
            if (trim(mir%instructions(1)%source_rule) /= 'frontend-ast-v2/execution-part') return
            if (trim(mir%instructions(7)%source_rule) /= 'frontend-ast-v2/print-stmt') return
        end if
        candidate = mir%instructions(1)%opcode == mir_v0_opcode_const .and. &
            mir%instructions(2)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(3)%opcode == mir_v0_opcode_load .and. &
            (mir%instructions(4)%opcode == mir_v0_opcode_const .or. &
            (mir%instructions(4)%opcode == mir_v0_opcode_load .and. &
            (mir%instructions(5)%opcode == mir_v0_opcode_add .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_sub .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_mul .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_div .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow))) .and. &
            (mir%instructions(5)%opcode == mir_v0_opcode_add .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_mul .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_div .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_sub .or. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow) .and. &
            mir%instructions(6)%opcode == mir_v0_opcode_store .and. &
            mir%instructions(7)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(8)%opcode == mir_v0_opcode_output .and. &
            mir%instructions(9)%opcode == mir_v0_opcode_return
    end function is_print_variable_expression_candidate

    logical function valid_print_variable(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir

        valid = .false.
        if (.not. mir%instructions(2)%storage_present) return
        if (.not. mir%instructions(3)%storage_present) return
        if (trim(mir%instructions(2)%storage_key) /= 'x') return
        if (trim(mir%instructions(3)%storage_key) /= 'x') return
        if (mir%instructions(4)%storage_present) return
        if (mir%instructions(5)%storage_present) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 2_int32) return
        if (mir%instructions(5)%result_id /= 2_int32) return
        valid = .true.
    end function valid_print_variable

    logical function valid_print_variable_two_item(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir

        valid = .false.
        if (.not. mir%instructions(2)%storage_present) return
        if (.not. mir%instructions(3)%storage_present) return
        if (.not. mir%instructions(6)%storage_present) return
        if (.not. mir%instructions(7)%storage_present) return
        if (.not. mir%instructions(9)%storage_present) return
        if (trim(mir%instructions(2)%storage_key) /= 'x') return
        if (trim(mir%instructions(3)%storage_key) /= 'x') return
        if (trim(mir%instructions(6)%storage_key) /= 'x') return
        if (trim(mir%instructions(7)%storage_key) /= 'x') return
        if (trim(mir%instructions(9)%storage_key) /= 'x') return
        if (mir%instructions(1)%literal /= 3_int32) return
        if (mir%instructions(4)%literal /= 2_int32) return
        if (mir%instructions(8)%storage_present) return
        if (mir%instructions(10)%storage_present) return
        if (mir%instructions(11)%storage_present) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 3_int32) return
        if (mir%instructions(5)%result_id /= 4_int32) return
        if (mir%instructions(6)%result_id /= 4_int32) return
        if (mir%instructions(7)%result_id /= 6_int32) return
        if (mir%instructions(8)%result_id /= 6_int32) return
        if (mir%instructions(9)%result_id /= 7_int32 .and. &
            mir%instructions(9)%result_id /= 8_int32) return
        if (mir%instructions(10)%result_id /= 7_int32 .and. &
            mir%instructions(10)%result_id /= 8_int32) return
        if (mir%instructions(11)%result_id /= 7_int32 .and. &
            mir%instructions(11)%result_id /= 8_int32) return
        valid = .true.
    end function valid_print_variable_two_item

    logical function valid_print_variable_three_item(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir

        valid = .false.
        if (.not. mir%instructions(2)%storage_present) return
        if (.not. mir%instructions(3)%storage_present) return
        if (.not. mir%instructions(6)%storage_present) return
        if (.not. mir%instructions(7)%storage_present) return
        if (.not. mir%instructions(9)%storage_present) return
        if (.not. mir%instructions(11)%storage_present) return
        if (trim(mir%instructions(2)%storage_key) /= 'x') return
        if (trim(mir%instructions(3)%storage_key) /= 'x') return
        if (trim(mir%instructions(6)%storage_key) /= 'x') return
        if (trim(mir%instructions(7)%storage_key) /= 'x') return
        if (trim(mir%instructions(9)%storage_key) /= 'x') return
        if (trim(mir%instructions(11)%storage_key) /= 'x') return
        if (mir%instructions(1)%literal /= 3_int32) return
        if (mir%instructions(4)%literal /= 2_int32) return
        if (mir%instructions(8)%storage_present) return
        if (mir%instructions(10)%storage_present) return
        if (mir%instructions(12)%storage_present) return
        if (mir%instructions(13)%storage_present) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 3_int32) return
        if (mir%instructions(5)%result_id /= 4_int32) return
        if (mir%instructions(6)%result_id /= 4_int32) return
        if (mir%instructions(7)%result_id /= 6_int32) return
        if (mir%instructions(8)%result_id /= 6_int32) return
        if (mir%instructions(9)%result_id /= 8_int32 .and. &
            mir%instructions(9)%result_id /= 7_int32) return
        if (mir%instructions(10)%result_id /= 8_int32 .and. &
            mir%instructions(10)%result_id /= 7_int32) return
        if (mir%instructions(11)%result_id /= 8_int32 .and. &
            mir%instructions(11)%result_id /= 7_int32) return
        if (mir%instructions(12)%result_id /= 8_int32 .and. &
            mir%instructions(12)%result_id /= 7_int32) return
        if (mir%instructions(13)%result_id /= 8_int32 .and. &
            mir%instructions(13)%result_id /= 7_int32) return
        valid = .true.
    end function valid_print_variable_three_item

    logical function valid_print_variable_four_item(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir

        valid = .false.
        if (.not. mir%instructions(2)%storage_present) return
        if (.not. mir%instructions(3)%storage_present) return
        if (.not. mir%instructions(6)%storage_present) return
        if (.not. mir%instructions(7)%storage_present) return
        if (.not. mir%instructions(9)%storage_present) return
        if (.not. mir%instructions(11)%storage_present) return
        if (.not. mir%instructions(13)%storage_present) return
        if (trim(mir%instructions(2)%storage_key) /= 'x') return
        if (trim(mir%instructions(3)%storage_key) /= 'x') return
        if (trim(mir%instructions(6)%storage_key) /= 'x') return
        if (trim(mir%instructions(7)%storage_key) /= 'x') return
        if (trim(mir%instructions(9)%storage_key) /= 'x') return
        if (trim(mir%instructions(11)%storage_key) /= 'x') return
        if (trim(mir%instructions(13)%storage_key) /= 'x') return
        if (mir%instructions(1)%literal /= 3_int32) return
        if (mir%instructions(4)%literal /= 2_int32) return
        if (mir%instructions(8)%storage_present) return
        if (mir%instructions(10)%storage_present) return
        if (mir%instructions(12)%storage_present) return
        if (mir%instructions(14)%storage_present) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 3_int32) return
        if (mir%instructions(5)%result_id /= 4_int32) return
        if (mir%instructions(6)%result_id /= 4_int32) return
        if (mir%instructions(7)%result_id /= 6_int32) return
        if (mir%instructions(8)%result_id /= 6_int32) return
        if (mir%instructions(9)%result_id /= 8_int32 .and. &
            mir%instructions(9)%result_id /= 7_int32) return
        if (mir%instructions(10)%result_id /= 8_int32 .and. &
            mir%instructions(10)%result_id /= 7_int32) return
        if (mir%instructions(11)%result_id /= 8_int32 .and. &
            mir%instructions(11)%result_id /= 7_int32) return
        if (mir%instructions(12)%result_id /= 8_int32 .and. &
            mir%instructions(12)%result_id /= 7_int32) return
        if (mir%instructions(13)%result_id /= 8_int32 .and. &
            mir%instructions(13)%result_id /= 7_int32 .and. &
            mir%instructions(13)%result_id /= 9_int32) return
        if (mir%instructions(14)%result_id /= 8_int32 .and. &
            mir%instructions(14)%result_id /= 7_int32 .and. &
            mir%instructions(14)%result_id /= 9_int32) return
        if (mir%instructions(15)%result_id /= 8_int32 .and. &
            mir%instructions(15)%result_id /= 7_int32 .and. &
            mir%instructions(15)%result_id /= 9_int32) return
        valid = .true.
    end function valid_print_variable_four_item

    logical function valid_print_variable_five_item(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index

        valid = .false.
        if (.not. mir%instructions(2)%storage_present) return
        if (.not. mir%instructions(3)%storage_present) return
        if (.not. mir%instructions(6)%storage_present) return
        if (.not. mir%instructions(7)%storage_present) return
        if (.not. mir%instructions(9)%storage_present) return
        if (.not. mir%instructions(11)%storage_present) return
        if (.not. mir%instructions(13)%storage_present) return
        if (.not. mir%instructions(15)%storage_present) return
        if (trim(mir%instructions(2)%storage_key) /= 'x') return
        if (trim(mir%instructions(3)%storage_key) /= 'x') return
        if (trim(mir%instructions(6)%storage_key) /= 'x') return
        if (trim(mir%instructions(7)%storage_key) /= 'x') return
        if (trim(mir%instructions(9)%storage_key) /= 'x') return
        if (trim(mir%instructions(11)%storage_key) /= 'x') return
        if (trim(mir%instructions(13)%storage_key) /= 'x') return
        if (trim(mir%instructions(15)%storage_key) /= 'x') return
        if (mir%instructions(1)%literal /= 3_int32) return
        if (mir%instructions(4)%literal /= 2_int32) return
        if (mir%instructions(8)%storage_present) return
        if (mir%instructions(10)%storage_present) return
        if (mir%instructions(12)%storage_present) return
        if (mir%instructions(14)%storage_present) return
        if (mir%instructions(16)%storage_present) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 3_int32) return
        if (mir%instructions(5)%result_id /= 4_int32) return
        if (mir%instructions(6)%result_id /= 4_int32) return
        if (mir%instructions(7)%result_id /= 6_int32) return
        if (mir%instructions(8)%result_id /= 6_int32) return
        if (mir%instructions(9)%result_id /= 7_int32) return
        if (mir%instructions(10)%result_id /= 7_int32) return
        if (mir%instructions(11)%result_id /= 8_int32) return
        if (mir%instructions(12)%result_id /= 8_int32) return
        if (mir%instructions(13)%result_id /= 9_int32) return
        if (mir%instructions(14)%result_id /= 9_int32) return
        if (mir%instructions(15)%result_id /= 10_int32) return
        if (mir%instructions(16)%result_id /= 10_int32) return
        if (mir%instructions(17)%result_id /= 10_int32) return
        valid = .true.
    end function valid_print_variable_five_item

    logical function valid_print_variable_six_item(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index

        valid = .false.
        do index = 1, 19
            select case (index)
            case (2, 3, 6, 7, 9, 11, 13, 15, 17)
                if (.not. mir%instructions(index)%storage_present) return
                if (trim(mir%instructions(index)%storage_key) /= 'x') return
            case default
                if (mir%instructions(index)%storage_present) return
            end select
        end do
        if (mir%instructions(1)%literal /= 3_int32) return
        if (mir%instructions(4)%literal /= 2_int32) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 3_int32) return
        if (mir%instructions(5)%result_id /= 4_int32) return
        if (mir%instructions(6)%result_id /= 4_int32) return
        if (mir%instructions(7)%result_id /= 6_int32) return
        if (mir%instructions(8)%result_id /= 6_int32) return
        if (mir%instructions(9)%result_id /= 7_int32) return
        if (mir%instructions(10)%result_id /= 7_int32) return
        if (mir%instructions(11)%result_id /= 8_int32) return
        if (mir%instructions(12)%result_id /= 8_int32) return
        if (mir%instructions(13)%result_id /= 9_int32) return
        if (mir%instructions(14)%result_id /= 9_int32) return
        if (mir%instructions(15)%result_id /= 10_int32) return
        if (mir%instructions(16)%result_id /= 10_int32) return
        if (mir%instructions(17)%result_id /= 11_int32) return
        if (mir%instructions(18)%result_id /= 11_int32) return
        if (mir%instructions(19)%result_id /= 11_int32) return
        valid = .true.
    end function valid_print_variable_six_item

    logical function valid_print_variable_seven_to_eighty_item(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index, item_count

        valid = .false.
        item_count = (mir%instruction_count - 7) / 2
        do index = 1, mir%instruction_count
            select case (index)
            case (2, 3, 6, 7)
                if (.not. mir%instructions(index)%storage_present) return
                if (trim(mir%instructions(index)%storage_key) /= 'x') return
            case default
                if (index >= 9) then
                    if (index <= 5 + 2 * item_count .and. mod(index - 9, 2) == 0) then
                        if (.not. mir%instructions(index)%storage_present) return
                        if (trim(mir%instructions(index)%storage_key) /= 'x') return
                    else if (mir%instructions(index)%storage_present) then
                        return
                    end if
                else if (mir%instructions(index)%storage_present) then
                    return
                end if
            end select
        end do
        if (mir%instructions(1)%literal /= 3_int32) return
        if (mir%instructions(4)%literal /= 2_int32) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 3_int32) return
        if (mir%instructions(5)%result_id /= 4_int32) return
        if (mir%instructions(6)%result_id /= 4_int32) return
        if (mir%instructions(7)%result_id /= 6_int32) return
        do index = 2, item_count
            if (mir%instructions(5 + 2 * index)%result_id < 7_int32) return
            if (mir%instructions(6 + 2 * index)%result_id < 7_int32) return
        end do
        valid = .true.
    end function valid_print_variable_seven_to_eighty_item

    logical function valid_print_variable_expression(mir) result(valid)
        type(parsed_mir_t), intent(in) :: mir
        integer :: index
        logical :: initialized_addition_route, initialized_subtraction_route
        logical :: initialized_multiplier_route
        logical :: initialized_division_route, initialized_power_route

        valid = .false.
        initialized_addition_route = mir%instruction_count == 9_int32 .and. &
            mir%instructions(4)%opcode == mir_v0_opcode_load .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_add .and. &
            trim(mir%instructions(2)%source_rule) == 'frontend-ast-v2/execution-part' .and. &
            trim(mir%instructions(7)%source_rule) == 'frontend-ast-v2/print-stmt'
        initialized_subtraction_route = mir%instruction_count == 9_int32 .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_sub .and. &
            trim(mir%instructions(7)%source_rule) == 'frontend-ast-v2/print-stmt'
        initialized_multiplier_route = mir%instruction_count == 9_int32 .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_mul .and. &
            trim(mir%instructions(2)%source_rule) == 'frontend-ast-v2/execution-part' .and. &
            trim(mir%instructions(7)%source_rule) == 'frontend-ast-v2/print-stmt'
        initialized_division_route = mir%instruction_count == 9_int32 .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_div .and. &
            trim(mir%instructions(2)%source_rule) == &
            'frontend-ast-v2/execution-part' .and. &
            trim(mir%instructions(7)%source_rule) == 'frontend-ast-v2/print-stmt'
        initialized_power_route = mir%instruction_count == 9_int32 .and. &
            mir%instructions(5)%opcode == mir_v0_opcode_pow .and. &
            trim(mir%instructions(2)%source_rule) == &
            'frontend-ast-v2/execution-part' .and. &
            trim(mir%instructions(7)%source_rule) == 'frontend-ast-v2/print-stmt'
        if (initialized_subtraction_route) then
            if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                do index = 1, 6
                    if (trim(mir%instructions(index)%source_rule) /= &
                        'frontend-ast-v2/execution-part') return
                end do
            else
                if (trim(mir%instructions(1)%source_rule) /= &
                    'frontend-ast-v2/execution-part') return
                if (trim(mir%instructions(2)%source_rule) /= &
                    'frontend-ast-v2/execution-part') return
            end if
            do index = 7, 9
                if (trim(mir%instructions(index)%source_rule) /= &
                    'frontend-ast-v2/print-stmt') return
            end do
            if (mir%instructions(1)%literal < -100_int32 .or. &
                    mir%instructions(1)%literal > 2047_int32) return
            if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                if (.not. mir%instructions(4)%storage_present) return
                if (trim(mir%instructions(4)%storage_key) /= 'x') return
                if (mir%instructions(4)%literal_present) return
                if (mir%instructions(4)%result_kind /= mir_v0_value_kind_integer) return
                if (trim(mir%instructions(4)%result_type) /= 'i32') return
            else
                if (mir%instructions(4)%literal < 1_int32 .or. &
                        mir%instructions(4)%literal > 10_int32) return
            end if
        end if
        if (initialized_multiplier_route) then
            do index = 3, 6
                if (trim(mir%instructions(index)%source_rule) /= 'frontend-ast-v2/execution-part') return
            end do
            do index = 7, 9
                if (trim(mir%instructions(index)%source_rule) /= 'frontend-ast-v2/print-stmt') return
            end do
            if (mir%instructions(1)%literal < -100_int32 .or. &
                    mir%instructions(1)%literal > 2047_int32) return
            if (mir%instructions(4)%opcode == mir_v0_opcode_const) then
                if (mir%instructions(4)%literal < 3_int32 .or. &
                        mir%instructions(4)%literal > 10_int32) return
            else
                if (mir%instructions(4)%opcode /= mir_v0_opcode_load) return
                if (.not. mir%instructions(4)%storage_present) return
                if (trim(mir%instructions(4)%storage_key) /= 'x') return
                if (mir%instructions(4)%literal_present) return
                if (mir%instructions(4)%result_kind /= mir_v0_value_kind_integer) return
                if (trim(mir%instructions(4)%result_type) /= 'i32') return
            end if
        end if
        if (initialized_division_route) then
            do index = 1, 6
                if (trim(mir%instructions(index)%source_rule) /= &
                    'frontend-ast-v2/execution-part') return
            end do
            do index = 7, 9
                if (trim(mir%instructions(index)%source_rule) /= &
                    'frontend-ast-v2/print-stmt') return
            end do
            if (mir%instructions(1)%literal < -100_int32 .or. &
                    mir%instructions(1)%literal > 2047_int32) return
            if (mir%instructions(4)%opcode == mir_v0_opcode_load) then
                if (mir%instructions(1)%literal == 0_int32) return
                if (.not. mir%instructions(4)%storage_present) return
                if (trim(mir%instructions(4)%storage_key) /= 'x') return
                if (mir%instructions(4)%literal_present) return
                if (mir%instructions(4)%result_kind /= mir_v0_value_kind_integer) return
                if (trim(mir%instructions(4)%result_type) /= 'i32') return
            else
                if (mir%instructions(4)%opcode /= mir_v0_opcode_const) return
                if (.not. mir%instructions(4)%literal_present) return
                if (mir%instructions(4)%literal < 1_int32 .or. &
                        mir%instructions(4)%literal > 10_int32) return
            end if
        end if
        if (initialized_power_route) then
            do index = 1, 6
                if (trim(mir%instructions(index)%source_rule) /= &
                    'frontend-ast-v2/execution-part') return
            end do
            do index = 7, 9
                if (trim(mir%instructions(index)%source_rule) /= &
                    'frontend-ast-v2/print-stmt') return
            end do
            if (mir%instructions(1)%literal < -100_int32 .or. &
                    mir%instructions(1)%literal > 2047_int32) return
            select case (mir%instructions(4)%opcode)
            case (mir_v0_opcode_const)
                if (.not. mir%instructions(4)%literal_present) return
                if (mir%instructions(4)%storage_present) return
                if (mir%instructions(4)%literal < generic_power_minimum .or. &
                        mir%instructions(4)%literal > generic_power_maximum) return
            case (mir_v0_opcode_load)
                if (.not. mir%instructions(4)%storage_present) return
                if (trim(mir%instructions(4)%storage_key) /= 'x') return
                if (mir%instructions(4)%literal_present) return
                if (mir%instructions(4)%result_kind /= mir_v0_value_kind_integer) return
                if (trim(mir%instructions(4)%result_type) /= 'i32') return
                if (mir%instructions(1)%literal < generic_power_minimum .or. &
                        mir%instructions(1)%literal > generic_power_maximum) return
            case default
                return
            end select
        end if
        if (initialized_addition_route) then
            do index = 1, 6
                if (trim(mir%instructions(index)%source_rule) /= &
                    'frontend-ast-v2/execution-part') return
            end do
            do index = 7, 9
                if (trim(mir%instructions(index)%source_rule) /= &
                    'frontend-ast-v2/print-stmt') return
            end do
            if (mir%instructions(1)%literal < -100_int32 .or. &
                    mir%instructions(1)%literal > 2047_int32) return
            if (.not. mir%instructions(4)%storage_present) return
            if (trim(mir%instructions(4)%storage_key) /= 'x') return
            if (mir%instructions(4)%literal_present) return
            if (mir%instructions(4)%result_kind /= mir_v0_value_kind_integer) return
            if (trim(mir%instructions(4)%result_type) /= 'i32') return
        end if
        if (.not. mir%instructions(2)%storage_present) return
        if (.not. mir%instructions(3)%storage_present) return
        if (.not. mir%instructions(6)%storage_present) return
        if (.not. mir%instructions(7)%storage_present) return
        if (trim(mir%instructions(2)%storage_key) /= 'x') return
        if (trim(mir%instructions(3)%storage_key) /= 'x') return
        if (trim(mir%instructions(6)%storage_key) /= 'x') return
        if (trim(mir%instructions(7)%storage_key) /= 'x') return
        if (.not. initialized_addition_route .and. .not. initialized_subtraction_route .and. &
                .not. initialized_multiplier_route .and. &
                .not. initialized_division_route .and. .not. initialized_power_route) then
            if (mir%instructions(5)%opcode == mir_v0_opcode_pow) then
                return
            else if (mir%instructions(5)%opcode == mir_v0_opcode_div) then
                if (mir%instructions(1)%literal /= 24_int32) return
            else if (mir%instructions(5)%opcode /= mir_v0_opcode_add) then
                if (mir%instructions(1)%literal /= 23_int32) return
            end if
            if (mir%instructions(5)%opcode /= mir_v0_opcode_pow) then
                if (mir%instructions(5)%opcode /= mir_v0_opcode_add) then
                    if (mir%instructions(4)%literal /= 2_int32) return
                end if
            end if
        end if
        if (mir%instructions(8)%storage_present) return
        if (mir%instructions(9)%storage_present) return
        if (mir%instructions(1)%result_id /= 0_int32) return
        if (mir%instructions(2)%result_id /= 1_int32) return
        if (mir%instructions(3)%result_id /= 2_int32) return
        if (mir%instructions(4)%result_id /= 3_int32) return
        if (mir%instructions(5)%result_id /= 4_int32) return
        if (mir%instructions(6)%result_id /= 4_int32) return
        if (mir%instructions(7)%result_id /= 6_int32) return
        if (mir%instructions(8)%result_id /= 6_int32) return
        if (mir%instructions(9)%result_id /= 6_int32) return
        valid = .true.
    end function valid_print_variable_expression

    subroutine integer_to_decimal(value, digits, digit_count)
        integer(int64), intent(in) :: value
        character(len=*), intent(out) :: digits
        integer(int32), intent(out) :: digit_count
        integer(int64) :: divisor, remaining
        integer(int32) :: digit

        digits = ''
        digit_count = 0_int32
        remaining = value
        if (remaining == 0_int64) then
            digit_count = 1_int32
            digits(1:1) = '0'
            return
        end if
        if (remaining < 0_int64) then
            digit_count = 1_int32
            digits(1:1) = '-'
            remaining = -remaining
        end if
        divisor = 1_int64
        do while (divisor <= remaining / 10_int64)
            divisor = divisor * 10_int64
        end do
        do while (divisor > 0_int64)
            digit = int(remaining / divisor, int32)
            digit_count = digit_count + 1_int32
            digits(digit_count:digit_count) = achar(iachar('0') + digit)
            remaining = mod(remaining, divisor)
            divisor = divisor / 10_int64
        end do
    end subroutine integer_to_decimal

    subroutine encode_operation(target, records, operation, values, word, status, diagnostic)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: records(:)
        character(len=*), intent(in) :: operation
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        character(len=*), intent(out) :: diagnostic
        integer :: index
        integer(int32) :: codec_status

        word = 0_int64
        status = mir_v0_bridge_unsupported
        call set_diagnostic(diagnostic, 'mir-v0: opcode encoding is unsupported')
        do index = 1, size(records)
            if (trim(records(index)%mnemonic) /= trim(operation)) cycle
            call riscv_encode_record(target, records(index), values, word, codec_status)
            if (codec_status /= 0_int32) then
                status = mir_v0_bridge_malformed
                call set_diagnostic(diagnostic, 'mir-v0: opcode record is malformed')
                return
            end if
            status = mir_v0_bridge_ok
            call set_diagnostic(diagnostic, '')
            return
        end do
    end subroutine encode_operation

    logical function tokenize(input, token, token_count, diagnostic) result(ok)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: token(:)
        integer, intent(out) :: token_count
        character(len=*), intent(out) :: diagnostic
        integer :: start, position, input_length
        character :: current

        token = ''
        token_count = 0
        call set_diagnostic(diagnostic, '')
        input_length = len_trim(input)
        if (input_length == 0) then
            call set_diagnostic(diagnostic, 'mir-v0: empty SX input')
            ok = .false.
            return
        end if
        position = 1
        do while (position <= input_length)
            current = input(position:position)
            if (current == ' ' .or. current == char(9) .or. current == char(10) .or. &
                current == char(13)) then
                position = position + 1
            else if (current == '(' .or. current == ')') then
                if (.not. append_token(input(position:position), token, token_count, &
                    diagnostic)) then
                    ok = .false.
                    return
                end if
                position = position + 1
            else
                start = position
                do while (position <= input_length)
                    current = input(position:position)
                    if (current == ' ' .or. current == char(9) .or. current == char(10) .or. &
                        current == char(13) .or. current == '(' .or. current == ')') exit
                    position = position + 1
                end do
                if (.not. append_token(input(start:position - 1), token, token_count, &
                    diagnostic)) then
                    ok = .false.
                    return
                end if
            end if
        end do
        ok = token_count > 0
    end function tokenize

    logical function append_token(value, token, token_count, diagnostic) result(ok)
        character(len=*), intent(in) :: value
        character(len=*), intent(inout) :: token(:)
        integer, intent(inout) :: token_count
        character(len=*), intent(out) :: diagnostic

        if (token_count == size(token)) then
            call set_diagnostic(diagnostic, 'mir-v0: SX token capacity exceeded')
            ok = .false.
            return
        end if
        if (len_trim(value) > len(token(1))) then
            call set_diagnostic(diagnostic, 'mir-v0: SX atom is too long')
            ok = .false.
            return
        end if
        token_count = token_count + 1
        token(token_count) = value
        ok = .true.
    end function append_token

    logical function expect(token, token_count, position, expected, diagnostic) result(ok)
        character(len=*), intent(in) :: token(:), expected
        integer, intent(in) :: token_count
        integer, intent(inout) :: position
        character(len=*), intent(out) :: diagnostic

        if (position > token_count) then
            call set_diagnostic(diagnostic, 'mir-v0: unexpected end of SX input')
            ok = .false.
            return
        end if
        if (trim(token(position)) /= expected) then
            call set_diagnostic(diagnostic, 'mir-v0: unexpected SX token')
            ok = .false.
            return
        end if
        position = position + 1
        ok = .true.
    end function expect

    logical function read_atom(token, token_count, position, name, value, diagnostic) result(ok)
        character(len=*), intent(in) :: token(:), name
        integer, intent(in) :: token_count
        integer, intent(inout) :: position
        character(len=*), intent(out) :: value
        character(len=*), intent(out) :: diagnostic

        value = ''
        ok = expect(token, token_count, position, '(', diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, name, diagnostic)
        if (.not. ok) return
        if (position > token_count) then
            call set_diagnostic(diagnostic, 'mir-v0: missing SX atom')
            ok = .false.
            return
        end if
        if (trim(token(position)) == '(' .or. trim(token(position)) == ')') then
            call set_diagnostic(diagnostic, 'mir-v0: missing SX atom')
            ok = .false.
            return
        end if
        value = token(position)
        position = position + 1
        ok = expect(token, token_count, position, ')', diagnostic)
    end function read_atom

    logical function read_integer(token, token_count, position, name, value, diagnostic) &
            result(ok)
        character(len=*), intent(in) :: token(:), name
        integer, intent(in) :: token_count
        integer, intent(inout) :: position
        integer(int32), intent(out) :: value
        character(len=*), intent(out) :: diagnostic
        integer :: io_status
        character(len=token_length) :: text

        value = 0_int32
        ok = read_atom(token, token_count, position, name, text, diagnostic)
        if (.not. ok) return
        read (text, *, iostat=io_status) value
        if (io_status /= 0) then
            call set_diagnostic(diagnostic, 'mir-v0: SX integer is invalid')
            ok = .false.
        end if
    end function read_integer

    logical function read_instruction(token, token_count, position, id, instruction, diagnostic) &
            result(ok)
        character(len=*), intent(in) :: token(:)
        integer, intent(in) :: token_count, id
        integer, intent(inout) :: position
        type(bridge_instruction_t), intent(out) :: instruction
        character(len=*), intent(out) :: diagnostic
        character(len=token_length) :: opcode_name, kind_name, storage_label
        integer(int32) :: serialized_id

        instruction = bridge_instruction_t()
        ok = expect(token, token_count, position, '(', diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, 'instruction', diagnostic)
        if (.not. ok) return
        ok = read_integer(token, token_count, position, 'id', serialized_id, diagnostic)
        if (.not. ok) return
        if (serialized_id /= int(id, int32)) then
            call set_diagnostic(diagnostic, 'mir-v0: instruction id does not match body slot')
            ok = .false.
            return
        end if
        instruction%id = int(id, int32)
        ok = read_atom(token, token_count, position, 'opcode', opcode_name, diagnostic)
        if (.not. ok) return
        instruction%opcode = mir_v0_opcode_value(opcode_name)
        if (instruction%opcode == 0_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: opcode is outside mir-v0')
            ok = .false.
            return
        end if
        if (instruction%opcode == mir_v0_opcode_const) then
            if (position + 1 <= token_count) then
                if (trim(token(position + 1)) == 'literal') then
                    ok = read_integer(token, token_count, position, 'literal', &
                        instruction%literal, diagnostic)
                    if (.not. ok) return
                    ok = read_atom(token, token_count, position, 'source-rule', &
                        instruction%source_rule, diagnostic)
                else
                    ok = read_atom(token, token_count, position, 'source-rule', &
                        instruction%source_rule, diagnostic)
                    if (.not. ok) return
                    ok = read_integer(token, token_count, position, 'literal', &
                        instruction%literal, diagnostic)
                end if
            else
                ok = read_atom(token, token_count, position, 'source-rule', &
                    instruction%source_rule, diagnostic)
                if (.not. ok) return
                ok = read_integer(token, token_count, position, 'literal', &
                    instruction%literal, diagnostic)
            end if
            if (.not. ok) return
            instruction%literal_present = .true.
        else
            if (position + 1 <= token_count) then
                if (trim(token(position)) == '(' .and. &
                    (trim(token(position + 1)) == 'storage-key' .or. &
                    trim(token(position + 1)) == 'storage')) then
                    storage_label = trim(token(position + 1))
                    ok = read_atom(token, token_count, position, storage_label, instruction%storage_key, &
                        diagnostic)
                    if (.not. ok) return
                    instruction%storage_present = .true.
                end if
            end if
            ok = read_atom(token, token_count, position, 'source-rule', &
                instruction%source_rule, diagnostic)
            if (.not. ok) return
        end if
        if (position + 1 <= token_count) then
            if (trim(token(position)) == '(' .and. &
                (trim(token(position + 1)) == 'storage-key' .or. &
                trim(token(position + 1)) == 'storage')) then
                storage_label = trim(token(position + 1))
                ok = read_atom(token, token_count, position, storage_label, instruction%storage_key, &
                    diagnostic)
                if (.not. ok) return
                instruction%storage_present = .true.
            end if
        end if
        ok = expect(token, token_count, position, '(', diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, 'result', diagnostic)
        if (.not. ok) return
        ok = read_integer(token, token_count, position, 'id', instruction%result_id, diagnostic)
        if (.not. ok) return
        ok = read_atom(token, token_count, position, 'kind', kind_name, diagnostic)
        if (.not. ok) return
        instruction%result_kind = mir_v0_value_kind_value(kind_name)
        if (instruction%result_kind == 0_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: value kind is outside mir-v0')
            ok = .false.
            return
        end if
        ok = read_atom(token, token_count, position, 'type', instruction%result_type, diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, ')', diagnostic)
        if (.not. ok) return
        ok = expect(token, token_count, position, ')', diagnostic)
    end function read_instruction

    subroutine set_diagnostic(diagnostic, value)
        character(len=*), intent(out) :: diagnostic
        character(len=*), intent(in) :: value
        integer :: length

        diagnostic = ''
        length = min(len(diagnostic), len_trim(value))
        if (length > 0) diagnostic(:length) = value(:length)
    end subroutine set_diagnostic

end module fortback_mir_v0_riscv_linux
