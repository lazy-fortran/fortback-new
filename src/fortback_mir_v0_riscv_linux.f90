module fortback_mir_v0_riscv_linux
    use iso_fortran_env, only: int8, int32, int64
    use fortback_elf64, only: elf64_machine_riscv, elf64_target_t, &
        write_elf64_executable
    use fortback_mir_v0_bridge_metadata, only: mir_v0_opcode_add, mir_v0_opcode_const, &
        mir_v0_opcode_load, mir_v0_opcode_value, &
        mir_v0_value_kind_value
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

    integer, parameter :: token_capacity = 2048
    integer, parameter :: token_length = 256
    integer, parameter :: instruction_capacity = 48

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
        type(riscv_opcode_record_t) :: records(8)
        integer(int64) :: words(41), values(3)
        integer(int32) :: count, index, source_status
        character(len=16) :: operation
        character(len=512) :: opcode_text
        integer(int32) :: emitted_count
        logical :: storage_route, storage_sequence_route, storage_sequence_3_route
        logical :: storage_sequence_4_route
        logical :: storage_sequence_5_route
        logical :: storage_sequence_6_route
        logical :: storage_sequence_generated_route
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
            new_line('a')//'ld rd rs1 imm12 14..12=3 6..2=0x00 1..0=3'// &
            new_line('a')//'sd rs2 rs1 imm12 14..12=3 6..2=0x08 1..0=3'// &
            new_line('a')//trim(mir_v0_riscv_linux_ecall_operation)// &
            ' rd rs1 imm12 '//trim(mir_v0_riscv_linux_ecall_encoding)
        call import_riscv_opcodes(opcode_text, opcode_source, records, count, source_status)
        if (source_status /= riscv_source_ok .or. count /= 8_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: machine record import failed')
            status = mir_v0_bridge_malformed
            return
        end if

        storage_route = mir%instruction_count == 5_int32 .and. &
            (mir%instructions(1)%storage_present .or. mir%instructions(4)%storage_present)
        storage_sequence_route = mir%instruction_count == 7_int32
        storage_sequence_3_route = mir%instruction_count == 11_int32
        storage_sequence_4_route = mir%instruction_count == 15_int32
        storage_sequence_5_route = mir%instruction_count == 19_int32
        storage_sequence_6_route = mir%instruction_count == 23_int32
        storage_sequence_generated_route = mir%instruction_count == 27_int32 .or. &
            mir%instruction_count == 31_int32 .or. mir%instruction_count == 35_int32 .or. &
            mir%instruction_count == 39_int32
        if (storage_sequence_generated_route) then
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
        if (.not. storage_route .and. .not. storage_sequence_route .and. &
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

        ok = .false.
        status = mir_v0_bridge_out_of_scope
        call set_diagnostic(diagnostic, '')
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
        if (.not. mir_v0_bridge_policy_instruction_count_matches(mir%name, &
            mir%instructions(1)%source_rule, mir%instruction_count)) then
            call set_diagnostic(diagnostic, 'mir-v0: function is out of scope')
            return
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
                call set_diagnostic(diagnostic, 'mir-v0: storage identity is out of scope')
                return
            end if
            if (.not. mir_v0_bridge_policy_accepts(mir%name, mir%instruction_count, &
                int(index - 1, int32), mir%instructions(index)%opcode, &
                mir%instructions(index)%result_id, &
                mir%instructions(index)%result_kind, mir%instructions(index)%result_type, &
                mir%instructions(index)%source_rule, mir%instructions(index)%literal_present, &
                mir%instructions(index)%literal)) then
                call set_diagnostic(diagnostic, 'mir-v0: witness is out of scope')
                return
            end if
        end do
        ok = .true.
        status = mir_v0_bridge_ok
    end function validate_scope

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
