module fortback_mir_v0_riscv_linux
    use iso_fortran_env, only: int8, int32, int64
    use fortback_elf64, only: elf64_machine_riscv, elf64_target_t, &
        write_elf64_executable
    use fortback_mir_v0_bridge_metadata, only: mir_v0_opcode_value, &
        mir_v0_value_kind_value
    use fortback_mir_v0_riscv_linux_ecall_policy, only: &
        mir_v0_riscv_linux_ecall_encoding, mir_v0_riscv_linux_ecall_operation, &
        mir_v0_riscv_linux_ecall_operands
    use fortback_mir_v0_riscv_linux_bridge_policy, only: &
        mir_v0_bridge_policy_accepts, mir_v0_bridge_policy_function_supported, &
        mir_v0_bridge_policy_instruction_count_for, mir_v0_bridge_policy_opcode_supported
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

    integer, parameter :: token_capacity = 128
    integer, parameter :: token_length = 256
    integer, parameter :: instruction_capacity = 16

    type :: bridge_instruction_t
        integer(int32) :: id = 0_int32
        integer(int32) :: opcode = 0_int32
        integer(int32) :: result_id = 0_int32
        integer(int32) :: result_kind = 0_int32
        character(len=token_length) :: result_type = ''
        character(len=token_length) :: source_rule = ''
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
        type(riscv_opcode_record_t) :: records(5)
        integer(int64) :: words(3), values(3)
        integer(int32) :: count, source_status
        character(len=256) :: opcode_text
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
        opcode_text = 'addi rd rs1 imm12 14..12=0 6..2=0x04 1..0=3'// &
            new_line('a')//'mul rd rs1 rs2 31..25=1 14..12=0 6..2=0x0c 1..0=3'// &
            new_line('a')//'div rd rs1 rs2 31..25=1 14..12=4 6..2=0x0c 1..0=3'// &
            new_line('a')//'sub rd rs1 rs2 31..25=0x20 14..12=0 6..2=0x0c 1..0=3'// &
            new_line('a')//trim(mir_v0_riscv_linux_ecall_operation)// &
            ' rd rs1 imm12 '//trim(mir_v0_riscv_linux_ecall_encoding)
        call import_riscv_opcodes(opcode_text, opcode_source, records, count, source_status)
        if (source_status /= riscv_source_ok .or. count /= 5_int32) then
            call set_diagnostic(diagnostic, 'mir-v0: machine record import failed')
            status = mir_v0_bridge_malformed
            return
        end if

        if (mir%instruction_count == 3_int32 .and. mir%instructions(1)%opcode == &
            mir_v0_opcode_value('mul')) then
            values = [10_int64, 0_int64, 0_int64]
            call encode_operation(target, records, 'mul', values, words(1), status, diagnostic)
        else if (mir%instruction_count == 3_int32 .and. mir%instructions(1)%opcode == &
                mir_v0_opcode_value('div')) then
            values = [10_int64, 0_int64, 0_int64]
            call encode_operation(target, records, 'div', values, words(1), status, diagnostic)
        else if (mir%instruction_count == 3_int32 .and. mir%instructions(1)%opcode == &
                mir_v0_opcode_value('sub')) then
            values = [10_int64, 0_int64, 0_int64]
            call encode_operation(target, records, 'sub', values, words(1), status, diagnostic)
        else
            values = [10_int64, 0_int64, 0_int64]
            call encode_operation(target, records, 'addi', values, words(1), status, diagnostic)
        end if
        if (status /= mir_v0_bridge_ok) return
        values = [17_int64, 0_int64, 93_int64]
        call encode_operation(target, records, 'addi', values, words(2), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        values = mir_v0_riscv_linux_ecall_operands
        call encode_operation(target, records, mir_v0_riscv_linux_ecall_operation, values, &
            words(3), status, diagnostic)
        if (status /= mir_v0_bridge_ok) return
        call write_elf64_executable(metadata, target_source, words(1:3), artifact%bytes, &
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
        if (mir%instruction_count /= mir_v0_bridge_policy_instruction_count_for( &
            mir%name, mir%instructions(1)%source_rule)) then
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
            if (.not. mir_v0_bridge_policy_accepts(mir%name, int(index - 1, int32), &
                mir%instructions(index)%opcode, mir%instructions(index)%result_id, &
                mir%instructions(index)%result_kind, mir%instructions(index)%result_type, &
                mir%instructions(index)%source_rule)) then
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
        character(len=token_length) :: opcode_name, kind_name
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
        ok = read_atom(token, token_count, position, 'source-rule', &
            instruction%source_rule, diagnostic)
        if (.not. ok) return
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
