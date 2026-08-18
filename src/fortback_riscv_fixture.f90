module fortback_riscv_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_opcode_table, only: riscv_add, riscv_addi, riscv_and, riscv_andi, &
        riscv_immediate_width_for_mnemonic, riscv_kind_for_mnemonic, riscv_or, riscv_ori, riscv_sll, &
        riscv_slli, riscv_srai, riscv_srli, riscv_sra, riscv_slti, riscv_sltiu, riscv_sub, &
        riscv_xor, riscv_xori
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    public :: riscv_add, riscv_addi, riscv_and, riscv_andi, riscv_or, riscv_ori, riscv_sll
    public :: riscv_slli, riscv_srai, riscv_srli, riscv_sra, riscv_slti, riscv_sltiu, riscv_sub
    public :: riscv_xor, riscv_xori

    integer(int32), parameter, public :: riscv_ok = 0_int32
    integer(int32), parameter, public :: riscv_invalid_target = 1_int32
    integer(int32), parameter, public :: riscv_invalid_operand = 2_int32
    integer(int32), parameter, public :: riscv_unsupported = 3_int32
    integer(int32), parameter, public :: riscv_malformed = 4_int32

    type, public :: riscv_instruction_t
        integer(int32) :: kind = 0_int32
        integer(int32) :: rd = 0_int32
        integer(int32) :: rs1 = 0_int32
        integer(int32) :: rs2 = 0_int32
        integer(int32) :: immediate = 0_int32
    end type riscv_instruction_t

    public :: riscv_encode_integer
    public :: riscv_decode_integer

contains

    subroutine riscv_encode_integer(target, instruction, word, status, records)
        type(target_ir_t), intent(in) :: target
        type(riscv_instruction_t), intent(in) :: instruction
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        type(riscv_opcode_record_t), intent(in), optional :: records(:)
        integer :: index
        integer(int64) :: operands, immediate_value_mask
        integer(int32) :: immediate_width

        word = 0_int64
        status = validate_target(target)
        if (status /= riscv_ok) return
        status = validate_registers(instruction)
        if (status /= riscv_ok) return

        if (.not. present(records)) then
            status = riscv_unsupported
            return
        end if
        index = find_record(instruction%kind, records)
        if (index == 0) then
            status = riscv_unsupported
            return
        end if
        immediate_width = record_immediate_width(records(index))
        if (immediate_width > 0_int32) then
            if (immediate_width == 12_int32) then
                if (instruction%immediate < -2048_int32 .or. &
                    instruction%immediate > 2047_int32) then
                    status = riscv_invalid_operand
                    return
                end if
            else if (instruction%immediate < 0_int32 .or. int(instruction%immediate, int64) > &
                    ishft(1_int64, immediate_width) - 1_int64) then
                status = riscv_invalid_operand
                return
            end if
        end if
        operands = ishft(int(instruction%rd, int64), 7)
        operands = ior(operands, ishft(int(instruction%rs1, int64), 15))
        if (immediate_width > 0_int32) then
            immediate_value_mask = ishft(1_int64, immediate_width) - 1_int64
            operands = ior(operands, ishft(iand(int(instruction%immediate, int64), &
                immediate_value_mask), 20))
        else
            operands = ior(operands, ishft(int(instruction%rs2, int64), 20))
        end if
        word = ior(records(index)%match, operands)
    end subroutine riscv_encode_integer

    subroutine riscv_decode_integer(target, word, instruction, status, records)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        type(riscv_instruction_t), intent(out) :: instruction
        integer(int32), intent(out) :: status
        type(riscv_opcode_record_t), intent(in), optional :: records(:)
        integer(int64) :: immediate, immediate_value_mask
        integer(int32) :: immediate_width
        integer :: index

        instruction = riscv_instruction_t()
        status = validate_target(target)
        if (status /= riscv_ok) return
        if (word < 0_int64 .or. word > int(z'FFFFFFFF', int64)) then
            status = riscv_malformed
            return
        end if
        if (.not. present(records)) then
            status = riscv_unsupported
            return
        end if

        instruction%rd = int(iand(ishft(word, -7), 31_int64), int32)
        instruction%rs1 = int(iand(ishft(word, -15), 31_int64), int32)
        index = find_word(word, records)
        if (index == 0) then
            status = riscv_unsupported
            return
        end if
        instruction%kind = riscv_kind_for_mnemonic(records(index)%mnemonic)
        if (records(index)%format == 'R') then
            instruction%rs2 = int(iand(ishft(word, -20), 31_int64), int32)
        else
            immediate_width = record_immediate_width(records(index))
            if (immediate_width == 0_int32) then
                status = riscv_malformed
                return
            end if
            immediate_value_mask = ishft(1_int64, immediate_width) - 1_int64
            immediate = iand(ishft(word, -20), immediate_value_mask)
            if (immediate_width == 12_int32) then
                if (immediate >= 2048_int64) immediate = immediate - 4096_int64
            end if
            instruction%immediate = int(immediate, int32)
        end if
    end subroutine riscv_decode_integer

    pure integer function find_record(kind, records)
        integer(int32), intent(in) :: kind
        type(riscv_opcode_record_t), intent(in) :: records(:)
        integer :: i

        find_record = 0
        do i = 1, size(records)
            if (kind == riscv_kind_for_mnemonic(records(i)%mnemonic)) then
                find_record = i
                return
            end if
        end do
    end function find_record

    pure integer function find_word(word, records)
        integer(int64), intent(in) :: word
        type(riscv_opcode_record_t), intent(in) :: records(:)
        integer :: i

        find_word = 0
        do i = 1, size(records)
            if (iand(word, records(i)%mask) == records(i)%match) then
                find_word = i
                return
            end if
        end do
    end function find_word

    pure integer(int32) function record_immediate_width(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_immediate_width = riscv_immediate_width_for_mnemonic(record%mnemonic)
        if (record_immediate_width == 0_int32 .and. record%format == 'I') then
            record_immediate_width = 12_int32
        end if
    end function record_immediate_width

    pure integer(int32) function validate_target(target)
        type(target_ir_t), intent(in) :: target

        validate_target = riscv_invalid_target
        if (trim(target%architecture) /= 'riscv64') return
        if (target%word_bits /= 64_int32) return
        if (.not. target%little_endian) return
        validate_target = riscv_ok
    end function validate_target

    pure integer(int32) function validate_registers(instruction)
        type(riscv_instruction_t), intent(in) :: instruction

        validate_registers = riscv_invalid_operand
        if (instruction%rd < 0_int32 .or. instruction%rd > 31_int32) return
        if (instruction%rs1 < 0_int32 .or. instruction%rs1 > 31_int32) return
        if (instruction%kind == riscv_add .or. instruction%kind == riscv_sub .or. &
            instruction%kind == riscv_and .or. instruction%kind == riscv_or .or. &
            instruction%kind == riscv_xor .or. instruction%kind == riscv_sll .or. &
            instruction%kind == riscv_sra) then
            if (instruction%rs2 < 0_int32 .or. instruction%rs2 > 31_int32) return
        end if
        validate_registers = riscv_ok
    end function validate_registers

end module fortback_riscv_fixture
