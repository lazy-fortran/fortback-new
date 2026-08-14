module fortback_riscv_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    integer(int32), parameter, public :: riscv_add = 1_int32
    integer(int32), parameter, public :: riscv_sub = 2_int32
    integer(int32), parameter, public :: riscv_addi = 3_int32
    integer(int32), parameter, public :: riscv_and = 4_int32
    integer(int32), parameter, public :: riscv_or = 5_int32
    integer(int32), parameter, public :: riscv_xor = 6_int32
    integer(int32), parameter, public :: riscv_sll = 7_int32
    integer(int32), parameter, public :: riscv_ori = 8_int32
    integer(int32), parameter, public :: riscv_andi = 9_int32
    integer(int32), parameter, public :: riscv_sra = 10_int32
    integer(int32), parameter, public :: riscv_slli = 11_int32
    integer(int32), parameter, public :: riscv_srli = 12_int32
    integer(int32), parameter, public :: riscv_srai = 13_int32
    integer(int32), parameter, public :: riscv_slti = 14_int32
    integer(int32), parameter, public :: riscv_sltiu = 15_int32
    integer(int32), parameter, public :: riscv_xori = 16_int32

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
        integer(int64) :: operands

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
        if (instruction%kind == riscv_slli .or. instruction%kind == riscv_srli .or. &
            instruction%kind == riscv_srai) then
            if (instruction%immediate < 0_int32 .or. &
                instruction%immediate > 63_int32) then
                status = riscv_invalid_operand
                return
            end if
        else if (instruction%kind == riscv_addi .or. &
                instruction%kind == riscv_ori .or. &
                instruction%kind == riscv_andi .or. &
                instruction%kind == riscv_slti .or. &
                instruction%kind == riscv_sltiu .or. &
                instruction%kind == riscv_xori) then
            if (instruction%immediate < -2048_int32 .or. &
                instruction%immediate > 2047_int32) then
                status = riscv_invalid_operand
                return
            end if
        end if
        operands = ishft(int(instruction%rd, int64), 7)
        operands = ior(operands, ishft(int(instruction%rs1, int64), 15))
        if (instruction%kind == riscv_addi .or. instruction%kind == riscv_ori .or. &
            instruction%kind == riscv_andi .or. instruction%kind == riscv_slti .or. &
            instruction%kind == riscv_sltiu .or. &
            instruction%kind == riscv_xori .or. &
            instruction%kind == riscv_slli .or. &
            instruction%kind == riscv_srli .or. instruction%kind == riscv_srai) then
            if (instruction%kind == riscv_slli .or. instruction%kind == riscv_srli .or. &
                instruction%kind == riscv_srai) then
                operands = ior(operands, ishft(iand(int(instruction%immediate, int64), &
                    63_int64), 20))
            else
                operands = ior(operands, ishft(iand(int(instruction%immediate, int64), &
                    4095_int64), 20))
            end if
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
        integer(int64) :: immediate
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
        if (records(index)%format == 'R') then
            if (trim(records(index)%mnemonic) == 'add') instruction%kind = riscv_add
            if (trim(records(index)%mnemonic) == 'sub') instruction%kind = riscv_sub
            if (trim(records(index)%mnemonic) == 'and') instruction%kind = riscv_and
            if (trim(records(index)%mnemonic) == 'or') instruction%kind = riscv_or
            if (trim(records(index)%mnemonic) == 'xor') instruction%kind = riscv_xor
            if (trim(records(index)%mnemonic) == 'sll') instruction%kind = riscv_sll
            if (trim(records(index)%mnemonic) == 'sra') instruction%kind = riscv_sra
            instruction%rs2 = int(iand(ishft(word, -20), 31_int64), int32)
        else
            if (trim(records(index)%mnemonic) == 'addi') instruction%kind = riscv_addi
            if (trim(records(index)%mnemonic) == 'ori') instruction%kind = riscv_ori
            if (trim(records(index)%mnemonic) == 'andi') instruction%kind = riscv_andi
            if (trim(records(index)%mnemonic) == 'slti') instruction%kind = riscv_slti
            if (trim(records(index)%mnemonic) == 'sltiu') instruction%kind = riscv_sltiu
            if (trim(records(index)%mnemonic) == 'xori') instruction%kind = riscv_xori
            if (trim(records(index)%mnemonic) == 'slli') then
                instruction%kind = riscv_slli
                immediate = iand(ishft(word, -20), 63_int64)
            else if (trim(records(index)%mnemonic) == 'srli') then
                instruction%kind = riscv_srli
                immediate = iand(ishft(word, -20), 63_int64)
            else if (trim(records(index)%mnemonic) == 'srai') then
                instruction%kind = riscv_srai
                immediate = iand(ishft(word, -20), 63_int64)
            else
                immediate = iand(ishft(word, -20), 4095_int64)
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
            if ((kind == riscv_add .and. trim(records(i)%mnemonic) == 'add') .or. &
                (kind == riscv_sub .and. trim(records(i)%mnemonic) == 'sub') .or. &
                (kind == riscv_and .and. trim(records(i)%mnemonic) == 'and') .or. &
                (kind == riscv_or .and. trim(records(i)%mnemonic) == 'or') .or. &
                (kind == riscv_xor .and. trim(records(i)%mnemonic) == 'xor') .or. &
                (kind == riscv_sll .and. trim(records(i)%mnemonic) == 'sll') .or. &
                (kind == riscv_sra .and. trim(records(i)%mnemonic) == 'sra') .or. &
                (kind == riscv_slli .and. trim(records(i)%mnemonic) == 'slli') .or. &
                (kind == riscv_srli .and. trim(records(i)%mnemonic) == 'srli') .or. &
                (kind == riscv_srai .and. trim(records(i)%mnemonic) == 'srai') .or. &
                (kind == riscv_addi .and. trim(records(i)%mnemonic) == 'addi') .or. &
                (kind == riscv_ori .and. trim(records(i)%mnemonic) == 'ori') .or. &
                (kind == riscv_andi .and. trim(records(i)%mnemonic) == 'andi') .or. &
                (kind == riscv_slti .and. trim(records(i)%mnemonic) == 'slti') .or. &
                (kind == riscv_sltiu .and. trim(records(i)%mnemonic) == 'sltiu') .or. &
                (kind == riscv_xori .and. trim(records(i)%mnemonic) == 'xori')) then
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
