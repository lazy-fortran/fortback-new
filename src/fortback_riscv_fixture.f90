module fortback_riscv_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    integer(int32), parameter, public :: riscv_add = 1_int32
    integer(int32), parameter, public :: riscv_sub = 2_int32
    integer(int32), parameter, public :: riscv_addi = 3_int32

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

    subroutine riscv_encode_integer(target, instruction, word, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_instruction_t), intent(in) :: instruction
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int64) :: funct7

        word = 0_int64
        status = validate_target(target)
        if (status /= riscv_ok) return
        status = validate_registers(instruction)
        if (status /= riscv_ok) return

        select case (instruction%kind)
        case (riscv_add)
            funct7 = 0_int64
        case (riscv_sub)
            funct7 = 32_int64
        case default
            if (instruction%kind == riscv_addi) then
                if (instruction%immediate < -2048_int32 .or. &
                    instruction%immediate > 2047_int32) then
                    status = riscv_invalid_operand
                    return
                end if
                word = ior(ishft(iand(int(instruction%immediate, int64), &
                    4095_int64), 20), ishft(int(instruction%rs1, int64), 15))
                word = ior(word, ishft(int(instruction%rd, int64), 7))
                word = ior(word, 19_int64)
                return
            end if
            status = riscv_unsupported
            return
        end select

        word = ior(ishft(funct7, 25), ishft(int(instruction%rs2, int64), 20))
        word = ior(word, ishft(int(instruction%rs1, int64), 15))
        word = ior(word, ishft(int(instruction%rd, int64), 7))
        word = ior(word, 51_int64)
    end subroutine riscv_encode_integer

    subroutine riscv_decode_integer(target, word, instruction, status)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        type(riscv_instruction_t), intent(out) :: instruction
        integer(int32), intent(out) :: status
        integer(int64) :: opcode, funct3, funct7, immediate

        instruction = riscv_instruction_t()
        status = validate_target(target)
        if (status /= riscv_ok) return
        if (word < 0_int64 .or. word > int(z'FFFFFFFF', int64)) then
            status = riscv_malformed
            return
        end if

        opcode = iand(word, 127_int64)
        funct3 = iand(ishft(word, -12), 7_int64)
        instruction%rd = int(iand(ishft(word, -7), 31_int64), int32)
        instruction%rs1 = int(iand(ishft(word, -15), 31_int64), int32)
        if (opcode == 51_int64 .and. funct3 == 0_int64) then
            funct7 = iand(ishft(word, -25), 127_int64)
            if (funct7 == 0_int64) then
                instruction%kind = riscv_add
            else if (funct7 == 32_int64) then
                instruction%kind = riscv_sub
            else
                status = riscv_unsupported
                return
            end if
            instruction%rs2 = int(iand(ishft(word, -20), 31_int64), int32)
        else if (opcode == 19_int64 .and. funct3 == 0_int64) then
            instruction%kind = riscv_addi
            immediate = iand(ishft(word, -20), 4095_int64)
            if (immediate >= 2048_int64) immediate = immediate - 4096_int64
            instruction%immediate = int(immediate, int32)
        else
            status = riscv_unsupported
            return
        end if
    end subroutine riscv_decode_integer

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
        if (instruction%kind == riscv_add .or. instruction%kind == riscv_sub) then
            if (instruction%rs2 < 0_int32 .or. instruction%rs2 > 31_int32) return
        end if
        validate_registers = riscv_ok
    end function validate_registers

end module fortback_riscv_fixture
