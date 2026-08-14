module fortback_aarch64_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    integer(int32), parameter, public :: aarch64_add = 1_int32
    integer(int32), parameter, public :: aarch64_sub = 2_int32
    integer(int32), parameter, public :: aarch64_nop = 3_int32
    integer(int32), parameter, public :: aarch64_adr = 4_int32
    integer(int32), parameter, public :: aarch64_adrp = 5_int32

    integer(int32), parameter, public :: aarch64_ok = 0_int32
    integer(int32), parameter, public :: aarch64_invalid_target = 1_int32
    integer(int32), parameter, public :: aarch64_invalid_operand = 2_int32
    integer(int32), parameter, public :: aarch64_unsupported = 3_int32
    integer(int32), parameter, public :: aarch64_malformed = 4_int32
    integer(int64), parameter :: aarch64_addsub_shift_bit = int(z'00400000', int64)

    type, public :: aarch64_instruction_t
        integer(int32) :: kind = 0_int32
        integer(int32) :: rd = 0_int32
        integer(int32) :: rn = 0_int32
        integer(int32) :: immediate = 0_int32
    end type aarch64_instruction_t

    public :: aarch64_encode_fixed
    public :: aarch64_decode_fixed

contains

    subroutine aarch64_encode_fixed(target, instruction, word, status, records)
        type(target_ir_t), intent(in) :: target
        type(aarch64_instruction_t), intent(in) :: instruction
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        type(aarch64_encoding_record_t), intent(in), optional :: records(:)
        integer :: index
        integer(int64) :: operands, immediate

        word = 0_int64
        status = validate_target(target)
        if (status /= aarch64_ok) return
        status = validate_operands(instruction)
        if (status /= aarch64_ok) return
        if (.not. present(records)) then
            status = aarch64_unsupported
            return
        end if
        index = find_record(instruction%kind, records)
        if (index == 0) then
            status = aarch64_unsupported
            return
        end if

        if (instruction%kind == aarch64_nop) then
            word = records(index)%match
        else if (instruction%kind == aarch64_adr .or. instruction%kind == aarch64_adrp) then
            immediate = iand(int(instruction%immediate, int64), int(z'001FFFFF', int64))
            operands = ishft(iand(immediate, 3_int64), 29)
            operands = ior(operands, ishft(iand(immediate, int(z'001FFFFC', int64)), 3))
            operands = ior(operands, int(instruction%rd, int64))
            word = ior(records(index)%match, operands)
        else
            operands = ishft(int(instruction%immediate, int64), 10)
            operands = ior(operands, ishft(int(instruction%rn, int64), 5))
            operands = ior(operands, int(instruction%rd, int64))
            word = ior(records(index)%match, operands)
        end if
    end subroutine aarch64_encode_fixed

    subroutine aarch64_decode_fixed(target, word, instruction, status, records)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        type(aarch64_instruction_t), intent(out) :: instruction
        integer(int32), intent(out) :: status
        type(aarch64_encoding_record_t), intent(in), optional :: records(:)
        integer :: index

        instruction = aarch64_instruction_t()
        status = validate_target(target)
        if (status /= aarch64_ok) return
        if (word < 0_int64 .or. word > int(z'FFFFFFFF', int64)) then
            status = aarch64_malformed
            return
        end if
        if (.not. present(records)) then
            status = aarch64_unsupported
            return
        end if
        index = find_word(word, records)
        if (index == 0) then
            status = aarch64_unsupported
            return
        end if
        if ((trim(records(index)%name) == 'ADD_64_addsub_imm' .or. &
            trim(records(index)%name) == 'SUB_64_addsub_imm') .and. &
            iand(word, aarch64_addsub_shift_bit) /= 0_int64) then
            status = aarch64_unsupported
            return
        end if

        if (trim(records(index)%name) == 'NOP_HI_hints') then
            instruction%kind = aarch64_nop
        else if (trim(records(index)%name) == 'ADR_only_pcreladdr') then
            instruction%kind = aarch64_adr
            instruction%rd = int(iand(word, 31_int64), int32)
            instruction%immediate = decode_adr_immediate(word)
        else if (trim(records(index)%name) == 'ADRP_only_pcreladdr') then
            instruction%kind = aarch64_adrp
            instruction%rd = int(iand(word, 31_int64), int32)
            instruction%immediate = decode_adrp_immediate(word)
        else if (trim(records(index)%name) == 'ADD_64_addsub_imm') then
            instruction%kind = aarch64_add
            instruction%immediate = int(iand(ishft(word, -10), 4095_int64), int32)
            instruction%rn = int(iand(ishft(word, -5), 31_int64), int32)
            instruction%rd = int(iand(word, 31_int64), int32)
        else if (trim(records(index)%name) == 'SUB_64_addsub_imm') then
            instruction%kind = aarch64_sub
            instruction%immediate = int(iand(ishft(word, -10), 4095_int64), int32)
            instruction%rn = int(iand(ishft(word, -5), 31_int64), int32)
            instruction%rd = int(iand(word, 31_int64), int32)
        else
            status = aarch64_unsupported
            return
        end if
        status = aarch64_ok
    end subroutine aarch64_decode_fixed

    pure integer function find_record(kind, records)
        integer(int32), intent(in) :: kind
        type(aarch64_encoding_record_t), intent(in) :: records(:)
        integer :: i

        find_record = 0
        do i = 1, size(records)
            if ((kind == aarch64_add .and. trim(records(i)%name) == 'ADD_64_addsub_imm') .or. &
                (kind == aarch64_sub .and. trim(records(i)%name) == 'SUB_64_addsub_imm') .or. &
                (kind == aarch64_nop .and. trim(records(i)%name) == 'NOP_HI_hints') .or. &
                (kind == aarch64_adr .and. trim(records(i)%name) == 'ADR_only_pcreladdr') .or. &
                (kind == aarch64_adrp .and. trim(records(i)%name) == 'ADRP_only_pcreladdr')) then
                find_record = i
                return
            end if
        end do
    end function find_record

    pure integer function find_word(word, records)
        integer(int64), intent(in) :: word
        type(aarch64_encoding_record_t), intent(in) :: records(:)
        integer :: i

        find_word = 0
        do i = 1, size(records)
            if (iand(word, records(i)%mask) == records(i)%match) then
                if (trim(records(i)%name) == 'NOP_HI_hints' .or. &
                    trim(records(i)%name) == 'ADD_64_addsub_imm' .or. &
                    trim(records(i)%name) == 'SUB_64_addsub_imm' .or. &
                    trim(records(i)%name) == 'ADR_only_pcreladdr' .or. &
                    trim(records(i)%name) == 'ADRP_only_pcreladdr') then
                    find_word = i
                    return
                end if
            end if
        end do
    end function find_word

    pure integer(int32) function validate_target(target)
        type(target_ir_t), intent(in) :: target

        validate_target = aarch64_invalid_target
        if (trim(target%architecture) /= 'aarch64') return
        if (target%word_bits /= 32_int32) return
        if (.not. target%little_endian) return
        validate_target = aarch64_ok
    end function validate_target

    pure integer(int32) function validate_operands(instruction)
        type(aarch64_instruction_t), intent(in) :: instruction

        validate_operands = aarch64_invalid_operand
        if (instruction%rd < 0_int32 .or. instruction%rd > 31_int32) return
        if (instruction%kind == aarch64_adr .or. instruction%kind == aarch64_adrp) then
            if (instruction%immediate < -1048576_int32 .or. instruction%immediate > 1048575_int32) return
            validate_operands = aarch64_ok
            return
        end if
        if (instruction%rn < 0_int32 .or. instruction%rn > 31_int32) return
        if (instruction%immediate < 0_int32 .or. instruction%immediate > 4095_int32) return
        validate_operands = aarch64_ok
    end function validate_operands

    pure integer(int32) function decode_adr_immediate(word)
        integer(int64), intent(in) :: word
        integer(int64) :: immediate

        immediate = ishft(iand(ishft(word, -5), int(z'0007FFFF', int64)), 2)
        immediate = ior(immediate, iand(ishft(word, -29), 3_int64))
        if (immediate >= 1048576_int64) immediate = immediate - 2097152_int64
        decode_adr_immediate = int(immediate, int32)
    end function decode_adr_immediate

    pure integer(int32) function decode_adrp_immediate(word)
        integer(int64), intent(in) :: word
        integer(int64) :: immediate

        immediate = ishft(iand(ishft(word, -5), int(z'0007FFFF', int64)), 2)
        immediate = ior(immediate, iand(ishft(word, -29), 3_int64))
        if (immediate >= 1048576_int64) immediate = immediate - 2097152_int64
        decode_adrp_immediate = int(immediate, int32)
    end function decode_adrp_immediate

end module fortback_aarch64_fixture
