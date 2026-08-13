module fortback_aarch64_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    integer(int32), parameter, public :: aarch64_add = 1_int32
    integer(int32), parameter, public :: aarch64_sub = 2_int32

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
        integer(int64) :: operands

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

        operands = ishft(int(instruction%immediate, int64), 10)
        operands = ior(operands, ishft(int(instruction%rn, int64), 5))
        operands = ior(operands, int(instruction%rd, int64))
        word = ior(records(index)%match, operands)
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
        if (iand(word, aarch64_addsub_shift_bit) /= 0_int64) then
            status = aarch64_unsupported
            return
        end if

        if (trim(records(index)%name) == 'ADD_64_addsub_imm') then
            instruction%kind = aarch64_add
        else if (trim(records(index)%name) == 'SUB_64_addsub_imm') then
            instruction%kind = aarch64_sub
        else
            status = aarch64_unsupported
            return
        end if
        instruction%immediate = int(iand(ishft(word, -10), 4095_int64), int32)
        instruction%rn = int(iand(ishft(word, -5), 31_int64), int32)
        instruction%rd = int(iand(word, 31_int64), int32)
        status = aarch64_ok
    end subroutine aarch64_decode_fixed

    pure integer function find_record(kind, records)
        integer(int32), intent(in) :: kind
        type(aarch64_encoding_record_t), intent(in) :: records(:)
        integer :: i

        find_record = 0
        do i = 1, size(records)
            if ((kind == aarch64_add .and. trim(records(i)%name) == 'ADD_64_addsub_imm') .or. &
                (kind == aarch64_sub .and. trim(records(i)%name) == 'SUB_64_addsub_imm')) then
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
                if (trim(records(i)%name) == 'ADD_64_addsub_imm' .or. &
                    trim(records(i)%name) == 'SUB_64_addsub_imm') then
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
        if (instruction%rn < 0_int32 .or. instruction%rn > 31_int32) return
        if (instruction%immediate < 0_int32 .or. instruction%immediate > 4095_int32) return
        validate_operands = aarch64_ok
    end function validate_operands

end module fortback_aarch64_fixture
