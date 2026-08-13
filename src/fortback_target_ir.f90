module fortback_target_ir
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: target_ir_t
    public :: make_target_ir
    public :: target_ir_valid
    public :: target_ir_word_bytes

    type :: target_ir_t
        character(len=32) :: architecture = ''
        integer(int32) :: word_bits = 0_int32
        logical :: little_endian = .false.
    end type target_ir_t

contains

    pure function make_target_ir(architecture, word_bits, little_endian) &
            result(target)
        character(len=*), intent(in) :: architecture
        integer(int32), intent(in) :: word_bits
        logical, intent(in) :: little_endian
        type(target_ir_t) :: target

        target%architecture = architecture
        target%word_bits = word_bits
        target%little_endian = little_endian
    end function make_target_ir

    pure logical function target_ir_valid(target)
        type(target_ir_t), intent(in) :: target

        target_ir_valid = len_trim(target%architecture) > 0 &
            .and. target%word_bits > 0_int32 &
            .and. mod(target%word_bits, 8_int32) == 0_int32
    end function target_ir_valid

    pure integer(int32) function target_ir_word_bytes(target)
        type(target_ir_t), intent(in) :: target

        if (target%word_bits <= 0_int32) then
            target_ir_word_bytes = 0_int32
        else
            target_ir_word_bytes = (target%word_bits + 7_int32) / 8_int32
        end if
    end function target_ir_word_bytes

end module fortback_target_ir
