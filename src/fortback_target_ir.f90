module fortback_target_ir
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: source_ref_t
    public :: target_ir_t
    public :: make_source_ref
    public :: make_target_ir
    public :: source_ref_valid
    public :: target_ir_valid
    public :: target_ir_word_bytes

    type :: source_ref_t
        character(len=64) :: artifact = ''
        character(len=64) :: object = ''
        character(len=64) :: source_hash = ''
    end type source_ref_t

    type :: target_ir_t
        character(len=32) :: architecture = ''
        integer(int32) :: word_bits = 0_int32
        logical :: little_endian = .false.
        type(source_ref_t) :: source
    end type target_ir_t

contains

    pure function make_source_ref(artifact, object, source_hash) result(source)
        character(len=*), intent(in) :: artifact, object, source_hash
        type(source_ref_t) :: source

        source%artifact = artifact
        source%object = object
        source%source_hash = source_hash
    end function make_source_ref

    pure function make_target_ir(architecture, word_bits, little_endian, source) &
            result(target)
        character(len=*), intent(in) :: architecture
        integer(int32), intent(in) :: word_bits
        logical, intent(in) :: little_endian
        type(source_ref_t), intent(in) :: source
        type(target_ir_t) :: target

        target%architecture = architecture
        target%word_bits = word_bits
        target%little_endian = little_endian
        target%source = source
    end function make_target_ir

    pure logical function source_ref_valid(source)
        type(source_ref_t), intent(in) :: source

        source_ref_valid = len_trim(source%artifact) > 0
        if (len_trim(source%object) == 0) source_ref_valid = .false.
        if (len_trim(source%source_hash) == 0) source_ref_valid = .false.
    end function source_ref_valid

    pure logical function target_ir_valid(target)
        type(target_ir_t), intent(in) :: target

        target_ir_valid = len_trim(target%architecture) > 0 &
            .and. target%word_bits > 0_int32 &
            .and. mod(target%word_bits, 8_int32) == 0_int32
        if (.not. source_ref_valid(target%source)) target_ir_valid = .false.
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
