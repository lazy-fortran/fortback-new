module fortback_aarch64_elf64
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_fixture, only: aarch64_encode_fixed, aarch64_instruction_t, &
        aarch64_invalid_operand, aarch64_malformed, aarch64_ok, aarch64_unsupported
    use fortback_aarch64_source, only: aarch64_encoding_record_t
    use fortback_elf64, only: elf64_empty_word, elf64_invalid_source, &
        elf64_invalid_target, elf64_io_error, elf64_ok, elf64_target_t, &
        write_elf64_object_to_unit
    use fortback_target_ir, only: source_ref_t, source_ref_valid, target_ir_t
    implicit none
    private

    integer(int32), parameter, public :: aarch64_elf_ok = elf64_ok
    integer(int32), parameter, public :: aarch64_elf_invalid_target = elf64_invalid_target
    integer(int32), parameter, public :: aarch64_elf_invalid_source = elf64_invalid_source
    integer(int32), parameter, public :: aarch64_elf_empty_word = elf64_empty_word
    integer(int32), parameter, public :: aarch64_elf_io_error = elf64_io_error
    integer(int32), parameter, public :: aarch64_elf_invalid_operand = aarch64_invalid_operand
    integer(int32), parameter, public :: aarch64_elf_unsupported = aarch64_unsupported
    integer(int32), parameter, public :: aarch64_elf_malformed = aarch64_malformed

    public :: write_aarch64_elf64_object_to_unit

contains

    subroutine write_aarch64_elf64_object_to_unit(metadata, source, instruction, records, &
            unit, status)
        type(elf64_target_t), intent(in) :: metadata
        type(source_ref_t), intent(in) :: source
        type(aarch64_instruction_t), intent(in) :: instruction
        type(aarch64_encoding_record_t), intent(in) :: records(:)
        integer, intent(in) :: unit
        integer(int32), intent(out) :: status
        integer(int32) :: encode_status
        integer(int64) :: word
        type(target_ir_t) :: encoding_target

        status = aarch64_elf_invalid_source
        if (.not. source_ref_valid(source)) return

        encoding_target = metadata%target
        encoding_target%word_bits = 32_int32
        call aarch64_encode_fixed(encoding_target, instruction, word, encode_status, records)
        if (encode_status /= aarch64_ok) then
            status = encode_status
            return
        end if

        call write_elf64_object_to_unit(metadata, source, word, unit, status)
    end subroutine write_aarch64_elf64_object_to_unit

end module fortback_aarch64_elf64
