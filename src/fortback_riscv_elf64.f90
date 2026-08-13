module fortback_riscv_elf64
    use iso_fortran_env, only: int32, int64
    use fortback_elf64, only: elf64_empty_word, elf64_invalid_source, &
        elf64_invalid_target, elf64_io_error, elf64_ok, &
        elf64_target_t, write_elf64_object_to_unit
    use fortback_riscv_fixture, only: riscv_encode_integer, riscv_instruction_t, &
        riscv_invalid_operand, riscv_malformed, riscv_ok, riscv_unsupported
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: source_ref_t, source_ref_valid
    implicit none
    private

    integer(int32), parameter, public :: riscv_elf_ok = elf64_ok
    integer(int32), parameter, public :: riscv_elf_invalid_target = elf64_invalid_target
    integer(int32), parameter, public :: riscv_elf_invalid_source = elf64_invalid_source
    integer(int32), parameter, public :: riscv_elf_empty_word = elf64_empty_word
    integer(int32), parameter, public :: riscv_elf_io_error = elf64_io_error
    integer(int32), parameter, public :: riscv_elf_invalid_operand = riscv_invalid_operand
    integer(int32), parameter, public :: riscv_elf_unsupported = riscv_unsupported
    integer(int32), parameter, public :: riscv_elf_malformed = riscv_malformed

    public :: write_riscv_elf64_object_to_unit

contains

    subroutine write_riscv_elf64_object_to_unit(metadata, source, instruction, records, &
            unit, status)
        type(elf64_target_t), intent(in) :: metadata
        type(source_ref_t), intent(in) :: source
        type(riscv_instruction_t), intent(in) :: instruction
        type(riscv_opcode_record_t), intent(in) :: records(:)
        integer, intent(in) :: unit
        integer(int32), intent(out) :: status
        integer(int32) :: encode_status
        integer(int64) :: word

        status = riscv_elf_invalid_source
        if (.not. source_ref_valid(source)) return

        call riscv_encode_integer(metadata%target, instruction, word, encode_status, records)
        if (encode_status /= riscv_ok) then
            status = encode_status
            return
        end if

        call write_elf64_object_to_unit(metadata, source, word, unit, status)
    end subroutine write_riscv_elf64_object_to_unit

end module fortback_riscv_elf64
