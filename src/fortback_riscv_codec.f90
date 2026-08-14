module fortback_riscv_codec
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_malformed, riscv_ok
    use fortback_riscv_i_format, only: riscv_decode_i_format, riscv_encode_i_format
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    integer(int32), parameter :: i_format_operand_count = 3_int32

    public :: riscv_encode_record
    public :: riscv_decode_record

contains

    subroutine riscv_encode_record(target, record, values, word, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int32) :: rd, rs1, immediate

        word = 0_int64
        status = riscv_invalid_operand
        if (size(values) /= i_format_operand_count) return
        if (any(values < int(-huge(0_int32), int64)) .or. &
            any(values > int(huge(0_int32), int64))) then
            status = riscv_invalid_operand
            return
        end if
        rd = int(values(1), int32)
        rs1 = int(values(2), int32)
        immediate = int(values(3), int32)
        call riscv_encode_i_format(target, record, rd, rs1, immediate, word, status)
        if (status /= riscv_ok) word = 0_int64
    end subroutine riscv_encode_record

    subroutine riscv_decode_record(target, record, word, values, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int64), intent(in) :: word
        integer(int64), allocatable, intent(out) :: values(:)
        integer(int32), intent(out) :: status
        type(riscv_opcode_record_t) :: records(1)
        integer(int32) :: record_index, rd, rs1, immediate

        status = riscv_malformed
        records(1) = record
        call riscv_decode_i_format(target, word, records, record_index, rd, rs1, immediate, &
            status)
        if (status /= riscv_ok) return
        allocate (values(i_format_operand_count))
        values(1) = int(rd, int64)
        values(2) = int(rs1, int64)
        values(3) = int(immediate, int64)
    end subroutine riscv_decode_record

end module fortback_riscv_codec
