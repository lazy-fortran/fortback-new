module fortback_riscv_s_format
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_invalid_target, riscv_ok
    use fortback_riscv_opcode_table, only: riscv_immediate_width_for_mnemonic, &
        riscv_rs1_lsb_for_mnemonic, riscv_rs1_width_for_mnemonic, &
        riscv_rs2_lsb_for_mnemonic, riscv_rs2_width_for_mnemonic
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: target_ir_t, target_ir_valid
    implicit none
    private
    public :: riscv_encode_s_format

contains
    subroutine riscv_encode_s_format(target, record, rs2, rs1, immediate, word, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int32), intent(in) :: rs2, rs1, immediate
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int32) :: rs1_lsb, rs1_width, rs2_lsb, rs2_width, immediate_width
        integer(int64) :: immediate_value

        word = 0_int64
        status = validate_target(target)
        if (status /= riscv_ok) return
        call record_fields(record, rs1_lsb, rs1_width, rs2_lsb, rs2_width, immediate_width)
        if (.not. register_valid(rs1, rs1_width) .or. .not. register_valid(rs2, rs2_width)) then
            status = riscv_invalid_operand
            return
        end if
        if (immediate < -2048_int32 .or. immediate > 2047_int32) then
            status = riscv_invalid_operand
            return
        end if
        immediate_value = iand(int(immediate, int64), int(z'FFF', int64))
        word = record%match
        word = ior(word, ishft(int(rs1, int64), rs1_lsb))
        word = ior(word, ishft(int(rs2, int64), rs2_lsb))
        word = ior(word, ishft(iand(immediate_value, int(z'1F', int64)), 7))
        word = ior(word, ishft(iand(immediate_value, int(z'FE0', int64)), 20))
    end subroutine riscv_encode_s_format

    integer(int32) function validate_target(target)
        type(target_ir_t), intent(in) :: target
        validate_target = riscv_invalid_target
        if (target_ir_valid(target)) validate_target = riscv_ok
    end function validate_target

    subroutine record_fields(record, rs1_lsb, rs1_width, rs2_lsb, rs2_width, immediate_width)
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int32), intent(out) :: rs1_lsb, rs1_width, rs2_lsb, rs2_width, immediate_width

        rs1_lsb = riscv_rs1_lsb_for_mnemonic(record%mnemonic)
        rs1_width = riscv_rs1_width_for_mnemonic(record%mnemonic)
        rs2_lsb = riscv_rs2_lsb_for_mnemonic(record%mnemonic)
        rs2_width = riscv_rs2_width_for_mnemonic(record%mnemonic)
        immediate_width = riscv_immediate_width_for_mnemonic(record%mnemonic)
    end subroutine record_fields

    logical function register_valid(value, width)
        integer(int32), intent(in) :: value, width
        register_valid = value >= 0_int32 .and. value < ishft(1_int32, width)
    end function register_valid
end module fortback_riscv_s_format
