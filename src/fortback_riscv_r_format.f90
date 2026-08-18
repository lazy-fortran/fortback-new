module fortback_riscv_r_format
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_invalid_target, &
        riscv_malformed, riscv_ok, riscv_unsupported
    use fortback_riscv_opcode_table, only: riscv_rd_lsb_for_mnemonic, &
        riscv_rd_width_for_mnemonic, riscv_rs1_lsb_for_mnemonic, &
        riscv_rs1_width_for_mnemonic, riscv_rs2_lsb_for_mnemonic, &
        riscv_rs2_width_for_mnemonic
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: source_ref_valid, target_ir_t, target_ir_valid
    implicit none
    private

    integer(int64), parameter :: word_max = int(z'FFFFFFFF', int64)

    public :: riscv_encode_r_format
    public :: riscv_decode_r_format

contains

    subroutine riscv_encode_r_format(target, record, rd, rs1, rs2, word, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int32), intent(in) :: rd, rs1, rs2
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int32) :: rd_lsb, rd_width, rs1_lsb, rs1_width, rs2_lsb, rs2_width

        word = 0_int64
        status = validate_target(target)
        if (status /= riscv_ok) return
        status = validate_record(record)
        if (status /= riscv_ok) return
        call record_fields(record, rd_lsb, rd_width, rs1_lsb, rs1_width, rs2_lsb, rs2_width)
        if (.not. register_valid(rd, rd_width)) then
            status = riscv_invalid_operand
            return
        end if
        if (.not. register_valid(rs1, rs1_width)) then
            status = riscv_invalid_operand
            return
        end if
        if (.not. register_valid(rs2, rs2_width)) then
            status = riscv_invalid_operand
            return
        end if

        word = record%match
        word = ior(word, ishft(int(rd, int64), rd_lsb))
        word = ior(word, ishft(int(rs1, int64), rs1_lsb))
        word = ior(word, ishft(int(rs2, int64), rs2_lsb))
    end subroutine riscv_encode_r_format

    subroutine riscv_decode_r_format(target, word, records, record_index, rd, rs1, rs2, status)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        type(riscv_opcode_record_t), intent(in) :: records(:)
        integer(int32), intent(out) :: record_index, rd, rs1, rs2, status
        integer(int32) :: rd_lsb, rd_width, rs1_lsb, rs1_width, rs2_lsb, rs2_width
        integer(int32) :: record_status
        integer :: i

        record_index = 0_int32
        rd = 0_int32
        rs1 = 0_int32
        rs2 = 0_int32
        status = validate_target(target)
        if (status /= riscv_ok) return
        if (word < 0_int64 .or. word > word_max) then
            status = riscv_malformed
            return
        end if
        if (size(records) == 0) then
            status = riscv_unsupported
            return
        end if

        do i = 1, size(records)
            if (records(i)%format /= 'R') cycle
            record_status = validate_record(records(i))
            if (record_status /= riscv_ok) then
                status = record_status
                return
            end if
            if (iand(word, records(i)%mask) /= records(i)%match) cycle
            call record_fields(records(i), rd_lsb, rd_width, rs1_lsb, rs1_width, rs2_lsb, &
                rs2_width)
            record_index = int(i, int32)
            rd = extract_register(word, rd_lsb, rd_width)
            rs1 = extract_register(word, rs1_lsb, rs1_width)
            rs2 = extract_register(word, rs2_lsb, rs2_width)
            status = riscv_ok
            return
        end do
        status = riscv_unsupported
    end subroutine riscv_decode_r_format

    pure logical function register_valid(register, width)
        integer(int32), intent(in) :: register, width

        register_valid = register >= 0_int32 .and. int(register, int64) < ishft(1_int64, width)
    end function register_valid

    pure integer(int32) function extract_register(word, lsb, width)
        integer(int64), intent(in) :: word
        integer(int32), intent(in) :: lsb, width

        extract_register = int(iand(ishft(word, -lsb), ishft(1_int64, width) - 1_int64), int32)
    end function extract_register

    pure integer(int32) function validate_target(target)
        type(target_ir_t), intent(in) :: target

        validate_target = riscv_invalid_target
        if (.not. target_ir_valid(target)) return
        if (trim(target%architecture) /= 'riscv64') return
        if (target%word_bits /= 64_int32) return
        if (.not. target%little_endian) return
        validate_target = riscv_ok
    end function validate_target

    pure integer(int32) function validate_record(record)
        type(riscv_opcode_record_t), intent(in) :: record

        validate_record = riscv_malformed
        if (record%format /= 'R') then
            validate_record = riscv_unsupported
            return
        end if
        if (.not. source_ref_valid(record%source)) return
        if (record%mask < 0_int64 .or. record%mask > word_max) return
        if (record%match < 0_int64 .or. record%match > word_max) return
        if (record%mask == 0_int64) return
        if (iand(record%match, not(record%mask)) /= 0_int64) return
        if (iand(record%mask, record_operand_mask(record)) /= 0_int64) return
        validate_record = riscv_ok
    end function validate_record

    pure subroutine record_fields(record, rd_lsb, rd_width, rs1_lsb, rs1_width, rs2_lsb, &
            rs2_width)
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int32), intent(out) :: rd_lsb, rd_width, rs1_lsb, rs1_width, rs2_lsb, rs2_width

        rd_lsb = riscv_rd_lsb_for_mnemonic(record%mnemonic)
        rd_width = riscv_rd_width_for_mnemonic(record%mnemonic)
        rs1_lsb = riscv_rs1_lsb_for_mnemonic(record%mnemonic)
        rs1_width = riscv_rs1_width_for_mnemonic(record%mnemonic)
        rs2_lsb = riscv_rs2_lsb_for_mnemonic(record%mnemonic)
        rs2_width = riscv_rs2_width_for_mnemonic(record%mnemonic)
    end subroutine record_fields

    pure integer(int64) function record_operand_mask(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_operand_mask = ishft(ishft(1_int64, riscv_rd_width_for_mnemonic(record%mnemonic)) - &
            1_int64, riscv_rd_lsb_for_mnemonic(record%mnemonic))
        record_operand_mask = ior(record_operand_mask, &
            ishft(ishft(1_int64, riscv_rs1_width_for_mnemonic(record%mnemonic)) - 1_int64, &
            riscv_rs1_lsb_for_mnemonic(record%mnemonic)))
        record_operand_mask = ior(record_operand_mask, &
            ishft(ishft(1_int64, riscv_rs2_width_for_mnemonic(record%mnemonic)) - 1_int64, &
            riscv_rs2_lsb_for_mnemonic(record%mnemonic)))
    end function record_operand_mask

end module fortback_riscv_r_format
