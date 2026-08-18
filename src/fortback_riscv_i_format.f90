module fortback_riscv_i_format
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_invalid_target, &
        riscv_malformed, riscv_ok, riscv_unsupported
    use fortback_riscv_opcode_table, only: riscv_immediate_lsb_for_mnemonic, &
        riscv_immediate_width_for_mnemonic, riscv_rd_lsb_for_mnemonic, &
        riscv_rd_width_for_mnemonic, riscv_rs1_lsb_for_mnemonic, &
        riscv_rs1_width_for_mnemonic
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: source_ref_valid, target_ir_t, target_ir_valid
    implicit none
    private

    integer(int64), parameter :: word_max = int(z'FFFFFFFF', int64)
    integer(int32), parameter :: default_rd_lsb = 7_int32
    integer(int32), parameter :: default_rd_width = 5_int32
    integer(int32), parameter :: default_rs1_lsb = 15_int32
    integer(int32), parameter :: default_rs1_width = 5_int32
    integer(int32), parameter :: default_immediate_lsb = 20_int32

    public :: riscv_encode_i_format
    public :: riscv_decode_i_format

contains

    subroutine riscv_encode_i_format(target, record, rd, rs1, immediate, word, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int32), intent(in) :: rd, rs1, immediate
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int32) :: immediate_width, rd_lsb, rd_width, rs1_lsb, rs1_width, immediate_lsb
        integer(int64) :: immediate_value_mask

        word = 0_int64
        status = validate_target(target)
        if (status /= riscv_ok) return
        status = validate_record(record)
        if (status /= riscv_ok) return
        call record_fields(record, rd_lsb, rd_width, rs1_lsb, rs1_width, immediate_lsb, &
            immediate_width)
        if (rd < 0_int32 .or. int(rd, int64) >= ishft(1_int64, rd_width)) then
            status = riscv_invalid_operand
            return
        end if
        if (rs1 < 0_int32 .or. int(rs1, int64) >= ishft(1_int64, rs1_width)) then
            status = riscv_invalid_operand
            return
        end if
        if (immediate_width == 12_int32) then
            if (immediate < -2048_int32 .or. immediate > 2047_int32) then
                status = riscv_invalid_operand
                return
            end if
        else
            immediate_value_mask = ishft(1_int64, immediate_width) - 1_int64
            if (immediate < 0_int32 .or. int(immediate, int64) > immediate_value_mask) then
                status = riscv_invalid_operand
                return
            end if
        end if

        word = record%match
        word = ior(word, ishft(int(rd, int64), rd_lsb))
        word = ior(word, ishft(int(rs1, int64), rs1_lsb))
        immediate_value_mask = ishft(1_int64, immediate_width) - 1_int64
        word = ior(word, ishft(iand(int(immediate, int64), immediate_value_mask), immediate_lsb))
    end subroutine riscv_encode_i_format

    subroutine riscv_decode_i_format(target, word, records, record_index, rd, rs1, immediate, &
            status)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        type(riscv_opcode_record_t), intent(in) :: records(:)
        integer(int32), intent(out) :: record_index, rd, rs1, immediate
        integer(int32), intent(out) :: status
        integer(int64) :: raw_immediate
        integer(int64) :: immediate_value_mask
        integer(int32) :: record_status
        integer(int32) :: immediate_width
        integer(int32) :: rd_lsb, rd_width, rs1_lsb, rs1_width, immediate_lsb
        integer :: i

        record_index = 0_int32
        rd = 0_int32
        rs1 = 0_int32
        immediate = 0_int32
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
            if (records(i)%format /= 'I') cycle
            record_status = validate_record(records(i))
            if (record_status /= riscv_ok) then
                status = record_status
                return
            end if
            if (iand(word, records(i)%mask) /= records(i)%match) cycle
            call record_fields(records(i), rd_lsb, rd_width, rs1_lsb, rs1_width, immediate_lsb, &
                immediate_width)
            immediate_value_mask = ishft(1_int64, immediate_width) - 1_int64
            record_index = int(i, int32)
            rd = int(iand(ishft(word, -rd_lsb), ishft(1_int64, rd_width) - 1_int64), int32)
            rs1 = int(iand(ishft(word, -rs1_lsb), ishft(1_int64, rs1_width) - 1_int64), int32)
            raw_immediate = iand(ishft(word, -immediate_lsb), immediate_value_mask)
            if (immediate_width == 12_int32) then
                if (raw_immediate >= 2048_int64) raw_immediate = raw_immediate - 4096_int64
            end if
            immediate = int(raw_immediate, int32)
            status = riscv_ok
            return
        end do
        status = riscv_unsupported
    end subroutine riscv_decode_i_format

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
        if (record%format /= 'I') then
            validate_record = riscv_unsupported
            return
        end if
        if (.not. source_ref_valid(record%source)) return
        if (record%mask < 0_int64 .or. record%mask > word_max) return
        if (record%match < 0_int64 .or. record%match > word_max) return
        if (record%mask == 0_int64) return
        if (iand(record%match, not(record%mask)) /= 0_int64) return
        if (iand(record%mask, record_operand_mask(record)) /= 0_int64) return
        if (record_immediate_width(record) == 0_int32) return
        validate_record = riscv_ok
    end function validate_record

    pure integer(int32) function record_immediate_width(record)
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int64) :: variable_mask, expected_mask
        integer(int32) :: width, generated_width, immediate_lsb

        immediate_lsb = record_immediate_lsb(record)
        variable_mask = iand(ishft(4095_int64, immediate_lsb), not(record%mask))
        generated_width = riscv_immediate_width_for_mnemonic(record%mnemonic)
        if (generated_width > 0_int32) then
            width = generated_width
        else
            width = 0_int32
            do while (width < 12_int32)
                if (.not. btest(variable_mask, 20 + width)) exit
                width = width + 1_int32
            end do
        end if
        expected_mask = ishft(ishft(1_int64, width) - 1_int64, immediate_lsb)
        if (variable_mask /= expected_mask) width = 0_int32
        record_immediate_width = width
    end function record_immediate_width

    pure subroutine record_fields(record, rd_lsb, rd_width, rs1_lsb, rs1_width, immediate_lsb, &
            immediate_width)
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int32), intent(out) :: rd_lsb, rd_width, rs1_lsb, rs1_width, immediate_lsb
        integer(int32), intent(out) :: immediate_width

        rd_lsb = record_rd_lsb(record)
        rd_width = record_rd_width(record)
        rs1_lsb = record_rs1_lsb(record)
        rs1_width = record_rs1_width(record)
        immediate_lsb = record_immediate_lsb(record)
        immediate_width = record_immediate_width(record)
    end subroutine record_fields

    pure integer(int32) function record_rd_lsb(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_rd_lsb = riscv_rd_lsb_for_mnemonic(record%mnemonic)
        if (record_rd_lsb == 0_int32) record_rd_lsb = default_rd_lsb
    end function record_rd_lsb

    pure integer(int32) function record_rd_width(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_rd_width = riscv_rd_width_for_mnemonic(record%mnemonic)
        if (record_rd_width == 0_int32) record_rd_width = default_rd_width
    end function record_rd_width

    pure integer(int32) function record_rs1_lsb(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_rs1_lsb = riscv_rs1_lsb_for_mnemonic(record%mnemonic)
        if (record_rs1_lsb == 0_int32) record_rs1_lsb = default_rs1_lsb
    end function record_rs1_lsb

    pure integer(int32) function record_rs1_width(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_rs1_width = riscv_rs1_width_for_mnemonic(record%mnemonic)
        if (record_rs1_width == 0_int32) record_rs1_width = default_rs1_width
    end function record_rs1_width

    pure integer(int32) function record_immediate_lsb(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_immediate_lsb = riscv_immediate_lsb_for_mnemonic(record%mnemonic)
        if (record_immediate_lsb == 0_int32) record_immediate_lsb = default_immediate_lsb
    end function record_immediate_lsb

    pure integer(int64) function record_operand_mask(record)
        type(riscv_opcode_record_t), intent(in) :: record

        record_operand_mask = ior(ishft(ishft(1_int64, record_rd_width(record)) - 1_int64, &
            record_rd_lsb(record)), ishft(ishft(1_int64, record_rs1_width(record)) - 1_int64, &
            record_rs1_lsb(record)))
    end function record_operand_mask

end module fortback_riscv_i_format
