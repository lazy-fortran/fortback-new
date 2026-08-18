module fortback_riscv_i_format
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_invalid_target, &
        riscv_malformed, riscv_ok, riscv_unsupported
    use fortback_riscv_opcode_table, only: riscv_immediate_width_for_mnemonic
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: source_ref_valid, target_ir_t, target_ir_valid
    implicit none
    private

    integer(int64), parameter :: word_max = int(z'FFFFFFFF', int64)
    integer(int64), parameter :: rd_mask = ishft(31_int64, 7)
    integer(int64), parameter :: rs1_mask = ishft(31_int64, 15)
    integer(int64), parameter :: immediate_mask = ishft(4095_int64, 20)
    integer(int64), parameter :: operand_mask = ior(rd_mask, rs1_mask)

    public :: riscv_encode_i_format
    public :: riscv_decode_i_format

contains

    subroutine riscv_encode_i_format(target, record, rd, rs1, immediate, word, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int32), intent(in) :: rd, rs1, immediate
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int32) :: immediate_width
        integer(int64) :: immediate_value_mask

        word = 0_int64
        status = validate_target(target)
        if (status /= riscv_ok) return
        status = validate_record(record)
        if (status /= riscv_ok) return
        immediate_width = record_immediate_width(record)
        if (rd < 0_int32 .or. rd > 31_int32) then
            status = riscv_invalid_operand
            return
        end if
        if (rs1 < 0_int32 .or. rs1 > 31_int32) then
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
        word = ior(word, ishft(int(rd, int64), 7))
        word = ior(word, ishft(int(rs1, int64), 15))
        immediate_value_mask = ishft(1_int64, immediate_width) - 1_int64
        word = ior(word, ishft(iand(int(immediate, int64), immediate_value_mask), 20))
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
            immediate_width = record_immediate_width(records(i))
            immediate_value_mask = ishft(1_int64, immediate_width) - 1_int64
            record_index = int(i, int32)
            rd = int(iand(ishft(word, -7), 31_int64), int32)
            rs1 = int(iand(ishft(word, -15), 31_int64), int32)
            raw_immediate = iand(ishft(word, -20), immediate_value_mask)
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
        if (iand(record%mask, operand_mask) /= 0_int64) return
        if (record_immediate_width(record) == 0_int32) return
        validate_record = riscv_ok
    end function validate_record

    pure integer(int32) function record_immediate_width(record)
        type(riscv_opcode_record_t), intent(in) :: record
        integer(int64) :: variable_mask, expected_mask
        integer(int32) :: width, generated_width

        variable_mask = iand(immediate_mask, not(record%mask))
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
        expected_mask = ishft(ishft(1_int64, width) - 1_int64, 20)
        if (variable_mask /= expected_mask) width = 0_int32
        record_immediate_width = width
    end function record_immediate_width

end module fortback_riscv_i_format
