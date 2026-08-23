module fortback_targetir_encoding
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_capacity
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: source_ref_t, source_ref_valid, target_ir_t, target_ir_valid
    implicit none
    private

    integer(int32), parameter, public :: targetir_encoding_ok = 0_int32
    integer(int32), parameter, public :: targetir_encoding_malformed = 1_int32
    integer(int32), parameter, public :: targetir_encoding_unsupported = 2_int32
    integer(int32), parameter, public :: targetir_encoding_invalid_target = 3_int32
    integer(int32), parameter, public :: targetir_encoding_capacity = 4_int32
    integer(int32), parameter, public :: targetir_encoding_field_capacity = 32_int32

    type, public :: targetir_variable_field_t
        integer(int32) :: ordinal = 0_int32
        integer(int32) :: start = 0_int32
        integer(int32) :: width = 0_int32
    end type targetir_variable_field_t

    type, public :: targetir_encoding_record_t
        type(target_ir_t) :: target
        character(len=64) :: operation_id = ''
        integer(int32) :: word_bits = 0_int32
        integer(int64) :: fixed_match = 0_int64
        integer(int64) :: fixed_mask = 0_int64
        integer(int32) :: variable_field_count = 0_int32
        type(targetir_variable_field_t) :: variable_fields(targetir_encoding_field_capacity)
        type(source_ref_t) :: source
    end type targetir_encoding_record_t

    public :: normalize_riscv_i_record
    public :: normalize_riscv_r_record
    public :: normalize_aarch64_record

contains

    subroutine normalize_riscv_i_record(target, record, normalized, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        type(targetir_encoding_record_t), intent(out) :: normalized
        integer(int32), intent(out) :: status
        integer(int32) :: width

        normalized = targetir_encoding_record_t()
        status = validate_riscv_target(target)
        if (status /= targetir_encoding_ok) return
        if (record%format /= 'I') then
            status = targetir_encoding_unsupported
            return
        end if
        if (.not. source_ref_valid(record%source)) then
            status = targetir_encoding_malformed
            return
        end if
        if (len_trim(record%mnemonic) == 0) then
            status = targetir_encoding_malformed
            return
        end if
        status = validate_fixed_bits(record%mask, record%match)
        if (status /= targetir_encoding_ok) return
        if (iand(record%mask, ior(ishft(31_int64, 7), ishft(31_int64, 15))) /= 0_int64) then
            status = targetir_encoding_malformed
            return
        end if
        width = riscv_immediate_width(record%mask)
        if (width == 0_int32) then
            status = targetir_encoding_malformed
            return
        end if

        normalized%target = target
        normalized%operation_id = record%mnemonic
        normalized%word_bits = 32_int32
        normalized%fixed_match = record%match
        normalized%fixed_mask = record%mask
        normalized%source = record%source
        call add_field(normalized, 7_int32, 5_int32, status)
        if (status /= targetir_encoding_ok) then
            normalized = targetir_encoding_record_t()
            return
        end if
        call add_field(normalized, 15_int32, 5_int32, status)
        if (status /= targetir_encoding_ok) then
            normalized = targetir_encoding_record_t()
            return
        end if
        call add_field(normalized, 20_int32, width, status)
        if (status /= targetir_encoding_ok) normalized = targetir_encoding_record_t()
    end subroutine normalize_riscv_i_record

    subroutine normalize_riscv_r_record(target, record, normalized, status)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        type(targetir_encoding_record_t), intent(out) :: normalized
        integer(int32), intent(out) :: status

        normalized = targetir_encoding_record_t()
        status = validate_riscv_target(target)
        if (status /= targetir_encoding_ok) return
        if (record%format /= 'R') then
            status = targetir_encoding_unsupported
            return
        end if
        if (.not. source_ref_valid(record%source)) then
            status = targetir_encoding_malformed
            return
        end if
        if (len_trim(record%mnemonic) == 0) then
            status = targetir_encoding_malformed
            return
        end if
        status = validate_fixed_bits(record%mask, record%match)
        if (status /= targetir_encoding_ok) return
        if (record%mask == 0_int64) then
            status = targetir_encoding_malformed
            return
        end if

        normalized%target = target
        normalized%operation_id = record%mnemonic
        normalized%word_bits = 32_int32
        normalized%fixed_match = record%match
        normalized%fixed_mask = record%mask
        normalized%source = record%source
        call add_field(normalized, 7_int32, 5_int32, status)
        if (status /= targetir_encoding_ok) then
            normalized = targetir_encoding_record_t()
            return
        end if
        call add_field(normalized, 15_int32, 5_int32, status)
        if (status /= targetir_encoding_ok) then
            normalized = targetir_encoding_record_t()
            return
        end if
        call add_field(normalized, 20_int32, 5_int32, status)
        if (status /= targetir_encoding_ok) normalized = targetir_encoding_record_t()
    end subroutine normalize_riscv_r_record

    subroutine normalize_aarch64_record(target, record, normalized, status)
        type(target_ir_t), intent(in) :: target
        type(aarch64_encoding_record_t), intent(in) :: record
        type(targetir_encoding_record_t), intent(out) :: normalized
        integer(int32), intent(out) :: status
        integer(int32) :: i

        normalized = targetir_encoding_record_t()
        status = validate_aarch64_target(target)
        if (status /= targetir_encoding_ok) return
        if (.not. target_matches(record%target)) then
            status = targetir_encoding_invalid_target
            return
        end if
        if (.not. source_ref_valid(record%source)) then
            status = targetir_encoding_malformed
            return
        end if
        if (len_trim(record%operation_id) == 0) then
            status = targetir_encoding_malformed
            return
        end if
        if (record%width /= 32_int32) then
            status = targetir_encoding_unsupported
            return
        end if
        status = validate_fixed_bits(record%mask, record%match)
        if (status /= targetir_encoding_ok) return
        if (record%variable_range_count < 0_int32 .or. &
            record%variable_range_count > aarch64_variable_range_capacity) then
            status = targetir_encoding_malformed
            return
        end if

        normalized%target = target
        normalized%operation_id = record%operation_id
        normalized%word_bits = record%width
        normalized%fixed_match = record%match
        normalized%fixed_mask = record%mask
        normalized%source = record%source
        do i = 1, record%variable_range_count
            call add_field(normalized, record%variable_ranges(i)%start, &
                record%variable_ranges(i)%width, status)
            if (status /= targetir_encoding_ok) then
                normalized = targetir_encoding_record_t()
                return
            end if
        end do
    end subroutine normalize_aarch64_record

    subroutine add_field(record, start, width, status)
        type(targetir_encoding_record_t), intent(inout) :: record
        integer(int32), intent(in) :: start, width
        integer(int32), intent(out) :: status
        integer(int32) :: i
        integer(int64) :: bits

        status = targetir_encoding_malformed
        if (start < 0_int32 .or. width <= 0_int32) return
        if (start + width > record%word_bits) return
        if (record%variable_field_count >= targetir_encoding_field_capacity) then
            status = targetir_encoding_capacity
            return
        end if
        bits = bit_range(start, width)
        if (iand(record%fixed_mask, bits) /= 0_int64) return
        do i = 1, record%variable_field_count
            if (iand(bits, bit_range(record%variable_fields(i)%start, &
                record%variable_fields(i)%width)) /= 0_int64) return
        end do
        record%variable_field_count = record%variable_field_count + 1_int32
        record%variable_fields(record%variable_field_count)%ordinal = &
            record%variable_field_count
        record%variable_fields(record%variable_field_count)%start = start
        record%variable_fields(record%variable_field_count)%width = width
        status = targetir_encoding_ok
    end subroutine add_field

    pure integer(int32) function validate_fixed_bits(mask, match)
        integer(int64), intent(in) :: mask, match

        validate_fixed_bits = targetir_encoding_malformed
        if (mask < 0_int64 .or. mask > int(z'FFFFFFFF', int64)) return
        if (match < 0_int64 .or. match > int(z'FFFFFFFF', int64)) return
        if (mask == 0_int64) return
        if (iand(match, not(mask)) /= 0_int64) return
        validate_fixed_bits = targetir_encoding_ok
    end function validate_fixed_bits

    pure integer(int32) function validate_riscv_target(target)
        type(target_ir_t), intent(in) :: target

        validate_riscv_target = targetir_encoding_invalid_target
        if (.not. target_ir_valid(target)) return
        if (trim(target%architecture) /= 'riscv64') return
        if (target%word_bits /= 64_int32) return
        if (.not. target%little_endian) return
        validate_riscv_target = targetir_encoding_ok
    end function validate_riscv_target

    pure integer(int32) function validate_aarch64_target(target)
        type(target_ir_t), intent(in) :: target

        validate_aarch64_target = targetir_encoding_invalid_target
        if (.not. target_ir_valid(target)) return
        if (trim(target%architecture) /= 'aarch64') return
        if (target%word_bits /= 32_int32) return
        if (.not. target%little_endian) return
        validate_aarch64_target = targetir_encoding_ok
    end function validate_aarch64_target

    pure logical function target_matches(target)
        type(target_ir_t), intent(in) :: target

        target_matches = .false.
        if (len_trim(target%architecture) == 0) return
        if (target%word_bits /= 32_int32) return
        if (.not. target%little_endian) return
        if (trim(target%architecture) /= 'aarch64') return
        if (.not. source_ref_valid(target%source)) return
        target_matches = .true.
    end function target_matches

    pure integer(int32) function riscv_immediate_width(mask)
        integer(int64), intent(in) :: mask
        integer(int64) :: variable_mask, expected_mask
        integer(int32) :: width

        variable_mask = iand(int(z'FFF00000', int64), not(mask))
        width = 0_int32
        do while (width < 12_int32)
            if (.not. btest(variable_mask, 20 + width)) exit
            width = width + 1_int32
        end do
        expected_mask = ishft(ishft(1_int64, width) - 1_int64, 20)
        if (variable_mask /= expected_mask) width = 0_int32
        riscv_immediate_width = width
    end function riscv_immediate_width

    pure integer(int64) function bit_range(start, width)
        integer(int32), intent(in) :: start, width

        bit_range = ishft(ishft(1_int64, width) - 1_int64, start)
    end function bit_range

end module fortback_targetir_encoding
