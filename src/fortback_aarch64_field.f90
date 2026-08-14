module fortback_aarch64_field
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_record, only: aarch64_record_invalid_target, &
        aarch64_record_malformed, aarch64_record_ok, aarch64_record_unsupported, &
        aarch64_validate_record
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_capacity
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    public :: aarch64_extract_variable_range
    public :: aarch64_insert_variable_range

contains

    subroutine aarch64_extract_variable_range(target, record, word, ordinal, value, status)
        type(target_ir_t), intent(in) :: target
        type(aarch64_encoding_record_t), intent(in) :: record
        integer(int64), intent(in) :: word
        integer(int32), intent(in) :: ordinal
        integer(int64), intent(out) :: value
        integer(int32), intent(out) :: status
        integer(int32) :: record_status
        integer(int32) :: start_bit, range_width
        integer(int64) :: range_mask

        value = 0_int64
        status = aarch64_record_invalid_target
        call aarch64_validate_record(target, record, record_status)
        if (record_status /= aarch64_record_ok) then
            status = record_status
            return
        end if
        if (word < 0_int64 .or. word > int(z'FFFFFFFF', int64)) then
            status = aarch64_record_malformed
            return
        end if
        if (record%variable_range_count < 0_int32 .or. &
            record%variable_range_count > aarch64_variable_range_capacity) then
            status = aarch64_record_malformed
            return
        end if
        if (record%variable_range_count == 0_int32) then
            status = aarch64_record_unsupported
            return
        end if
        if (ordinal < 1_int32 .or. ordinal > record%variable_range_count) then
            status = aarch64_record_unsupported
            return
        end if

        call validate_variable_ranges(record, status)
        if (status /= aarch64_record_ok) return
        start_bit = record%variable_ranges(ordinal)%start
        range_width = record%variable_ranges(ordinal)%width
        range_mask = ishft(1_int64, range_width) - 1_int64
        value = iand(ishft(word, -start_bit), range_mask)
        status = aarch64_record_ok
    end subroutine aarch64_extract_variable_range

    subroutine aarch64_insert_variable_range(target, record, word, ordinal, value, result, status)
        type(target_ir_t), intent(in) :: target
        type(aarch64_encoding_record_t), intent(in) :: record
        integer(int64), intent(in) :: word, value
        integer(int32), intent(in) :: ordinal
        integer(int64), intent(out) :: result
        integer(int32), intent(out) :: status
        integer(int32) :: record_status
        integer(int32) :: start_bit, range_width
        integer(int64) :: range_mask, field_mask

        result = 0_int64
        status = aarch64_record_invalid_target
        call aarch64_validate_record(target, record, record_status)
        if (record_status /= aarch64_record_ok) then
            status = record_status
            return
        end if
        if (word < 0_int64 .or. word > int(z'FFFFFFFF', int64)) then
            status = aarch64_record_malformed
            return
        end if
        if (record%variable_range_count < 0_int32 .or. &
            record%variable_range_count > aarch64_variable_range_capacity) then
            status = aarch64_record_malformed
            return
        end if
        if (record%variable_range_count == 0_int32) then
            status = aarch64_record_unsupported
            return
        end if
        if (ordinal < 1_int32 .or. ordinal > record%variable_range_count) then
            status = aarch64_record_unsupported
            return
        end if

        call validate_variable_ranges(record, status)
        if (status /= aarch64_record_ok) return
        start_bit = record%variable_ranges(ordinal)%start
        range_width = record%variable_ranges(ordinal)%width
        range_mask = ishft(1_int64, range_width) - 1_int64
        if (value < 0_int64 .or. value > range_mask) then
            status = aarch64_record_malformed
            return
        end if
        field_mask = ishft(range_mask, start_bit)
        result = ior(iand(word, not(field_mask)), ishft(value, start_bit))
        status = aarch64_record_ok
    end subroutine aarch64_insert_variable_range

    subroutine validate_variable_ranges(record, status)
        type(aarch64_encoding_record_t), intent(in) :: record
        integer(int32), intent(out) :: status
        integer(int32) :: i, start_bit, range_width
        integer(int64) :: occupied, bit_mask

        status = aarch64_record_malformed
        occupied = 0_int64
        do i = 1, record%variable_range_count
            start_bit = record%variable_ranges(i)%start
            range_width = record%variable_ranges(i)%width
            if (start_bit < 0_int32 .or. start_bit > 31_int32) return
            if (range_width <= 0_int32 .or. range_width > 32_int32) return
            if (start_bit > 32_int32 - range_width) return
            bit_mask = ishft(1_int64, range_width) - 1_int64
            bit_mask = ishft(bit_mask, start_bit)
            if (iand(occupied, bit_mask) /= 0_int64) return
            occupied = ior(occupied, bit_mask)
        end do
        status = aarch64_record_ok
    end subroutine validate_variable_ranges

end module fortback_aarch64_field
