module fortback_aarch64_codec
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_field, only: aarch64_extract_variable_range, &
        aarch64_insert_variable_range
    use fortback_aarch64_record, only: aarch64_find_fixed_record, &
        aarch64_record_malformed, aarch64_record_ok, aarch64_validate_record
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_capacity
    use fortback_target_ir, only: target_ir_t
    implicit none
    private

    public :: aarch64_encode_record
    public :: aarch64_decode_record

contains

    subroutine aarch64_encode_record(target, record, values, word, status)
        type(target_ir_t), intent(in) :: target
        type(aarch64_encoding_record_t), intent(in) :: record
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int32) :: i
        integer(int64) :: next_word

        word = 0_int64
        status = aarch64_record_malformed
        call aarch64_validate_record(target, record, status)
        if (status /= aarch64_record_ok) return
        if (record%variable_range_count < 0_int32 .or. &
            record%variable_range_count > aarch64_variable_range_capacity) then
            status = aarch64_record_malformed
            return
        end if
        if (size(values) /= record%variable_range_count) then
            status = aarch64_record_malformed
            return
        end if

        word = record%match
        if (record%variable_range_count == 0_int32) then
            status = aarch64_record_ok
            return
        end if

        do i = 1, record%variable_range_count
            call aarch64_insert_variable_range(target, record, word, i, values(i), &
                next_word, status)
            if (status /= aarch64_record_ok) then
                word = 0_int64
                return
            end if
            word = next_word
        end do
        status = aarch64_record_ok
    end subroutine aarch64_encode_record

    subroutine aarch64_decode_record(target, record, word, values, status)
        type(target_ir_t), intent(in) :: target
        type(aarch64_encoding_record_t), intent(in) :: record
        integer(int64), intent(in) :: word
        integer(int64), allocatable, intent(out) :: values(:)
        integer(int32), intent(out) :: status
        type(aarch64_encoding_record_t) :: candidates(1)
        integer(int32) :: record_index, i
        integer(int64) :: value

        status = aarch64_record_malformed
        candidates(1) = record
        call aarch64_find_fixed_record(target, word, candidates, record_index, status)
        if (status /= aarch64_record_ok) return

        if (record%variable_range_count < 0_int32 .or. &
            record%variable_range_count > aarch64_variable_range_capacity) then
            status = aarch64_record_malformed
            return
        end if
        allocate (values(record%variable_range_count))
        values = 0_int64
        do i = 1, record%variable_range_count
            call aarch64_extract_variable_range(target, record, word, i, value, status)
            if (status /= aarch64_record_ok) then
                deallocate (values)
                return
            end if
            values(i) = value
        end do
        status = aarch64_record_ok
    end subroutine aarch64_decode_record

end module fortback_aarch64_codec
