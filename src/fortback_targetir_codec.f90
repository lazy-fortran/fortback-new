module fortback_targetir_codec
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: source_ref_valid, target_ir_t, target_ir_valid
    use fortback_targetir_encoding, only: targetir_encoding_capacity, &
        targetir_encoding_field_capacity, &
        targetir_encoding_invalid_target, targetir_encoding_malformed, &
        targetir_encoding_ok, targetir_encoding_record_t, targetir_encoding_unsupported
    implicit none
    private

    integer(int64), parameter :: instruction_word_max = int(z'FFFFFFFF', int64)

    public :: targetir_encode_record
    public :: targetir_decode_record
    public :: targetir_lookup_candidates

    integer(int32), parameter, public :: targetir_lookup_no_match = 5_int32
    integer(int32), parameter, public :: targetir_lookup_ambiguous = 6_int32
    integer(int32), parameter, public :: targetir_lookup_unsupported_word = 7_int32
    integer(int32), parameter, public :: targetir_lookup_capacity = targetir_encoding_capacity

contains

    subroutine targetir_encode_record(target, record, values, word, status)
        type(target_ir_t), intent(in) :: target
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(out) :: word
        integer(int32), intent(out) :: status
        integer(int32) :: i
        integer(int64) :: value_mask

        word = 0_int64
        status = validate_record(target, record)
        if (status /= targetir_encoding_ok) return
        if (size(values) /= record%variable_field_count) then
            status = targetir_encoding_malformed
            return
        end if

        word = record%fixed_match
        do i = 1, record%variable_field_count
            value_mask = value_mask_for(record%variable_fields(i))
            if (values(i) < 0_int64 .or. values(i) > value_mask) then
                word = 0_int64
                status = targetir_encoding_malformed
                return
            end if
            word = ior(word, ishft(values(i), record%variable_fields(i)%start))
        end do
        status = targetir_encoding_ok
    end subroutine targetir_encode_record

    subroutine targetir_decode_record(target, record, word, values, status)
        type(target_ir_t), intent(in) :: target
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int64), intent(in) :: word
        integer(int64), allocatable, intent(out) :: values(:)
        integer(int32), intent(out) :: status
        integer(int32) :: i
        integer(int64) :: value_mask

        status = validate_record(target, record)
        if (status /= targetir_encoding_ok) return
        if (word < 0_int64 .or. word > instruction_word_max) then
            status = targetir_encoding_malformed
            return
        end if
        if (iand(word, record%fixed_mask) /= record%fixed_match) then
            status = targetir_encoding_unsupported
            return
        end if

        allocate (values(record%variable_field_count))
        do i = 1, record%variable_field_count
            value_mask = value_mask_for(record%variable_fields(i))
            values(i) = iand(ishft(word, -record%variable_fields(i)%start), value_mask)
        end do
        status = targetir_encoding_ok
    end subroutine targetir_decode_record

    subroutine targetir_lookup_candidates(target, records, word, indices, match_count, status)
        type(target_ir_t), intent(in) :: target
        type(targetir_encoding_record_t), intent(in) :: records(:)
        integer(int64), intent(in) :: word
        integer(int32), intent(out) :: indices(:)
        integer(int32), intent(out) :: match_count
        integer(int32), intent(out) :: status
        integer(int32) :: i

        indices = 0_int32
        match_count = 0_int32
        status = targetir_encoding_invalid_target
        if (.not. target_ir_valid(target)) return
        if (target%word_bits /= 32_int32) return
        if (word < 0_int64 .or. word > instruction_word_max) then
            status = targetir_lookup_unsupported_word
            return
        end if
        do i = 1, size(records)
            status = validate_record(target, records(i))
            if (status /= targetir_encoding_ok) then
                indices = 0_int32
                match_count = 0_int32
                return
            end if
            if (iand(word, records(i)%fixed_mask) == records(i)%fixed_match) then
                match_count = match_count + 1_int32
            end if
        end do

        if (match_count > size(indices)) then
            indices = 0_int32
            match_count = 0_int32
            status = targetir_encoding_capacity
            return
        end if

        match_count = 0_int32
        do i = 1, size(records)
            if (iand(word, records(i)%fixed_mask) == records(i)%fixed_match) then
                match_count = match_count + 1_int32
                indices(match_count) = i
            end if
        end do

        if (match_count == 0_int32) then
            status = targetir_lookup_no_match
        else if (match_count > 1_int32) then
            status = targetir_lookup_ambiguous
        else
            status = targetir_encoding_ok
        end if
    end subroutine targetir_lookup_candidates

    pure integer(int32) function validate_record(target, record)
        type(target_ir_t), intent(in) :: target
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int32) :: i
        integer(int64) :: occupied, field_mask

        validate_record = targetir_encoding_invalid_target
        if (.not. target_ir_valid(target)) return
        if (.not. target_ir_valid(record%target)) return
        if (trim(target%architecture) /= trim(record%target%architecture)) return
        if (target%word_bits /= record%target%word_bits) return
        if (target%little_endian .neqv. record%target%little_endian) return

        validate_record = targetir_encoding_malformed
        if (.not. source_ref_valid(record%source)) return
        if (len_trim(record%operation_id) == 0) return
        if (record%word_bits /= 32_int32) return
        if (record%fixed_mask < 0_int64 .or. record%fixed_mask > instruction_word_max) return
        if (record%fixed_match < 0_int64 .or. record%fixed_match > instruction_word_max) return
        if (iand(record%fixed_match, not(record%fixed_mask)) /= 0_int64) return
        if (record%variable_field_count < 0_int32 .or. &
            record%variable_field_count > targetir_encoding_field_capacity) return

        occupied = record%fixed_mask
        do i = 1, record%variable_field_count
            if (record%variable_fields(i)%ordinal /= i) return
            if (record%variable_fields(i)%width <= 0_int32 .or. &
                record%variable_fields(i)%width > 32_int32) return
            if (record%variable_fields(i)%start < 0_int32) return
            if (record%variable_fields(i)%start > 32_int32 - &
                record%variable_fields(i)%width) return
            field_mask = field_mask_for(record%variable_fields(i))
            if (iand(occupied, field_mask) /= 0_int64) return
            occupied = ior(occupied, field_mask)
        end do
        validate_record = targetir_encoding_ok
    end function validate_record

    pure integer(int64) function field_mask_for(field)
        use fortback_targetir_encoding, only: targetir_variable_field_t
        type(targetir_variable_field_t), intent(in) :: field

        field_mask_for = ishft(ishft(1_int64, field%width) - 1_int64, field%start)
    end function field_mask_for

    pure integer(int64) function value_mask_for(field)
        use fortback_targetir_encoding, only: targetir_variable_field_t
        type(targetir_variable_field_t), intent(in) :: field

        value_mask_for = ishft(1_int64, field%width) - 1_int64
    end function value_mask_for

end module fortback_targetir_codec
