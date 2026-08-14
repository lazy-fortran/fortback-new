module fortback_targetir_table
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: source_ref_t, source_ref_valid, target_ir_t, &
        target_ir_valid
    use fortback_targetir_codec, only: targetir_lookup_candidates, &
        targetir_lookup_ambiguous, targetir_lookup_no_match, targetir_validate_record
    use fortback_targetir_encoding, only: targetir_encoding_capacity, &
        targetir_encoding_invalid_target, targetir_encoding_malformed, &
        targetir_encoding_ok, targetir_encoding_record_t
    implicit none
    private

    integer(int32), parameter, public :: targetir_table_ok = targetir_encoding_ok
    integer(int32), parameter, public :: targetir_table_malformed = &
        targetir_encoding_malformed
    integer(int32), parameter, public :: targetir_table_invalid_target = &
        targetir_encoding_invalid_target
    integer(int32), parameter, public :: targetir_table_capacity = &
        targetir_encoding_capacity
    integer(int32), parameter, public :: targetir_table_duplicate = 8_int32

    type, public :: targetir_encoding_table_t
        type(targetir_encoding_record_t) :: records(targetir_encoding_capacity)
        integer(int32) :: count = 0_int32
    end type targetir_encoding_table_t

    public :: targetir_encoding_table_append
    public :: targetir_encoding_table_clear
    public :: targetir_encoding_table_finalize
    public :: targetir_encoding_table_lookup_candidates
    public :: targetir_encoding_table_lookup_origin
    public :: targetir_encoding_table_lookup_source

contains

    subroutine targetir_encoding_table_append(table, target, record, status)
        type(targetir_encoding_table_t), intent(inout) :: table
        type(target_ir_t), intent(in) :: target
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int32), intent(out) :: status
        integer(int32) :: i

        status = validate_table(table)
        if (status == targetir_table_ok) status = targetir_validate_record(target, record)
        if (status /= targetir_table_ok) then
            call targetir_encoding_table_clear(table)
            return
        end if
        do i = 1, table%count
            if (same_target(table%records(i)%target, record%target)) then
                if (trim(table%records(i)%operation_id) == trim(record%operation_id)) then
                    status = targetir_table_duplicate
                    call targetir_encoding_table_clear(table)
                    return
                end if
            end if
        end do
        if (table%count >= targetir_table_capacity) then
            status = targetir_table_capacity
            call targetir_encoding_table_clear(table)
            return
        end if

        table%count = table%count + 1_int32
        table%records(table%count) = record
        status = targetir_table_ok
    end subroutine targetir_encoding_table_append

    subroutine targetir_encoding_table_clear(table)
        type(targetir_encoding_table_t), intent(out) :: table

        table = targetir_encoding_table_t()
    end subroutine targetir_encoding_table_clear

    subroutine targetir_encoding_table_finalize(table, records, count, status)
        type(targetir_encoding_table_t), intent(inout) :: table
        type(targetir_encoding_record_t), intent(out) :: records(:)
        integer(int32), intent(out) :: count, status

        records = targetir_encoding_record_t()
        count = 0_int32
        status = validate_table(table)
        if (status /= targetir_table_ok) then
            call targetir_encoding_table_clear(table)
            return
        end if
        if (size(records) < table%count) then
            status = targetir_table_capacity
            call targetir_encoding_table_clear(table)
            return
        end if
        if (table%count > 0_int32) records(1:table%count) = &
            table%records(1:table%count)
        count = table%count
        status = targetir_table_ok
    end subroutine targetir_encoding_table_finalize

    subroutine targetir_encoding_table_lookup_candidates(table, target, word, &
            indices, match_count, status)
        type(targetir_encoding_table_t), intent(in) :: table
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        integer(int32), intent(out) :: indices(:)
        integer(int32), intent(out) :: match_count, status
        type(targetir_encoding_record_t) :: selected(targetir_table_capacity)
        integer(int32) :: local_indices(targetir_table_capacity)
        integer(int32) :: mapping(targetir_table_capacity)
        integer(int32) :: selected_count, mapped_count

        indices = 0_int32
        match_count = 0_int32
        status = validate_table(table)
        if (status /= targetir_table_ok) return
        if (.not. target_ir_valid(target)) then
            status = targetir_table_invalid_target
            return
        end if

        selected = targetir_encoding_record_t()
        local_indices = 0_int32
        mapping = 0_int32
        call select_target_records(table, target, selected, mapping, selected_count)
        call targetir_lookup_candidates(target, selected(1:selected_count), word, &
            local_indices, mapped_count, status)
        call remap_lookup_result(local_indices, mapping, mapped_count, indices, &
            match_count, status)
    end subroutine targetir_encoding_table_lookup_candidates

    subroutine targetir_encoding_table_lookup_origin(table, origin, indices, &
            match_count, status)
        type(targetir_encoding_table_t), intent(in) :: table
        character(len=*), intent(in) :: origin
        integer(int32), intent(out) :: indices(:)
        integer(int32), intent(out) :: match_count, status
        integer(int32) :: i, selected_count

        indices = 0_int32
        match_count = 0_int32
        status = validate_table(table)
        if (status /= targetir_table_ok) return
        if (len_trim(origin) == 0) then
            status = targetir_table_malformed
            return
        end if

        selected_count = 0_int32
        do i = 1, table%count
            if (trim(table%records(i)%source%origin) == trim(origin)) then
                selected_count = selected_count + 1_int32
            end if
        end do
        if (selected_count > size(indices)) then
            status = targetir_table_capacity
            return
        end if
        do i = 1, table%count
            if (trim(table%records(i)%source%origin) == trim(origin)) then
                match_count = match_count + 1_int32
                indices(match_count) = i
            end if
        end do
        status = targetir_table_ok
    end subroutine targetir_encoding_table_lookup_origin

    subroutine targetir_encoding_table_lookup_source(table, source, indices, &
            match_count, status)
        type(targetir_encoding_table_t), intent(in) :: table
        type(source_ref_t), intent(in) :: source
        integer(int32), intent(out) :: indices(:)
        integer(int32), intent(out) :: match_count, status
        integer(int32) :: i, selected_count

        indices = 0_int32
        match_count = 0_int32
        status = validate_table(table)
        if (status /= targetir_table_ok) return
        if (.not. source_ref_valid(source)) then
            status = targetir_table_malformed
            return
        end if

        selected_count = 0_int32
        do i = 1, table%count
            if (same_source(table%records(i)%source, source)) then
                selected_count = selected_count + 1_int32
            end if
        end do
        if (selected_count > size(indices)) then
            status = targetir_table_capacity
            return
        end if
        do i = 1, table%count
            if (same_source(table%records(i)%source, source)) then
                match_count = match_count + 1_int32
                indices(match_count) = i
            end if
        end do
        status = targetir_table_ok
    end subroutine targetir_encoding_table_lookup_source

    subroutine select_target_records(table, target, selected, mapping, count)
        type(targetir_encoding_table_t), intent(in) :: table
        type(target_ir_t), intent(in) :: target
        type(targetir_encoding_record_t), intent(out) :: selected(:)
        integer(int32), intent(out) :: mapping(:), count
        integer(int32) :: i

        selected = targetir_encoding_record_t()
        mapping = 0_int32
        count = 0_int32
        do i = 1, table%count
            if (same_target(table%records(i)%target, target)) then
                count = count + 1_int32
                selected(count) = table%records(i)
                mapping(count) = i
            end if
        end do
    end subroutine select_target_records

    subroutine remap_lookup_result(local_indices, mapping, local_count, indices, &
            count, status)
        integer(int32), intent(in) :: local_indices(:), mapping(:), local_count
        integer(int32), intent(out) :: indices(:), count, status
        integer(int32) :: i

        indices = 0_int32
        count = 0_int32
        if (status /= targetir_table_ok .and. status /= targetir_lookup_no_match .and. &
            status /= targetir_lookup_ambiguous) then
            return
        end if
        if (local_count > size(indices)) then
            status = targetir_table_capacity
            return
        end if
        do i = 1, local_count
            indices(i) = mapping(local_indices(i))
        end do
        count = local_count
    end subroutine remap_lookup_result

    pure integer(int32) function validate_table(table)
        type(targetir_encoding_table_t), intent(in) :: table
        integer(int32) :: i, j, record_status

        validate_table = validate_table_shape(table)
        if (validate_table /= targetir_table_ok) return
        do i = 1, table%count
            record_status = targetir_validate_record(table%records(i)%target, &
                table%records(i))
            if (record_status /= targetir_table_ok) then
                validate_table = record_status
                return
            end if
            do j = 1, i - 1
                if (same_target(table%records(j)%target, table%records(i)%target)) then
                    if (trim(table%records(j)%operation_id) == &
                        trim(table%records(i)%operation_id)) then
                        validate_table = targetir_table_duplicate
                        return
                    end if
                end if
            end do
        end do
        validate_table = targetir_table_ok
    end function validate_table

    pure integer(int32) function validate_table_shape(table)
        type(targetir_encoding_table_t), intent(in) :: table

        validate_table_shape = targetir_table_malformed
        if (table%count < 0_int32) return
        if (table%count > targetir_table_capacity) then
            validate_table_shape = targetir_table_capacity
            return
        end if
        validate_table_shape = targetir_table_ok
    end function validate_table_shape

    pure logical function same_target(left, right)
        type(target_ir_t), intent(in) :: left, right

        same_target = .false.
        if (trim(left%architecture) /= trim(right%architecture)) return
        if (left%word_bits /= right%word_bits) return
        if (left%little_endian .neqv. right%little_endian) return
        same_target = .true.
    end function same_target

    pure logical function same_source(left, right)
        type(source_ref_t), intent(in) :: left, right

        same_source = trim(left%artifact) == trim(right%artifact)
        if (trim(left%object) /= trim(right%object)) same_source = .false.
        if (trim(left%source_hash) /= trim(right%source_hash)) same_source = .false.
        if (trim(left%origin) /= trim(right%origin)) same_source = .false.
    end function same_source

end module fortback_targetir_table
