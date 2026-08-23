module fortback_targetir_source_batch_codec
    use iso_fortran_env, only: int32, int64
    use fortback_targetir_codec, only: targetir_encode_record
    use fortback_targetir_encoding, only: targetir_encoding_malformed, targetir_encoding_ok, &
        targetir_encoding_record_t
    use fortback_targetir_source_batch, only: normalize_targetir_source_batch, &
        targetir_source_batch_capacity, targetir_source_batch_item_t
    use fortback_targetir_table, only: targetir_encoding_table_clear, targetir_encoding_table_t, &
        targetir_table_capacity
    implicit none
    private

    public :: encode_targetir_source_batch

contains

    ! operand_values(item, ordinal) follows variable_fields(ordinal).  The
    ! rectangular second dimension may be wider than an individual record;
    ! operand_counts selects the meaningful prefix for each item.
    subroutine encode_targetir_source_batch(items, operand_values, operand_counts, table, &
            words, encoded_count, status)
        type(targetir_source_batch_item_t), intent(in) :: items(:)
        integer(int64), intent(in) :: operand_values(:, :)
        integer(int32), intent(in) :: operand_counts(:)
        type(targetir_encoding_table_t), intent(inout) :: table
        integer(int64), intent(out) :: words(:)
        integer(int32), intent(out) :: encoded_count, status
        integer(int32) :: i
        integer(int64) :: word
        type(targetir_encoding_record_t) :: record

        call targetir_encoding_table_clear(table)
        words = 0_int64
        encoded_count = 0_int32
        status = targetir_encoding_malformed
        if (size(operand_counts) /= size(items)) return
        if (size(operand_values, 1) /= size(items)) return
        if (size(words) < size(items)) then
            status = targetir_source_batch_capacity
            return
        end if
        if (size(items) > targetir_table_capacity) then
            status = targetir_source_batch_capacity
            return
        end if

        call normalize_targetir_source_batch(items, table, status)
        if (status /= targetir_encoding_ok) then
            call rollback(table, words, encoded_count)
            return
        end if

        do i = 1, size(items)
            if (operand_counts(i) < 0_int32 .or. &
                operand_counts(i) /= table%records(i)%variable_field_count) then
                call rollback(table, words, encoded_count)
                status = targetir_encoding_malformed
                return
            end if
            if (operand_counts(i) > size(operand_values, 2)) then
                call rollback(table, words, encoded_count)
                status = targetir_encoding_malformed
                return
            end if
        end do

        do i = 1, size(items)
            record = table%records(i)
            call targetir_encode_record(record%target, record, &
                operand_values(i, 1:operand_counts(i)), word, status)
            if (status /= targetir_encoding_ok) then
                call rollback(table, words, encoded_count)
                return
            end if
            words(i) = word
        end do
        encoded_count = int(size(items), int32)
    end subroutine encode_targetir_source_batch

    subroutine rollback(table, words, encoded_count)
        type(targetir_encoding_table_t), intent(out) :: table
        integer(int64), intent(out) :: words(:)
        integer(int32), intent(out) :: encoded_count

        call targetir_encoding_table_clear(table)
        words = 0_int64
        encoded_count = 0_int32
    end subroutine rollback

end module fortback_targetir_source_batch_codec
