program test_targetir_source_batch_codec
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_t
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, target_ir_t
    use fortback_targetir_encoding, only: targetir_encoding_malformed
    use fortback_targetir_source_batch, only: make_aarch64_source_batch_item, &
        make_riscv_i_source_batch_item, make_riscv_r_source_batch_item, &
        targetir_source_batch_capacity, targetir_source_batch_item_t
    use fortback_targetir_source_batch_codec, only: encode_targetir_source_batch
    use fortback_targetir_table, only: targetir_encoding_table_t
    implicit none

    type(targetir_source_batch_item_t) :: items(3), bad_items(3)
    type(targetir_encoding_table_t) :: table
    type(riscv_opcode_record_t) :: i_record, r_record
    type(aarch64_encoding_record_t) :: a_record
    type(target_ir_t) :: riscv_target, aarch_target
    type(source_ref_t) :: riscv_source, aarch_source, riscv_target_source, aarch_target_source
    integer(int64) :: values(3, 3), words(4), short_words(2)
    integer(int32) :: counts(3), encoded_count, status
    integer(int64) :: expected_i, expected_r, expected_a

    call make_witnesses(riscv_target, aarch_target, riscv_source, aarch_source, &
        riscv_target_source, aarch_target_source, i_record, r_record, a_record)
    items(1) = make_riscv_i_source_batch_item(riscv_target, i_record)
    items(2) = make_riscv_r_source_batch_item(riscv_target, r_record)
    items(3) = make_aarch64_source_batch_item(aarch_target, a_record)
    values = 0_int64
    values(1, :) = [10_int64, 11_int64, int(z'123', int64)]
    values(2, :) = [10_int64, 11_int64, 12_int64]
    values(3, 1:2) = [3_int64, 4_int64]
    counts = [3_int32, 3_int32, 2_int32]

    expected_i = ior(int(z'00000013', int64), ishft(10_int64, 7))
    expected_i = ior(expected_i, ishft(11_int64, 15))
    expected_i = ior(expected_i, ishft(int(z'123', int64), 20))
    expected_r = ior(int(z'00000033', int64), ishft(10_int64, 7))
    expected_r = ior(expected_r, ishft(11_int64, 15))
    expected_r = ior(expected_r, ishft(12_int64, 20))
    expected_a = ior(int(z'91000000', int64), 3_int64)
    expected_a = ior(expected_a, ishft(4_int64, 5))

    call encode_targetir_source_batch(items, values, counts, table, words, encoded_count, status)
    call assert_status(status, 0_int32, 'mixed source batch rejected')
    call assert_int(encoded_count, 3_int32, 'encoded count changed')
    if (words(1) /= expected_i .or. words(2) /= expected_r .or. words(3) /= expected_a) &
        error stop 'independent word oracle disagrees'
    if (words(4) /= 0_int64) error stop 'unused word output was not zero'
    call assert_order_and_provenance(table, riscv_source, aarch_source, riscv_target_source, &
        aarch_target_source)

    bad_items = items
    bad_items(2)%riscv_r%mask = 0_int64
    call poison(table, words)
    call encode_targetir_source_batch(bad_items, values, counts, table, words, encoded_count, &
        status)
    call assert_status(status, targetir_encoding_malformed, 'malformed source accepted')
    call assert_rollback(table, words, encoded_count, 'malformed source rollback failed')

    call poison(table, words)
    counts(2) = 2_int32
    call encode_targetir_source_batch(items, values, counts, table, words, encoded_count, status)
    call assert_status(status, targetir_encoding_malformed, 'wrong operand count accepted')
    call assert_rollback(table, words, encoded_count, 'wrong count rollback failed')
    counts(2) = 3_int32

    call poison(table, words)
    values(1, 3) = int(z'1000', int64)
    call encode_targetir_source_batch(items, values, counts, table, words, encoded_count, status)
    call assert_status(status, targetir_encoding_malformed, 'out-of-range operand accepted')
    call assert_rollback(table, words, encoded_count, 'out-of-range rollback failed')
    values(1, 3) = int(z'123', int64)

    short_words = 77_int64
    call poison(table, words)
    call encode_targetir_source_batch(items, values, counts, table, short_words, encoded_count, &
        status)
    call assert_status(status, targetir_source_batch_capacity, 'short word output accepted')
    call assert_rollback(table, short_words, encoded_count, 'capacity rollback failed')

    call encode_targetir_source_batch(items, values, counts, table, words, encoded_count, status)
    call assert_status(status, 0_int32, 'successful retry rejected')
    if (words(1) /= expected_i .or. words(2) /= expected_r .or. words(3) /= expected_a) &
        error stop 'successful retry changed words'

    write (*, '(a)') 'TargetIR source batch codec behavioral checks: ok'

contains

    subroutine make_witnesses(riscv_target, aarch_target, riscv_source, aarch_source, &
            riscv_target_source, aarch_target_source, i_record, r_record, a_record)
        type(target_ir_t), intent(out) :: riscv_target, aarch_target
        type(source_ref_t), intent(out) :: riscv_source, aarch_source
        type(source_ref_t), intent(out) :: riscv_target_source, aarch_target_source
        type(riscv_opcode_record_t), intent(out) :: i_record, r_record
        type(aarch64_encoding_record_t), intent(out) :: a_record

        riscv_source = make_source_ref('riscv-opcodes', 'rv', 'riscv-record-hash', 'IMPORTED')
        riscv_target_source = make_source_ref('riscv-isa', 'unprivileged', &
            'riscv-target-hash', 'IMPORTED')
        riscv_target = make_target_ir('riscv64', 64_int32, .true., riscv_target_source)
        i_record = riscv_opcode_record_t('addi', 'I', int(z'00000013', int64), &
            int(z'0000707F', int64), riscv_source)
        r_record = riscv_opcode_record_t('add', 'R', int(z'00000033', int64), &
            int(z'FE00707F', int64), riscv_source)

        aarch_source = make_source_ref('aarchmrs', 'A64', 'aarch-record-hash', 'PROSE')
        aarch_target_source = make_source_ref('arm-architecture', 'A-profile', &
            'aarch-target-hash', 'IMPORTED')
        aarch_target = make_target_ir('aarch64', 32_int32, .true., aarch_target_source)
        a_record = aarch64_encoding_record_t()
        a_record%operation_id = 'ADD_addsub_imm'
        a_record%width = 32_int32
        a_record%match = int(z'91000000', int64)
        a_record%mask = int(z'FF800000', int64)
        a_record%variable_range_count = 2_int32
        a_record%variable_ranges(1) = aarch64_variable_range_t(0_int32, 5_int32)
        a_record%variable_ranges(2) = aarch64_variable_range_t(5_int32, 5_int32)
        a_record%target = aarch_target
        a_record%source = aarch_source
    end subroutine make_witnesses

    subroutine assert_order_and_provenance(table, riscv_source, aarch_source, &
            riscv_target_source, aarch_target_source)
        type(targetir_encoding_table_t), intent(in) :: table
        type(source_ref_t), intent(in) :: riscv_source, aarch_source
        type(source_ref_t), intent(in) :: riscv_target_source, aarch_target_source

        if (table%count /= 3_int32) error stop 'normalized table count changed'
        if (trim(table%records(1)%operation_id) /= 'addi') error stop 'I record order changed'
        if (trim(table%records(2)%operation_id) /= 'add') error stop 'R record order changed'
        if (trim(table%records(3)%operation_id) /= 'ADD_addsub_imm') &
            error stop 'AArch64 record order changed'
        call assert_source(table%records(1)%source, riscv_source)
        call assert_source(table%records(2)%source, riscv_source)
        call assert_source(table%records(3)%source, aarch_source)
        call assert_source(table%records(1)%target%source, riscv_target_source)
        call assert_source(table%records(2)%target%source, riscv_target_source)
        call assert_source(table%records(3)%target%source, aarch_target_source)
        if (table%records(1)%variable_fields(3)%start /= 20_int32 .or. &
            table%records(2)%variable_fields(3)%start /= 20_int32 .or. &
            table%records(3)%variable_fields(1)%start /= 0_int32 .or. &
            table%records(3)%variable_fields(2)%start /= 5_int32) &
            error stop 'variable-field ordinal order changed'
    end subroutine assert_order_and_provenance

    subroutine poison(table, words)
        type(targetir_encoding_table_t), intent(out) :: table
        integer(int64), intent(out) :: words(:)

        table%count = 1_int32
        table%records(1)%operation_id = 'stale'
        words = 99_int64
    end subroutine poison

    subroutine assert_rollback(table, words, encoded_count, message)
        type(targetir_encoding_table_t), intent(in) :: table
        integer(int64), intent(in) :: words(:)
        integer(int32), intent(in) :: encoded_count
        character(len=*), intent(in) :: message
        integer :: i

        if (table%count /= 0_int32 .or. encoded_count /= 0_int32) error stop message
        do i = 1, size(table%records)
            if (len_trim(table%records(i)%operation_id) /= 0) error stop message
        end do
        do i = 1, size(words)
            if (words(i) /= 0_int64) error stop message
        end do
    end subroutine assert_rollback

    subroutine assert_source(actual, expected)
        type(source_ref_t), intent(in) :: actual, expected

        if (actual%artifact /= expected%artifact .or. actual%object /= expected%object .or. &
            actual%source_hash /= expected%source_hash .or. actual%origin /= expected%origin) &
            error stop 'source provenance changed'
    end subroutine assert_source

    subroutine assert_status(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_status

    subroutine assert_int(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

end program test_targetir_source_batch_codec
