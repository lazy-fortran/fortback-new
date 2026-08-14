program test_targetir_source_batch
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_t
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, target_ir_t
    use fortback_targetir_encoding, only: normalize_aarch64_record, &
        normalize_riscv_i_record, targetir_encoding_record_t
    use fortback_targetir_source_batch, only: make_aarch64_source_batch_item, &
        make_riscv_i_source_batch_item, normalize_targetir_source_batch, &
        targetir_source_batch_aarch64, targetir_source_batch_capacity, &
        targetir_source_batch_duplicate, targetir_source_batch_invalid_target, &
        targetir_source_batch_item_t, targetir_source_batch_malformed, &
        targetir_source_batch_ok, targetir_source_batch_riscv_i, &
        targetir_source_batch_unsupported
    use fortback_targetir_table, only: targetir_encoding_table_t, targetir_table_capacity
    implicit none

    type(source_ref_t) :: riscv_source, riscv_target_source
    type(source_ref_t) :: aarch_source, aarch_target_source
    type(target_ir_t) :: riscv_target, aarch_target, bad_target
    type(riscv_opcode_record_t) :: riscv_addi, riscv_xori, bad_riscv
    type(aarch64_encoding_record_t) :: aarch_add, aarch_sub, bad_aarch
    type(targetir_source_batch_item_t) :: riscv_items(2), aarch_items(2)
    type(targetir_source_batch_item_t) :: mixed_items(4)
    type(targetir_source_batch_item_t) :: controls(2)
    type(targetir_source_batch_item_t) :: capacity_items(targetir_table_capacity + 1)
    type(targetir_encoding_record_t) :: normalized
    type(targetir_encoding_table_t) :: table
    integer(int32) :: status, i
    character(len=16) :: operation

    call make_witnesses(riscv_source, riscv_target_source, riscv_target, &
        riscv_addi, riscv_xori)
    call make_aarch_witnesses(aarch_source, aarch_target_source, aarch_target, &
        aarch_add, aarch_sub)

    riscv_items(1) = make_riscv_i_source_batch_item(riscv_target, riscv_addi)
    riscv_items(2) = make_riscv_i_source_batch_item(riscv_target, riscv_xori)
    aarch_items(1) = make_aarch64_source_batch_item(aarch_target, aarch_add)
    aarch_items(2) = make_aarch64_source_batch_item(aarch_target, aarch_sub)
    mixed_items(1) = riscv_items(1)
    mixed_items(2) = aarch_items(1)
    mixed_items(3) = riscv_items(2)
    mixed_items(4) = aarch_items(2)

    call normalize_targetir_source_batch(riscv_items, table, status)
    call assert_status(status, targetir_source_batch_ok, 'RISC-V batch rejected')
    call assert_int(table%count, 2_int32, 'RISC-V batch count changed')
    call assert_riscv_record(table%records(1), 'addi', riscv_target, riscv_source, &
        int(z'00000013', int64), int(z'0000707F', int64), 'RISC-V order changed')
    call assert_riscv_record(table%records(2), 'xori', riscv_target, riscv_source, &
        int(z'00004013', int64), int(z'0000707F', int64), &
        'RISC-V second record changed')

    call normalize_targetir_source_batch(aarch_items, table, status)
    call assert_status(status, targetir_source_batch_ok, 'AArch64 batch rejected')
    call assert_int(table%count, 2_int32, 'AArch64 batch count changed')
    call assert_aarch_record(table%records(1), 'ADD_addsub_imm', aarch_target, &
        aarch_source, int(z'91000000', int64), int(z'FF800000', int64), &
        'AArch64 order changed')
    call assert_aarch_record(table%records(2), 'SUB_addsub_imm', aarch_target, &
        aarch_source, int(z'D1000000', int64), int(z'FF800000', int64), &
        'AArch64 second record changed')

    call normalize_targetir_source_batch(mixed_items, table, status)
    call assert_status(status, targetir_source_batch_ok, 'mixed batch rejected')
    call assert_int(table%count, 4_int32, 'mixed batch count changed')
    call assert_riscv_record(table%records(1), 'addi', riscv_target, riscv_source, &
        int(z'00000013', int64), int(z'0000707F', int64), 'mixed order changed')
    call assert_aarch_record(table%records(2), 'ADD_addsub_imm', aarch_target, &
        aarch_source, int(z'91000000', int64), int(z'FF800000', int64), &
        'mixed AArch64 order changed')
    call assert_riscv_record(table%records(3), 'xori', riscv_target, riscv_source, &
        int(z'00004013', int64), int(z'0000707F', int64), &
        'mixed RISC-V order changed')
    call assert_aarch_record(table%records(4), 'SUB_addsub_imm', aarch_target, &
        aarch_source, int(z'D1000000', int64), int(z'FF800000', int64), &
        'mixed final order changed')
    call assert_equivalent_to_single(mixed_items, table, riscv_target, aarch_target)

    bad_riscv = riscv_addi
    bad_riscv%mask = 0_int64
    controls(1) = make_riscv_i_source_batch_item(riscv_target, riscv_addi)
    controls(2) = make_riscv_i_source_batch_item(riscv_target, bad_riscv)
    call normalize_targetir_source_batch(controls, table, status)
    call assert_status(status, targetir_source_batch_malformed, &
        'malformed source record accepted')
    call assert_table_empty(table, 'malformed batch retained a prefix')

    bad_target = make_target_ir('aarch64', 32_int32, .true., aarch_target_source)
    controls(2) = make_riscv_i_source_batch_item(bad_target, riscv_addi)
    call normalize_targetir_source_batch(controls, table, status)
    call assert_status(status, targetir_source_batch_invalid_target, &
        'wrong target accepted')
    call assert_table_empty(table, 'wrong-target batch retained a prefix')

    bad_aarch = aarch_add
    bad_aarch%width = 64_int32
    controls(1) = make_aarch64_source_batch_item(aarch_target, aarch_add)
    controls(2) = make_aarch64_source_batch_item(aarch_target, bad_aarch)
    call normalize_targetir_source_batch(controls, table, status)
    call assert_status(status, targetir_source_batch_unsupported, &
        'unsupported shape accepted')
    call assert_table_empty(table, 'unsupported batch retained a prefix')

    controls(1) = make_riscv_i_source_batch_item(riscv_target, riscv_addi)
    controls(2) = make_riscv_i_source_batch_item(riscv_target, riscv_addi)
    call normalize_targetir_source_batch(controls, table, status)
    call assert_status(status, targetir_source_batch_duplicate, 'duplicate accepted')
    call assert_table_empty(table, 'duplicate batch retained a prefix')

    do i = 1, size(capacity_items)
        operation = ''
        write (operation, '(a,i3.3)') 'op', i
        capacity_items(i) = make_riscv_i_source_batch_item(riscv_target, riscv_addi)
        capacity_items(i)%riscv_i%mnemonic = operation
    end do
    call normalize_targetir_source_batch(capacity_items, table, status)
    call assert_status(status, targetir_source_batch_capacity, 'capacity accepted')
    call assert_table_empty(table, 'capacity batch retained a prefix')

    controls(1)%kind = 99_int32
    controls(2) = make_riscv_i_source_batch_item(riscv_target, riscv_addi)
    call normalize_targetir_source_batch(controls, table, status)
    call assert_status(status, targetir_source_batch_unsupported, &
        'unknown source family accepted')
    call assert_table_empty(table, 'unknown family retained a prefix')

    write (*, '(a)') 'TargetIR source batch behavioral checks: ok'

contains

    subroutine make_witnesses(source, target_source, target, addi, xori)
        type(source_ref_t), intent(out) :: source, target_source
        type(target_ir_t), intent(out) :: target
        type(riscv_opcode_record_t), intent(out) :: addi, xori

        source = make_source_ref('riscv-opcodes', 'rv_i', 'riscv-record-hash', 'IMPORTED')
        target_source = make_source_ref('riscv-isa', 'unprivileged', &
            'riscv-target-hash', 'IMPORTED')
        target = make_target_ir('riscv64', 64_int32, .true., target_source)
        addi = riscv_opcode_record_t('addi', 'I', int(z'00000013', int64), &
            int(z'0000707F', int64), source)
        xori = riscv_opcode_record_t('xori', 'I', int(z'00004013', int64), &
            int(z'0000707F', int64), source)
    end subroutine make_witnesses

    subroutine make_aarch_witnesses(source, target_source, target, add, sub)
        type(source_ref_t), intent(out) :: source, target_source
        type(target_ir_t), intent(out) :: target
        type(aarch64_encoding_record_t), intent(out) :: add, sub

        source = make_source_ref('aarchmrs', 'A64/addsub', 'aarch-record-hash', 'PROSE')
        target_source = make_source_ref('arm-architecture', 'A-profile', &
            'aarch-target-hash', 'IMPORTED')
        target = make_target_ir('aarch64', 32_int32, .true., target_source)
        add = aarch64_encoding_record_t()
        add%operation_id = 'ADD_addsub_imm'
        add%width = 32_int32
        add%match = int(z'91000000', int64)
        add%mask = int(z'FF800000', int64)
        add%variable_range_count = 2_int32
        add%variable_ranges(1) = aarch64_variable_range_t(0_int32, 5_int32)
        add%variable_ranges(2) = aarch64_variable_range_t(5_int32, 5_int32)
        add%target = target
        add%source = source
        sub = add
        sub%operation_id = 'SUB_addsub_imm'
        sub%match = int(z'D1000000', int64)
    end subroutine make_aarch_witnesses

    subroutine assert_equivalent_to_single(items, table, riscv_target, aarch_target)
        type(targetir_source_batch_item_t), intent(in) :: items(:)
        type(targetir_encoding_table_t), intent(in) :: table
        type(target_ir_t), intent(in) :: riscv_target, aarch_target
        type(targetir_encoding_record_t) :: expected
        integer(int32) :: status, i

        do i = 1, size(items)
            if (items(i)%kind == targetir_source_batch_riscv_i) then
                call normalize_riscv_i_record(riscv_target, items(i)%riscv_i, expected, status)
            else if (items(i)%kind == targetir_source_batch_aarch64) then
                call normalize_aarch64_record(aarch_target, items(i)%aarch64, expected, status)
            else
                error stop 'unexpected source family in equivalence witness'
            end if
            call assert_status(status, targetir_source_batch_ok, &
                'single-record normalization failed')
            call assert_record_equal(table%records(i), expected, &
                'batch differs from single-record normalization')
        end do
    end subroutine assert_equivalent_to_single

    subroutine assert_riscv_record(actual, operation, target, source, match, mask, message)
        type(targetir_encoding_record_t), intent(in) :: actual
        character(len=*), intent(in) :: operation, message
        type(target_ir_t), intent(in) :: target
        type(source_ref_t), intent(in) :: source
        integer(int64), intent(in) :: match, mask

        if (trim(actual%operation_id) /= operation) error stop message
        if (trim(actual%target%architecture) /= trim(target%architecture)) error stop message
        if (actual%fixed_match /= match .or. actual%fixed_mask /= mask) error stop message
        if (actual%word_bits /= 32_int32 .or. actual%variable_field_count /= 3_int32) &
            error stop message
        call assert_source(actual%target%source, target%source, message)
        call assert_source(actual%source, source, message)
        call assert_field(actual, 1_int32, 7_int32, 5_int32, message)
        call assert_field(actual, 2_int32, 15_int32, 5_int32, message)
        call assert_field(actual, 3_int32, 20_int32, 12_int32, message)
    end subroutine assert_riscv_record

    subroutine assert_aarch_record(actual, operation, target, source, match, mask, message)
        type(targetir_encoding_record_t), intent(in) :: actual
        character(len=*), intent(in) :: operation, message
        type(target_ir_t), intent(in) :: target
        type(source_ref_t), intent(in) :: source
        integer(int64), intent(in) :: match, mask

        if (trim(actual%operation_id) /= operation) error stop message
        if (trim(actual%target%architecture) /= trim(target%architecture)) error stop message
        if (actual%fixed_match /= match .or. actual%fixed_mask /= mask) error stop message
        if (actual%word_bits /= 32_int32 .or. actual%variable_field_count /= 2_int32) &
            error stop message
        call assert_source(actual%target%source, target%source, message)
        call assert_source(actual%source, source, message)
        call assert_field(actual, 1_int32, 0_int32, 5_int32, message)
        call assert_field(actual, 2_int32, 5_int32, 5_int32, message)
    end subroutine assert_aarch_record

    subroutine assert_record_equal(actual, expected, message)
        type(targetir_encoding_record_t), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        integer :: i

        if (actual%target%architecture /= expected%target%architecture) error stop message
        if (actual%target%word_bits /= expected%target%word_bits) error stop message
        if (actual%target%little_endian .neqv. expected%target%little_endian) &
            error stop message
        if (actual%operation_id /= expected%operation_id) error stop message
        if (actual%word_bits /= expected%word_bits .or. &
            actual%fixed_match /= expected%fixed_match .or. &
            actual%fixed_mask /= expected%fixed_mask .or. &
            actual%variable_field_count /= expected%variable_field_count) error stop message
        do i = 1, actual%variable_field_count
            if (actual%variable_fields(i)%ordinal /= expected%variable_fields(i)%ordinal) &
                error stop message
            if (actual%variable_fields(i)%start /= expected%variable_fields(i)%start .or. &
                actual%variable_fields(i)%width /= expected%variable_fields(i)%width) &
                error stop message
        end do
        call assert_source(actual%target%source, expected%target%source, message)
        call assert_source(actual%source, expected%source, message)
    end subroutine assert_record_equal

    subroutine assert_field(record, ordinal, start, width, message)
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int32), intent(in) :: ordinal, start, width
        character(len=*), intent(in) :: message

        if (record%variable_fields(ordinal)%ordinal /= ordinal .or. &
            record%variable_fields(ordinal)%start /= start .or. &
            record%variable_fields(ordinal)%width /= width) error stop message
    end subroutine assert_field

    subroutine assert_source(actual, expected, message)
        type(source_ref_t), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual%artifact /= expected%artifact .or. actual%object /= expected%object .or. &
            actual%source_hash /= expected%source_hash .or. actual%origin /= expected%origin) &
            error stop message
    end subroutine assert_source

    subroutine assert_table_empty(table, message)
        type(targetir_encoding_table_t), intent(in) :: table
        character(len=*), intent(in) :: message
        integer :: i

        if (table%count /= 0_int32) error stop message
        do i = 1, size(table%records)
            if (len_trim(table%records(i)%operation_id) /= 0) error stop message
        end do
    end subroutine assert_table_empty

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

end program test_targetir_source_batch
