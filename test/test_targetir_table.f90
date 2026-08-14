program test_targetir_table
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_t
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, target_ir_t
    use fortback_targetir_codec, only: targetir_decode_record, targetir_encode_record
    use fortback_targetir_encoding, only: normalize_aarch64_record, &
        normalize_riscv_i_record, targetir_encoding_ok, targetir_encoding_record_t
    use fortback_targetir_table, only: targetir_encoding_table_append, &
        targetir_encoding_table_clear, &
        targetir_encoding_table_finalize, targetir_encoding_table_lookup_candidates, &
        targetir_encoding_table_lookup_origin, targetir_encoding_table_lookup_source, &
        targetir_encoding_table_t, targetir_table_capacity, targetir_table_duplicate, &
        targetir_table_invalid_target, targetir_table_malformed, targetir_table_ok
    implicit none

    type(source_ref_t) :: riscv_source, aarch_source, riscv_target_source
    type(target_ir_t) :: riscv_target, aarch_target
    type(riscv_opcode_record_t) :: riscv_input
    type(aarch64_encoding_record_t) :: aarch_input
    type(targetir_encoding_record_t) :: riscv_record, aarch_record, bad_record
    type(targetir_encoding_record_t) :: records(targetir_table_capacity)
    type(targetir_encoding_table_t) :: table
    integer(int32) :: count, match_count, status
    integer(int32) :: indices(targetir_table_capacity)
    integer(int64) :: word
    integer(int64), allocatable :: values(:)

    riscv_source = make_source_ref('riscv-opcodes', 'rv_i/addi', 'riscv-record-hash', 'IMPORTED')
    aarch_source = make_source_ref('aarchmrs', 'A64/ADD', 'aarch-record-hash', 'IMPORTED')
    riscv_target_source = make_source_ref('riscv-isa', 'unprivileged', &
        'riscv-target-hash', 'IMPORTED')
    riscv_target = make_target_ir('riscv64', 64_int32, .true., riscv_target_source)
    aarch_target = make_target_ir('aarch64', 32_int32, .true., aarch_source)
    riscv_input = riscv_opcode_record_t('addi', 'I', int(z'00000013', int64), &
        int(z'0000707F', int64), riscv_source)
    call normalize_riscv_i_record(riscv_target, riscv_input, riscv_record, status)
    call assert_status(status, targetir_encoding_ok, 'RISC-V normalization failed')

    aarch_input = aarch64_encoding_record_t()
    aarch_input%operation_id = 'ADD_addsub_imm'
    aarch_input%width = 32_int32
    aarch_input%match = int(z'91000000', int64)
    aarch_input%mask = int(z'FF800000', int64)
    aarch_input%variable_range_count = 2_int32
    aarch_input%variable_ranges(1) = aarch64_variable_range_t(0_int32, 5_int32)
    aarch_input%variable_ranges(2) = aarch64_variable_range_t(5_int32, 5_int32)
    aarch_input%target = aarch_target
    aarch_input%source = aarch_source
    call normalize_aarch64_record(aarch_target, aarch_input, aarch_record, status)
    call assert_status(status, targetir_encoding_ok, 'AArch64 normalization failed')

    call targetir_encoding_table_append(table, riscv_target, riscv_record, status)
    call assert_status(status, targetir_table_ok, 'RISC-V append failed')
    call targetir_encoding_table_append(table, aarch_target, aarch_record, status)
    call assert_status(status, targetir_table_ok, 'AArch64 append failed')
    call targetir_encoding_table_finalize(table, records, count, status)
    call assert_status(status, targetir_table_ok, 'mixed table finalization failed')
    call assert_int(count, 2_int32, 'mixed table count changed')
    call assert_record_identity(records(1), riscv_record, 'RISC-V order or identity changed')
    call assert_record_identity(records(2), aarch_record, 'AArch64 order or identity changed')

    indices = 99_int32
    call targetir_encoding_table_lookup_candidates(table, riscv_target, &
        int(z'00000013', int64), indices, match_count, status)
    call assert_status(status, targetir_table_ok, 'RISC-V table lookup failed')
    call assert_int(match_count, 1_int32, 'RISC-V table lookup count changed')
    call assert_indices(indices, [1_int32, 0_int32, 0_int32, 0_int32], &
        'RISC-V table lookup order changed')

    indices = 99_int32
    call targetir_encoding_table_lookup_candidates(table, aarch_target, &
        int(z'91000083', int64), indices, match_count, status)
    call assert_status(status, targetir_table_ok, 'AArch64 table lookup failed')
    call assert_int(match_count, 1_int32, 'AArch64 table lookup count changed')
    call assert_indices(indices, [2_int32, 0_int32, 0_int32, 0_int32], &
        'AArch64 table lookup order changed')

    indices = 99_int32
    call targetir_encoding_table_lookup_source(table, riscv_source, indices, &
        match_count, status)
    call assert_status(status, targetir_table_ok, 'source query failed')
    call assert_int(match_count, 1_int32, 'source query count changed')
    call assert_indices(indices, [1_int32, 0_int32, 0_int32, 0_int32], &
        'source query order changed')

    indices = 99_int32
    call targetir_encoding_table_lookup_source(table, aarch_source, indices, &
        match_count, status)
    call assert_status(status, targetir_table_ok, 'second source query failed')
    call assert_int(match_count, 1_int32, 'second source query count changed')
    call assert_indices(indices, [2_int32, 0_int32, 0_int32, 0_int32], &
        'second source query order changed')

    indices = 99_int32
    call targetir_encoding_table_lookup_origin(table, 'IMPORTED', indices, &
        match_count, status)
    call assert_status(status, targetir_table_ok, 'origin query failed')
    call assert_int(match_count, 2_int32, 'origin query count changed')
    call assert_indices(indices, [1_int32, 2_int32, 0_int32, 0_int32], &
        'origin query order changed')

    indices = 99_int32
    call targetir_encoding_table_lookup_origin(table, 'PROSE', indices, &
        match_count, status)
    call assert_status(status, targetir_table_ok, 'second origin query failed')
    call assert_int(match_count, 0_int32, 'second origin query count changed')
    call assert_indices(indices, [0_int32, 0_int32, 0_int32, 0_int32], &
        'second origin query order changed')

    indices = 99_int32
    call targetir_encoding_table_lookup_origin(table, ' ', indices, match_count, status)
    call assert_status(status, targetir_table_malformed, 'blank origin query accepted')
    call assert_int(match_count, 0_int32, 'failed origin query retained count')
    call assert_indices(indices, [0_int32, 0_int32, 0_int32, 0_int32], &
        'failed origin query retained output')

    indices = 99_int32
    call targetir_encoding_table_lookup_origin(table, 'IMPORTED', indices(1:0), &
        match_count, status)
    call assert_status(status, targetir_table_capacity, 'undersized origin query accepted')
    call assert_int(match_count, 0_int32, 'capacity query retained count')

    indices = 99_int32
    call targetir_encoding_table_lookup_source(table, &
        make_source_ref('unknown', 'object', 'hash', 'IMPORTED'), indices, &
        match_count, status)
    call assert_status(status, targetir_table_ok, 'unknown source query failed')
    call assert_int(match_count, 0_int32, 'unknown source query matched a record')
    call assert_indices(indices, [0_int32, 0_int32, 0_int32, 0_int32], &
        'unknown source query retained output')

    call targetir_encode_record(riscv_target, records(1), [1_int64, 2_int64, 3_int64], &
        word, status)
    call assert_status(status, targetir_encoding_ok, 'RISC-V table record did not encode')
    call assert64(word, int(z'00310093', int64), 'RISC-V encoded word changed')
    call targetir_decode_record(riscv_target, records(1), word, values, status)
    call assert_status(status, targetir_encoding_ok, 'RISC-V table record did not decode')
    call assert_values(values, [1_int64, 2_int64, 3_int64], 'RISC-V values changed')

    call targetir_encode_record(aarch_target, records(2), [3_int64, 4_int64], word, status)
    call assert_status(status, targetir_encoding_ok, 'AArch64 table record did not encode')
    call assert64(word, int(z'91000083', int64), 'AArch64 encoded word changed')
    call targetir_decode_record(aarch_target, records(2), word, values, status)
    call assert_status(status, targetir_encoding_ok, 'AArch64 table record did not decode')
    call assert_values(values, [3_int64, 4_int64], 'AArch64 values changed')

    call targetir_encoding_table_clear(table)
    call targetir_encoding_table_append(table, riscv_target, riscv_record, status)
    call assert_status(status, targetir_table_ok, 'duplicate setup append failed')
    call targetir_encoding_table_append(table, riscv_target, riscv_record, status)
    call assert_status(status, targetir_table_duplicate, 'duplicate operation accepted')
    call assert_int(table%count, 0_int32, 'duplicate retained partial table')

    call targetir_encoding_table_append(table, riscv_target, riscv_record, status)
    bad_record = riscv_record
    bad_record%operation_id = 'malformed'
    bad_record%fixed_match = int(z'02000000', int64)
    call targetir_encoding_table_append(table, riscv_target, bad_record, status)
    call assert_status(status, targetir_table_malformed, 'malformed record accepted')
    call assert_int(table%count, 0_int32, 'malformed append retained partial table')

    call targetir_encoding_table_append(table, riscv_target, riscv_record, status)
    call targetir_encoding_table_append(table, riscv_target, aarch_record, status)
    call assert_status(status, targetir_table_invalid_target, 'wrong target record accepted')
    call assert_int(table%count, 0_int32, 'wrong target retained partial table')

    call append_capacity_witness(table, riscv_target, riscv_record, status)
    call assert_status(status, targetir_table_capacity, 'capacity overflow accepted')
    call assert_int(table%count, 0_int32, 'capacity failure retained partial table')

    call targetir_encoding_table_append(table, riscv_target, riscv_record, status)
    table%records(1)%fixed_match = int(z'02000000', int64)
    indices = 99_int32
    match_count = 99_int32
    call targetir_encoding_table_lookup_source(table, riscv_source, indices, &
        match_count, status)
    call assert_status(status, targetir_table_malformed, 'malformed source table accepted')
    call assert_int(match_count, 0_int32, 'failed source query retained count')
    call assert_indices(indices, [0_int32, 0_int32, 0_int32, 0_int32], &
        'failed source query retained output')
    records = targetir_encoding_record_t()
    records(1)%operation_id = 'stale'
    count = 99_int32
    call targetir_encoding_table_finalize(table, records, count, status)
    call assert_status(status, targetir_table_malformed, 'finalization accepted malformed table')
    call assert_int(count, 0_int32, 'failed finalization retained count')
    call assert_blank(records, 'failed finalization retained records')
    call assert_int(table%count, 0_int32, 'failed finalization retained table')

    write (*, '(a)') 'TargetIR bounded table behavioral checks: ok'

contains

    subroutine append_capacity_witness(value, target, record, result)
        type(targetir_encoding_table_t), intent(inout) :: value
        type(target_ir_t), intent(in) :: target
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int32), intent(out) :: result
        type(targetir_encoding_record_t) :: candidate
        integer(int32) :: i

        do i = 1, targetir_table_capacity
            candidate = record
            write (candidate%operation_id, '(a,i1)') 'capacity-', i
            call targetir_encoding_table_append(value, target, candidate, result)
            if (result /= targetir_table_ok) return
        end do
        candidate = record
        candidate%operation_id = 'capacity-overflow'
        call targetir_encoding_table_append(value, target, candidate, result)
    end subroutine append_capacity_witness

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

    subroutine assert64(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert64

    subroutine assert_indices(actual, expected, message)
        integer(int32), intent(in) :: actual(:), expected(:)
        character(len=*), intent(in) :: message

        if (size(actual) /= size(expected)) error stop message
        if (any(actual /= expected)) error stop message
    end subroutine assert_indices

    subroutine assert_values(actual, expected, message)
        integer(int64), allocatable, intent(in) :: actual(:)
        integer(int64), intent(in) :: expected(:)
        character(len=*), intent(in) :: message

        if (.not. allocated(actual)) error stop message
        if (size(actual) /= size(expected)) error stop message
        if (any(actual /= expected)) error stop message
    end subroutine assert_values

    subroutine assert_record_identity(actual, expected, message)
        type(targetir_encoding_record_t), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (trim(actual%target%architecture) /= trim(expected%target%architecture)) &
            error stop message
        if (actual%target%word_bits /= expected%target%word_bits) error stop message
        if (actual%target%little_endian .neqv. expected%target%little_endian) &
            error stop message
        call assert_source(actual%target%source, expected%target%source, message)
        if (trim(actual%operation_id) /= trim(expected%operation_id)) error stop message
        if (actual%word_bits /= expected%word_bits) error stop message
        if (actual%fixed_match /= expected%fixed_match) error stop message
        if (actual%fixed_mask /= expected%fixed_mask) error stop message
        if (actual%variable_field_count /= expected%variable_field_count) &
            error stop message
        call assert_source(actual%source, expected%source, message)
    end subroutine assert_record_identity

    subroutine assert_source(actual, expected, message)
        type(source_ref_t), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (trim(actual%artifact) /= trim(expected%artifact)) error stop message
        if (trim(actual%object) /= trim(expected%object)) error stop message
        if (trim(actual%source_hash) /= trim(expected%source_hash)) error stop message
        if (trim(actual%origin) /= trim(expected%origin)) error stop message
    end subroutine assert_source

    subroutine assert_blank(values, message)
        type(targetir_encoding_record_t), intent(in) :: values(:)
        character(len=*), intent(in) :: message
        integer(int32) :: i

        do i = 1, size(values)
            if (len_trim(values(i)%operation_id) /= 0) error stop message
        end do
    end subroutine assert_blank

end program test_targetir_table
