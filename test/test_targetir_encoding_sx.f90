program test_targetir_encoding_sx
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, target_ir_t
    use fortback_targetir_encoding, only: targetir_encoding_record_t, &
        targetir_variable_field_t
    use fortback_targetir_encoding_sx, only: read_targetir_encoding_sx, &
        targetir_encoding_sx_capacity, targetir_encoding_sx_malformed, &
        targetir_encoding_sx_ok, targetir_encoding_sx_unsupported, &
        write_targetir_encoding_sx
    implicit none

    type(source_ref_t) :: source, target_source
    type(target_ir_t) :: target
    type(targetir_encoding_record_t) :: record, round_trip
    character(len=4096) :: text
    character(len=64) :: short_text
    integer(int32) :: status

    source = make_source_ref('riscv-opcodes', 'rv32_i/addi', 'record-hash', 'MACHINE')
    target_source = make_source_ref('riscv-isa', 'unprivileged', 'target-hash', 'IMPORTED')
    target = make_target_ir('riscv64', 64_int32, .true., target_source)
    record = targetir_encoding_record_t()
    record%target = target
    record%operation_id = 'addi'
    record%word_bits = 32_int32
    record%fixed_mask = int(z'0000707F', int64)
    record%fixed_match = int(z'00000013', int64)
    record%variable_field_count = 3_int32
    record%variable_fields(1) = targetir_variable_field_t(1_int32, 7_int32, 5_int32)
    record%variable_fields(2) = targetir_variable_field_t(2_int32, 15_int32, 5_int32)
    record%variable_fields(3) = targetir_variable_field_t(3_int32, 20_int32, 12_int32)
    record%source = source
    call write_targetir_encoding_sx(record, text, status)
    call assert_status(status, targetir_encoding_sx_ok, 'RISC-V record was not written')
    call read_targetir_encoding_sx(trim(text), round_trip, status)
    call assert_status(status, targetir_encoding_sx_ok, 'RISC-V record was not read')
    call assert_record(round_trip, record, 'RISC-V record changed in round trip')

    source = make_source_ref('aarchmrs', 'A64/ADD', 'arm-record-hash', 'PROSE')
    target_source = make_source_ref('arm-architecture', 'A-profile', 'arm-target-hash', 'IMPORTED')
    target = make_target_ir('aarch64', 32_int32, .true., target_source)
    record%target = target
    record%operation_id = 'ADD'
    record%word_bits = 32_int32
    record%fixed_mask = int(z'FF800000', int64)
    record%fixed_match = int(z'91000000', int64)
    record%variable_field_count = 2_int32
    record%variable_fields(1) = targetir_variable_field_t(1_int32, 0_int32, 5_int32)
    record%variable_fields(2) = targetir_variable_field_t(2_int32, 5_int32, 5_int32)
    record%source = source
    call write_targetir_encoding_sx(record, text, status)
    call assert_status(status, targetir_encoding_sx_ok, 'AArch64 record was not written')
    call read_targetir_encoding_sx(trim(text), round_trip, status)
    call assert_status(status, targetir_encoding_sx_ok, 'AArch64 record was not read')
    call assert_record(round_trip, record, 'AArch64 record changed in round trip')

    call read_targetir_encoding_sx('(targetir-encoding-v0 (target (architecture aarch64)', &
        round_trip, status)
    call assert_status(status, targetir_encoding_sx_malformed, 'truncated SX was accepted')
    call assert_blank(round_trip, 'malformed read retained a record')

    record%variable_fields(2) = targetir_variable_field_t(2_int32, 4_int32, 5_int32)
    call write_targetir_encoding_sx(record, text, status)
    call assert_status(status, targetir_encoding_sx_malformed, 'overlap was serialized')
    call assert_blank_text(text, 'failed write retained text')

    record%variable_fields(2) = targetir_variable_field_t(2_int32, 5_int32, 5_int32)
    record%variable_fields(2)%ordinal = 3_int32
    call write_targetir_encoding_sx(record, text, status)
    call assert_status(status, targetir_encoding_sx_malformed, 'out-of-order fields were serialized')
    call assert_blank_text(text, 'field-order failure retained text')
    record%variable_fields(2)%ordinal = 2_int32
    record%source%origin = ''
    call write_targetir_encoding_sx(record, text, status)
    call assert_status(status, targetir_encoding_sx_malformed, 'incomplete provenance was serialized')
    call assert_blank_text(text, 'provenance failure retained text')
    record%source = source

    short_text = ''
    call write_targetir_encoding_sx(record, short_text, status)
    call assert_status(status, targetir_encoding_sx_capacity, 'short output was accepted')
    call assert_blank_text(short_text, 'capacity failure retained text')

    record%word_bits = 64_int32
    call write_targetir_encoding_sx(record, text, status)
    call assert_status(status, targetir_encoding_sx_unsupported, &
        'unsupported encoding width was accepted')
    call assert_blank_text(text, 'unsupported-width failure retained text')

    write (*, '(a)') 'TargetIR encoding SX behavioral checks: ok'

contains

    subroutine assert_status(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_status

    subroutine assert_record(actual, expected, message)
        type(targetir_encoding_record_t), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        integer(int32) :: i

        if (trim(actual%target%architecture) /= trim(expected%target%architecture)) error stop message
        if (actual%target%word_bits /= expected%target%word_bits) error stop message
        if (actual%target%little_endian .neqv. expected%target%little_endian) error stop message
        call assert_source(actual%target%source, expected%target%source, message)
        if (trim(actual%operation_id) /= trim(expected%operation_id)) error stop message
        if (actual%word_bits /= expected%word_bits) error stop message
        if (actual%fixed_mask /= expected%fixed_mask) error stop message
        if (actual%fixed_match /= expected%fixed_match) error stop message
        if (actual%variable_field_count /= expected%variable_field_count) error stop message
        do i = 1, actual%variable_field_count
            if (actual%variable_fields(i)%ordinal /= expected%variable_fields(i)%ordinal) error stop message
            if (actual%variable_fields(i)%start /= expected%variable_fields(i)%start) error stop message
            if (actual%variable_fields(i)%width /= expected%variable_fields(i)%width) error stop message
        end do
        call assert_source(actual%source, expected%source, message)
    end subroutine assert_record

    subroutine assert_source(actual, expected, message)
        type(source_ref_t), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (trim(actual%artifact) /= trim(expected%artifact)) error stop message
        if (trim(actual%object) /= trim(expected%object)) error stop message
        if (trim(actual%source_hash) /= trim(expected%source_hash)) error stop message
        if (trim(actual%origin) /= trim(expected%origin)) error stop message
    end subroutine assert_source

    subroutine assert_blank(record, message)
        type(targetir_encoding_record_t), intent(in) :: record
        character(len=*), intent(in) :: message

        if (len_trim(record%operation_id) /= 0) error stop message
        if (record%variable_field_count /= 0) error stop message
    end subroutine assert_blank

    subroutine assert_blank_text(value, message)
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: message

        if (len_trim(value) /= 0) error stop message
    end subroutine assert_blank_text

end program test_targetir_encoding_sx
