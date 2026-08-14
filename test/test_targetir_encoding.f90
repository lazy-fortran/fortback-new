program test_targetir_encoding
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_t
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, target_ir_t
    use fortback_targetir_encoding, only: normalize_aarch64_record, &
        normalize_riscv_i_record, targetir_encoding_invalid_target, &
        targetir_encoding_malformed, targetir_encoding_ok, targetir_encoding_unsupported, &
        targetir_encoding_record_t
    implicit none

    type(source_ref_t) :: riscv_source, aarch_source
    type(target_ir_t) :: riscv_target, aarch_target, bad_target
    type(riscv_opcode_record_t) :: riscv_record, bad_riscv
    type(aarch64_encoding_record_t) :: aarch_record, bad_aarch
    type(targetir_encoding_record_t) :: normalized
    integer(int32) :: status

    riscv_source = make_source_ref('riscv-opcodes', 'rv_i', 'riscv-sha', 'IMPORTED')
    aarch_source = make_source_ref('aarchmrs-instructions', 'Instructions.json', &
        'aarch-sha', 'IMPORTED')
    riscv_target = make_target_ir('riscv64', 64_int32, .true., riscv_source)
    aarch_target = make_target_ir('aarch64', 32_int32, .true., aarch_source)

    riscv_record = riscv_opcode_record_t('addi', 'I', int(z'00000013', int64), &
        int(z'0000707F', int64), riscv_source)
    call normalize_riscv_i_record(riscv_target, riscv_record, normalized, status)
    call assert_status(status, targetir_encoding_ok, 'RISC-V record rejected')
    call assert_equal(trim(normalized%target%architecture), 'riscv64', 'RISC-V target lost')
    call assert_equal(trim(normalized%operation_id), 'addi', 'RISC-V operation lost')
    call assert_int(normalized%word_bits, 32_int32, 'RISC-V width changed')
    call assert64(normalized%fixed_match, int(z'13', int64), 'RISC-V match changed')
    call assert64(normalized%fixed_mask, int(z'707F', int64), 'RISC-V mask changed')
    call assert_int(normalized%variable_field_count, 3_int32, 'RISC-V fields not derived')
    call assert_field(normalized, 1_int32, 7_int32, 5_int32)
    call assert_field(normalized, 2_int32, 15_int32, 5_int32)
    call assert_field(normalized, 3_int32, 20_int32, 12_int32)
    call assert_equal(trim(normalized%source%source_hash), 'riscv-sha', &
        'RISC-V provenance lost')

    bad_riscv = riscv_record
    bad_riscv%format = 'R'
    call normalize_riscv_i_record(riscv_target, bad_riscv, normalized, status)
    call assert_status(status, targetir_encoding_unsupported, 'RISC-V format accepted')
    call assert_empty(normalized, 'RISC-V unsupported output not cleared')
    bad_riscv = riscv_record
    bad_riscv%mask = 0_int64
    call normalize_riscv_i_record(riscv_target, bad_riscv, normalized, status)
    call assert_status(status, targetir_encoding_malformed, 'RISC-V malformed mask accepted')
    bad_target = make_target_ir('aarch64', 32_int32, .true., aarch_source)
    call normalize_riscv_i_record(bad_target, riscv_record, normalized, status)
    call assert_status(status, targetir_encoding_invalid_target, 'wrong RISC-V target accepted')
    call assert_empty(normalized, 'wrong-target output not cleared')

    aarch_record = aarch64_encoding_record_t()
    aarch_record%operation_id = 'ADD_addsub_imm'
    aarch_record%width = 32_int32
    aarch_record%match = int(z'91000000', int64)
    aarch_record%mask = int(z'FF800000', int64)
    aarch_record%variable_range_count = 1_int32
    aarch_record%variable_ranges(1) = aarch64_variable_range_t(0_int32, 23_int32)
    aarch_record%target = aarch_target
    aarch_record%source = aarch_source
    call normalize_aarch64_record(aarch_target, aarch_record, normalized, status)
    call assert_status(status, targetir_encoding_ok, 'AArch64 record rejected')
    call assert_equal(trim(normalized%operation_id), 'ADD_addsub_imm', &
        'AArch64 operation lost')
    call assert_int(normalized%word_bits, 32_int32, 'AArch64 width changed')
    call assert64(normalized%fixed_match, int(z'91000000', int64), 'AArch64 match changed')
    call assert64(normalized%fixed_mask, int(z'FF800000', int64), 'AArch64 mask changed')
    call assert_int(normalized%variable_field_count, 1_int32, 'AArch64 field count changed')
    call assert_field(normalized, 1_int32, 0_int32, 23_int32)
    call assert_equal(trim(normalized%source%artifact), 'aarchmrs-instructions', &
        'AArch64 provenance lost')

    bad_aarch = aarch_record
    bad_aarch%variable_ranges(1)%start = 23_int32
    bad_aarch%variable_ranges(1)%width = 2_int32
    call normalize_aarch64_record(aarch_target, bad_aarch, normalized, status)
    call assert_status(status, targetir_encoding_malformed, 'fixed-bit field accepted')
    call assert_empty(normalized, 'AArch64 malformed output not cleared')
    bad_aarch = aarch_record
    bad_aarch%target%architecture = 'riscv64'
    call normalize_aarch64_record(aarch_target, bad_aarch, normalized, status)
    call assert_status(status, targetir_encoding_invalid_target, 'AArch64 wrong record target accepted')
    bad_aarch = aarch_record
    bad_aarch%width = 64_int32
    call normalize_aarch64_record(aarch_target, bad_aarch, normalized, status)
    call assert_status(status, targetir_encoding_unsupported, 'AArch64 width accepted')

    write (*, '(a)') 'TargetIR encoding normalization checks: ok'

contains

    subroutine assert_status(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_status

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal

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

    subroutine assert_field(record, ordinal, start, width)
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int32), intent(in) :: ordinal, start, width

        if (record%variable_fields(ordinal)%ordinal /= ordinal) error stop 'field ordinal changed'
        if (record%variable_fields(ordinal)%start /= start) error stop 'field start changed'
        if (record%variable_fields(ordinal)%width /= width) error stop 'field width changed'
    end subroutine assert_field

    subroutine assert_empty(record, message)
        type(targetir_encoding_record_t), intent(in) :: record
        character(len=*), intent(in) :: message

        if (len_trim(record%operation_id) /= 0) error stop message
        if (record%word_bits /= 0_int32) error stop message
        if (record%variable_field_count /= 0_int32) error stop message
    end subroutine assert_empty

end program test_targetir_encoding
