program test_targetir_codec
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_variable_range_t
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, target_ir_t
    use fortback_targetir_codec, only: targetir_decode_record, targetir_encode_record
    use fortback_targetir_encoding, only: normalize_aarch64_record, &
        normalize_riscv_i_record, targetir_encoding_invalid_target, &
        targetir_encoding_malformed, targetir_encoding_ok, targetir_encoding_unsupported, &
        targetir_encoding_record_t, targetir_variable_field_t
    implicit none

    type(source_ref_t) :: source
    type(target_ir_t) :: target, wrong_target
    type(targetir_encoding_record_t) :: record, normalized
    type(riscv_opcode_record_t) :: riscv_record
    type(aarch64_encoding_record_t) :: aarch_record
    integer(int64) :: values(2), word
    integer(int64), allocatable :: decoded(:)
    integer(int32) :: status

    source = make_source_ref('target-spec', 'encoding', 'codec-hash', 'IMPORTED')
    target = make_target_ir('test64', 64_int32, .true., source)
    record = targetir_encoding_record_t()
    record%target = target
    record%operation_id = 'test-op'
    record%word_bits = 32_int32
    record%fixed_mask = int(z'F0000000', int64)
    record%fixed_match = int(z'A0000000', int64)
    record%variable_field_count = 2_int32
    record%variable_fields(1) = targetir_variable_field_t(1_int32, 0_int32, 5_int32)
    record%variable_fields(2) = targetir_variable_field_t(2_int32, 8_int32, 4_int32)
    record%source = source

    values = [17_int64, 9_int64]
    call targetir_encode_record(target, record, values, word, status)
    call assert_status(status, targetir_encoding_ok, 'generic record rejected')
    call assert64(word, int(z'A0000911', int64), 'generic encoding changed')
    call targetir_decode_record(target, record, word, decoded, status)
    call assert_status(status, targetir_encoding_ok, 'generic word rejected')
    call assert_values(decoded, values, 'generic round trip changed')

    call targetir_encode_record(target, record, [32_int64, 0_int64], word, status)
    call assert_status(status, targetir_encoding_malformed, 'out-of-range operand accepted')
    call assert64(word, 0_int64, 'failed encode did not clear word')
    call targetir_encode_record(target, record, [1_int64], word, status)
    call assert_status(status, targetir_encoding_malformed, 'wrong operand count accepted')

    call targetir_decode_record(target, record, int(z'B0000911', int64), decoded, status)
    call assert_status(status, targetir_encoding_unsupported, 'fixed-bit mismatch accepted')
    call assert_unallocated(decoded, 'failed decode retained values')
    call targetir_decode_record(target, record, int(z'100000000', int64), decoded, status)
    call assert_status(status, targetir_encoding_malformed, 'wide word accepted')

    record%variable_fields(2) = targetir_variable_field_t(2_int32, 4_int32, 4_int32)
    call targetir_encode_record(target, record, values, word, status)
    call assert_status(status, targetir_encoding_malformed, 'overlapping fields accepted')
    record%variable_fields(2) = targetir_variable_field_t(2_int32, 31_int32, 2_int32)
    call targetir_encode_record(target, record, values, word, status)
    call assert_status(status, targetir_encoding_malformed, 'out-of-range field accepted')

    record%variable_fields(2) = targetir_variable_field_t(2_int32, 8_int32, 4_int32)
    wrong_target = target
    wrong_target%architecture = 'other'
    call targetir_encode_record(wrong_target, record, values, word, status)
    call assert_status(status, targetir_encoding_invalid_target, 'wrong target accepted')
    call targetir_decode_record(wrong_target, record, 0_int64, decoded, status)
    call assert_status(status, targetir_encoding_invalid_target, 'wrong target decoded')

    riscv_record = riscv_opcode_record_t('addi', 'I', int(z'13', int64), &
        int(z'707F', int64), source)
    target = make_target_ir('riscv64', 64_int32, .true., source)
    call normalize_riscv_i_record(target, riscv_record, normalized, status)
    call assert_status(status, targetir_encoding_ok, 'RISC-V adapter record rejected')
    call targetir_encode_record(target, normalized, [3_int64, 4_int64, 7_int64], word, status)
    call assert_status(status, targetir_encoding_ok, 'RISC-V normalized record rejected')
    call targetir_decode_record(target, normalized, word, decoded, status)
    call assert_status(status, targetir_encoding_ok, 'RISC-V normalized word rejected')
    call assert_values(decoded, [3_int64, 4_int64, 7_int64], 'RISC-V adapter round trip changed')

    aarch_record = aarch64_encoding_record_t()
    aarch_record%name = 'ADD'
    aarch_record%operation_id = 'ADD'
    aarch_record%width = 32_int32
    aarch_record%match = int(z'91000000', int64)
    aarch_record%mask = int(z'FF800000', int64)
    aarch_record%variable_range_count = 1_int32
    aarch_record%variable_ranges(1) = aarch64_variable_range_t(0_int32, 23_int32)
    aarch_record%target = target
    aarch_record%source = source
    call normalize_aarch64_record(target, aarch_record, normalized, status)
    call assert_status(status, targetir_encoding_invalid_target, 'mismatched AArch64 target accepted')
    target = make_target_ir('aarch64', 32_int32, .true., source)
    aarch_record%target = target
    call normalize_aarch64_record(target, aarch_record, normalized, status)
    call assert_status(status, targetir_encoding_ok, 'AArch64 adapter record rejected')
    call targetir_encode_record(target, normalized, [int(z'123456', int64)], word, status)
    call assert_status(status, targetir_encoding_ok, 'AArch64 normalized record rejected')
    call targetir_decode_record(target, normalized, word, decoded, status)
    call assert_status(status, targetir_encoding_ok, 'AArch64 normalized word rejected')
    call assert_values(decoded, [int(z'123456', int64)], 'AArch64 adapter round trip changed')

    write (*, '(a)') 'TargetIR generic codec behavioral checks: ok'

contains

    subroutine assert_status(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_status

    subroutine assert64(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert64

    subroutine assert_values(actual, expected, message)
        integer(int64), allocatable, intent(in) :: actual(:)
        integer(int64), intent(in) :: expected(:)
        character(len=*), intent(in) :: message

        if (.not. allocated(actual)) error stop message
        if (size(actual) /= size(expected)) error stop message
        if (any(actual /= expected)) error stop message
    end subroutine assert_values

    subroutine assert_unallocated(actual, message)
        integer(int64), allocatable, intent(in) :: actual(:)
        character(len=*), intent(in) :: message

        if (allocated(actual)) error stop message
    end subroutine assert_unallocated

end program test_targetir_codec
