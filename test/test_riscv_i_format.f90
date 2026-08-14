program test_riscv_i_format
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_invalid_target, &
        riscv_malformed, riscv_ok, riscv_unsupported
    use fortback_riscv_i_format, only: riscv_decode_i_format, riscv_encode_i_format
    use fortback_riscv_source, only: import_riscv_opcodes, riscv_opcode_record_t, &
        riscv_source_ok
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    implicit none

    character(len=*), parameter :: source_text = &
        'xori rd rs1 imm12 14..12=4 6..2=0x04 1..0=3' // new_line('a') // &
        'slli rd rs1 shamt 31..26=0 14..12=1 6..2=0x04 1..0=3'
    type(target_ir_t) :: target, bad_target
    type(riscv_opcode_record_t) :: records(2), jalr
    integer(int32) :: count, status, record_index, rd, rs1, immediate
    integer(int64) :: word

    target = make_target_ir('riscv64', 64_int32, .true., &
        make_source_ref('riscv-opcodes', 'rv_i', 'fixture-sha256', 'IMPORTED'))
    call import_riscv_opcodes(source_text, target%source, records, count, status)
    call assert_equal_int(status, riscv_source_ok, 'imported I-format record rejected')
    call assert_equal_int(count, 2_int32, 'imported I-format records missing')

    call riscv_encode_i_format(target, records(1), 10_int32, 10_int32, -1_int32, word, status)
    call assert_equal_int(status, riscv_ok, 'XORI encoding rejected')
    call assert_equal64(word, int(z'FFF54513', int64), 'XORI canonical encoding changed')
    call riscv_decode_i_format(target, word, records, record_index, rd, rs1, immediate, status)
    call assert_equal_int(status, riscv_ok, 'XORI decoding rejected')
    call assert_equal_int(record_index, 1_int32, 'XORI source record was not selected')
    call assert_equal_int(rd, 10_int32, 'XORI rd decoding changed')
    call assert_equal_int(rs1, 10_int32, 'XORI rs1 decoding changed')
    call assert_equal_int(immediate, -1_int32, 'XORI immediate decoding changed')
    call assert_equal(trim(records(1)%source%artifact), 'riscv-opcodes', &
        'XORI source artifact provenance changed')
    call assert_equal(trim(records(1)%source%object), 'rv_i', &
        'XORI source object provenance changed')
    call assert_equal(trim(records(1)%source%source_hash), 'fixture-sha256', &
        'XORI source hash provenance changed')
    call assert_equal(trim(records(1)%source%origin), 'IMPORTED', &
        'XORI source origin provenance changed')

    call riscv_encode_i_format(target, records(2), 10_int32, 10_int32, 63_int32, word, status)
    call assert_equal_int(status, riscv_ok, 'SLLI-shaped record rejected')
    call assert_equal64(word, int(z'03F51513', int64), 'SLLI canonical encoding changed')
    call riscv_decode_i_format(target, word, records, record_index, rd, rs1, immediate, status)
    call assert_equal_int(status, riscv_ok, 'SLLI-shaped record decoding rejected')
    call assert_equal_int(record_index, 2_int32, 'SLLI source record was not selected')
    call assert_equal_int(immediate, 63_int32, 'SLLI immediate decoding changed')

    jalr = riscv_opcode_record_t('jalr', 'I', int(z'00000067', int64), &
        int(z'0000707F', int64), target%source)
    call riscv_encode_i_format(target, jalr, 1_int32, 2_int32, 0_int32, word, status)
    call assert_equal_int(status, riscv_ok, 'JALR-shaped record rejected')
    call assert_equal64(word, int(z'000100E7', int64), 'JALR canonical encoding changed')
    records(1) = jalr
    call riscv_decode_i_format(target, word, records, record_index, rd, rs1, immediate, status)
    call assert_equal_int(status, riscv_ok, 'JALR-shaped record decoding rejected')
    call assert_equal_int(rd, 1_int32, 'JALR rd decoding changed')
    call assert_equal_int(rs1, 2_int32, 'JALR rs1 decoding changed')
    call assert_equal_int(immediate, 0_int32, 'JALR immediate decoding changed')

    jalr%mask = ior(jalr%mask, int(z'00000080', int64))
    call riscv_encode_i_format(target, jalr, 1_int32, 2_int32, 0_int32, word, status)
    call assert_equal_int(status, riscv_malformed, 'overlapping metadata accepted')
    jalr = riscv_opcode_record_t('jalr', 'R', int(z'00000067', int64), &
        int(z'0000707F', int64), target%source)
    call riscv_encode_i_format(target, jalr, 1_int32, 2_int32, 0_int32, word, status)
    call assert_equal_int(status, riscv_unsupported, 'unsupported format accepted')
    jalr%format = 'I'
    jalr%source%source_hash = ''
    call riscv_encode_i_format(target, jalr, 1_int32, 2_int32, 0_int32, word, status)
    call assert_equal_int(status, riscv_malformed, 'missing source provenance accepted')
    jalr%source = target%source
    call riscv_encode_i_format(target, jalr, 32_int32, 2_int32, 0_int32, word, status)
    call assert_equal_int(status, riscv_invalid_operand, 'invalid rd accepted')
    call riscv_encode_i_format(target, jalr, 1_int32, 2_int32, 2048_int32, word, status)
    call assert_equal_int(status, riscv_invalid_operand, 'out-of-range immediate accepted')

    records(1) = jalr
    records(1)%format = 'R'
    call riscv_decode_i_format(target, int(z'000100E7', int64), records, record_index, rd, &
        rs1, immediate, status)
    call assert_equal_int(status, riscv_unsupported, 'unsupported decode format accepted')
    records(1)%format = 'I'
    bad_target = target
    bad_target%architecture = 'aarch64'
    call riscv_encode_i_format(bad_target, records(1), 1_int32, 2_int32, 0_int32, word, status)
    call assert_equal_int(status, riscv_invalid_target, 'wrong target accepted')
    call riscv_decode_i_format(bad_target, int(z'000100E7', int64), records, record_index, &
        rd, rs1, immediate, status)
    call assert_equal_int(status, riscv_invalid_target, 'wrong target decode accepted')

    write (*, '(a)') 'RISC-V generic I-format checks: ok'

contains

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_equal_int(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal_int

    subroutine assert_equal64(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal64

end program test_riscv_i_format
