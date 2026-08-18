program test_riscv_codec
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_codec, only: riscv_decode_record, riscv_encode_record
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_invalid_target, &
        riscv_malformed, riscv_ok, riscv_unsupported
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    implicit none

    type(target_ir_t) :: target, bad_target
    type(riscv_opcode_record_t) :: record, bad_record
    integer(int64), allocatable :: values(:), decoded(:)
    integer(int64) :: word
    integer(int32) :: status

    target = make_target_ir('riscv64', 64_int32, .true., make_source_ref( &
        'codec-test', 'rv_i', &
        '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef', &
        'IMPORTED'))
    record = riscv_opcode_record_t('xori', 'I', int(z'00004013', int64), &
        int(z'0000707F', int64), target%source)

    values = [10_int64, 10_int64, -1_int64]
    call riscv_encode_record(target, record, values, word, status)
    call assert_int(status, riscv_ok, 'generic RISC-V encode failed')
    call assert64(word, int(z'FFF54513', int64), 'generic RISC-V word changed')
    call riscv_decode_record(target, record, word, decoded, status)
    call assert_int(status, riscv_ok, 'generic RISC-V decode failed')
    call assert_vector(decoded, values, 'generic RISC-V round trip changed')

    values = [10_int64, 10_int64]
    call riscv_encode_record(target, record, values, word, status)
    call assert_int(status, riscv_invalid_operand, 'invalid operand count accepted')
    call assert64(word, 0_int64, 'invalid operand count left stale word')

    values = [32_int64, 10_int64, 0_int64]
    call riscv_encode_record(target, record, values, word, status)
    call assert_int(status, riscv_invalid_operand, 'invalid register accepted')
    call assert64(word, 0_int64, 'invalid register left stale word')

    bad_record = record
    bad_record%mask = ior(bad_record%mask, int(z'00000080', int64))
    values = [10_int64, 10_int64, 0_int64]
    call riscv_encode_record(target, bad_record, values, word, status)
    call assert_int(status, riscv_malformed, 'malformed metadata accepted')
    call assert64(word, 0_int64, 'malformed metadata left stale word')

    call riscv_decode_record(target, record, int(z'00000000', int64), decoded, status)
    call assert_int(status, riscv_unsupported, 'unsupported word accepted')
    call assert_unallocated(decoded, 'unsupported word left stale values')

    bad_target = target
    bad_target%architecture = 'aarch64'
    call riscv_encode_record(bad_target, record, values, word, status)
    call assert_int(status, riscv_invalid_target, 'wrong target accepted')
    call assert64(word, 0_int64, 'wrong target left stale word')
    call riscv_decode_record(bad_target, record, int(z'FFF54513', int64), decoded, status)
    call assert_int(status, riscv_invalid_target, 'wrong decode target accepted')
    call assert_unallocated(decoded, 'wrong target left stale values')

    record = riscv_opcode_record_t('add', 'R', int(z'00000033', int64), &
        int(z'FE00707F', int64), target%source)
    values = [31_int64, 30_int64, 29_int64]
    call riscv_encode_record(target, record, values, word, status)
    call assert_int(status, riscv_ok, 'generic R-format encode failed')
    call assert64(word, int(z'01DF0FB3', int64), 'generic R-format word changed')
    call riscv_decode_record(target, record, word, decoded, status)
    call assert_int(status, riscv_ok, 'generic R-format decode failed')
    call assert_vector(decoded, values, 'generic R-format round trip changed')

    write (*, '(a)') 'RISC-V generic whole-record codec checks: ok'

contains

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

    subroutine assert_vector(actual, expected, message)
        integer(int64), intent(in) :: actual(:), expected(:)
        character(len=*), intent(in) :: message

        if (size(actual) /= size(expected)) error stop message
        if (any(actual /= expected)) error stop message
    end subroutine assert_vector

    subroutine assert_unallocated(actual, message)
        integer(int64), allocatable, intent(in) :: actual(:)
        character(len=*), intent(in) :: message

        if (allocated(actual)) error stop message
    end subroutine assert_unallocated

end program test_riscv_codec
