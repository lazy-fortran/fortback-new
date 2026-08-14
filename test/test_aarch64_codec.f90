program test_aarch64_codec
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_codec, only: aarch64_decode_record, aarch64_encode_record
    use fortback_aarch64_record, only: aarch64_record_invalid_target, &
        aarch64_record_malformed, aarch64_record_ok, aarch64_record_unsupported
    use fortback_aarch64_source, only: aarch64_encoding_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    implicit none

    type(target_ir_t) :: target, bad_target
    type(aarch64_encoding_record_t) :: add, sub, nop, bad_record
    integer(int64), allocatable :: values(:), decoded(:)
    integer(int64) :: word
    integer(int32) :: status

    target = make_target_ir('aarch64', 32_int32, .true., make_source_ref( &
        'codec-test', 'records', &
        '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef', &
        'IMPORTED'))
    add = make_record(target, int(z'91000000', int64), int(z'FF800000', int64), 1_int32)
    sub = make_record(target, int(z'D1000000', int64), int(z'FF800000', int64), 1_int32)
    nop = make_record(target, int(z'D503201F', int64), int(z'FFFFFFFF', int64), 0_int32)

    values = [12345_int64]
    call aarch64_encode_record(target, add, values, word, status)
    call assert_int(status, aarch64_record_ok, 'ADD encode failed')
    call assert64(word, int(z'91003039', int64), 'ADD encoded word changed')
    call aarch64_decode_record(target, add, word, decoded, status)
    call assert_int(status, aarch64_record_ok, 'ADD decode failed')
    call assert_vector(decoded, [12345_int64], 'ADD round trip changed')

    values = [7654321_int64]
    call aarch64_encode_record(target, sub, values, word, status)
    call assert_int(status, aarch64_record_ok, 'SUB encode failed')
    call assert64(word, ior(int(z'D1000000', int64), 7654321_int64), &
        'SUB encoded word changed')
    call aarch64_decode_record(target, sub, word, decoded, status)
    call assert_int(status, aarch64_record_ok, 'SUB decode failed')
    call assert_vector(decoded, [7654321_int64], 'SUB round trip changed')

    values = [0_int64]
    call aarch64_encode_record(target, nop, values(1:0), word, status)
    call assert_int(status, aarch64_record_ok, 'zero-range NOP encode failed')
    call assert64(word, int(z'D503201F', int64), 'zero-range NOP word changed')
    call aarch64_decode_record(target, nop, word, decoded, status)
    call assert_int(status, aarch64_record_ok, 'zero-range NOP decode failed')
    call assert_int(size(decoded), 0_int32, 'zero-range NOP returned fields')

    bad_record = nop
    bad_record%width = 16_int32
    word = -1_int64
    call aarch64_encode_record(target, bad_record, values(1:0), word, status)
    call assert_int(status, aarch64_record_malformed, 'malformed zero-range record accepted')
    call assert64(word, 0_int64, 'malformed zero-range record did not clear word')

    values = [0_int64]
    word = -1_int64
    call aarch64_encode_record(target, add, values(1:0), word, status)
    call assert_int(status, aarch64_record_malformed, 'invalid value count accepted')
    call assert64(word, 0_int64, 'encode error did not clear word')
    call aarch64_decode_record(target, add, int(z'14000000', int64), decoded, status)
    call assert_int(status, aarch64_record_unsupported, 'unsupported word accepted')
    call assert_unallocated(decoded, 'decode error did not clear fields')

    bad_record = add
    bad_record%variable_range_count = 2_int32
    bad_record%variable_ranges(2)%start = 1_int32
    bad_record%variable_ranges(2)%width = 2_int32
    values = [1_int64, 2_int64]
    call aarch64_encode_record(target, bad_record, values, word, status)
    call assert_int(status, aarch64_record_malformed, 'overlap metadata accepted')
    call assert64(word, 0_int64, 'malformed metadata did not clear word')

    bad_record = add
    bad_record%variable_ranges(1)%start = 31_int32
    bad_record%variable_ranges(1)%width = 2_int32
    values = [1_int64]
    call aarch64_encode_record(target, bad_record, values, word, status)
    call assert_int(status, aarch64_record_malformed, 'out-of-range metadata accepted')

    bad_target = target
    bad_target%architecture = 'riscv64'
    call aarch64_encode_record(bad_target, add, values, word, status)
    call assert_int(status, aarch64_record_invalid_target, 'wrong target accepted')
    call assert64(word, 0_int64, 'wrong target did not clear word')
    call aarch64_decode_record(bad_target, add, int(z'91000001', int64), decoded, status)
    call assert_int(status, aarch64_record_invalid_target, 'wrong decode target accepted')
    call assert_unallocated(decoded, 'wrong decode target did not clear fields')

    write (*, '(a)') 'AArch64 whole-record codec checks: ok'

contains

    function make_record(target, match, mask, count) result(record)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: match, mask
        integer(int32), intent(in) :: count
        type(aarch64_encoding_record_t) :: record

        record = aarch64_encoding_record_t()
        record%target = target
        record%source = target%source
        record%match = match
        record%mask = mask
        record%width = 32_int32
        record%variable_range_count = count
        if (count > 0_int32) then
            record%variable_ranges(1)%start = 0_int32
            record%variable_ranges(1)%width = 23_int32
        end if
    end function make_record

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

end program test_aarch64_codec
