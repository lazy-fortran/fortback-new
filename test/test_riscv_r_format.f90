program test_riscv_r_format
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_fixture, only: riscv_invalid_operand, riscv_invalid_target, riscv_malformed, &
        riscv_ok, riscv_unsupported
    use fortback_riscv_r_format, only: riscv_decode_r_format, riscv_encode_r_format
    use fortback_riscv_source, only: import_riscv_opcodes, riscv_opcode_record_t, riscv_source_ok
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    implicit none

    character(len=*), parameter :: source_text = &
        'add rd rs1 rs2 31..25=0 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'sub rd rs1 rs2 31..25=0x20 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'and rd rs1 rs2 31..25=0 14..12=7 6..2=0x0C 1..0=3' // new_line('a') // &
        'or rd rs1 rs2 31..25=0 14..12=6 6..2=0x0C 1..0=3' // new_line('a') // &
        'xor rd rs1 rs2 31..25=0 14..12=4 6..2=0x0C 1..0=3' // new_line('a') // &
        'sll rd rs1 rs2 31..25=0 14..12=1 6..2=0x0C 1..0=3' // new_line('a') // &
        'sra rd rs1 rs2 31..25=0x20 14..12=5 6..2=0x0C 1..0=3'
    type(target_ir_t) :: target, bad_target
    type(riscv_opcode_record_t) :: records(7), bad_record
    integer(int32) :: count, status, record_index, rd, rs1, rs2
    integer(int64) :: word
    integer(int64), parameter :: expected_words(7) = [int(z'01DF0FB3', int64), &
        int(z'41DF0FB3', int64), int(z'01DF7FB3', int64), int(z'01DF6FB3', int64), &
        int(z'01DF4FB3', int64), int(z'01DF1FB3', int64), int(z'41DF5FB3', int64)]
    integer :: i

    target = make_target_ir('riscv64', 64_int32, .true., make_source_ref( &
        'riscv-opcodes', 'rv_i', 'fixture-sha256', 'IMPORTED'))
    call import_riscv_opcodes(source_text, target%source, records, count, status)
    call assert_int(status, riscv_source_ok, 'R-format source rejected')
    call assert_int(count, 7_int32, 'R-format source records missing')

    call riscv_encode_r_format(target, records(1), 0_int32, 0_int32, 0_int32, word, status)
    call assert_int(status, riscv_ok, 'R-format lower boundary rejected')
    call assert64(word, int(z'00000033', int64), 'R-format lower boundary changed')
    do i = 1, size(records)
        call riscv_encode_r_format(target, records(i), 31_int32, 30_int32, 29_int32, word, status)
        call assert_int(status, riscv_ok, 'R-format upper boundary rejected')
        call assert64(word, expected_words(i), 'R-format operation encoding changed')
        call riscv_decode_r_format(target, word, records, record_index, rd, rs1, rs2, status)
        call assert_int(status, riscv_ok, 'R-format boundary decode rejected')
        call assert_int(record_index, int(i, int32), 'R-format record lookup changed')
        call assert_int(rd, 31_int32, 'R-format rd boundary decode changed')
        call assert_int(rs1, 30_int32, 'R-format rs1 boundary decode changed')
        call assert_int(rs2, 29_int32, 'R-format rs2 boundary decode changed')
    end do

    call riscv_encode_r_format(target, records(1), 32_int32, 0_int32, 0_int32, word, status)
    call assert_int(status, riscv_invalid_operand, 'invalid R-format rd accepted')
    call assert64(word, 0_int64, 'invalid R-format rd left stale word')
    call riscv_encode_r_format(target, records(1), 0_int32, 32_int32, 0_int32, word, status)
    call assert_int(status, riscv_invalid_operand, 'invalid R-format rs1 accepted')
    call riscv_encode_r_format(target, records(1), 0_int32, 0_int32, 32_int32, word, status)
    call assert_int(status, riscv_invalid_operand, 'invalid R-format rs2 accepted')

    bad_record = records(1)
    bad_record%mask = ior(bad_record%mask, int(z'00000080', int64))
    call riscv_encode_r_format(target, bad_record, 0_int32, 0_int32, 0_int32, word, status)
    call assert_int(status, riscv_malformed, 'overlapping R-format metadata accepted')
    call riscv_decode_r_format(target, int(z'00000000', int64), records, record_index, rd, rs1, &
        rs2, status)
    call assert_int(status, riscv_unsupported, 'unsupported R-format word accepted')

    bad_target = target
    bad_target%architecture = 'aarch64'
    call riscv_encode_r_format(bad_target, records(1), 0_int32, 0_int32, 0_int32, word, status)
    call assert_int(status, riscv_invalid_target, 'wrong target accepted for R-format')

    write (*, '(a)') 'RISC-V generated R-format checks: ok'

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

end program test_riscv_r_format
