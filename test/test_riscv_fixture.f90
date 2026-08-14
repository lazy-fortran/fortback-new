program test_riscv_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    use fortback_riscv_source, only: import_riscv_opcodes, riscv_opcode_record_t, &
        riscv_source_ok
    use fortback_riscv_fixture, only: riscv_add, riscv_addi, riscv_and, riscv_decode_integer, &
        riscv_encode_integer, riscv_instruction_t, riscv_invalid_operand, &
        riscv_invalid_target, riscv_malformed, riscv_ok, riscv_sub, riscv_unsupported
    implicit none

    type(target_ir_t) :: target, bad_target
    type(riscv_instruction_t) :: instruction, decoded
    integer(int64) :: word
    integer(int32) :: count, status
    type(riscv_opcode_record_t) :: records(4)
    ! Fixed witness for the pinned rv_i object; no upstream payload is vendored.
    character(len=*), parameter :: source_text = &
        'add rd rs1 rs2 31..25=0 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'sub rd rs1 rs2 31..25=0x20 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'and rd rs1 rs2 31..25=0 14..12=7 6..2=0x0C 1..0=3' // new_line('a') // &
        'addi rd rs1 imm12 14..12=0 6..2=0x04 1..0=3' // new_line('a')

    target = make_target_ir('riscv64', 64_int32, .true., &
        make_source_ref('riscv-opcodes', 'rv_i', &
        '76a20ca8fb1b01c33a31f6ae4104f79914f9d3b90d2ba60f2ca493e9a46928b6', &
        'IMPORTED'))

    call import_riscv_opcodes(source_text, target%source, records, count, status)
    call assert_equal_integer(status, riscv_source_ok, 'source records were rejected')
    call assert_equal_integer(count, 4_int32, 'source record count changed')

    instruction = riscv_instruction_t(riscv_add, 10_int32, 10_int32, 11_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'add was rejected')
    call assert_equal_integer64(word, int(z'00B50533', int64), 'add encoding changed')
    call check_decode(target, word, instruction, 'add decode')

    instruction = riscv_instruction_t(riscv_sub, 10_int32, 10_int32, 11_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer64(word, int(z'40B50533', int64), 'sub encoding changed')
    call check_decode(target, word, instruction, 'sub decode')

    instruction = riscv_instruction_t(riscv_and, 10_int32, 10_int32, 11_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'and was rejected')
    call assert_equal_integer64(word, int(z'00B57533', int64), 'and encoding changed')
    call check_decode(target, word, instruction, 'and decode')

    instruction = riscv_instruction_t(riscv_addi, 10_int32, 10_int32, 0_int32, -1_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer64(word, int(z'FFF50513', int64), 'addi encoding changed')
    call check_decode(target, word, instruction, 'addi decode')

    instruction%rd = 32_int32
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'invalid register accepted')
    instruction = riscv_instruction_t(riscv_addi, 1_int32, 1_int32, 0_int32, 2048_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide immediate accepted')

    call riscv_decode_integer(target, int(z'00001013', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_unsupported, 'unsupported instruction accepted')
    call riscv_decode_integer(target, int(z'100000000', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_malformed, 'wide word accepted')

    bad_target = target
    bad_target%architecture = 'aarch64'
    instruction = riscv_instruction_t(riscv_add, 1_int32, 2_int32, 3_int32, 0_int32)
    call riscv_encode_integer(bad_target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted')

    write (*, '(a)') 'RISC-V fixture behavioral checks: ok'

contains

    subroutine check_decode(target, word, expected, message)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        type(riscv_instruction_t), intent(in) :: expected
        character(len=*), intent(in) :: message

        call riscv_decode_integer(target, word, decoded, status, records)
        call assert_equal_integer(status, riscv_ok, message)
        call assert_equal_integer(decoded%kind, expected%kind, message)
        call assert_equal_integer(decoded%rd, expected%rd, message)
        call assert_equal_integer(decoded%rs1, expected%rs1, message)
        call assert_equal_integer(decoded%rs2, expected%rs2, message)
        call assert_equal_integer(decoded%immediate, expected%immediate, message)
    end subroutine check_decode

    subroutine assert_equal_integer(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal_integer

    subroutine assert_equal_integer64(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal_integer64

end program test_riscv_fixture
