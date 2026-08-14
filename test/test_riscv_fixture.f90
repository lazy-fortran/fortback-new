program test_riscv_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    use fortback_riscv_source, only: import_riscv_opcodes, riscv_opcode_record_t, &
        riscv_source_ok
    use fortback_riscv_fixture, only: riscv_add, riscv_addi, riscv_and, &
        riscv_decode_integer, &
        riscv_encode_integer, riscv_instruction_t, riscv_invalid_operand, &
        riscv_invalid_target, riscv_malformed, riscv_ok, riscv_or, riscv_sub, &
        riscv_andi, riscv_ori, riscv_sll, riscv_slli, riscv_srli, riscv_srai, riscv_sra, &
        riscv_slti, riscv_unsupported, &
        riscv_xor
    implicit none

    type(target_ir_t) :: target, bad_target
    type(riscv_instruction_t) :: instruction, decoded
    integer(int64) :: word
    integer(int32) :: count, status
    type(riscv_opcode_record_t) :: records(14)
    ! Fixed witness for the pinned rv_i object; no upstream payload is vendored.
    character(len=*), parameter :: source_text = &
        'add rd rs1 rs2 31..25=0 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'sub rd rs1 rs2 31..25=0x20 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'and rd rs1 rs2 31..25=0 14..12=7 6..2=0x0C 1..0=3' // new_line('a') // &
        'or rd rs1 rs2 31..25=0 14..12=6 6..2=0x0C 1..0=3' // new_line('a') // &
        'xor rd rs1 rs2 31..25=0 14..12=4 6..2=0x0C 1..0=3' // new_line('a') // &
        'sll rd rs1 rs2 31..25=0 14..12=1 6..2=0x0C 1..0=3' // new_line('a') // &
        'sra rd rs1 rs2 31..25=0x20 14..12=5 6..2=0x0C 1..0=3' // new_line('a') // &
        'slli rd rs1 shamt 31..26=0 14..12=1 6..2=0x04 1..0=3' // new_line('a') // &
        'srli rd rs1 shamt 31..26=0 14..12=5 6..2=0x04 1..0=3' // new_line('a') // &
        'srai rd rs1 shamt 31..26=0x10 14..12=5 6..2=0x04 1..0=3' // new_line('a') // &
        'addi rd rs1 imm12 14..12=0 6..2=0x04 1..0=3' // new_line('a') // &
        'ori rd rs1 imm12 14..12=6 6..2=0x04 1..0=3' // new_line('a') // &
        'andi rd rs1 imm12 14..12=7 6..2=0x04 1..0=3' // new_line('a') // &
        'slti rd rs1 imm12 14..12=2 6..2=0x04 1..0=3'

    target = make_target_ir('riscv64', 64_int32, .true., &
        make_source_ref('riscv-opcodes', 'rv_i', &
        '76a20ca8fb1b01c33a31f6ae4104f79914f9d3b90d2ba60f2ca493e9a46928b6', &
        'IMPORTED'))

    call import_riscv_opcodes(source_text, target%source, records, count, status)
    call assert_equal_integer(status, riscv_source_ok, 'source records were rejected')
    call assert_equal_integer(count, 14_int32, 'source record count changed')

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

    instruction = riscv_instruction_t(riscv_or, 10_int32, 10_int32, 11_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'or was rejected')
    call assert_equal_integer64(word, int(z'00B56533', int64), 'or encoding changed')
    call check_decode(target, word, instruction, 'or decode')

    instruction = riscv_instruction_t(riscv_xor, 10_int32, 10_int32, 11_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'xor was rejected')
    call assert_equal_integer64(word, int(z'00B54533', int64), 'xor encoding changed')
    call check_decode(target, word, instruction, 'xor decode')

    instruction = riscv_instruction_t(riscv_sll, 10_int32, 10_int32, 11_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'sll was rejected')
    call assert_equal_integer64(word, int(z'00B51533', int64), 'sll encoding changed')
    call check_decode(target, word, instruction, 'sll decode')

    instruction = riscv_instruction_t(riscv_sra, 10_int32, 10_int32, 11_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'sra was rejected')
    call assert_equal_integer64(word, int(z'40B55533', int64), 'sra encoding changed')
    call check_decode(target, word, instruction, 'sra decode')

    instruction = riscv_instruction_t(riscv_slli, 10_int32, 10_int32, 0_int32, 63_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'slli was rejected')
    call assert_equal_integer64(word, int(z'03F51513', int64), 'slli encoding changed')
    call check_decode(target, word, instruction, 'slli decode')

    instruction = riscv_instruction_t(riscv_srli, 10_int32, 10_int32, 0_int32, 63_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'srli was rejected')
    call assert_equal_integer64(word, int(z'03F55513', int64), 'srli encoding changed')
    call check_decode(target, word, instruction, 'srli decode')

    instruction = riscv_instruction_t(riscv_srai, 10_int32, 10_int32, 0_int32, 63_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'srai was rejected')
    call assert_equal_integer64(word, int(z'43F55513', int64), 'srai encoding changed')
    call check_decode(target, word, instruction, 'srai decode')

    instruction = riscv_instruction_t(riscv_addi, 10_int32, 10_int32, 0_int32, -1_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer64(word, int(z'FFF50513', int64), 'addi encoding changed')
    call check_decode(target, word, instruction, 'addi decode')

    instruction = riscv_instruction_t(riscv_ori, 10_int32, 10_int32, 0_int32, -1_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'ori was rejected')
    call assert_equal_integer64(word, int(z'FFF56513', int64), 'ori encoding changed')
    call check_decode(target, word, instruction, 'ori decode')

    instruction = riscv_instruction_t(riscv_andi, 10_int32, 10_int32, 0_int32, -1_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'andi was rejected')
    call assert_equal_integer64(word, int(z'FFF57513', int64), 'andi encoding changed')
    call check_decode(target, word, instruction, 'andi decode')

    instruction = riscv_instruction_t(riscv_slti, 10_int32, 10_int32, 0_int32, -2048_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'slti lower boundary was rejected')
    call assert_equal_integer64(word, int(z'80052513', int64), 'slti lower encoding changed')
    call check_decode(target, word, instruction, 'slti lower boundary decode')
    instruction%immediate = 2047_int32
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_ok, 'slti upper boundary was rejected')
    call assert_equal_integer64(word, int(z'7FF52513', int64), 'slti upper encoding changed')
    call check_decode(target, word, instruction, 'slti upper boundary decode')

    instruction%rd = 32_int32
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'invalid register accepted')
    instruction = riscv_instruction_t(riscv_slti, 32_int32, 1_int32, 0_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'invalid slti rd accepted')
    instruction = riscv_instruction_t(riscv_slti, 1_int32, 32_int32, 0_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'invalid slti rs1 accepted')
    instruction = riscv_instruction_t(riscv_or, 1_int32, 2_int32, 32_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, &
        'invalid or register accepted')
    instruction = riscv_instruction_t(riscv_xor, 1_int32, 2_int32, 32_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, &
        'invalid xor register accepted')
    instruction = riscv_instruction_t(riscv_sll, 1_int32, 2_int32, 32_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, &
        'invalid sll register accepted')
    instruction = riscv_instruction_t(riscv_sra, 1_int32, 2_int32, 32_int32, 0_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, &
        'invalid sra register accepted')
    instruction = riscv_instruction_t(riscv_addi, 1_int32, 1_int32, 0_int32, 2048_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide immediate accepted')
    instruction = riscv_instruction_t(riscv_slti, 1_int32, 1_int32, 0_int32, -2049_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide negative slti accepted')
    instruction%immediate = 2048_int32
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide positive slti accepted')
    instruction = riscv_instruction_t(riscv_ori, 1_int32, 1_int32, 0_int32, -2049_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide ori immediate accepted')
    instruction = riscv_instruction_t(riscv_andi, 1_int32, 1_int32, 0_int32, 2048_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide andi immediate accepted')
    instruction = riscv_instruction_t(riscv_slli, 1_int32, 1_int32, 0_int32, -1_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'negative slli accepted')
    instruction%immediate = 64_int32
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide slli accepted')
    instruction = riscv_instruction_t(riscv_srli, 1_int32, 1_int32, 0_int32, -1_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'negative srli accepted')
    instruction%immediate = 64_int32
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide srli accepted')
    instruction = riscv_instruction_t(riscv_srai, 1_int32, 1_int32, 0_int32, -1_int32)
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'negative srai accepted')
    instruction%immediate = 64_int32
    call riscv_encode_integer(target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_operand, 'wide srai accepted')

    call riscv_decode_integer(target, int(z'00000000', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_unsupported, 'unsupported instruction accepted')
    call riscv_decode_integer(target, int(z'04051513', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_unsupported, 'invalid slli shift accepted')
    call riscv_decode_integer(target, int(z'04055513', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_unsupported, 'invalid srli shift accepted')
    call riscv_decode_integer(target, int(z'43F51513', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_unsupported, 'unsupported SRAI funct3 accepted')
    call riscv_decode_integer(target, int(z'FFF53513', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_unsupported, 'unsupported SLTIU encoding accepted')
    call riscv_decode_integer(target, int(z'100000000', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_malformed, 'wide word accepted')

    bad_target = target
    bad_target%architecture = 'aarch64'
    instruction = riscv_instruction_t(riscv_sll, 1_int32, 2_int32, 3_int32, 0_int32)
    call riscv_encode_integer(bad_target, instruction, word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted')
    call riscv_decode_integer(bad_target, int(z'003110B3', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target decoded')
    call riscv_encode_integer(bad_target, riscv_instruction_t(riscv_sra, 1_int32, 2_int32, &
        3_int32, 0_int32), word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted SRA')
    call riscv_decode_integer(bad_target, int(z'403150B3', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target decoded SRA')
    call riscv_encode_integer(bad_target, riscv_instruction_t(riscv_andi, 1_int32, 2_int32, &
        0_int32, -1_int32), word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted ANDI')
    call riscv_decode_integer(bad_target, int(z'FFF17113', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target decoded ANDI')
    call riscv_encode_integer(bad_target, riscv_instruction_t(riscv_slli, 1_int32, 2_int32, &
        0_int32, 63_int32), word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted SLLI')
    call riscv_decode_integer(bad_target, int(z'03F11093', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target decoded SLLI')
    call riscv_encode_integer(bad_target, riscv_instruction_t(riscv_srli, 1_int32, 2_int32, &
        0_int32, 63_int32), word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted SRLI')
    call riscv_decode_integer(bad_target, int(z'03F15093', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target decoded SRLI')
    call riscv_encode_integer(bad_target, riscv_instruction_t(riscv_srai, 1_int32, 2_int32, &
        0_int32, 63_int32), word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted SRAI')
    call riscv_decode_integer(bad_target, int(z'43F15093', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target decoded SRAI')
    call riscv_encode_integer(bad_target, riscv_instruction_t(riscv_slti, 1_int32, 2_int32, &
        0_int32, 2047_int32), word, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target accepted SLTI')
    call riscv_decode_integer(bad_target, int(z'7FF12113', int64), decoded, status, records)
    call assert_equal_integer(status, riscv_invalid_target, 'wrong target decoded SLTI')

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
