program test_aarch64_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    use fortback_aarch64_source, only: aarch64_encoding_record_t, import_aarch64_instructions, &
        aarch64_source_ok
    use fortback_aarch64_fixture, only: aarch64_add, aarch64_decode_fixed, aarch64_encode_fixed, &
        aarch64_instruction_t, aarch64_invalid_operand, aarch64_invalid_target, aarch64_malformed, &
        aarch64_ok, aarch64_sub, aarch64_unsupported
    implicit none

    type(target_ir_t) :: target, bad_target
    type(aarch64_instruction_t) :: instruction, decoded
    type(aarch64_encoding_record_t) :: records(2)
    integer(int32) :: count, status
    integer(int64) :: word
    character(len=*), parameter :: add = &
        '{"_type":"Instruction.Instruction","name":"ADD_64_addsub_imm",' // &
        '"operation_id":"ADD_addsub_imm","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits","range":{"_type":"Range",' // &
        '"start":31,"width":1},"value":{"_type":"Values.Value","value":"1"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":30,"width":1},' // &
        '"value":{"_type":"Values.Value","value":"0"}},{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":29,"width":1},"value":{"_type":"Values.Value",' // &
        '"value":"0"}},{"_type":"Instruction.Encodeset.Bits","range":{"_type":"Range",' // &
        '"start":26,"width":3},"value":{"_type":"Values.Value","value":"100"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":23,"width":3},' // &
        '"value":{"_type":"Values.Value","value":"010"}}]}}'
    character(len=*), parameter :: sub = &
        '{"_type":"Instruction.Instruction","name":"SUB_64_addsub_imm",' // &
        '"operation_id":"SUB_addsub_imm","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits","range":{"_type":"Range",' // &
        '"start":31,"width":1},"value":{"_type":"Values.Value","value":"1"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":30,"width":1},' // &
        '"value":{"_type":"Values.Value","value":"1"}},{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":29,"width":1},"value":{"_type":"Values.Value",' // &
        '"value":"0"}},{"_type":"Instruction.Encodeset.Bits","range":{"_type":"Range",' // &
        '"start":26,"width":3},"value":{"_type":"Values.Value","value":"100"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":23,"width":3},' // &
        '"value":{"_type":"Values.Value","value":"010"}}]}}'

    target = make_target_ir('aarch64', 32_int32, .true., make_source_ref( &
        'aarchmrs-instructions', 'Instructions.json', &
        '439a0003e7904a4c93df27efd2702453336e00023d5f4c8ef3f0aa28291a10e3', 'IMPORTED'))
    call import_aarch64_instructions(add // new_line('a') // sub, target%source, records, count, status)
    call assert_int(status, aarch64_source_ok, 'source records were rejected')
    call assert_int(count, 2_int32, 'source record count changed')

    instruction = aarch64_instruction_t(aarch64_add, 10_int32, 11_int32, 16_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'ADD was rejected')
    call assert64(word, int(z'9100416A', int64), 'ADD fixed encoding changed')
    call check_decode(word, instruction, 'ADD decode')

    instruction = aarch64_instruction_t(aarch64_sub, 2_int32, 3_int32, int(z'123', int32))
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'SUB was rejected')
    call assert64(word, int(z'D1048C62', int64), 'SUB fixed encoding changed')
    call check_decode(word, instruction, 'SUB decode')

    instruction%rd = 32_int32
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'invalid register accepted')
    instruction = aarch64_instruction_t(aarch64_add, 1_int32, 2_int32, 4096_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'wide immediate accepted')
    call aarch64_decode_fixed(target, int(z'100000000', int64), decoded, status, records)
    call assert_int(status, aarch64_malformed, 'wide word accepted')
    call aarch64_decode_fixed(target, int(z'14000000', int64), decoded, status, records)
    call assert_int(status, aarch64_unsupported, 'unsupported word accepted')
    instruction = aarch64_instruction_t(aarch64_add, 1_int32, 2_int32, 0_int32)
    call aarch64_encode_fixed(target, instruction, word, status)
    call assert_int(status, aarch64_unsupported, 'missing records accepted')
    bad_target = target
    bad_target%architecture = 'riscv64'
    instruction = aarch64_instruction_t(aarch64_add, 1_int32, 2_int32, 0_int32)
    call aarch64_encode_fixed(bad_target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_target, 'wrong target accepted')

    write (*, '(a)') 'AArch64 fixed codec behavioral checks: ok'

contains

    subroutine check_decode(expected_word, expected, message)
        integer(int64), intent(in) :: expected_word
        type(aarch64_instruction_t), intent(in) :: expected
        character(len=*), intent(in) :: message

        call aarch64_decode_fixed(target, expected_word, decoded, status, records)
        call assert_int(status, aarch64_ok, message)
        call assert_int(decoded%kind, expected%kind, message)
        call assert_int(decoded%rd, expected%rd, message)
        call assert_int(decoded%rn, expected%rn, message)
        call assert_int(decoded%immediate, expected%immediate, message)
    end subroutine check_decode

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

end program test_aarch64_fixture
