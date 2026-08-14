program test_aarch64_fixture
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    use fortback_aarch64_source, only: aarch64_encoding_record_t, import_aarch64_instructions, &
        aarch64_source_ok
    use fortback_aarch64_fixture, only: aarch64_add, aarch64_decode_fixed, aarch64_encode_fixed, &
        aarch64_instruction_t, aarch64_invalid_operand, aarch64_invalid_target, aarch64_malformed, &
        aarch64_adr, aarch64_adrp, aarch64_ldr_literal, aarch64_nop, aarch64_ok, aarch64_sub, &
        aarch64_unsupported
    implicit none

    type(target_ir_t) :: target, bad_target
    type(aarch64_instruction_t) :: instruction, decoded
    type(aarch64_encoding_record_t) :: records(6)
    integer(int32) :: count, status
    integer(int64) :: word
    integer(int64), parameter :: add_word = int(z'9100416A', int64)
    integer(int64), parameter :: sub_word = int(z'D1048C62', int64)
    integer(int64), parameter :: nop_word = int(z'D503201F', int64)
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
    character(len=*), parameter :: nop = &
        '{"_type":"Instruction.Instruction","name":"NOP_HI_hints",' // &
        '"operation_id":"NOP_hints","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":0,"width":32},' // &
        '"value":{"_type":"Values.Value",' // &
        '"value":"11010101000000110010000000011111"}}]}}'

    character(len=*), parameter :: adr = &
        '{"_type":"Instruction.Instruction","name":"ADR_only_pcreladdr",' // &
        '"operation_id":"ADR_pcreladdr","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":31,"width":1},' // &
        '"value":{"_type":"Values.Value","value":"0"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":24,"width":5},' // &
        '"value":{"_type":"Values.Value","value":"10000"}}]}}'
    character(len=*), parameter :: adrp = &
        '{"_type":"Instruction.Instruction","name":"ADRP_only_pcreladdr",' // &
        '"operation_id":"ADRP_pcreladdr","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":31,"width":1},' // &
        '"value":{"_type":"Values.Value","value":"1"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":24,"width":5},' // &
        '"value":{"_type":"Values.Value","value":"10000"}}]}}'
    character(len=*), parameter :: ldr_literal = &
        '{"_type":"Instruction.Instruction","name":"LDR_32_ldst_immliteral",' // &
        '"operation_id":"LDR_32_ldst_immliteral","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":24,"width":8},' // &
        '"value":{"_type":"Values.Value","value":"00011000"}}]}}'

    target = make_target_ir('aarch64', 32_int32, .true., make_source_ref( &
        'aarchmrs-instructions', 'Instructions.json', &
        '439a0003e7904a4c93df27efd2702453336e00023d5f4c8ef3f0aa28291a10e3', 'IMPORTED'))
    call import_aarch64_instructions(add // new_line('a') // sub // new_line('a') // nop // &
        new_line('a') // adr // new_line('a') // adrp // new_line('a') // ldr_literal, &
        target%source, records, count, status)
    call assert_int(status, aarch64_source_ok, 'source records were rejected')
    call assert_int(count, 6_int32, 'source record count changed')

    instruction = aarch64_instruction_t(aarch64_add, 10_int32, 11_int32, 16_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'ADD was rejected')
    call assert64(word, add_word, 'ADD fixed encoding changed')
    call check_decode(add_word, instruction, 'ADD fixed word decode')
    call check_decode(word, instruction, 'ADD encode/decode round trip')

    instruction = aarch64_instruction_t(aarch64_sub, 2_int32, 3_int32, int(z'123', int32))
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'SUB was rejected')
    call assert64(word, sub_word, 'SUB fixed encoding changed')
    call check_decode(sub_word, instruction, 'SUB fixed word decode')
    call check_decode(word, instruction, 'SUB encode/decode round trip')

    instruction = aarch64_instruction_t(aarch64_nop, 31_int32, 31_int32, 4095_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'NOP was rejected')
    call assert64(word, nop_word, 'NOP fixed encoding changed')
    call check_decode(nop_word, aarch64_instruction_t(aarch64_nop, 0_int32, 0_int32, 0_int32), &
        'NOP fixed word decode')
    call check_decode(word, aarch64_instruction_t(aarch64_nop, 0_int32, 0_int32, 0_int32), &
        'NOP encode/decode round trip')

    instruction = aarch64_instruction_t(aarch64_adr, 5_int32, 0_int32, -4_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'ADR was rejected')
    call assert64(word, int(z'10FFFFE5', int64), 'ADR fixed encoding changed')
    call check_decode(word, instruction, 'ADR encode/decode round trip')

    instruction = aarch64_instruction_t(aarch64_adrp, 5_int32, 0_int32, -4_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'ADRP was rejected')
    call assert64(word, int(z'90FFFFE5', int64), 'ADRP fixed encoding changed')
    call check_decode(word, instruction, 'ADRP encode/decode round trip')
    instruction = aarch64_instruction_t(aarch64_adrp, 7_int32, 0_int32, 1048575_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'maximum ADRP page offset was rejected')
    call assert64(word, int(z'F07FFFE7', int64), 'maximum ADRP encoding changed')
    call check_decode(word, instruction, 'maximum ADRP page offset decode')

    instruction = aarch64_instruction_t(aarch64_ldr_literal, 5_int32, 0_int32, -4_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'LDR literal was rejected')
    call assert64(word, int(z'18FFFFE5', int64), 'LDR literal negative encoding changed')
    call check_decode(word, instruction, 'LDR literal negative offset decode')
    instruction = aarch64_instruction_t(aarch64_ldr_literal, 7_int32, 0_int32, 1048572_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_ok, 'maximum LDR literal offset was rejected')
    call assert64(word, int(z'187FFFE7', int64), 'maximum LDR literal encoding changed')
    call check_decode(word, instruction, 'maximum LDR literal offset decode')

    instruction = aarch64_instruction_t(aarch64_add, 1_int32, 2_int32, 0_int32)
    instruction%rd = 32_int32
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'invalid destination register accepted')
    instruction%rd = 2_int32
    instruction%rn = 32_int32
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'invalid source register accepted')
    instruction%rn = 3_int32
    instruction = aarch64_instruction_t(aarch64_add, 1_int32, 2_int32, 4096_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'wide immediate accepted')
    instruction = aarch64_instruction_t(aarch64_adr, 1_int32, 0_int32, 1048576_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'wide ADR immediate accepted')
    instruction = aarch64_instruction_t(aarch64_adrp, 1_int32, 0_int32, 1048576_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'wide ADRP page offset accepted')
    instruction = aarch64_instruction_t(aarch64_adrp, 1_int32, 0_int32, -1048577_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'negative wide ADRP page offset accepted')
    instruction = aarch64_instruction_t(aarch64_ldr_literal, 1_int32, 0_int32, 2_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'unaligned LDR literal offset accepted')
    instruction = aarch64_instruction_t(aarch64_ldr_literal, 1_int32, 0_int32, 1048576_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'wide LDR literal offset accepted')
    instruction = aarch64_instruction_t(aarch64_ldr_literal, 32_int32, 0_int32, -4_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_operand, 'invalid LDR literal destination accepted')
    instruction = aarch64_instruction_t(99_int32, 1_int32, 2_int32, 0_int32)
    call aarch64_encode_fixed(target, instruction, word, status, records)
    call assert_int(status, aarch64_unsupported, 'wrong opcode accepted')
    call aarch64_decode_fixed(target, int(z'100000000', int64), decoded, status, records)
    call assert_int(status, aarch64_malformed, 'wide word accepted')
    call aarch64_decode_fixed(target, int(z'14000000', int64), decoded, status, records)
    call assert_int(status, aarch64_unsupported, 'unsupported word accepted')
    call aarch64_decode_fixed(target, int(z'9140416A', int64), decoded, status, records)
    call assert_int(status, aarch64_unsupported, 'shifted word accepted')
    call aarch64_decode_fixed(target, int(z'D503201E', int64), decoded, status, records)
    call assert_int(status, aarch64_unsupported, 'nearby NOP word accepted')
    instruction = aarch64_instruction_t(aarch64_add, 1_int32, 2_int32, 0_int32)
    call aarch64_encode_fixed(target, instruction, word, status)
    call assert_int(status, aarch64_unsupported, 'missing records accepted')
    bad_target = target
    bad_target%architecture = 'riscv64'
    instruction = aarch64_instruction_t(aarch64_add, 1_int32, 2_int32, 0_int32)
    call aarch64_encode_fixed(bad_target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_target, 'wrong target accepted')
    call aarch64_decode_fixed(bad_target, int(z'10FFFFE5', int64), decoded, status, records)
    call assert_int(status, aarch64_invalid_target, 'wrong target decoded')
    instruction = aarch64_instruction_t(aarch64_adrp, 5_int32, 0_int32, -4_int32)
    call aarch64_encode_fixed(bad_target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_target, 'wrong target accepted ADRP')
    call aarch64_decode_fixed(bad_target, int(z'90FFFFE5', int64), decoded, status, records)
    call assert_int(status, aarch64_invalid_target, 'wrong target decoded ADRP')
    instruction = aarch64_instruction_t(aarch64_ldr_literal, 5_int32, 0_int32, -4_int32)
    call aarch64_encode_fixed(bad_target, instruction, word, status, records)
    call assert_int(status, aarch64_invalid_target, 'wrong target accepted LDR literal')
    call aarch64_decode_fixed(bad_target, int(z'18FFFFE5', int64), decoded, status, records)
    call assert_int(status, aarch64_invalid_target, 'wrong target decoded LDR literal')

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
