program test_aarch64_field
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_field, only: aarch64_extract_variable_range
    use fortback_aarch64_record, only: aarch64_record_invalid_target, &
        aarch64_record_malformed, aarch64_record_ok, aarch64_record_unsupported, &
        aarch64_validate_record
    use fortback_aarch64_source, only: aarch64_encoding_record_t, aarch64_source_ok, &
        import_aarch64_instructions
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t
    implicit none

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
        '"value":{"_type":"Values.Value","value":"010"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":0,"width":23},' // &
        '"value":{"_type":"Values.Value","value":"xxxxxxxxxxxxxxxxxxxxxxx"}}]}}'
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
        '"value":{"_type":"Values.Value","value":"010"}},{"_type":' // &
        '"Instruction.Encodeset.Bits","range":{"_type":"Range","start":0,"width":23},' // &
        '"value":{"_type":"Values.Value","value":"xxxxxxxxxxxxxxxxxxxxxxx"}}]}}'
    character(len=*), parameter :: nop = &
        '{"_type":"Instruction.Instruction","name":"NOP_HI_hints",' // &
        '"operation_id":"NOP_hints","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":0,"width":32},' // &
        '"value":{"_type":"Values.Value","value":"11010101000000110010000000011111"}]}}'
    type(target_ir_t) :: target, bad_target
    type(aarch64_encoding_record_t) :: records(3), bad_record
    integer(int32) :: count, status
    integer(int64) :: value
    integer(int64), parameter :: add_word = int(z'9100416A', int64)
    integer(int64), parameter :: sub_word = int(z'D1048C62', int64)
    integer(int64), parameter :: nop_word = int(z'D503201F', int64)

    target = make_target_ir('aarch64', 32_int32, .true., make_source_ref( &
        'aarchmrs-instructions', 'Instructions.json', &
        '439a0003e7904a4c93df27efd2702453336e00023d5f4c8ef3f0aa28291a10e3', 'IMPORTED'))
    call import_aarch64_instructions(add // new_line('a') // sub // new_line('a') // nop, &
        target%source, records, count, status)
    call assert_int(status, aarch64_source_ok, 'AARCHMRS records were rejected')
    call assert_int(count, 3_int32, 'AARCHMRS record count changed')

    call aarch64_validate_record(target, records(1), status)
    call assert_int(status, aarch64_record_ok, 'ADD record provenance failed')
    call aarch64_extract_variable_range(target, records(1), add_word, 1_int32, value, status)
    call assert_int64(value, 16746_int64, 'ADD range extraction changed')
    call assert_int(status, aarch64_record_ok, 'ADD range extraction failed')
    call aarch64_extract_variable_range(target, records(2), sub_word, 1_int32, value, status)
    call assert_int64(value, 298082_int64, 'SUB range extraction changed')
    call assert_int(status, aarch64_record_ok, 'SUB range extraction failed')
    call aarch64_extract_variable_range(target, records(3), nop_word, 1_int32, value, status)
    call assert_int64(value, 0_int64, 'zero-range NOP returned a value')
    call assert_int(status, aarch64_record_unsupported, 'zero-range NOP was supported')

    call aarch64_extract_variable_range(target, records(1), add_word, 0_int32, value, status)
    call assert_int(status, aarch64_record_unsupported, 'zero ordinal was accepted')
    call aarch64_extract_variable_range(target, records(1), add_word, 2_int32, value, status)
    call assert_int(status, aarch64_record_unsupported, 'out-of-range ordinal was accepted')
    call aarch64_extract_variable_range(target, records(1), -1_int64, 1_int32, value, status)
    call assert_int(status, aarch64_record_malformed, 'negative word was accepted')
    call aarch64_extract_variable_range(target, records(1), int(z'100000000', int64), 1_int32, &
        value, status)
    call assert_int(status, aarch64_record_malformed, 'wide word was accepted')

    bad_record = records(1)
    bad_record%source%source_hash = ''
    call aarch64_extract_variable_range(target, bad_record, add_word, 1_int32, value, status)
    call assert_int(status, aarch64_record_malformed, 'missing provenance was accepted')
    bad_record = records(1)
    bad_record%variable_ranges(1)%width = 0_int32
    call aarch64_extract_variable_range(target, bad_record, add_word, 1_int32, value, status)
    call assert_int(status, aarch64_record_malformed, 'malformed range was accepted')
    bad_target = target
    bad_target%architecture = 'riscv64'
    call aarch64_extract_variable_range(bad_target, records(1), add_word, 1_int32, value, status)
    call assert_int(status, aarch64_record_invalid_target, 'wrong target was accepted')

    write (*, '(a)') 'AArch64 variable-range extraction checks: ok'

contains

    subroutine assert_int(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

    subroutine assert_int64(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int64

end program test_aarch64_field
