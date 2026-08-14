program test_aarch64_record
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_record, only: aarch64_find_fixed_record, &
        aarch64_record_invalid_target, aarch64_record_malformed, aarch64_record_ok, &
        aarch64_record_unsupported, aarch64_validate_record
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_source_ok, import_aarch64_instructions
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
        '"value":{"_type":"Values.Value","value":"010"}]}}'
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
        '"value":{"_type":"Values.Value","value":"010"}]}}'
    character(len=*), parameter :: nop = &
        '{"_type":"Instruction.Instruction","name":"NOP_HI_hints",' // &
        '"operation_id":"NOP_hints","encoding":{"_type":"Instruction.Encodeset.Encodeset",' // &
        '"width":32,"values":[{"_type":"Instruction.Encodeset.Bits",' // &
        '"range":{"_type":"Range","start":0,"width":32},' // &
        '"value":{"_type":"Values.Value","value":"11010101000000110010000000011111"}]}}'
    type(target_ir_t) :: target, bad_target
    type(aarch64_encoding_record_t) :: records(3), bad_record
    integer(int32) :: count, record_index, status
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
    call assert_int(status, aarch64_record_ok, 'ADD record validation failed')
    call aarch64_validate_record(target, records(2), status)
    call assert_int(status, aarch64_record_ok, 'SUB record validation failed')
    call aarch64_validate_record(target, records(3), status)
    call assert_int(status, aarch64_record_ok, 'NOP record validation failed')

    call aarch64_find_fixed_record(target, add_word, records, record_index, status)
    call assert_int(status, aarch64_record_ok, 'ADD fixed match failed')
    call assert_int(record_index, 1_int32, 'ADD record selection changed')
    call aarch64_find_fixed_record(target, sub_word, records, record_index, status)
    call assert_int(status, aarch64_record_ok, 'SUB fixed match failed')
    call assert_int(record_index, 2_int32, 'SUB record selection changed')
    call aarch64_find_fixed_record(target, nop_word, records, record_index, status)
    call assert_int(status, aarch64_record_ok, 'NOP fixed match failed')
    call assert_int(record_index, 3_int32, 'NOP record selection changed')
    call aarch64_find_fixed_record(target, int(z'14000000', int64), records, record_index, status)
    call assert_int(status, aarch64_record_unsupported, 'unmatched fixed word accepted')
    call assert_int(record_index, 0_int32, 'unmatched fixed word selected a record')

    records(1)%name = 'metadata-only-name'
    records(1)%operation_id = 'metadata-only-operation'
    call aarch64_find_fixed_record(target, add_word, records, record_index, status)
    call assert_int(status, aarch64_record_ok, 'name-independent fixed match failed')
    call assert_int(record_index, 1_int32, 'name-independent record selection changed')

    bad_record = records(1)
    bad_record%width = 16_int32
    call aarch64_validate_record(target, bad_record, status)
    call assert_int(status, aarch64_record_malformed, 'wrong record width accepted')
    bad_record = records(1)
    bad_record%mask = int(z'100000000', int64)
    call aarch64_validate_record(target, bad_record, status)
    call assert_int(status, aarch64_record_malformed, 'out-of-range mask accepted')
    bad_record = records(1)
    bad_record%match = int(z'100000000', int64)
    call aarch64_validate_record(target, bad_record, status)
    call assert_int(status, aarch64_record_malformed, 'out-of-range match accepted')
    bad_record = records(1)
    bad_record%match = ior(bad_record%match, 1_int64)
    call aarch64_validate_record(target, bad_record, status)
    call assert_int(status, aarch64_record_malformed, 'match outside mask accepted')

    bad_record = records(1)
    bad_record%source%source_hash = ''
    call aarch64_validate_record(target, bad_record, status)
    call assert_int(status, aarch64_record_malformed, 'missing record provenance accepted')
    bad_record = records(1)
    bad_record%target%source%source_hash = ''
    call aarch64_validate_record(target, bad_record, status)
    call assert_int(status, aarch64_record_malformed, 'missing TargetIR provenance accepted')

    bad_record = records(1)
    bad_record%target%architecture = 'riscv64'
    call aarch64_validate_record(target, bad_record, status)
    call assert_int(status, aarch64_record_invalid_target, 'wrong record target accepted')
    bad_target = target
    bad_target%architecture = 'riscv64'
    call aarch64_find_fixed_record(bad_target, add_word, records, record_index, status)
    call assert_int(status, aarch64_record_invalid_target, 'wrong target accepted')

    write (*, '(a)') 'AArch64 generic record checks: ok'

contains

    subroutine assert_int(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

end program test_aarch64_record
