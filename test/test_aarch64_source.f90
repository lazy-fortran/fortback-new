program test_aarch64_source
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_source_malformed, aarch64_source_ok, aarch64_source_unsupported, &
        import_aarch64_instructions
    use fortback_target_ir, only: make_source_ref, source_ref_t
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
    type(source_ref_t) :: source
    type(aarch64_encoding_record_t) :: records(5)
    integer(int32) :: count, status

    source = make_source_ref('aarchmrs-instructions', 'Instructions.json', &
        '439a0003e7904a4c93df27efd2702453336e00023d5f4c8ef3f0aa28291a10e3', 'IMPORTED')
    call import_aarch64_instructions(add // new_line('a') // sub // new_line('a') // nop // &
        new_line('a') // adr // new_line('a') // adrp, &
        source, records, count, status)
    call assert_int(status, aarch64_source_ok, 'AArch64 witness rejected')
    call assert_int(count, 5_int32, 'AArch64 record count changed')
    call assert_int(records(1)%width, 32_int32, 'encoding width was not retained')
    call assert64(records(1)%match, int(z'91000000', int64), 'ADD match changed')
    call assert64(records(1)%mask, int(z'FF800000', int64), 'ADD mask changed')
    call assert64(records(2)%match, int(z'D1000000', int64), 'SUB match changed')
    call assert64(records(3)%match, int(z'D503201F', int64), 'NOP match changed')
    call assert64(records(3)%mask, int(z'FFFFFFFF', int64), 'NOP mask changed')
    call assert_equal(trim(records(4)%name), 'ADR_only_pcreladdr', 'ADR was not normalized')
    call assert64(records(4)%match, int(z'10000000', int64), 'ADR match changed')
    call assert64(records(4)%mask, int(z'9F000000', int64), 'ADR mask changed')
    call assert_equal(trim(records(5)%name), 'ADRP_only_pcreladdr', 'ADRP was not normalized')
    call assert64(records(5)%match, int(z'90000000', int64), 'ADRP match changed')
    call assert64(records(5)%mask, int(z'9F000000', int64), 'ADRP mask changed')
    call assert_equal(trim(records(1)%source%artifact), 'aarchmrs-instructions', 'artifact lost')
    call assert_equal(trim(records(1)%source%object), 'Instructions.json', 'object lost')
    call assert_equal(trim(records(1)%source%source_hash), trim(source%source_hash), 'hash lost')
    call assert_equal(trim(records(1)%source%origin), 'IMPORTED', 'origin lost')
    call assert_equal(trim(records(1)%target%architecture), 'aarch64', 'TargetIR architecture lost')
    call assert_equal(trim(records(3)%source%source_hash), trim(source%source_hash), &
        'NOP source hash lost')
    call assert_equal(trim(records(4)%source%artifact), trim(source%artifact), &
        'ADR source artifact lost')
    call assert_equal(trim(records(4)%source%object), trim(source%object), &
        'ADR source object lost')
    call assert_equal(trim(records(4)%source%source_hash), trim(source%source_hash), &
        'ADR source hash lost')
    call assert_equal(trim(records(4)%source%origin), trim(source%origin), &
        'ADR source origin lost')
    call assert_equal(trim(records(5)%source%artifact), trim(source%artifact), &
        'ADRP source artifact lost')
    call assert_equal(trim(records(5)%source%object), trim(source%object), &
        'ADRP source object lost')
    call assert_equal(trim(records(5)%source%source_hash), trim(source%source_hash), &
        'ADRP source hash lost')
    call assert_equal(trim(records(5)%source%origin), trim(source%origin), &
        'ADRP source origin lost')

    call import_aarch64_instructions('not-json', source, records, count, status)
    call assert_int(status, aarch64_source_malformed, 'malformed object accepted')
    call import_aarch64_instructions(substitute(add, 'ADD_64_addsub_imm', 'LDR_64_imm'), &
        source, records, count, status)
    call assert_int(status, aarch64_source_unsupported, 'unsupported instruction accepted')
    write (*, '(a)') 'AArch64 source importer checks: ok'

contains

    function substitute(text, old, new) result(out)
        character(len=*), intent(in) :: text, old, new
        character(len=len(text) - len(old) + len(new)) :: out

        out = text(1:index(text, old) - 1) // new // text(index(text, old) + len(old):)
    end function substitute

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message
        if (actual /= expected) error stop message
    end subroutine assert_equal

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

end program test_aarch64_source
