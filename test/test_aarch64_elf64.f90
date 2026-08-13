program test_aarch64_elf64
    use iso_fortran_env, only: int8, int32
    use fortback_aarch64_elf64, only: aarch64_elf_invalid_source, &
        aarch64_elf_invalid_target, aarch64_elf_ok, aarch64_elf_unsupported, &
        write_aarch64_elf64_object_to_unit
    use fortback_aarch64_fixture, only: aarch64_add, aarch64_instruction_t, aarch64_sub
    use fortback_aarch64_source, only: aarch64_encoding_record_t, &
        aarch64_source_ok, import_aarch64_instructions
    use fortback_elf64, only: elf64_machine_aarch64, elf64_target_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t
    implicit none

    type(elf64_target_t) :: metadata, bad_metadata
    type(aarch64_instruction_t) :: instruction
    type(aarch64_encoding_record_t) :: records(2)
    type(source_ref_t) :: source, bad_source
    integer(int8) :: bytes(280)
    integer(int32) :: count, status
    integer :: io_status, unit
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

    source = make_source_ref('aarchmrs-instructions', 'Instructions.json', &
        '439a0003e7904a4c93df27efd2702453336e00023d5f4c8ef3f0aa28291a10e3', 'IMPORTED')
    metadata%target = make_target_ir('aarch64', 64_int32, .true., source)
    metadata%machine = elf64_machine_aarch64
    instruction = aarch64_instruction_t(aarch64_add, 10_int32, 11_int32, 16_int32)

    call import_aarch64_instructions(add // new_line('a') // sub, source, records, count, status)
    call assert_int(status, aarch64_source_ok, 'imported records were rejected')
    call assert_int(count, 2_int32, 'add/sub records were not imported')

    open (newunit=unit, status='scratch', access='stream', form='unformatted', &
        iostat=io_status)
    call assert_int(io_status, 0, 'scratch stream open failed')
    call write_aarch64_elf64_object_to_unit(metadata, source, instruction, records, unit, status)
    call assert_int(status, aarch64_elf_ok, 'AArch64 ELF pipeline failed')
    rewind (unit, iostat=io_status)
    call assert_int(io_status, 0, 'scratch stream rewind failed')
    read (unit, iostat=io_status) bytes
    call assert_int(io_status, 0, 'scratch stream read failed')
    call check_output(bytes, 183, 106, 65, 0, 145)

    instruction = aarch64_instruction_t(aarch64_sub, 2_int32, 3_int32, int(z'123', int32))
    rewind (unit, iostat=io_status)
    call assert_int(io_status, 0, 'SUB stream rewind before write failed')
    call write_aarch64_elf64_object_to_unit(metadata, source, instruction, records, unit, status)
    call assert_int(status, aarch64_elf_ok, 'AArch64 SUB pipeline failed')
    rewind (unit, iostat=io_status)
    call assert_int(io_status, 0, 'SUB stream rewind failed')
    read (unit, iostat=io_status) bytes
    call assert_int(io_status, 0, 'SUB stream read failed')
    call check_output(bytes, 183, 98, 140, 4, 209)

    bad_metadata = metadata
    bad_metadata%target%architecture = 'riscv64'
    call write_aarch64_elf64_object_to_unit(bad_metadata, source, instruction, records, unit, status)
    call assert_int(status, aarch64_elf_invalid_target, 'wrong target was accepted')

    bad_source = source_ref_t()
    call write_aarch64_elf64_object_to_unit(metadata, bad_source, instruction, records, unit, status)
    call assert_int(status, aarch64_elf_invalid_source, 'invalid source was accepted')

    instruction%kind = 99_int32
    call write_aarch64_elf64_object_to_unit(metadata, source, instruction, records, unit, status)
    call assert_int(status, aarch64_elf_unsupported, 'unsupported instruction was accepted')
    close (unit)

    write (*, '(a)') 'AArch64 ELF pipeline behavioral checks: ok'

contains

    subroutine check_output(bytes, machine, word1, word2, word3, word4)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: machine, word1, word2, word3, word4

        call assert_byte(bytes, 1, 127, 'ELF magic')
        call assert_byte(bytes, 2, 69, 'ELF magic')
        call assert_byte(bytes, 3, 76, 'ELF magic')
        call assert_byte(bytes, 4, 70, 'ELF magic')
        call assert_byte(bytes, 5, 2, 'ELF class')
        call assert_byte(bytes, 6, 1, 'ELF endian')
        call assert_byte(bytes, 19, machine, 'AArch64 machine low byte')
        call assert_byte(bytes, 20, 0, 'AArch64 machine high byte')
        call assert_byte(bytes, 65, word1, 'encoded instruction byte 1')
        call assert_byte(bytes, 66, word2, 'encoded instruction byte 2')
        call assert_byte(bytes, 67, word3, 'encoded instruction byte 3')
        call assert_byte(bytes, 68, word4, 'encoded instruction byte 4')
    end subroutine check_output

    subroutine assert_byte(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected
        character(len=*), intent(in) :: message

        if (iand(int(bytes(index), int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

end program test_aarch64_elf64
