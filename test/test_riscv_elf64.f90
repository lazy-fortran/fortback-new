program test_riscv_elf64
    use iso_fortran_env, only: int8, int32
    use fortback_elf64, only: elf64_machine_riscv, elf64_target_t
    use fortback_riscv_elf64, only: riscv_elf_invalid_source, riscv_elf_invalid_target, &
        riscv_elf_ok, riscv_elf_unsupported, write_riscv_elf64_object_to_unit
    use fortback_riscv_fixture, only: riscv_add, riscv_instruction_t
    use fortback_riscv_source, only: import_riscv_opcodes, riscv_opcode_record_t, &
        riscv_source_ok
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t
    implicit none

    type(elf64_target_t) :: metadata, bad_metadata
    type(riscv_instruction_t) :: instruction
    type(riscv_opcode_record_t) :: records(2)
    type(source_ref_t) :: source, bad_source
    integer(int8) :: bytes(280)
    integer(int32) :: count, status
    integer :: io_status, unit
    character(len=*), parameter :: source_text = &
        'add rd rs1 rs2 31..25=0 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'sub rd rs1 rs2 31..25=0x20 14..12=0 6..2=0x0C 1..0=3'

    source = make_source_ref('riscv-opcodes', 'rv_i', 'fixture-sha256', 'IMPORTED')
    metadata%target = make_target_ir('riscv64', 64_int32, .true., source)
    metadata%machine = elf64_machine_riscv
    instruction = riscv_instruction_t(riscv_add, 10_int32, 10_int32, 11_int32, 0_int32)

    call import_riscv_opcodes(source_text, source, records, count, status)
    call assert_int(status, riscv_source_ok, 'imported records were rejected')
    call assert_int(count, 2_int32, 'add/sub records were not imported')

    open (newunit=unit, status='scratch', access='stream', form='unformatted', &
        iostat=io_status)
    call assert_int(io_status, 0, 'scratch stream open failed')
    call write_riscv_elf64_object_to_unit(metadata, source, instruction, records, unit, status)
    call assert_int(status, riscv_elf_ok, 'RISC-V ELF pipeline failed')
    rewind (unit, iostat=io_status)
    call assert_int(io_status, 0, 'scratch stream rewind failed')
    read (unit, iostat=io_status) bytes
    call assert_int(io_status, 0, 'scratch stream read failed')
    call check_output(bytes)

    bad_metadata = metadata
    bad_metadata%target%architecture = 'aarch64'
    call write_riscv_elf64_object_to_unit(bad_metadata, source, instruction, records, unit, status)
    call assert_int(status, riscv_elf_invalid_target, 'unsupported target was accepted')

    instruction%kind = 99_int32
    call write_riscv_elf64_object_to_unit(metadata, source, instruction, records, unit, status)
    call assert_int(status, riscv_elf_unsupported, 'unsupported instruction was accepted')

    bad_source = source_ref_t()
    instruction%kind = riscv_add
    call write_riscv_elf64_object_to_unit(metadata, bad_source, instruction, records, unit, status)
    call assert_int(status, riscv_elf_invalid_source, 'invalid source was accepted')
    close (unit)

    write (*, '(a)') 'RISC-V ELF pipeline behavioral checks: ok'

contains

    subroutine check_output(bytes)
        integer(int8), intent(in) :: bytes(:)

        call assert_byte(bytes, 1, 127, 'ELF magic')
        call assert_byte(bytes, 2, 69, 'ELF magic')
        call assert_byte(bytes, 3, 76, 'ELF magic')
        call assert_byte(bytes, 4, 70, 'ELF magic')
        call assert_byte(bytes, 41, 88, 'section table placement')
        call assert_byte(bytes, 42, 0, 'section table placement high byte')
        call assert_byte(bytes, 65, 51, 'encoded instruction byte 1')
        call assert_byte(bytes, 66, 5, 'encoded instruction byte 2')
        call assert_byte(bytes, 67, 181, 'encoded instruction byte 3')
        call assert_byte(bytes, 68, 0, 'encoded instruction byte 4')
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

end program test_riscv_elf64
