program test_elf64
    use iso_fortran_env, only: int8, int16, int32, int64
    use fortback_elf64, only: elf64_empty_word, elf64_invalid_source, &
        elf64_invalid_target, elf64_machine_riscv, elf64_ok, elf64_target_t, &
        write_elf64_object
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t
    implicit none

    type(elf64_target_t) :: metadata
    type(source_ref_t) :: source
    integer(int8), allocatable :: bytes(:), changed(:)
    integer(int32) :: status

    metadata%target = make_target_ir('riscv64', 64_int32, .true., &
        make_source_ref('riscv-opcodes', 'rv_i', 'fixture-sha256', 'HUMAN'))
    metadata%machine = elf64_machine_riscv
    source = make_source_ref('riscv-opcodes', 'rv_i', 'fixture-sha256', 'HUMAN')
    call write_elf64_object(metadata, source, int(z'00B50533', int64), bytes, status)
    call assert_int(status, elf64_ok, 'ELF witness rejected')
    call assert_int(size(bytes), 280, 'ELF size changed')
    call check_fixed_bytes(bytes)

    changed = bytes
    changed(19) = 0_int8
    call assert_true(changed(19) /= bytes(19), 'mutation did not change fixture')
    call assert_true(bytes(1) == 127_int8 .and. bytes(2) == 69_int8, &
        'independent ELF magic check failed')

    call write_elf64_object(metadata, source, 0_int64, changed, status)
    call assert_int(status, elf64_empty_word, 'empty word accepted')
    metadata%target%architecture = 'aarch64'
    call write_elf64_object(metadata, source, 1_int64, changed, status)
    call assert_int(status, elf64_invalid_target, 'unsupported target accepted')
    metadata%target%architecture = 'riscv64'
    metadata%machine = 1_int16
    call write_elf64_object(metadata, source, 1_int64, changed, status)
    call assert_int(status, elf64_invalid_target, 'unsupported machine accepted')
    metadata%machine = elf64_machine_riscv
    source%source_hash = ''
    call write_elf64_object(metadata, source, 1_int64, changed, status)
    call assert_int(status, elf64_invalid_source, 'incomplete source accepted')

    write (*, '(a)') 'ELF64 object behavioral checks: ok'

contains

    subroutine check_fixed_bytes(bytes)
        integer(int8), intent(in) :: bytes(:)

        call assert_byte(bytes, 1, 127, 'ELF magic')
        call assert_byte(bytes, 5, 2, 'ELF class')
        call assert_byte(bytes, 6, 1, 'ELF endian')
        call assert_byte(bytes, 17, 1, 'ET_REL')
        call assert_byte(bytes, 19, 243, 'machine')
        call assert_byte(bytes, 65, 51, 'word byte 1')
        call assert_byte(bytes, 66, 5, 'word byte 2')
        call assert_byte(bytes, 67, 181, 'word byte 3')
        call assert_byte(bytes, 68, 0, 'word byte 4')
        call assert_byte(bytes, 63, 2, 'section-name index')
        call assert_byte(bytes, 64, 0, 'section-name index high byte')
        call assert_byte(bytes, 89, 0, 'null section name')
        call assert_byte(bytes, 93, 0, 'null section type')
        call assert_byte(bytes, 153, 1, 'text section name')
        call assert_byte(bytes, 157, 1, 'text section type')
        call assert_byte(bytes, 161, 6, 'text section flags')
        call assert_byte(bytes, 177, 64, 'text section offset')
        call assert_byte(bytes, 185, 4, 'text section size')
        call assert_byte(bytes, 201, 4, 'text section alignment')
        call assert_byte(bytes, 217, 11, 'string section name')
        call assert_byte(bytes, 221, 3, 'string section type')
        call assert_byte(bytes, 241, 68, 'string section offset')
        call assert_byte(bytes, 249, 16, 'string section size')
        call assert_byte(bytes, 265, 1, 'string section alignment')
    end subroutine check_fixed_bytes

    subroutine assert_byte(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected
        character(len=*), intent(in) :: message

        if (iand(int(bytes(index), int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_true(value, message)
        logical, intent(in) :: value
        character(len=*), intent(in) :: message

        if (.not. value) error stop message
    end subroutine assert_true

    subroutine assert_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

end program test_elf64
