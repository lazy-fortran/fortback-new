program test_elf64_unit
    use iso_fortran_env, only: int8, int32, int64
    use fortback_elf64, only: elf64_empty_word, elf64_io_error, elf64_machine_riscv, &
        elf64_ok, elf64_target_t, write_elf64_object_to_unit
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t
    implicit none

    type(elf64_target_t) :: metadata
    type(source_ref_t) :: source
    integer(int8) :: bytes(280)
    integer(int32) :: status
    integer :: io_status, unit

    metadata%target = make_target_ir('riscv64', 64_int32, .true., &
        make_source_ref('riscv-opcodes', 'rv_i', 'fixture-sha256', 'HUMAN'))
    metadata%machine = elf64_machine_riscv
    source = make_source_ref('riscv-opcodes', 'rv_i', 'fixture-sha256', 'HUMAN')

    open (newunit=unit, status='scratch', access='stream', form='unformatted', &
        iostat=io_status)
    call assert_int(io_status, 0, 'scratch stream open failed')
    call write_elf64_object_to_unit(metadata, source, int(z'00B50533', int64), unit, status)
    call assert_int(status, elf64_ok, 'ELF unit write failed')
    rewind (unit, iostat=io_status)
    call assert_int(io_status, 0, 'scratch stream rewind failed')
    read (unit, iostat=io_status) bytes
    call assert_int(io_status, 0, 'scratch stream read failed')
    call check_output(bytes)

    call write_elf64_object_to_unit(metadata, source, 0_int64, unit, status)
    call assert_int(status, elf64_empty_word, 'invalid word was written')
    close (unit)
    call write_elf64_object_to_unit(metadata, source, int(z'00B50533', int64), unit, status)
    call assert_int(status, elf64_io_error, 'closed stream failure was hidden')

    write (*, '(a)') 'ELF64 unit behavioral checks: ok'

contains

    subroutine check_output(bytes)
        integer(int8), intent(in) :: bytes(:)

        call assert_byte(bytes, 1, 127, 'ELF magic')
        call assert_byte(bytes, 2, 69, 'ELF magic')
        call assert_byte(bytes, 3, 76, 'ELF magic')
        call assert_byte(bytes, 4, 70, 'ELF magic')
        call assert_byte(bytes, 41, 88, 'section table placement')
        call assert_byte(bytes, 42, 0, 'section table placement high byte')
        call assert_byte(bytes, 65, 51, 'encoded word byte 1')
        call assert_byte(bytes, 66, 5, 'encoded word byte 2')
        call assert_byte(bytes, 67, 181, 'encoded word byte 3')
        call assert_byte(bytes, 68, 0, 'encoded word byte 4')
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

end program test_elf64_unit
