program test_target_ir
    use iso_fortran_env, only: int32
    use fortback_target_ir, only: make_source_ref, make_target_ir, target_ir_t, &
        target_ir_valid, target_ir_word_bytes
    implicit none

    type(target_ir_t) :: target

    target = make_target_ir('riscv64', 64_int32, .true., &
        make_source_ref('riscv-opcodes', 'add', 'fixture-sha256', 'HUMAN'))
    call assert_true(target_ir_valid(target), 'valid target rejected')
    call assert_equal(trim(target%architecture), 'riscv64', &
        'architecture was not retained')
    call assert_equal_integer(target_ir_word_bytes(target), 8_int32, &
        '64-bit word did not occupy eight bytes')
    call assert_true(target%little_endian, 'endianness was not retained')

    target = make_target_ir('incomplete', 0_int32, .false., &
        make_source_ref('fixture', 'target', 'fixture-sha256', 'HUMAN'))
    call assert_true(.not. target_ir_valid(target), &
        'zero-width target was accepted')
    call assert_equal_integer(target_ir_word_bytes(target), 0_int32, &
        'invalid target did not report zero word bytes')

    write (*, '(a)') 'target IR behavioral checks: ok'

contains

    subroutine assert_true(value, message)
        logical, intent(in) :: value
        character(len=*), intent(in) :: message

        if (.not. value) error stop message
    end subroutine assert_true

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_equal_integer(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal_integer

end program test_target_ir
