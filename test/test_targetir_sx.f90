program test_targetir_sx
    use fortback_target_ir, only: make_source_ref
    use fortback_targetir_sx, only: targetir_sx_instruction_t, targetir_sx_ok, &
        targetir_sx_target_t, validate_targetir_v0, write_targetir_v0
    implicit none

    character(len=2048) :: text
    type(targetir_sx_target_t) :: target
    type(targetir_sx_instruction_t) :: instructions(2)
    integer :: status

    target%name = 'riscv64'
    target%architecture = 'riscv64'
    target%source = make_source_ref('riscv-opcodes', 'rv_i', 'fixture', &
        'authoritative-machine-readable')
    instructions(1)%name = 'add'
    instructions(1)%feature = 'rv_i'
    instructions(1)%source = target%source
    instructions(1)%origin = 'authoritative-machine-readable'
    instructions(2) = instructions(1)
    instructions(2)%name = 'sub'

    call write_targetir_v0(target, instructions, 2, text, status)
    call assert_equal(status, targetir_sx_ok, 'canonical witness was not written')
    call assert_true(validate_targetir_v0(trim(text)), 'canonical witness was rejected')
    call assert_contains(trim(text), '(instruction (name add)', 'add was not emitted')
    call assert_contains(trim(text), '(source-hash fixture)', 'source hash was not emitted')

    call assert_true(.not. validate_targetir_v0( &
        replace(trim(text), 'authoritative-machine-readable', 'IMPORTED')), &
        'unknown origin was accepted')
    call assert_true(.not. validate_targetir_v0( &
        replace(trim(text), '(source-hash fixture)', '(source-hash )')), &
        'incomplete provenance was accepted')
    call assert_true(.not. validate_targetir_v0( &
        replace(trim(text), '(feature rv_i)', '(unexpected rv_i)')), &
        'unknown field was accepted')
    write (*, '(a)') 'TargetIR v0 SX checks: ok'

contains

    function replace(text, old, new) result(out)
        character(len=*), intent(in) :: text, old, new
        character(len=len(text) - len(old) + len(new)) :: out
        integer :: location

        location = index(text, old)
        out = text(1:location - 1) // new // text(location + len(old):)
    end function replace

    subroutine assert_equal(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_true(value, message)
        logical, intent(in) :: value
        character(len=*), intent(in) :: message

        if (.not. value) error stop message
    end subroutine assert_true

    subroutine assert_contains(value, expected, message)
        character(len=*), intent(in) :: value, expected, message

        if (index(value, expected) == 0) error stop message
    end subroutine assert_contains

end program test_targetir_sx
