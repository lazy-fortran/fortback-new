program test_targetir_sx
    use fortback_target_ir, only: make_source_ref
    use fortback_targetir_sx, only: targetir_sx_instruction_t, targetir_sx_invalid, &
        targetir_sx_capacity, targetir_sx_ok, targetir_sx_target_t, &
        query_targetir_v0_feature, validate_targetir_v0, write_targetir_v0
    implicit none

    character(len=2048) :: text
    type(targetir_sx_target_t) :: target
    type(targetir_sx_instruction_t) :: instructions(4)
    integer :: indices(4), match_count
    integer :: status
    character(len=64) :: expected_artifacts(2, 2)
    character(len=32) :: expected_origins(2, 2)

    expected_artifacts(1, :) = 'riscv-opcodes'
    expected_artifacts(2, :) = 'aarchmrs'
    expected_origins(1, :) = 'authoritative-machine-readable'
    expected_origins(2, :) = 'authoritative-prose'

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
    instructions(3) = instructions(1)
    instructions(3)%name = 'ldr'
    instructions(3)%feature = 'aarch64_base'
    instructions(3)%source = make_source_ref('aarchmrs', 'a64', 'fixture', &
        'authoritative-prose')
    instructions(3)%origin = 'authoritative-prose'
    instructions(4) = instructions(3)
    instructions(4)%name = 'str'

    call write_targetir_v0(target, instructions, 2, text, status)
    call assert_equal(status, targetir_sx_ok, 'canonical witness was not written')
    call assert_true(validate_targetir_v0(trim(text)), 'canonical witness was rejected')
    call assert_contains(trim(text), '(instruction (name add)', 'add was not emitted')
    call assert_contains(trim(text), '(source-hash fixture)', 'source hash was not emitted')

    indices = 99
    call query_targetir_v0_feature(instructions, 4, 'rv_i', indices, match_count, status)
    call assert_equal(status, targetir_sx_ok, 'feature query was not accepted')
    call assert_equal(match_count, 2, 'feature query count changed')
    call assert_indices(indices, [1, 2, 0, 0], 'feature query order changed')
    call assert_identity(instructions, indices, [1, 2], expected_artifacts(1, :), &
        expected_origins(1, :), 'RISC-V provenance changed')

    indices = 99
    call query_targetir_v0_feature(instructions, 4, 'aarch64_base', indices, &
        match_count, status)
    call assert_equal(status, targetir_sx_ok, 'second feature query was not accepted')
    call assert_equal(match_count, 2, 'second feature query count changed')
    call assert_indices(indices, [3, 4, 0, 0], 'second feature query order changed')
    call assert_identity(instructions, indices, [3, 4], expected_artifacts(2, :), &
        expected_origins(2, :), 'AArch64 provenance changed')

    indices = 99
    call query_targetir_v0_feature(instructions, 4, 'unknown', indices, match_count, status)
    call assert_equal(status, targetir_sx_invalid, 'unknown feature was accepted')
    call assert_equal(match_count, 0, 'unknown feature retained count')
    call assert_indices(indices, [0, 0, 0, 0], 'unknown feature retained output')

    indices = 99
    call query_targetir_v0_feature(instructions, 4, 'rv_i', indices(:1), match_count, status)
    call assert_equal(status, targetir_sx_capacity, 'capacity failure was accepted')
    call assert_equal(match_count, 0, 'capacity failure retained count')
    call assert_equal(indices(1), 0, 'capacity failure retained output')

    instructions(2)%feature = ''
    indices = 99
    call query_targetir_v0_feature(instructions, 4, 'rv_i', indices, match_count, status)
    call assert_equal(status, targetir_sx_invalid, 'malformed record was accepted')
    call assert_equal(match_count, 0, 'malformed query retained count')
    call assert_indices(indices, [0, 0, 0, 0], 'malformed query retained output')

    call write_targetir_v0(target, instructions, 0, text, status)
    call assert_equal(status, targetir_sx_invalid, 'empty handoff was accepted')
    call assert_true(len_trim(text) == 0, 'empty handoff emitted self-invalid SX')

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

    subroutine assert_indices(actual, expected, message)
        integer, intent(in) :: actual(:), expected(:)
        character(len=*), intent(in) :: message

        if (any(actual /= expected)) error stop message
    end subroutine assert_indices

    subroutine assert_identity(records, actual_indices, expected_indices, artifacts, origins, &
            message)
        type(targetir_sx_instruction_t), intent(in) :: records(:)
        integer, intent(in) :: actual_indices(:), expected_indices(:)
        character(len=*), intent(in) :: artifacts(:), origins(:), message
        integer :: i

        do i = 1, size(expected_indices)
            if (actual_indices(i) /= expected_indices(i)) error stop message
            if (trim(records(actual_indices(i))%source%artifact) /= trim(artifacts(i))) &
                error stop message
            if (trim(records(actual_indices(i))%origin) /= trim(origins(i))) error stop message
        end do
    end subroutine assert_identity

end program test_targetir_sx
