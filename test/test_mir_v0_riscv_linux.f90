program test_mir_v0_riscv_linux
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, &
        mir_v0_bridge_unsupported, riscv_linux_artifact_provenance_valid, &
        riscv_linux_artifact_t, write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: first, second
    character(len=4096) :: input
    character(len=4096) :: malformed
    character(len=4096) :: unsupported
    character(len=4096) :: out_of_scope
    character(len=256) :: diagnostic
    character(len=256) :: path, command
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit

    input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input, first, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'frontend-v0 witness rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(first), &
        'artifact provenance boundary is not explicit')
    call assert_true(allocated(first%bytes), 'artifact bytes were not produced')
    call assert_byte(first%bytes, 1, 127, 'ELF magic missing')
    call assert_byte(first%bytes, 17, 2, 'artifact is not an executable')
    call assert_byte(first%bytes, 19, 243, 'artifact machine is not RV64')

    call compile_mir_v0_riscv_linux(input, second, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'second deterministic compile failed')
    call assert_true(size(first%bytes) == size(second%bytes), 'artifact size is nondeterministic')
    call assert_true(all(first%bytes == second%bytes), 'artifact bytes are nondeterministic')

    path = '/tmp/fortback-mir-v0-riscv-linux-test.elf'
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'artifact file write failed')
    call execute_command_line('chmod 755 -- '//trim(path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'artifact chmod command failed')
    call assert_int(exit_status, 0, 'artifact chmod failed')
    command = 'qemu-riscv64 '//trim(path)
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'qemu-riscv64 could not run artifact')
    call assert_int(exit_status, 0, 'artifact did not return stable Linux status')
    open (newunit=unit, file=trim(path), status='old', iostat=io_status)
    call assert_int(io_status, 0, 'artifact was not written')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'artifact cleanup failed')

    malformed = input(:len_trim(input) - 1)
    call compile_mir_v0_riscv_linux(malformed, second, status, diagnostic)
    call assert_status(status, mir_v0_bridge_malformed, 'malformed MIR was accepted')
    call assert_equal(trim(diagnostic), 'mir-v0: unexpected end of SX input', &
        'malformed diagnostic changed')

    unsupported = input
    unsupported(index(unsupported, 'opcode add'):index(unsupported, 'opcode add') + 9) = &
        'opcode mul'
    call compile_mir_v0_riscv_linux(unsupported, second, status, diagnostic)
    call assert_status(status, mir_v0_bridge_unsupported, 'unsupported MIR was accepted')
    call assert_equal(trim(diagnostic), 'mir-v0: opcode is unsupported', &
        'unsupported diagnostic changed')

    out_of_scope = input
    out_of_scope(index(out_of_scope, 'name main'):index(out_of_scope, 'name main') + 8) = &
        'name test'
    call compile_mir_v0_riscv_linux(out_of_scope, second, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'out-of-scope MIR was accepted')
    call assert_equal(trim(diagnostic), 'mir-v0: function is out of scope', &
        'out-of-scope diagnostic changed')

    write (*, '(a)') 'MIR-v0 RISC-V Linux bridge behavioral checks: ok'

contains

    subroutine assert_byte(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected
        character(len=*), intent(in) :: message

        if (iand(int(bytes(index), int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

    subroutine assert_status(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_status

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_riscv_linux
