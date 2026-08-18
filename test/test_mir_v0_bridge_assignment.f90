program test_mir_v0_bridge_assignment
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, &
        mir_v0_bridge_unsupported, riscv_linux_artifact_t, write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact, legacy
    character(len=4096) :: input, malformed, wrong_opcode, wrong_type, wrong_source
    character(len=256) :: diagnostic, path, command
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit

    input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode store) '// &
        '(source-rule frontend-ast-v1/assignment) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/assignment) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'integer assignment MIR was rejected')
    call assert_equal(size(artifact%bytes), 400, 'assignment artifact size changed')
    call assert_byte(artifact%bytes, 1, 127, 'ELF magic changed')
    call assert_byte(artifact%bytes, 17, 2, 'artifact is not executable')
    call assert_byte(artifact%bytes, 19, 243, 'artifact is not RV64')
    call assert_byte(artifact%bytes, 177, 19, 'exit artifact encoding changed')
    call assert_byte(artifact%bytes, 185, 115, 'exit artifact encoding changed')
    call compile_mir_v0_riscv_linux(legacy_input(), legacy, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'legacy exit artifact could not be built')
    call assert_true(all(artifact%bytes == legacy%bytes), &
        'assignment artifact bytes differ from the established exit artifact')

    path = '/tmp/fortback-mir-v0-assignment-riscv-linux-test.elf'
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'assignment ELF write failed')
    call execute_command_line('chmod 755 -- '//trim(path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'assignment ELF chmod command failed')
    call assert_int(exit_status, 0, 'assignment ELF chmod failed')
    command = 'qemu-riscv64 '//trim(path)
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'assignment qemu could not run artifact')
    call assert_int(exit_status, 0, 'assignment artifact did not return zero')
    open (newunit=unit, file=trim(path), status='old', iostat=io_status)
    call assert_int(io_status, 0, 'assignment ELF was not written')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'assignment ELF cleanup failed')

    malformed = input(:len_trim(input) - 1)
    call compile_mir_v0_riscv_linux(malformed, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_malformed, 'malformed assignment was accepted')
    wrong_opcode = input
    wrong_opcode(index(wrong_opcode, 'opcode store'):index(wrong_opcode, 'opcode store') + 11) = &
        'opcode add  '
    call compile_mir_v0_riscv_linux(wrong_opcode, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong assignment opcode was accepted')
    wrong_type = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode store) '// &
        '(source-rule frontend-ast-v1/assignment) (result (id 1) (kind integer) '// &
        '(type f32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/assignment) (result (id 1) (kind integer) '// &
        '(type f32)))))'
    call compile_mir_v0_riscv_linux(wrong_type, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong assignment type was accepted')
    wrong_source = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode store) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(wrong_source, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong assignment source was accepted')
    write (*, '(a)') 'MIR-v0 assignment bridge behavioral checks: ok'

contains

    subroutine assert_byte(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected
        character(len=*), intent(in) :: message

        if (iand(int(bytes(index), int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_equal(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

    function legacy_input() result(value)
        character(len=4096) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
            '(instructions (instruction (id 0) (opcode add) '// &
            '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 1) (opcode return) '// &
            '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
            '(type i32)))))'
    end function legacy_input

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_bridge_assignment
