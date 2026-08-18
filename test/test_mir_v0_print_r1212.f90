program test_mir_v0_print_r1212
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    character(len=4096) :: input, wrong_literal, wrong_shape, wrong_opcode
    character(len=256) :: diagnostic
    character(len=*), parameter :: path = '/tmp/fortback-print-r1212.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-print-r1212.out'
    integer(int8) :: output(2)
    type(riscv_linux_artifact_t) :: artifact
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit

    input = '(mir-function (name p) (entry-block 0) (instruction-count 3) '// &
        '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 2) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'PRINT MIR was rejected')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'PRINT chmod command failed')
    call assert_int(exit_status, 0, 'PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'PRINT qemu command failed')
    call assert_int(exit_status, 0, 'PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'PRINT output was not written')
    read (unit, iostat=io_status) output
    call assert_int(io_status, 0, 'PRINT output length or bytes changed')
    call assert_byte(output(1), 55, 'PRINT did not write ASCII 7')
    call assert_byte(output(2), 10, 'PRINT did not write exactly one newline')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'PRINT wrote bytes beyond 7 and newline')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'PRINT output cleanup failed')

    wrong_literal = input
    wrong_literal(index(wrong_literal, 'literal 7'):index(wrong_literal, 'literal 7') + 8) = &
        'literal 6'
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'PRINT literal mutation was accepted')

    wrong_shape = input
    wrong_shape(index(wrong_shape, 'type i32'):index(wrong_shape, 'type i32') + 7) = &
        'type real'
    call compile_mir_v0_riscv_linux(wrong_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'PRINT type mutation was accepted')

    wrong_opcode = input
    wrong_opcode(index(wrong_opcode, 'opcode output'):index(wrong_opcode, 'opcode output') + 12) = &
        'opcode return '
    call compile_mir_v0_riscv_linux(wrong_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'PRINT output mutation was accepted')
    write (*, '(a)') 'MIR-v0 PRINT R1212 qemu checks: ok'

contains

    subroutine assert_byte(actual, expected, message)
        integer(int8), intent(in) :: actual
        integer, intent(in) :: expected
        character(len=*), intent(in) :: message

        if (iand(int(actual, int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

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

    subroutine assert_true(value, message)
        logical, intent(in) :: value
        character(len=*), intent(in) :: message

        if (.not. value) error stop message
    end subroutine assert_true

end program test_mir_v0_print_r1212
