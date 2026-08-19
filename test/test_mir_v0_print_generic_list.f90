program test_mir_v0_print_generic_list
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=65536) :: input
    character(len=256) :: diagnostic
    integer(int8) :: output(29)
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit, byte_index
    character(len=*), parameter :: path = '/tmp/fortback-print-generic-list.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-print-generic-list.out'
    character(len=*), parameter :: expected = &
        '20'//achar(10)//'21'//achar(10)//'22'//achar(10)// &
        '100'//achar(10)//'200'//achar(10)//'300'//achar(10)// &
        '400'//achar(10)//'500'//achar(10)

    input = generic_literal_input()
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic literal PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic literal PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic literal PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic literal PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic literal PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic literal PRINT output was not written')
    read (unit, iostat=io_status) output
    call assert_int(io_status, 0, 'generic literal PRINT output length changed')
    do byte_index = 1, size(output)
        call assert_byte(output(byte_index), iachar(expected(byte_index:byte_index)), &
            'generic literal PRINT output mismatch')
    end do
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic literal PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic literal PRINT output cleanup failed')

    input = generic_add_constant_input()
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x+2 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x+2 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+2 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+2 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x+2 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+2 PRINT output was not written')
    read (unit, iostat=io_status) output(1:2)
    call assert_int(io_status, 0, 'generic x+2 PRINT output length changed')
    call assert_byte(output(1), iachar('7'), 'generic x+2 PRINT output mismatch')
    call assert_byte(output(2), iachar(achar(10)), 'generic x+2 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x+2 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+2 PRINT output cleanup failed')

    input = generic_add_constant_input()
    input(index(input, 'literal 2'):index(input, 'literal 2') + 8) = 'literal 3'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x+3 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x+3 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+3 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+3 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x+3 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+3 PRINT output was not written')
    read (unit, iostat=io_status) output(1:2)
    call assert_int(io_status, 0, 'generic x+3 PRINT output length changed')
    call assert_byte(output(1), iachar('8'), 'generic x+3 PRINT output mismatch')
    call assert_byte(output(2), iachar(achar(10)), 'generic x+3 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x+3 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+3 PRINT output cleanup failed')

    input(index(input, 'literal 3'):index(input, 'literal 3') + 8) = 'literal 4'
    input(index(input, 'opcode add'):index(input, 'opcode add') + 9) = 'opcode sub '
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x-4 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x-4 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-4 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-4 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x-4 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-4 PRINT output was not written')
    read (unit, iostat=io_status) output(1:2)
    call assert_int(io_status, 0, 'generic x-4 PRINT output length changed')
    call assert_byte(output(1), iachar('1'), 'generic x-4 PRINT output mismatch')
    call assert_byte(output(2), iachar(achar(10)), 'generic x-4 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x-4 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-4 PRINT output cleanup failed')

    input = generic_add_constant_input()
    input(index(input, 'literal 2'):index(input, 'literal 2') + 8) = 'literal 0'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x+0 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x+0 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+0 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+0 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x+0 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+0 PRINT output was not written')
    read (unit, iostat=io_status) output(1:2)
    call assert_int(io_status, 0, 'generic x+0 PRINT output length changed')
    call assert_byte(output(1), iachar('5'), 'generic x+0 PRINT output mismatch')
    call assert_byte(output(2), iachar(achar(10)), 'generic x+0 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x+0 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+0 PRINT output cleanup failed')

    input(index(input, 'opcode add'):index(input, 'opcode add') + 9) = 'opcode sub '
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x-0 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x-0 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-0 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-0 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x-0 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-0 PRINT output was not written')
    read (unit, iostat=io_status) output(1:2)
    call assert_int(io_status, 0, 'generic x-0 PRINT output length changed')
    call assert_byte(output(1), iachar('5'), 'generic x-0 PRINT output mismatch')
    call assert_byte(output(2), iachar(achar(10)), 'generic x-0 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x-0 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-0 PRINT output cleanup failed')

    input = generic_add_constant_input()
    input(index(input, 'literal 2'):index(input, 'literal 2') + 10) = 'literal 10)'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x+10 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x+10 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+10 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+10 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x+10 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+10 PRINT output was not written')
    read (unit, iostat=io_status) output(1:3)
    call assert_int(io_status, 0, 'generic x+10 PRINT output length changed')
    call assert_byte(output(1), iachar('1'), 'generic x+10 PRINT output mismatch')
    call assert_byte(output(2), iachar('5'), 'generic x+10 PRINT output mismatch')
    call assert_byte(output(3), iachar(achar(10)), 'generic x+10 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x+10 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+10 PRINT output cleanup failed')

    input(index(input, 'opcode add'):index(input, 'opcode add') + 9) = 'opcode sub '
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x-10 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x-10 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-10 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-10 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x-10 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-10 PRINT output was not written')
    read (unit, iostat=io_status) output(1:3)
    call assert_int(io_status, 0, 'generic x-10 PRINT output length changed')
    call assert_byte(output(1), iachar('-'), 'generic x-10 PRINT output mismatch')
    call assert_byte(output(2), iachar('5'), 'generic x-10 PRINT output mismatch')
    call assert_byte(output(3), iachar(achar(10)), 'generic x-10 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x-10 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-10 PRINT output cleanup failed')

    input = generic_add_constant_input()
    input(index(input, 'literal 2'):index(input, 'literal 2') + 10) = 'literal 11)'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic x-11 PRINT MIR was accepted')
    write (*, '(a)') 'MIR-v0 generic literal PRINT QEMU check: ok'

contains

    function generic_literal_input() result(value)
        character(len=65536) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 19) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 0) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) '// &
            literal_output(2, 20)//literal_output(4, 21)//literal_output(6, 22)// &
            literal_output(8, 100)//literal_output(10, 200)//literal_output(12, 300)// &
            literal_output(14, 400)//literal_output(16, 500)// &
            '(instruction (id 18) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 16) '// &
            '(kind integer) (type i32)))))'
    end function generic_literal_input

    function generic_add_constant_input() result(value)
        character(len=65536) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 7) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 5) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) '// &
            '(instruction (id 2) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 3) (opcode const) '// &
            '(literal 2) (source-rule frontend-ast-v2/print-stmt) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 4) (opcode add) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 5) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 6) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 0) '// &
            '(kind integer) (type i32)))))'
    end function generic_add_constant_input

    function literal_output(id, literal) result(value)
        integer, intent(in) :: id, literal
        character(len=512) :: value

        value = '(instruction (id '//trim(int_text(id))//') (opcode const) (literal '// &
            trim(int_text(literal))//') (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id '//trim(int_text(id))//') (kind integer) (type i32))) '// &
            '(instruction (id '//trim(int_text(id + 1))//') (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id '// &
            trim(int_text(id + 1))//') (kind integer) (type i32))) '
    end function literal_output

    function int_text(number) result(value)
        integer, intent(in) :: number
        character(len=32) :: value

        write (value, '(i0)') number
    end function int_text

    subroutine assert_status(actual, expected_status, message)
        integer(int32), intent(in) :: actual, expected_status
        character(len=*), intent(in) :: message

        if (actual /= expected_status) error stop message
    end subroutine assert_status

    subroutine assert_byte(actual, expected_byte, message)
        integer(int8), intent(in) :: actual
        integer, intent(in) :: expected_byte
        character(len=*), intent(in) :: message

        if (iand(int(actual, int32), 255_int32) /= expected_byte) error stop message
    end subroutine assert_byte

    subroutine assert_int(actual, expected_value, message)
        integer, intent(in) :: actual, expected_value
        character(len=*), intent(in) :: message

        if (actual /= expected_value) error stop message
    end subroutine assert_int

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_print_generic_list
