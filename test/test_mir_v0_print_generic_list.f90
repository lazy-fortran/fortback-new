program test_mir_v0_print_generic_list
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, &
        riscv_linux_artifact_t, &
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
        '1'//achar(10)//'2'//achar(10)//'3'//achar(10)//'4'//achar(10)// &
        '5'//achar(10)//'6'//achar(10)//'7'//achar(10)//'8'//achar(10)

    call check_literal_list(1, '1'//achar(10))
    call check_literal_list(4, '1'//achar(10)//'2'//achar(10)//'3'//achar(10)//'4'//achar(10))
    call check_literal_list(10, '1'//achar(10)//'2'//achar(10)//'3'//achar(10)//'4'//achar(10)// &
        '5'//achar(10)//'6'//achar(10)//'7'//achar(10)//'8'//achar(10)//'9'//achar(10)//'10'//achar(10))
    call check_negative_literal(-1, '-1'//achar(10))
    call check_negative_literal(-20, '-20'//achar(10))
    call check_negative_literal(-100, '-100'//achar(10))

    input = generic_literal_input(11)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'generic eleven-item PRINT was accepted')

    input = generic_literal_input(4)
    input(index(input, 'opcode output'):index(input, 'opcode output') + 12) = 'opcode store  '
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'generic malformed PRINT was accepted')

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
    read (unit, iostat=io_status) output(1:len(expected))
    call assert_int(io_status, 0, 'generic literal PRINT output length changed')
    do byte_index = 1, len(expected)
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

    input = generic_add_constant_input(100)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x+100 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x+100 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+100 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x+100 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x+100 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+100 PRINT output was not written')
    read (unit, iostat=io_status) output(1:4)
    call assert_int(io_status, 0, 'generic x+100 PRINT output length changed')
    call assert_byte(output(1), iachar('1'), 'generic x+100 PRINT output mismatch')
    call assert_byte(output(2), iachar('0'), 'generic x+100 PRINT output mismatch')
    call assert_byte(output(3), iachar('5'), 'generic x+100 PRINT output mismatch')
    call assert_byte(output(4), iachar(achar(10)), 'generic x+100 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x+100 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x+100 PRINT output cleanup failed')

    input = generic_add_constant_input(100)
    input(index(input, 'opcode add'):index(input, 'opcode add') + 9) = 'opcode sub '
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, &
        'generic x-100 PRINT MIR was rejected: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'generic x-100 PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-100 PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'generic x-100 PRINT qemu command failed')
    call assert_int(exit_status, 0, 'generic x-100 PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-100 PRINT output was not written')
    read (unit, iostat=io_status) output(1:4)
    call assert_int(io_status, 0, 'generic x-100 PRINT output length changed')
    call assert_byte(output(1), iachar('-'), 'generic x-100 PRINT output mismatch')
    call assert_byte(output(2), iachar('9'), 'generic x-100 PRINT output mismatch')
    call assert_byte(output(3), iachar('5'), 'generic x-100 PRINT output mismatch')
    call assert_byte(output(4), iachar(achar(10)), 'generic x-100 PRINT newline mismatch')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic x-100 PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic x-100 PRINT output cleanup failed')

    input = generic_add_constant_input(101)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic x-101 PRINT MIR was accepted')

    input = generic_literal_input(1, -101)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic -101 PRINT MIR was accepted')

    input = generic_literal_input(1)
    input(index(input, 'literal 1'):index(input, 'literal 1') + 8) = 'literal -'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_malformed, &
        'generic malformed negative PRINT MIR was accepted')
    write (*, '(a)') 'MIR-v0 generic literal PRINT QEMU check: ok'

contains

    subroutine check_literal_list(item_count, expected_output)
        integer, intent(in) :: item_count
        character(len=*), intent(in) :: expected_output
        integer :: index

        input = generic_literal_input(item_count)
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic literal PRINT MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic literal PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic literal PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic literal PRINT qemu command failed')
        call assert_int(exit_status, 0, 'generic literal PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'generic literal PRINT output was not written')
        read (unit, iostat=io_status) output(1:len(expected_output))
        call assert_int(io_status, 0, 'generic literal PRINT output length changed')
        do index = 1, len(expected_output)
            call assert_byte(output(index), iachar(expected_output(index:index)), &
                'generic literal PRINT output mismatch')
        end do
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'generic literal PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'generic literal PRINT output cleanup failed')
    end subroutine check_literal_list

    subroutine check_negative_literal(literal, expected_output)
        integer, intent(in) :: literal
        character(len=*), intent(in) :: expected_output
        integer :: index

        input = generic_literal_input(1, literal)
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'generic negative literal PRINT MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic negative literal ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'generic negative literal chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic negative literal qemu command failed')
        call assert_int(exit_status, 0, 'generic negative literal artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'generic negative literal output was not written')
        read (unit, iostat=io_status) output(1:len(expected_output))
        call assert_int(io_status, 0, 'generic negative literal output length changed')
        do index = 1, len(expected_output)
            call assert_byte(output(index), iachar(expected_output(index:index)), &
                'generic negative literal output mismatch')
        end do
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'generic negative literal wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'generic negative literal output cleanup failed')
    end subroutine check_negative_literal

    function generic_literal_input(item_count, literal_override) result(value)
        integer, intent(in), optional :: item_count
        integer, intent(in), optional :: literal_override
        character(len=65536) :: value
        integer :: count, item, item_id, selected_literal

        count = 8
        if (present(item_count)) count = item_count
        selected_literal = 0
        if (present(literal_override)) selected_literal = literal_override
        value = '(mir-function (name main) (entry-block 0) (instruction-count '// &
            trim(int_text(3 + 2 * count))//') '// &
            '(instructions (instruction (id 0) (opcode const) (literal 0) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) '
        do item = 1, count
            item_id = 2 + 2 * (item - 1)
            if (present(literal_override)) then
                value = trim(value)//literal_output(item_id, selected_literal)
            else
                value = trim(value)//literal_output(item_id, item)
            end if
        end do
        value = trim(value)//'(instruction (id '//trim(int_text(2 + 2 * count))//') (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id '//trim(int_text(2 * count))//') '// &
            '(kind integer) (type i32)))))'
    end function generic_literal_input

    function generic_add_constant_input(constant_value) result(value)
        integer, intent(in), optional :: constant_value
        character(len=65536) :: value
        integer :: selected_value

        selected_value = 2
        if (present(constant_value)) selected_value = constant_value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 7) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 5) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) '// &
            '(instruction (id 2) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 3) (opcode const) '// &
            '(literal '//trim(int_text(selected_value))//') '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 0) '// &
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
