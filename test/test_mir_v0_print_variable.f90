program test_mir_v0_print_variable
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=8192) :: input, wrong_storage, wrong_output, wrong_literal
    character(len=256) :: diagnostic
    integer(int8) :: output(3)
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit
    character(len=*), parameter :: path = '/tmp/fortback-print-variable.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-print-variable.out'

    input = print_variable_input('x', 'x', .false., 17)
    call run_print_variable(input, path, output_path, 49, 55)

    input = print_variable_input('x', 'x', .false., 23)
    call run_print_variable(input, path, output_path, 50, 51)

    input = print_variable_expression_input()
    call run_print_variable(input, path, output_path, 50, 52)

    input = print_variable_multiply_expression_input()
    call run_print_variable(input, path, output_path, 52, 54)

    input = print_variable_subtract_expression_input()
    call run_print_variable(input, path, output_path, 50, 49)

    input = print_variable_divide_expression_input()
    call run_print_variable(input, path, output_path, 49, 50)

    input = print_variable_power_expression_input()
    call run_print_variable_power(input, path, output_path, 56)

    input = print_variable_power_value_expression_input()
    call run_print_variable_power(input, path, output_path, 57)

    input = print_variable_input('x', 'x', .false., 17)
    wrong_storage = print_variable_input('x', 'x', .true., 17)
    call compile_mir_v0_riscv_linux(wrong_storage, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'missing storage was accepted')
    wrong_output = input
    wrong_output(index(wrong_output, 'opcode output'):index(wrong_output, 'opcode output') + 12) = &
        'opcode return '
    call compile_mir_v0_riscv_linux(wrong_output, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'malformed output shape was accepted')
    wrong_literal = print_variable_input('x', 'x', .false., 24)
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'unsupported stored literal was accepted')
    wrong_literal = print_variable_expression_input()
    wrong_literal = replace_text(wrong_literal, '(literal 23)', '(literal 24)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported expression literal was accepted')
    wrong_literal = print_variable_multiply_expression_input()
    wrong_literal = replace_text(wrong_literal, '(literal 2)', '(literal 3)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported multiplication literal was accepted')
    wrong_literal = print_variable_subtract_expression_input()
    wrong_literal = replace_text(wrong_literal, '(literal 2)', '(literal 3)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported subtraction literal was accepted')
    wrong_literal = print_variable_divide_expression_input()
    wrong_literal = replace_text(wrong_literal, '(literal 2)', '(literal 3)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported division literal was accepted')
    wrong_literal = print_variable_subtract_expression_input()
    wrong_literal = replace_text(wrong_literal, '(opcode sub)', '(opcode div)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'wrong arithmetic operator neighbor was accepted')
    wrong_literal = print_variable_power_expression_input()
    wrong_literal = replace_text(wrong_literal, '(name main)', '(name other)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'wrong function name was accepted')
    wrong_literal = print_variable_power_expression_input()
    wrong_literal = replace_text(wrong_literal, '(opcode pow)', '(opcode mul)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'wrong power operator was accepted')
    wrong_literal = print_variable_power_expression_input()
    wrong_literal = replace_text(wrong_literal, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'WRITE neighbor was accepted')
    wrong_literal = print_variable_power_value_expression_input()
    wrong_literal = replace_text(wrong_literal, '(name main)', '(name other)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'second power wrong function name was accepted')
    wrong_literal = print_variable_power_value_expression_input()
    wrong_literal = replace_text(wrong_literal, '(opcode pow)', '(opcode mul)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'second power wrong operator was accepted')
    wrong_literal = print_variable_power_value_expression_input()
    wrong_literal = replace_text(wrong_literal, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'second power WRITE neighbor was accepted')
    write (*, '(a)') 'MIR-v0 stored-variable PRINT qemu checks: ok'

contains
    subroutine run_print_variable(input, path, output_path, first_byte, second_byte)
        character(len=*), intent(in) :: input, path, output_path
        integer, intent(in) :: first_byte, second_byte
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(3)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'stored-variable PRINT MIR was rejected')
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'stored-variable PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'stored-variable PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'stored-variable PRINT qemu command failed')
        call assert_int(exit_status, 0, 'stored-variable PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'stored-variable PRINT output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'stored-variable PRINT output length changed')
        call assert_byte(output(1), first_byte, 'stored-variable PRINT missed first value byte')
        call assert_byte(output(2), second_byte, 'stored-variable PRINT missed second value byte')
        call assert_byte(output(3), 10, 'stored-variable PRINT missed newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'stored-variable PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'stored-variable PRINT output cleanup failed')
    end subroutine run_print_variable

    subroutine run_print_variable_power(input, path, output_path, expected_byte)
        character(len=*), intent(in) :: input, path, output_path
        integer, intent(in) :: expected_byte
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(2)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'power PRINT MIR was rejected')
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'power PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'power PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'power PRINT qemu command failed')
        call assert_int(exit_status, 0, 'power PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'power PRINT output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'power PRINT output length changed')
        call assert_byte(output(1), expected_byte, 'power PRINT missed value byte')
        call assert_byte(output(2), 10, 'power PRINT missed newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'power PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'power PRINT output cleanup failed')
    end subroutine run_print_variable_power

    function print_variable_input(store_key, load_key, omit_storage, literal) result(value)
        character(len=*), intent(in) :: store_key, load_key
        logical, intent(in) :: omit_storage
        integer, intent(in) :: literal
        character(len=4096) :: value
        character(len=128) :: store_text, load_text
        character(len=16) :: literal_text

        store_text = '(storage-key '//trim(store_key)//')'
        load_text = '(storage-key '//trim(load_key)//')'
        write (literal_text, '(i0)') literal
        if (omit_storage) then
            store_text = ''
            load_text = ''
        end if
        value = '(mir-function (name main) (entry-block 0) (instruction-count 5) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '//trim(literal_text)//') '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) '// &
            '(type i32))) (instruction (id 1) (opcode store) '//trim(store_text)// &
            ' (source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 2) (opcode load) '//trim(load_text)// &
            ' (source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) '// &
            '(type i32)))))'
    end function print_variable_input

    function print_variable_expression_input() result(value)
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 9) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 23) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) '// &
            '(type i32))) (instruction (id 1) (opcode store) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 2) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode const) (literal 1) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 3) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode add) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 5) (opcode store) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 7) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 8) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) '// &
            '(type i32)))))'
    end function print_variable_expression_input

    function print_variable_multiply_expression_input() result(value)
        character(len=8192) :: value

        value = replace_text(print_variable_expression_input(), '(literal 1)', '(literal 2)')
        value = replace_text(value, '(opcode add)', '(opcode mul)')
    end function print_variable_multiply_expression_input

    function print_variable_subtract_expression_input() result(value)
        character(len=8192) :: value

        value = print_variable_expression_input()
        value = replace_text(value, '(literal 1)', '(literal 2)')
        value = replace_text(value, '(opcode add)', '(opcode sub)')
    end function print_variable_subtract_expression_input

    function print_variable_divide_expression_input() result(value)
        character(len=8192) :: value

        value = replace_text(print_variable_expression_input(), '(literal 23)', '(literal 24)')
        value = replace_text(value, '(literal 1)', '(literal 2)')
        value = replace_text(value, '(opcode add)', '(opcode div)')
    end function print_variable_divide_expression_input

    function print_variable_power_expression_input() result(value)
        character(len=8192) :: value

        value = print_variable_expression_input()
        value = replace_text(value, '(literal 23)', '(literal 2)')
        value = replace_text(value, '(literal 1)', '(literal 3)')
        value = replace_text(value, '(opcode add)', '(opcode pow)')
    end function print_variable_power_expression_input

    function print_variable_power_value_expression_input() result(value)
        character(len=8192) :: value

        value = print_variable_power_expression_input()
        value = replace_text(value, '(literal 2)', '(literal 99)')
        value = replace_text(value, '(literal 3)', '(literal 2)')
        value = replace_text(value, '(literal 99)', '(literal 3)')
    end function print_variable_power_value_expression_input

    function replace_text(value, old, new) result(replaced)
        character(len=*), intent(in) :: value, old, new
        character(len=8192) :: replaced
        integer :: location

        replaced = value
        location = index(replaced, old)
        if (location > 0) replaced = replaced(:location - 1)//new//replaced(location + len(old):)
    end function replace_text

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
end program test_mir_v0_print_variable
