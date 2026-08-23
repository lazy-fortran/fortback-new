program test_mir_v0_print_variable
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, &
        riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=65536) :: input, two_item_input, three_item_input, four_item_input
    character(len=8192) :: wrong_storage, wrong_output, wrong_literal
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

    input = print_variable_input('x', 'x', .false., 0)
    call run_print_variable_signed(input, path, output_path, '0'//achar(10))

    input = print_variable_input('x', 'x', .false., 2047)
    call run_print_variable_signed(input, path, output_path, '2047'//achar(10))

    input = print_variable_input('x', 'x', .false., 42)
    call run_print_variable_signed(input, path, output_path, '42'//achar(10))

    input = raw_scalar_print_variable_input()
    call run_print_variable_signed(input, path, output_path, '42'//achar(10))
    wrong_storage = replace_text(input, '(storage-key counter_2)', '(storage-key 2counter)')
    call compile_mir_v0_riscv_linux(wrong_storage, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'malformed generic storage key was accepted')

    input = print_variable_input('x', 'x', .false., -42)
    call run_print_variable_signed(input, path, output_path, '-42'//achar(10))

    input = print_variable_input('x', 'x', .false., -5)
    call run_print_variable_signed(input, path, output_path, '-5'//achar(10))

    input = print_variable_input('x', 'x', .false., -100)
    call run_print_variable_signed(input, path, output_path, '-100'//achar(10))
    input = print_variable_add_input(42)
    call run_print_variable_signed(input, path, output_path, '43'//achar(10))
    input = print_variable_add_input(-42)
    call run_print_variable_signed(input, path, output_path, '-41'//achar(10))
    wrong_literal = replace_text(print_variable_add_input(42), &
        'frontend-ast-v2/execution-part', 'frontend-ast-v2/unknown')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'initialized x+1 source mutation was accepted')
    wrong_literal = replace_text(print_variable_add_input(42), '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'initialized x+1 storage mutation was accepted')
    wrong_literal = print_variable_add_input(-101)
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'initialized x+1 lower boundary was accepted')
    wrong_literal = print_variable_add_input(2048)
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'initialized x+1 upper boundary was accepted')
    wrong_literal = print_variable_input('y', 'x', .false., -5)
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'signed wrong storage mutation was accepted')
    wrong_literal = replace_text(print_variable_input('x', 'x', .false., -100), &
        '(opcode output)', '(opcode return)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'signed wrong opcode mutation was accepted')

    input = print_variable_expression_input()
    call run_print_variable(input, path, output_path, 50, 52)

    input = print_variable_multiply_expression_input()
    call run_print_variable(input, path, output_path, 52, 54)

    input = print_variable_subtract_expression_input()
    call run_print_variable(input, path, output_path, 50, 49)

    input = print_variable_divide_expression_input()
    call run_print_variable(input, path, output_path, 49, 50)

    input = print_variable_generic_expression_input()
    call run_print_generic_expression(input, path, output_path)
    wrong_literal = replace_text(input, '(opcode add)', '(opcode load)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic expression opcode ordering mutation was accepted')
    wrong_literal = replace_text(input, '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic expression storage mutation was accepted')
    wrong_literal = replace_text(input, '(opcode output)', '(opcode return)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic expression output mutation was accepted')
    wrong_literal = replace_text(input, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic expression source mutation was accepted')

    input = print_variable_generic_multiply_input()
    call run_print_generic_multiply(input, path, output_path)
    wrong_literal = replace_text(input, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/unknown')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic multiplication source mutation was accepted')
    wrong_literal = replace_text(input, '(opcode mul)', '(opcode output)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic multiplication malformed opcode was accepted')
    wrong_literal = replace_text(input, '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic multiplication storage mutation was accepted')
    wrong_literal = replace_text(input, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic multiplication source mutation was accepted')

    input = print_variable_generic_subtract_input()
    call run_print_generic_subtract(input, path, output_path)
    wrong_literal = replace_text(input, '(literal 2)', '(literal 101)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic subtraction literal mutation was accepted')
    wrong_literal = replace_text(input, '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic subtraction storage mutation was accepted')
    wrong_literal = replace_text(input, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic subtraction source mutation was accepted')

    input = print_variable_generic_divide_input()
    call run_print_generic_divide(input, path, output_path)
    wrong_literal = replace_text(input, '(literal 2)', '(literal 3)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic division literal mutation was accepted')
    wrong_literal = replace_text(replace_text(input, '(literal 2)', '(literal 101)'), &
        '(opcode div)', '(opcode add)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic division opcode mutation was accepted')
    wrong_literal = replace_text(input, '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic division storage mutation was accepted')
    wrong_literal = replace_text(input, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic division source mutation was accepted')
    wrong_literal = replace_text(input, '(opcode div)', '(opcode output)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic division instruction-order mutation was accepted')

    input = print_variable_generic_power_two_input()
    call run_print_generic_power(input, path, output_path, 9)
    input = print_variable_generic_power_input()
    call run_print_generic_power(input, path, output_path, 27)
    input = print_variable_generic_power_four_input()
    call run_print_generic_power(input, path, output_path, 81)
    input = print_variable_generic_power_five_input()
    call run_print_generic_power(input, path, output_path, 243)
    input = print_variable_generic_power_seven_input()
    call run_print_generic_power(input, path, output_path, 2187)
    input = print_variable_generic_power_ten_input()
    call run_print_generic_power(input, path, output_path, 59049)
    wrong_literal = replace_text(input, '(literal 10) (source-rule frontend-ast-v2/print-stmt)', &
        '(literal 11) (source-rule frontend-ast-v2/print-stmt)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic power literal mutation was accepted')
    wrong_literal = replace_text(replace_text(input, '(literal 10)', '(literal 101)'), &
        '(opcode pow)', '(opcode sub)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic power opcode mutation was accepted')
    wrong_literal = replace_text(input, '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic power storage mutation was accepted')
    wrong_literal = replace_text(input, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic power source mutation was accepted')
    wrong_literal = replace_text(input, '(opcode pow)', '(opcode output)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'generic power instruction-order mutation was accepted')
    input = print_variable_generic_variable_power_input()
    call run_print_generic_power(input, path, output_path, 27)
    input = print_variable_generic_variable_power_four_input()
    call run_print_generic_power(input, path, output_path, 256, 4)

    input = print_variable_power_expression_input()
    call run_print_variable_power(input, path, output_path, 56)

    input = print_variable_power_value_expression_input()
    call run_print_variable_power(input, path, output_path, 57)

    two_item_input = print_variable_power_two_item_input()
    call run_print_variable_two_item(two_item_input, path, output_path)

    three_item_input = print_variable_power_three_item_input()
    call run_print_variable_three_item(three_item_input, path, output_path)

    four_item_input = print_variable_power_four_item_input()
    call run_print_variable_four_item(four_item_input, path, output_path)
    call run_print_variable_many_items(print_variable_power_many_item_input(11), path, output_path, 11)
    call run_print_variable_many_items(print_variable_power_many_item_input(20), path, output_path, 20)
    call run_print_variable_many_items(print_variable_power_many_item_input(21), path, output_path, 21)
    call run_print_variable_many_items(print_variable_power_many_item_input(30), path, output_path, 30)
    call run_print_variable_many_items(print_variable_power_many_item_input(40), path, output_path, 40)
    call run_print_variable_many_items(print_variable_power_many_item_input(41), path, output_path, 41)
    call run_print_variable_many_items(print_variable_power_many_item_input(50), path, output_path, 50)
    call run_print_variable_many_items(print_variable_power_many_item_input(60), path, output_path, 60)
    call run_print_variable_many_items(print_variable_power_many_item_input(61), path, output_path, 61)
    call run_print_variable_many_items(print_variable_power_many_item_input(70), path, output_path, 70)
    call run_print_variable_many_items(print_variable_power_many_item_input(80), path, output_path, 80)
    call run_print_variable_many_items(print_variable_power_many_item_input(81), path, output_path, 81)
    call run_print_variable_many_items(print_variable_power_many_item_input(90), path, output_path, 90)
    call run_print_variable_many_items(print_variable_power_many_item_input(100), path, output_path, 100)
    wrong_literal = replace_text(four_item_input, '(opcode output) (source-rule frontend-ast-v2/print-stmt)', &
        '(opcode return) (source-rule frontend-ast-v2/print-stmt)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'four-item malformed route was accepted')

    wrong_literal = replace_text(two_item_input, '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'wrong second storage was accepted')
    wrong_literal = replace_text(two_item_input, '(opcode output) (source-rule frontend-ast-v2/print-stmt)', &
        '(opcode return) (source-rule frontend-ast-v2/print-stmt)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'one-output neighbor was accepted')
    wrong_literal = replace_text(two_item_input, '(opcode pow)', '(opcode mul)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'wrong second operator was accepted')
    wrong_literal = replace_text(two_item_input, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'two-output WRITE neighbor was accepted')

    input = print_variable_input('x', 'x', .false., 17)
    wrong_storage = print_variable_input('x', 'x', .true., 17)
    call compile_mir_v0_riscv_linux(wrong_storage, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'missing storage was accepted')
    wrong_output = input
    wrong_output(index(wrong_output, 'opcode output'):index(wrong_output, 'opcode output') + 12) = &
        'opcode return '
    call compile_mir_v0_riscv_linux(wrong_output, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'malformed output shape was accepted')
    wrong_literal = print_variable_input('x', 'x', .false., 42)
    wrong_literal = replace_text(wrong_literal, '(opcode output)', '(opcode return)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'stored initializer opcode mutation was accepted')
    wrong_literal = print_variable_input('x', 'x', .false., 2048)
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'out-of-range positive stored literal was accepted')
    wrong_literal = print_variable_input('x', 'x', .false., -101)
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'out-of-range negative stored literal was accepted')
    wrong_literal = print_variable_input('x', 'x', .false., 42)
    wrong_literal = replace_text(wrong_literal, '(storage-key x)', '(storage-key y)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'stored initializer storage mutation was accepted')
    wrong_literal = print_variable_input('x', 'x', .false., 0)
    wrong_literal = replace_text(wrong_literal, '(literal 0)', '(literal - 5)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_malformed, 'malformed negative literal was accepted')
    wrong_literal = print_variable_expression_input()
    wrong_literal = replace_text(wrong_literal, '(literal 23)', '(literal 2048)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported expression literal was accepted')
    wrong_literal = print_variable_multiply_expression_input()
    wrong_literal = replace_text(wrong_literal, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/unknown')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported multiplication source mutation was accepted')
    wrong_literal = print_variable_subtract_expression_input()
    wrong_literal = replace_text(wrong_literal, '(literal 2)', '(literal 11)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported subtraction literal was accepted')
    wrong_literal = print_variable_divide_expression_input()
    wrong_literal = replace_text(wrong_literal, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/unknown')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'unsupported division source mutation was accepted')
    wrong_literal = print_variable_subtract_expression_input()
    wrong_literal = replace_text(wrong_literal, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/unknown')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'wrong arithmetic source neighbor was accepted')
    wrong_literal = print_variable_power_expression_input()
    wrong_literal = replace_text(wrong_literal, '(name main)', '(name other)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'wrong function name was accepted')
    wrong_literal = print_variable_power_expression_input()
    wrong_literal = replace_text(wrong_literal, '(opcode pow)', '(opcode output)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'malformed power operator was accepted')
    wrong_literal = print_variable_power_expression_input()
    wrong_literal = replace_text(wrong_literal, 'frontend-ast-v2/print-stmt', 'frontend-ast-v2/write-stmt')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'WRITE neighbor was accepted')
    wrong_literal = print_variable_power_value_expression_input()
    wrong_literal = replace_text(wrong_literal, '(name main)', '(name other)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'second power wrong function name was accepted')
    wrong_literal = print_variable_power_value_expression_input()
    wrong_literal = replace_text(wrong_literal, '(opcode pow)', '(opcode output)')
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'second power malformed operator was accepted')
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

    subroutine run_print_variable_signed(input, path, output_path, expected)
        character(len=*), intent(in) :: input, path, output_path, expected
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        character(len=len(expected)) :: output
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'signed stored-variable PRINT MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'signed stored-variable PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'signed stored-variable PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'signed stored-variable PRINT qemu command failed')
        call assert_int(exit_status, 0, 'signed stored-variable PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'signed stored-variable PRINT output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'signed stored-variable PRINT output length changed')
        call assert_true(output == expected, 'signed stored-variable PRINT output mismatch')
        read (unit, iostat=io_status) output(1:1)
        call assert_true(io_status /= 0, 'signed stored-variable PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'signed stored-variable PRINT output cleanup failed')
    end subroutine run_print_variable_signed

    subroutine run_print_generic_expression(input, path, output_path)
        character(len=*), intent(in) :: input, path, output_path
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(2)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'generic expression MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic expression ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'generic expression chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic expression qemu command failed')
        call assert_int(exit_status, 0, 'generic expression artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'generic expression output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'generic expression output length changed')
        call assert_byte(output(1), 54, 'generic expression missed 6')
        call assert_byte(output(2), 10, 'generic expression missed first newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'generic expression wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'generic expression output cleanup failed')
    end subroutine run_print_generic_expression

    subroutine run_print_generic_multiply(input, path, output_path)
        character(len=*), intent(in) :: input, path, output_path
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(6)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'generic multiplication MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic multiplication ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'generic multiplication chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic multiplication qemu command failed')
        call assert_int(exit_status, 0, 'generic multiplication artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'generic multiplication output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'generic multiplication output length changed')
        call assert_byte(output(1), 54, 'generic multiplication missed 6')
        call assert_byte(output(2), 10, 'generic multiplication missed first newline')
        call assert_byte(output(3), 55, 'generic multiplication missed 7')
        call assert_byte(output(4), 10, 'generic multiplication missed second newline')
        call assert_byte(output(5), 51, 'generic multiplication missed 3')
        call assert_byte(output(6), 10, 'generic multiplication missed third newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'generic multiplication wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'generic multiplication output cleanup failed')
    end subroutine run_print_generic_multiply

    subroutine run_print_generic_subtract(input, path, output_path)
        character(len=*), intent(in) :: input, path, output_path
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(2)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'generic subtraction MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic subtraction ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'generic subtraction chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic subtraction qemu command failed')
        call assert_int(exit_status, 0, 'generic subtraction artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'generic subtraction output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'generic subtraction output length changed')
        call assert_byte(output(1), 49, 'generic subtraction missed 1')
        call assert_byte(output(2), 10, 'generic subtraction missed newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'generic subtraction wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'generic subtraction output cleanup failed')
    end subroutine run_print_generic_subtract

    subroutine run_print_generic_divide(input, path, output_path)
        character(len=*), intent(in) :: input, path, output_path
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(6)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'generic division MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic division ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'generic division chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic division qemu command failed')
        call assert_int(exit_status, 0, 'generic division artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'generic division output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'generic division output length changed')
        call assert_byte(output(1), 49, 'generic division missed 1')
        call assert_byte(output(2), 10, 'generic division missed first newline')
        call assert_byte(output(3), 55, 'generic division missed 7')
        call assert_byte(output(4), 10, 'generic division missed second newline')
        call assert_byte(output(5), 51, 'generic division missed 3')
        call assert_byte(output(6), 10, 'generic division missed third newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'generic division wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'generic division output cleanup failed')
    end subroutine run_print_generic_divide

    subroutine run_print_generic_power(input, path, output_path, expected_value, expected_tail)
        character(len=*), intent(in) :: input, path, output_path
        integer, intent(in) :: expected_value
        integer, intent(in), optional :: expected_tail
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(64)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit, expected_length, index
        character(len=32) :: expected_text
        integer :: tail_value

        tail_value = 3
        if (present(expected_tail)) tail_value = expected_tail

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'generic power MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'generic power ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'generic power chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'generic power qemu command failed')
        call assert_int(exit_status, 0, 'generic power artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'generic power output was not written')
        write (expected_text, '(i0)') expected_value
        expected_length = len_trim(expected_text) + 5
        read (unit, iostat=io_status) output(:expected_length)
        call assert_int(io_status, 0, 'generic power output length changed')
        do index = 1, len_trim(expected_text)
            call assert_byte(output(index), iachar(expected_text(index:index)), &
                'generic power digit mismatch')
        end do
        call assert_byte(output(len_trim(expected_text) + 1), 10, 'generic power missed first newline')
        call assert_byte(output(len_trim(expected_text) + 2), 55, 'generic power missed 7')
        call assert_byte(output(len_trim(expected_text) + 3), 10, 'generic power missed second newline')
        call assert_byte(output(len_trim(expected_text) + 4), 48 + tail_value, 'generic power missed stored value')
        call assert_byte(output(len_trim(expected_text) + 5), 10, 'generic power missed third newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'generic power wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'generic power output cleanup failed')
    end subroutine run_print_generic_power

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

    subroutine run_print_variable_two_item(input, path, output_path)
        character(len=*), intent(in) :: input, path, output_path
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(4)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'two-item power PRINT MIR was rejected')
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'two-item power PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'two-item power PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'two-item power PRINT qemu command failed')
        call assert_int(exit_status, 0, 'two-item power PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'two-item power PRINT output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'two-item power PRINT output length changed')
        call assert_byte(output(1), 57, 'two-item power PRINT missed first 9')
        call assert_byte(output(2), 10, 'two-item power PRINT missed first newline')
        call assert_byte(output(3), 57, 'two-item power PRINT missed second 9')
        call assert_byte(output(4), 10, 'two-item power PRINT missed second newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'two-item power PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'two-item power PRINT output cleanup failed')
    end subroutine run_print_variable_two_item

    subroutine run_print_variable_three_item(input, path, output_path)
        character(len=*), intent(in) :: input, path, output_path
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(6)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'three-item power PRINT MIR was rejected')
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'three-item power PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'three-item power PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'three-item power PRINT qemu command failed')
        call assert_int(exit_status, 0, 'three-item power PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'three-item power PRINT output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'three-item power PRINT output length changed')
        call assert_byte(output(1), 57, 'three-item power PRINT missed first 9')
        call assert_byte(output(2), 10, 'three-item power PRINT missed first newline')
        call assert_byte(output(3), 57, 'three-item power PRINT missed second 9')
        call assert_byte(output(4), 10, 'three-item power PRINT missed second newline')
        call assert_byte(output(5), 57, 'three-item power PRINT missed third 9')
        call assert_byte(output(6), 10, 'three-item power PRINT missed third newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'three-item power PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'three-item power PRINT output cleanup failed')
    end subroutine run_print_variable_three_item

    subroutine run_print_variable_four_item(input, path, output_path)
        character(len=*), intent(in) :: input, path, output_path
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8) :: output(8)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit

        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'four-item power PRINT MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'four-item power PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'four-item power PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'four-item power PRINT qemu command failed')
        call assert_int(exit_status, 0, 'four-item power PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'four-item power PRINT output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'four-item power PRINT output length changed')
        call assert_byte(output(1), 57, 'four-item power PRINT missed first 9')
        call assert_byte(output(2), 10, 'four-item power PRINT missed first newline')
        call assert_byte(output(3), 57, 'four-item power PRINT missed second 9')
        call assert_byte(output(4), 10, 'four-item power PRINT missed second newline')
        call assert_byte(output(5), 57, 'four-item power PRINT missed third 9')
        call assert_byte(output(6), 10, 'four-item power PRINT missed third newline')
        call assert_byte(output(7), 57, 'four-item power PRINT missed fourth 9')
        call assert_byte(output(8), 10, 'four-item power PRINT missed fourth newline')
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'four-item power PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'four-item power PRINT output cleanup failed')
    end subroutine run_print_variable_four_item

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

    function raw_scalar_print_variable_input() result(value)
        character(len=4096) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 5) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 42) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 1) (opcode store) (storage-key counter_2) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 2) (opcode load) (storage-key counter_2) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 1) (kind integer) '// &
            '(type i32)))))'
    end function raw_scalar_print_variable_input

    function print_variable_expression_input() result(value)
        character(len=65536) :: value

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

    function print_variable_add_input(initializer) result(value)
        integer, intent(in) :: initializer
        character(len=65536) :: value
        character(len=32) :: initializer_text

        write (initializer_text, '(i0)') initializer
        value = replace_text(print_variable_expression_input(), '(literal 23)', &
            '(literal '//trim(initializer_text)//')')
    end function print_variable_add_input

    function print_variable_generic_expression_input() result(value)
        character(len=65536) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 8) '// &
            '(instructions (instruction (id 0) (opcode const) '// &
            '(literal 3) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 2) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 3) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode add) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 5) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 7) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32)))))'
    end function print_variable_generic_expression_input

    function print_variable_generic_multiply_input() result(value)
        character(len=65536) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 11) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 3) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) '// &
            '(type i32))) (instruction (id 1) (opcode store) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 2) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode const) (literal 2) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 3) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode mul) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 5) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode const) (literal 7) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 7) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 8) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 9) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 10) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32)))))'
    end function print_variable_generic_multiply_input

    function print_variable_generic_divide_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_multiply_input()
        value = replace_text(value, '(opcode mul)', '(opcode div)')
    end function print_variable_generic_divide_input

    function print_variable_generic_subtract_input() result(value)
        character(len=65536) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 7) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 3) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) '// &
            '(type i32))) (instruction (id 1) (opcode store) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 2) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode const) (literal 2) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 3) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode sub) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 5) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) '// &
            '(type i32)))))'
    end function print_variable_generic_subtract_input

    function print_variable_generic_power_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_multiply_input()
        value = replace_text(value, '(literal 2)', '(literal 3)')
        value = replace_text(value, '(opcode mul)', '(opcode pow)')
    end function print_variable_generic_power_input

    function print_variable_generic_power_four_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_power_input()
        value = replace_text(value, '(literal 3) (source-rule frontend-ast-v2/print-stmt)', &
            '(literal 4) (source-rule frontend-ast-v2/print-stmt)')
    end function print_variable_generic_power_four_input

    function print_variable_generic_power_five_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_power_four_input()
        value = replace_text(value, '(literal 4) (source-rule frontend-ast-v2/print-stmt)', &
            '(literal 5) (source-rule frontend-ast-v2/print-stmt)')
    end function print_variable_generic_power_five_input

    function print_variable_generic_power_seven_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_power_five_input()
        value = replace_text(value, '(literal 5) (source-rule frontend-ast-v2/print-stmt)', &
            '(literal 7) (source-rule frontend-ast-v2/print-stmt)')
    end function print_variable_generic_power_seven_input

    function print_variable_generic_power_ten_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_power_seven_input()
        value = replace_text(value, '(literal 7) (source-rule frontend-ast-v2/print-stmt)', &
            '(literal 10) (source-rule frontend-ast-v2/print-stmt)')
    end function print_variable_generic_power_ten_input

    function print_variable_generic_power_two_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_multiply_input()
        value = replace_text(value, '(opcode mul)', '(opcode pow)')
    end function print_variable_generic_power_two_input

    function print_variable_generic_variable_power_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_power_input()
        value = replace_text(value, '(instruction (id 3) (opcode const) (literal 3)', &
            '(instruction (id 3) (opcode load) (storage-key x)')
    end function print_variable_generic_variable_power_input

    function print_variable_generic_variable_power_four_input() result(value)
        character(len=65536) :: value

        value = print_variable_generic_variable_power_input()
        value = replace_text(value, '(literal 3) (source-rule frontend-ast-v2/execution-part)', &
            '(literal 4) (source-rule frontend-ast-v2/execution-part)')
    end function print_variable_generic_variable_power_four_input

    function print_variable_multiply_expression_input() result(value)
        character(len=65536) :: value

        value = replace_text(print_variable_expression_input(), '(literal 1)', '(literal 2)')
        value = replace_text(value, '(opcode add)', '(opcode mul)')
    end function print_variable_multiply_expression_input

    function print_variable_subtract_expression_input() result(value)
        character(len=65536) :: value

        value = print_variable_expression_input()
        value = replace_text(value, '(literal 1)', '(literal 2)')
        value = replace_text(value, '(opcode add)', '(opcode sub)')
    end function print_variable_subtract_expression_input

    function print_variable_divide_expression_input() result(value)
        character(len=65536) :: value

        value = replace_text(print_variable_expression_input(), '(literal 23)', '(literal 24)')
        value = replace_text(value, '(literal 1)', '(literal 2)')
        value = replace_text(value, '(opcode add)', '(opcode div)')
    end function print_variable_divide_expression_input

    function print_variable_power_expression_input() result(value)
        character(len=65536) :: value

        value = print_variable_expression_input()
        value = replace_text(value, '(literal 23)', '(literal 2)')
        value = replace_text(value, '(literal 1)', '(literal 3)')
        value = replace_text(value, '(opcode add)', '(opcode pow)')
    end function print_variable_power_expression_input

    function print_variable_power_value_expression_input() result(value)
        character(len=65536) :: value

        value = print_variable_power_expression_input()
        value = replace_text(value, '(literal 2)', '(literal 99)')
        value = replace_text(value, '(literal 3)', '(literal 2)')
        value = replace_text(value, '(literal 99)', '(literal 3)')
    end function print_variable_power_value_expression_input

    function print_variable_power_two_item_input() result(value)
        character(len=65536) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 11) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 3) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) '// &
            '(type i32))) (instruction (id 1) (opcode store) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) '// &
            '(type i32))) (instruction (id 2) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode const) (literal 2) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 3) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode pow) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 5) (opcode store) (storage-key x) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 7) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 8) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 9) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 10) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32)))))'
    end function print_variable_power_two_item_input

    function print_variable_power_three_item_input() result(value)
        character(len=65536) :: value

        value = print_variable_power_two_item_input()
        value = replace_text(value, 'instruction-count 11', 'instruction-count 13')
        value = replace_text(value, '(instruction (id 10) (opcode return)', &
            '(instruction (id 10) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 11) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 12) (opcode return)')
    end function print_variable_power_three_item_input

    function print_variable_power_four_item_input() result(value)
        character(len=65536) :: value

        value = print_variable_power_three_item_input()
        value = replace_text(value, 'instruction-count 13', 'instruction-count 15')
        value = replace_text(value, '(instruction (id 12) (opcode return)', &
            '(instruction (id 12) (opcode load) (storage-key x) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 13) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 14) (opcode return)')
    end function print_variable_power_four_item_input

    function print_variable_power_many_item_input(item_count) result(value)
        character(len=65536) :: value
        character(len=32) :: old_id, new_id, new_instruction, result_id
        integer, intent(in) :: item_count
        integer :: item_index, instruction_id

        value = print_variable_power_two_item_input()
        write (new_id, '(i0)') 2 * item_count + 7
        value = replace_text(value, 'instruction-count 11', 'instruction-count '//trim(new_id))
        value = replace_text(value, '(result (id 8)', '(result (id 7)')
        value = replace_text(value, '(result (id 8)', '(result (id 7)')
        value = replace_text(value, '(result (id 8)', '(result (id 7)')
        instruction_id = 10
        do item_index = 3, item_count
            write (old_id, '(i0)') instruction_id
            write (new_id, '(i0)') instruction_id + 1
            write (new_instruction, '(i0)') instruction_id + 2
            write (result_id, '(i0)') item_index + 5
            value = replace_text(value, '(instruction (id '//trim(old_id)//') (opcode return)', &
                '(instruction (id '//trim(old_id)//') (opcode load) (storage-key x) '// &
                '(source-rule frontend-ast-v2/print-stmt) (result (id '//trim(result_id)//') '// &
                '(kind integer) (type i32))) (instruction (id '//trim(new_id)//') (opcode output) '// &
                '(source-rule frontend-ast-v2/print-stmt) (result (id '//trim(result_id)//') '// &
                '(kind integer) (type i32))) (instruction (id '//trim(new_instruction)//') '// &
                '(opcode return)')
            instruction_id = instruction_id + 2
        end do
        write (result_id, '(i0)') item_count + 5
        value = replace_text(value, '(instruction (id '//trim(new_instruction)//') (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 7)', &
            '(instruction (id '//trim(new_instruction)//') (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id '//trim(result_id)//')')
    end function print_variable_power_many_item_input

    subroutine run_print_variable_many_items(input, path, output_path, item_count)
        character(len=*), intent(in) :: input, path, output_path
        integer, intent(in) :: item_count
        type(riscv_linux_artifact_t) :: artifact
        character(len=256) :: diagnostic
        integer(int8), allocatable :: output(:)
        integer(int32) :: status
        integer :: command_status, exit_status, io_status, unit, item_index

        allocate (output(2 * item_count))
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, &
            'many-item stored-variable PRINT MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_status(status, mir_v0_bridge_ok, 'many-item stored-variable PRINT ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_int(command_status, 0, 'many-item stored-variable PRINT chmod failed')
        call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_int(command_status, 0, 'many-item stored-variable PRINT qemu command failed')
        call assert_int(exit_status, 0, 'many-item stored-variable PRINT artifact did not exit successfully')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_int(io_status, 0, 'many-item stored-variable PRINT output was not written')
        read (unit, iostat=io_status) output
        call assert_int(io_status, 0, 'many-item stored-variable PRINT output length changed')
        do item_index = 1, item_count
            call assert_byte(output(2 * item_index - 1), 57, 'many-item PRINT missed value 9')
            call assert_byte(output(2 * item_index), 10, 'many-item PRINT missed newline')
        end do
        read (unit, iostat=io_status) output(1)
        call assert_true(io_status /= 0, 'many-item stored-variable PRINT wrote extra bytes')
        close (unit, status='delete', iostat=io_status)
        call assert_int(io_status, 0, 'many-item stored-variable PRINT output cleanup failed')
        deallocate (output)
    end subroutine run_print_variable_many_items

    function replace_text(value, old, new) result(replaced)
        character(len=*), intent(in) :: value, old, new
        character(len=65536) :: replaced
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
