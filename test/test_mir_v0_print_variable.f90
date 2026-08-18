program test_mir_v0_print_variable
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=4096) :: input, wrong_storage, wrong_output, wrong_literal
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
