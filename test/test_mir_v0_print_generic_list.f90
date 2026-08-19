program test_mir_v0_print_generic_list
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, riscv_linux_artifact_t, write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=65536) :: input
    character(len=256) :: diagnostic
    integer(int8) :: output(31)
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit, index
    character(len=*), parameter :: path = '/tmp/fortback-print-generic-list.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-print-generic-list.out'
    character(len=*), parameter :: expected = &
        '0'//achar(10)//'20'//achar(10)//'21'//achar(10)//'22'//achar(10)// &
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
    do index = 1, size(output)
        call assert_byte(output(index), iachar(expected(index:index)), &
            'generic literal PRINT output mismatch')
    end do
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'generic literal PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'generic literal PRINT output cleanup failed')
    write (*, '(a)') 'MIR-v0 generic literal PRINT QEMU check: ok'

contains

    function generic_literal_input() result(value)
        character(len=65536) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 21) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 0) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) '// &
            '(opcode load) (storage-key x) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 3) '// &
            '(opcode output) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 2) (kind integer) (type i32))) '// &
            literal_output(4, 20)//literal_output(6, 21)//literal_output(8, 22)// &
            literal_output(10, 100)//literal_output(12, 200)//literal_output(14, 300)// &
            literal_output(16, 400)//literal_output(18, 500)// &
            '(instruction (id 20) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 18) '// &
            '(kind integer) (type i32)))))'
    end function generic_literal_input

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
