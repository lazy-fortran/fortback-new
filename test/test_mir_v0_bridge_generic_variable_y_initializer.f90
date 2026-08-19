program test_mir_v0_bridge_generic_variable_y_initializer
    use iso_fortran_env, only: int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=8192) :: input, mutated
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit
    character(len=*), parameter :: elf_path = &
        '/tmp/fortback-mir-v0-generic-variable-y-initializer.elf'
    character(len=*), parameter :: output_path = &
        '/tmp/fortback-mir-v0-generic-variable-y-initializer.out'

    call assert_qemu(3, '3'//achar(10))
    call assert_qemu(-4, '-4'//achar(10))

    input = initialized_variable_y_input(3)
    mutated = input
    call replace_token(mutated, '(storage-key y)', '(storage-key x)')
    call assert_rejected(mutated, 'wrong initialized y storage key was accepted')
    mutated = input
    call replace_token(mutated, '(name main)', '(name test)')
    call assert_rejected(mutated, 'wrong initialized y function name was accepted')

    write (*, '(a)') 'MIR-v0 generic initialized variable y checks: ok'

contains

    subroutine assert_qemu(initializer, expected)
        integer, intent(in) :: initializer
        character(len=*), intent(in) :: expected
        character(len=16) :: output
        integer :: byte_index

        input = initialized_variable_y_input(initializer)
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, &
            'initialized y MIR was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, elf_path, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'initialized y ELF write failed')
        call execute_command_line('chmod 755 -- '//elf_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized y chmod command failed')
        call assert_equal(exit_status, 0, 'initialized y chmod failed')
        call execute_command_line('qemu-riscv64 '//elf_path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized y QEMU command failed')
        call assert_equal(exit_status, 0, 'initialized y QEMU returned failure')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized y output was not written')
        read (unit, iostat=io_status) output(1:len(expected))
        call assert_equal(io_status, 0, 'initialized y output read failed')
        do byte_index = 1, len(expected)
            call assert_true(output(byte_index:byte_index) == expected(byte_index:byte_index), &
                'initialized y output value changed')
        end do
        read (unit, iostat=io_status) output(1:1)
        call assert_true(io_status /= 0, 'initialized y wrote extra output')
        close (unit, status='delete', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized y output cleanup failed')
    end subroutine assert_qemu

    function initialized_variable_y_input(initializer) result(value)
        integer, intent(in) :: initializer
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 5) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '// &
            int_text(initializer)//') (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode store) (storage-key y) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) '// &
            '(opcode load) (storage-key y) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 3) '// &
            '(opcode output) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 4) '// &
            '(opcode return) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 1) (kind integer) (type i32)))))'
    end function initialized_variable_y_input

    function int_text(number) result(value)
        integer, intent(in) :: number
        character(len=32) :: value

        write (value, '(i0)') number
    end function int_text

    subroutine assert_rejected(value, message)
        character(len=*), intent(in) :: value, message

        call compile_mir_v0_riscv_linux(value, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_out_of_scope, message)
        call assert_true(len_trim(diagnostic) > 0, 'rejected initialized y returned no diagnostic')
    end subroutine assert_rejected

    subroutine replace_token(value, old_token, new_token)
        character(len=*), intent(inout) :: value
        character(len=*), intent(in) :: old_token, new_token
        integer :: offset

        call assert_true(len_trim(new_token) <= len_trim(old_token), &
            'test mutation replacement token is too long')
        offset = index(value, trim(old_token))
        call assert_true(offset > 0, 'test mutation token was not found')
        value(offset:offset + len_trim(old_token) - 1) = new_token
    end subroutine replace_token

    subroutine assert_equal(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_bridge_generic_variable_y_initializer
