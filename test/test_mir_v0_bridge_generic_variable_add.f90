program test_mir_v0_bridge_generic_variable_add
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, &
        riscv_linux_artifact_t, write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=8192) :: input, mutated
    character(len=256) :: diagnostic
    integer(int8) :: bytes(16)
    character(len=3) :: output
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit
    character(len=*), parameter :: elf_path = &
        '/tmp/fortback-mir-v0-generic-variable-add.elf'
    character(len=*), parameter :: output_path = &
        '/tmp/fortback-mir-v0-generic-variable-add.out'

    input = initialized_variable_add_input(42)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'initialized x+x was rejected')
    call assert_word(artifact%bytes, 185, [35, 48, 161, 0], 'x store encoding changed')
    call assert_word(artifact%bytes, 189, [3, 53, 1, 0], 'first x load encoding changed')
    call assert_word(artifact%bytes, 193, [131, 53, 1, 0], 'second x load encoding changed')
    call assert_word(artifact%bytes, 197, [51, 5, 181, 0], 'x+x add encoding changed')

    call write_mir_v0_riscv_linux(input, elf_path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'initialized x+x ELF write failed')
    call execute_command_line('chmod 755 -- '//elf_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_equal(command_status, 0, 'initialized x+x chmod failed')
    call assert_equal(exit_status, 0, 'initialized x+x chmod returned failure')
    call execute_command_line('qemu-riscv64 '//elf_path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_equal(command_status, 0, 'initialized x+x QEMU command failed')
    call assert_equal(exit_status, 0, 'initialized x+x QEMU returned failure')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_equal(io_status, 0, 'initialized x+x output was not written')
    read (unit, iostat=io_status) output
    call assert_equal(io_status, 0, 'initialized x+x output read failed')
    call assert_true(output == '84'//achar(10), 'initialized x+x value changed')
    read (unit, iostat=io_status) bytes(1:1)
    call assert_true(io_status /= 0, 'initialized x+x wrote extra output')
    close (unit, status='delete', iostat=io_status)
    call assert_equal(io_status, 0, 'initialized x+x output cleanup failed')

    mutated = input
    call replace_token(mutated, 'storage-key x', 'storage-key y')
    call assert_rejected(mutated, mir_v0_bridge_out_of_scope, &
        'wrong initialized x+x storage key was accepted')
    mutated = input(:len_trim(input) - 1)
    call assert_rejected(mutated, mir_v0_bridge_malformed, &
        'malformed initialized x+x was accepted')
    write (*, '(a)') 'MIR-v0 generic initialized variable x+x checks: ok'

contains

    function initialized_variable_add_input(initializer) result(value)
        integer, intent(in) :: initializer
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 9) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '// &
            int_text(initializer)//') (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode store) (storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) '// &
            '(opcode load) (storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 3) '// &
            '(opcode load) (storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 3) (kind integer) (type i32))) (instruction (id 4) '// &
            '(opcode add) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 5) '// &
            '(opcode store) (storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 6) '// &
            '(opcode load) (storage-key x) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 7) '// &
            '(opcode output) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 8) '// &
            '(opcode return) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32)))))'
    end function initialized_variable_add_input

    function int_text(number) result(value)
        integer, intent(in) :: number
        character(len=32) :: value

        write (value, '(i0)') number
    end function int_text

    subroutine assert_rejected(value, expected, message)
        character(len=*), intent(in) :: value, message
        integer(int32), intent(in) :: expected

        call compile_mir_v0_riscv_linux(value, artifact, status, diagnostic)
        call assert_equal(status, expected, message)
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

    subroutine assert_word(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected(4)
        character(len=*), intent(in) :: message

        call assert_byte(bytes(index), expected(1), 1, message)
        call assert_byte(bytes(index + 1), expected(2), 2, message)
        call assert_byte(bytes(index + 2), expected(3), 3, message)
        call assert_byte(bytes(index + 3), expected(4), 4, message)
    end subroutine assert_word

    subroutine assert_byte(actual, expected, byte_index, message)
        integer(int8), intent(in) :: actual
        integer, intent(in) :: expected, byte_index
        character(len=*), intent(in) :: message

        if (iand(int(actual, int32), 255_int32) /= expected) then
            error stop trim(message)//' byte '//int_text(byte_index)
        end if
    end subroutine assert_byte

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

end program test_mir_v0_bridge_generic_variable_add
