program test_mir_v0_bridge_generic_subtrahend
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
    character(len=*), parameter :: elf_path = '/tmp/fortback-mir-v0-generic-subtrahend.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-mir-v0-generic-subtrahend.out'

    call assert_qemu(42, 1, '41'//achar(10))

    call assert_qemu(42, 2, '40'//achar(10))
    call assert_qemu(-42, 10, '-52'//achar(10))

    call assert_mul_qemu('84'//achar(10))

    input = initialized_sub_input(42, 0)
    call assert_rejected(input, 'sub literal below the accepted bound was accepted')
    input = initialized_sub_input(42, 11)
    call assert_rejected(input, 'sub literal above the accepted bound was accepted')

    input = initialized_sub_input(42, 1)
    mutated = input
    call replace_token(mutated, 'opcode sub', 'opcode pow')
    call assert_rejected(mutated, 'wrong subtraction opcode mutation was accepted')
    mutated = input
    call replace_token(mutated, 'opcode store', 'opcode add')
    call assert_rejected(mutated, 'wrong subtraction storage opcode was accepted')
    mutated = input
    call replace_token(mutated, 'storage-key counter_2', 'storage-key x')
    call assert_rejected(mutated, 'mismatched counter_2 storage key was accepted')

    input = initialized_add_input(42, 0)
    call assert_rejected(input, 'add literal below the accepted bound was accepted')
    input = initialized_add_input(42, 11)
    call assert_rejected(input, 'add literal above the accepted bound was accepted')

    input = initialized_add_input(42, 2)
    mutated = input
    call replace_token(mutated, 'opcode add', 'opcode mul')
    call assert_rejected(mutated, 'unsupported add opcode mutation was accepted')
    mutated = input
    call replace_token(mutated, 'opcode store', 'opcode add')
    call assert_rejected(mutated, 'add storage opcode mutation was accepted')
    mutated = input
    call replace_token(mutated, 'storage-key x', 'storage-key y')
    call assert_rejected(mutated, 'add storage key mutation was accepted')

    input = initialized_mul_counter2_input()
    mutated = input
    call replace_token(mutated, 'opcode store', 'opcode add')
    call assert_rejected(mutated, 'wrong multiplication store opcode mutation was accepted')
    mutated = input
    call replace_token(mutated, 'storage-key counter_2', 'storage-key x')
    call assert_rejected(mutated, 'multiplication storage key mutation was accepted')
    mutated = input
    call replace_token(mutated, 'literal 2', 'literal 0')
    call assert_rejected(mutated, 'multiplication literal mutation was accepted')
    write (*, '(a)') 'MIR-v0 generic initialized subtrahend QEMU checks: ok'

contains

    subroutine assert_qemu(left, subtrahend, expected)
        integer, intent(in) :: left, subtrahend
        character(len=*), intent(in) :: expected
        character(len=16) :: bytes
        integer :: byte_index

        input = initialized_sub_input(left, subtrahend)
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, &
            'accepted initialized sub was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, elf_path, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'initialized sub ELF write failed')
        call execute_command_line('chmod 755 -- '//elf_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized sub chmod failed')
        call assert_equal(exit_status, 0, 'initialized sub chmod returned failure')
        call execute_command_line('qemu-riscv64 '//elf_path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized sub QEMU command failed')
        call assert_equal(exit_status, 0, 'initialized sub QEMU returned failure')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized sub output was not written')
        read (unit, iostat=io_status) bytes(1:len(expected))
        call assert_equal(io_status, 0, 'initialized sub output read failed')
        do byte_index = 1, len(expected)
            call assert_byte(bytes(byte_index:byte_index), expected(byte_index:byte_index), &
                'initialized sub output changed')
        end do
        read (unit, iostat=io_status) bytes(1:1)
        call assert_true(io_status /= 0, 'initialized sub wrote extra output')
        close (unit, status='delete', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized sub output cleanup failed')
    end subroutine assert_qemu

    subroutine assert_mul_qemu(expected)
        character(len=*), intent(in) :: expected
        character(len=16) :: bytes
        integer :: byte_index

        input = initialized_mul_counter2_input()
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, &
            'initialized counter_2 multiplication was rejected: '//trim(diagnostic))
        call write_mir_v0_riscv_linux(input, elf_path, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'initialized counter_2 multiplication ELF write failed')
        call execute_command_line('chmod 755 -- '//elf_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized counter_2 multiplication chmod failed')
        call assert_equal(exit_status, 0, 'initialized counter_2 multiplication chmod returned failure')
        call execute_command_line('qemu-riscv64 '//elf_path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized counter_2 multiplication QEMU command failed')
        call assert_equal(exit_status, 0, 'initialized counter_2 multiplication QEMU returned failure')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized counter_2 multiplication output was not written')
        read (unit, iostat=io_status) bytes(1:len(expected))
        call assert_equal(io_status, 0, 'initialized counter_2 multiplication output read failed')
        do byte_index = 1, len(expected)
            call assert_byte(bytes(byte_index:byte_index), expected(byte_index:byte_index), &
                'initialized counter_2 multiplication output changed')
        end do
        read (unit, iostat=io_status) bytes(1:1)
        call assert_true(io_status /= 0, 'initialized counter_2 multiplication wrote extra output')
        close (unit, status='delete', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized counter_2 multiplication output cleanup failed')
    end subroutine assert_mul_qemu

    subroutine assert_rejected(value, message)
        character(len=*), intent(in) :: value, message

        call compile_mir_v0_riscv_linux(value, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_out_of_scope, message)
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

    function initialized_sub_input(left, subtrahend) result(value)
        integer, intent(in) :: left, subtrahend
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 9) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '//int_text(left)//') '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) (type i32))) '// &
            '(instruction (id 1) (opcode store) (storage-key counter_2) '// &
            '(source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key counter_2) (source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) (type i32))) '// &
            '(instruction (id 3) (opcode const) (literal '//int_text(subtrahend)//') '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 3) (kind integer) (type i32))) '// &
            '(instruction (id 4) (opcode sub) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 5) (opcode store) '// &
            '(storage-key counter_2) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 6) (opcode load) '// &
            '(storage-key counter_2) (source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32))) '// &
            '(instruction (id 7) (opcode output) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 8) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32)))))'
    end function initialized_sub_input

    function initialized_add_input(left, addend) result(value)
        integer, intent(in) :: left, addend
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 9) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '//int_text(left)//') '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) (type i32))) '// &
            '(instruction (id 1) (opcode store) (storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key x) (source-rule frontend-ast-v2/print-stmt) (result (id 2) (kind integer) (type i32))) '// &
            '(instruction (id 3) (opcode const) (literal '//int_text(addend)//') '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 3) (kind integer) (type i32))) '// &
            '(instruction (id 4) (opcode add) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 5) (opcode store) '// &
            '(storage-key x) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 6) (opcode load) '// &
            '(storage-key x) (source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32))) '// &
            '(instruction (id 7) (opcode output) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 8) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32)))))'
    end function initialized_add_input

    function initialized_mul_counter2_input() result(value)
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 9) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 42) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) (type i32))) '// &
            '(instruction (id 1) (opcode store) (storage-key counter_2) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 1) (kind integer) (type i32))) '// &
            '(instruction (id 2) (opcode load) (storage-key counter_2) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 2) (kind integer) (type i32))) '// &
            '(instruction (id 3) (opcode const) (literal 2) '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 3) (kind integer) (type i32))) '// &
            '(instruction (id 4) (opcode mul) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 5) (opcode store) '// &
            '(storage-key counter_2) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 6) (opcode load) '// &
            '(storage-key counter_2) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 7) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32))) '// &
            '(instruction (id 8) (opcode return) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32)))))'
    end function initialized_mul_counter2_input

    function int_text(number) result(value)
        integer, intent(in) :: number
        character(len=32) :: value

        write (value, '(i0)') number
    end function int_text

    subroutine assert_equal(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_byte(actual, expected, message)
        character(len=1), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_bridge_generic_subtrahend
