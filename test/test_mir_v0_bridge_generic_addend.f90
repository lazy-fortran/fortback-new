program test_mir_v0_bridge_generic_addend
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=8192) :: input, mutated
    character(len=256) :: diagnostic
    integer(int8) :: output(16)
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit
    character(len=*), parameter :: elf_path = '/tmp/fortback-mir-v0-generic-addend.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-mir-v0-generic-addend.out'

    call assert_qemu(42, 2, '44'//achar(10))
    call assert_qemu(-42, 10, '-32'//achar(10))

    input = initialized_add_input(42, 0)
    call assert_rejected(input, 'addend zero was accepted')
    input = initialized_add_input(42, 11)
    call assert_rejected(input, 'addend eleven was accepted')

    input = initialized_add_input(42, 2)
    mutated = input
    mutated(index(mutated, 'opcode store'):index(mutated, 'opcode store') + 11) = 'opcode add  '
    call assert_rejected(mutated, 'wrong storage opcode was accepted')
    mutated = input
    mutated(index(mutated, 'storage-key x'):index(mutated, 'storage-key x') + 12) = 'storage-key y'
    call assert_rejected(mutated, 'wrong storage key was accepted')
    write (*, '(a)') 'MIR-v0 generic initialized addend QEMU checks: ok'

contains

    subroutine assert_qemu(left, addend, expected)
        integer, intent(in) :: left, addend
        character(len=*), intent(in) :: expected
        character(len=16) :: bytes
        integer :: index

        input = initialized_add_input(left, addend)
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'accepted initialized add was rejected')
        call write_mir_v0_riscv_linux(input, elf_path, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'initialized add ELF write failed')
        call execute_command_line('chmod 755 -- '//elf_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized add chmod failed')
        call assert_equal(exit_status, 0, 'initialized add chmod returned failure')
        call execute_command_line('qemu-riscv64 '//elf_path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized add QEMU command failed')
        call assert_equal(exit_status, 0, 'initialized add QEMU returned failure')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized add output was not written')
        read (unit, iostat=io_status) bytes(1:len(expected))
        call assert_equal(io_status, 0, 'initialized add output read failed')
        do index = 1, len(expected)
            call assert_byte(bytes(index:index), expected(index:index), 'initialized add output changed')
        end do
        read (unit, iostat=io_status) bytes(1:1)
        call assert_true(io_status /= 0, 'initialized add wrote extra output')
        close (unit, status='delete', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized add output cleanup failed')
    end subroutine assert_qemu

    subroutine assert_rejected(value, message)
        character(len=*), intent(in) :: value, message

        call compile_mir_v0_riscv_linux(value, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_out_of_scope, message)
    end subroutine assert_rejected

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
            '(storage-key x) (source-rule frontend-ast-v2/print-stmt) (result (id 4) (kind integer) (type i32))) '// &
            '(instruction (id 6) (opcode load) (storage-key x) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 7) (opcode output) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32))) '// &
            '(instruction (id 8) (opcode return) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32)))))'
    end function initialized_add_input

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

end program test_mir_v0_bridge_generic_addend
