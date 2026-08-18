program test_mir_v0_bridge_materialized_add
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, &
        riscv_linux_artifact_t, write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=4096) :: input
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer :: command_status, exit_status
    character(len=*), parameter :: path = '/tmp/fortback-mir-v0-materialized-add.elf'

    input = materialized_add_input(1, 2, 2)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'materialized add MIR was rejected')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'materialized add ELF write failed')
    call assert_byte(artifact%bytes, 177, 19, 'left literal addi encoding changed')
    call assert_byte(artifact%bytes, 178, 5, 'left literal addi encoding changed')
    call assert_byte(artifact%bytes, 179, 16, 'left literal addi encoding changed')
    call assert_byte(artifact%bytes, 180, 0, 'left literal addi encoding changed')
    call assert_byte(artifact%bytes, 181, 147, 'right literal addi encoding changed')
    call assert_byte(artifact%bytes, 182, 5, 'right literal addi encoding changed')
    call assert_byte(artifact%bytes, 183, 32, 'right literal addi encoding changed')
    call assert_byte(artifact%bytes, 184, 0, 'right literal addi encoding changed')
    call assert_byte(artifact%bytes, 185, 51, 'add encoding changed')
    call assert_byte(artifact%bytes, 186, 5, 'add encoding changed')
    call assert_byte(artifact%bytes, 187, 181, 'add encoding changed')
    call assert_byte(artifact%bytes, 188, 0, 'add encoding changed')
    call assert_byte(artifact%bytes, 189, 147, 'exit setup encoding changed')
    call assert_byte(artifact%bytes, 190, 8, 'exit setup encoding changed')
    call assert_byte(artifact%bytes, 191, 208, 'exit setup encoding changed')
    call assert_byte(artifact%bytes, 192, 5, 'exit setup encoding changed')
    call assert_byte(artifact%bytes, 193, 115, 'ecall encoding changed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_equal(command_status, 0, 'materialized add chmod command failed')
    call assert_equal(exit_status, 0, 'materialized add chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_equal(command_status, 0, 'materialized add qemu command failed')
    call assert_equal(exit_status, 3, 'materialized add did not return 1 + 2')

    call compile_mir_v0_riscv_linux(materialized_add_input(3, 2, 2), artifact, status, &
        diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'disallowed literal was accepted')
    call compile_mir_v0_riscv_linux(materialized_add_input(1, 2, 1), artifact, status, &
        diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong result shape was accepted')
    call compile_mir_v0_riscv_linux(input(:len_trim(input) - 1), artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_malformed, 'malformed materialized add was accepted')
    write (*, '(a)') 'MIR-v0 materialized integer add encoding and qemu checks: ok'

contains

    function materialized_add_input(left, right, add_result_id) result(value)
        integer, intent(in) :: left, right, add_result_id
        character(len=4096) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 5) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '// &
            int_text(left)//') (source-rule frontend-ast-v1/expression) '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode const) (literal '//int_text(right)//')'// &
            ' (source-rule frontend-ast-v1/expression) (result (id 1) '// &
            '(kind integer) (type i32))) (instruction (id 2) (opcode add) '// &
            '(source-rule frontend-ast-v1/expression) (result (id '// &
            int_text(add_result_id)//') (kind integer) (type i32))) '// &
            '(instruction (id 3) (opcode store) (source-rule '// &
            'frontend-ast-v1/expression) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 4) (opcode return) '// &
            '(source-rule frontend-ast-v1/expression) (result (id 2) '// &
            '(kind integer) (type i32)))))'
    end function materialized_add_input

    function int_text(number) result(value)
        integer, intent(in) :: number
        character(len=16) :: value

        write (value, '(i0)') number
    end function int_text

    subroutine assert_byte(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected
        character(len=*), intent(in) :: message

        if (iand(int(bytes(index), int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_equal(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

end program test_mir_v0_bridge_materialized_add
