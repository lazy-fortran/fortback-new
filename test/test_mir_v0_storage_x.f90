program test_mir_v0_storage_x
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=4096) :: input, wrong_storage
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer :: command_status, exit_status
    character(len=*), parameter :: path = '/tmp/fortback-mir-v0-storage-x.elf'

    input = storage_input('x', 'x')
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'storage MIR was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'frame setup changed')
    call assert_word(artifact%bytes, 181, [35, 48, 1, 0], 'initialization slot changed')
    call assert_word(artifact%bytes, 185, [3, 53, 1, 0], 'load slot changed')
    call assert_word(artifact%bytes, 197, [35, 48, 161, 0], 'store slot changed')
    call assert_word(artifact%bytes, 201, [147, 8, 208, 5], 'exit setup changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'storage ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'storage chmod command failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'storage qemu command failed')
    call assert_equal(exit_status, 1, 'storage route did not return one')

    wrong_storage = storage_input('y', 'x')
    call compile_mir_v0_riscv_linux(wrong_storage, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'mismatched storage key was accepted')
    write (*, '(a)') 'MIR-v0 storage x stack-slot and key checks: ok'

contains
    function storage_input(load_key, store_key) result(value)
        character(len=*), intent(in) :: load_key, store_key
        character(len=4096) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 5) '// &
            '(instructions (instruction (id 0) (opcode load) '// &
            '(source-rule frontend-ast-v1/expression) (storage '//trim(load_key)//') '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode const) (literal 1) (source-rule frontend-ast-v1/expression) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) (opcode add) '// &
            '(source-rule frontend-ast-v1/expression) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 3) (opcode store) '// &
            '(source-rule frontend-ast-v1/expression) (storage '//trim(store_key)//') '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 4) (opcode return) '// &
            '(source-rule frontend-ast-v1/expression) (result (id 2) (kind integer) '// &
            '(type i32)))))'
    end function storage_input

    subroutine assert_word(bytes, first, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: first, expected(4)
        character(len=*), intent(in) :: message
        integer :: index

        do index = 1, 4
            if (iand(int(bytes(first + index - 1), int32), 255_int32) /= expected(index)) &
                error stop message
        end do
    end subroutine assert_word

    subroutine assert_equal(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        if (actual /= expected) error stop message
    end subroutine assert_equal
end program test_mir_v0_storage_x
