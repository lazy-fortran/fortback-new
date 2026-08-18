program test_mir_v0_storage_sequence
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    use fortback_mir_v0_riscv_linux_bridge_policy, only: &
        mir_v0_bridge_policy_frame_size, mir_v0_bridge_policy_load_operation, &
        mir_v0_bridge_policy_route_operation_for, mir_v0_bridge_policy_storage_offset, &
        mir_v0_bridge_policy_store_operation
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=8192) :: input
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer :: command_status, exit_status, index
    character(len=*), parameter :: path = '/tmp/fortback-mir-v0-storage-sequence.elf'

    call assert_equal(mir_v0_bridge_policy_frame_size, 16_int32, 'frame policy changed')
    call assert_equal(mir_v0_bridge_policy_storage_offset, 0_int32, 'slot policy changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_load_operation), 'ld', 'load policy changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_store_operation), 'sd', 'store policy changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence', 1_int32)), 'sd', 'store route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence', 2_int32)), 'ld', 'load route changed')

    input = sequence_input('x', 'x', 2, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'storage sequence was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'frame encoding changed')
    call assert_word(artifact%bytes, 181, [19, 5, 112, 0], 'const 7 encoding changed')
    call assert_word(artifact%bytes, 185, [35, 48, 161, 0], 'store offset changed')
    call assert_word(artifact%bytes, 189, [3, 53, 1, 0], 'load offset changed')
    call assert_word(artifact%bytes, 201, [35, 48, 161, 0], 'result store offset changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'storage sequence ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'storage sequence chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'storage sequence qemu command failed')
    call assert_equal(exit_status, 8, 'storage sequence did not return 8')

    call compile_mir_v0_riscv_linux(sequence_input('x', 'x', 2, .true.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_input('y', 'y', 2, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong storage key was accepted')
    call compile_mir_v0_riscv_linux(sequence_input('x', 'x', 1, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong result was accepted')
    write (*, '(a)') 'MIR-v0 seven-instruction storage sequence: ok'

contains

    function sequence_input(load_key, store_key, return_id, wrong_order) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=8192) :: value
        character(len=16) :: first_opcode, second_opcode, operation_opcode

        first_opcode = 'const'
        second_opcode = 'store'
        operation_opcode = 'add'
        if (wrong_order) then
            operation_opcode = 'store'
        end if
        value = '(mir-function (name main) (entry-block 0) (instruction-count 7) '// &
            '(instructions (instruction (id 0) (opcode '//trim(first_opcode)//') '// &
            '(literal 7) (source-rule frontend-ast-v1/storage-sequence) '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode '//trim(second_opcode)//') (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key '//trim(load_key)//') (source-rule frontend-ast-v1/storage-sequence) '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 3) '// &
            '(opcode const) (literal 1) (source-rule frontend-ast-v1/storage-sequence) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 4) '// &
            '(opcode '//trim(operation_opcode)//') (source-rule frontend-ast-v1/storage-sequence) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 5) '// &
            '(opcode store) (storage-key '//trim(store_key)//') (source-rule '// &
            'frontend-ast-v1/storage-sequence) (result (id 2) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode return) (source-rule '// &
            'frontend-ast-v1/storage-sequence) (result (id '//int_text(return_id)//') '// &
            '(kind integer) (type i32)))))'
    end function sequence_input

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text

        write (text, '(i0)') value
    end function int_text

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

    subroutine assert_equal_text(actual, expected, message)
        character(len=*), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (trim(actual) /= trim(expected)) error stop message
    end subroutine assert_equal_text

    subroutine assert_equal(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

end program test_mir_v0_storage_sequence
