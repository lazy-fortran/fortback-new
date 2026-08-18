program test_mir_v0_storage_sequence
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    use fortback_mir_v0_riscv_linux_bridge_policy, only: &
        mir_v0_bridge_policy_frame_size, mir_v0_bridge_policy_load_operation, &
        mir_v0_bridge_policy_instruction_count_for, &
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
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
        'frontend-ast-v2/execution-part'), 7_int32, 'execution-part route count changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_load_operation), 'ld', 'load policy changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_store_operation), 'sd', 'store policy changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence', 1_int32)), 'sd', 'store route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence', 2_int32)), 'ld', 'load route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 0_int32)), 'addi', 'three-step const route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 1_int32)), 'sd', 'three-step store route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 2_int32)), 'ld', 'three-step load route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 3_int32)), 'addi', 'three-step const route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 4_int32)), 'add', 'three-step add route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 5_int32)), 'sd', 'three-step store route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 6_int32)), 'ld', 'three-step load route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 7_int32)), 'addi', 'three-step const route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 8_int32)), 'add', 'three-step add route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 9_int32)), 'sd', 'three-step store route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-3', 10_int32)), 'addi', 'three-step return route changed')
    call assert_sequence_route('frontend-ast-v2/execution-part')
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
        'frontend-ast-v1/storage-sequence-4'), 15_int32, 'four-step route count changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-4', 10_int32)), 'ld', 'four-step load route changed')

    input = sequence_input('x', 'x', 4, .false., 'frontend-ast-v1/storage-sequence')
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

    input = sequence_input('x', 'x', 4, .false., 'frontend-ast-v2/execution-part')
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'execution-part storage sequence was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'execution-part frame encoding changed')
    call assert_word(artifact%bytes, 181, [19, 5, 112, 0], 'execution-part const 7 encoding changed')
    call assert_word(artifact%bytes, 185, [35, 48, 161, 0], 'execution-part store encoding changed')
    call assert_word(artifact%bytes, 189, [3, 53, 1, 0], 'execution-part load encoding changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'execution-part ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'execution-part chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'execution-part qemu command failed')
    call assert_equal(exit_status, 8, 'execution-part route did not return 8')

    call compile_mir_v0_riscv_linux(sequence_input('x', 'x', 8, .true., &
        'frontend-ast-v2/execution-part'), artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'execution-part wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_input('y', 'x', 8, .false., &
        'frontend-ast-v2/execution-part'), artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, &
        'execution-part wrong storage key was accepted')

    call compile_mir_v0_riscv_linux(sequence_input('x', 'x', 2, .true., &
        'frontend-ast-v1/storage-sequence'), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_input('y', 'y', 2, .false., &
        'frontend-ast-v1/storage-sequence'), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong storage key was accepted')
    call compile_mir_v0_riscv_linux(sequence_input('x', 'x', 1, .false., &
        'frontend-ast-v1/storage-sequence'), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong result was accepted')

    input = sequence_three_input('x', 'x', 8, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'three-step storage sequence was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'three-step frame encoding changed')
    call assert_word(artifact%bytes, 181, [19, 5, 112, 0], 'three-step const 7 encoding changed')
    call assert_word(artifact%bytes, 185, [35, 48, 161, 0], 'three-step first store encoding changed')
    call assert_word(artifact%bytes, 189, [3, 53, 1, 0], 'three-step first load encoding changed')
    call assert_word(artifact%bytes, 193, [147, 5, 16, 0], 'three-step first increment encoding changed')
    call assert_word(artifact%bytes, 197, [51, 5, 181, 0], 'three-step first add encoding changed')
    call assert_word(artifact%bytes, 201, [35, 48, 161, 0], 'three-step second store encoding changed')
    call assert_word(artifact%bytes, 205, [3, 53, 1, 0], 'three-step second load encoding changed')
    call assert_word(artifact%bytes, 209, [147, 5, 16, 0], 'three-step second increment encoding changed')
    call assert_word(artifact%bytes, 213, [51, 5, 181, 0], 'three-step second add encoding changed')
    call assert_word(artifact%bytes, 217, [35, 48, 161, 0], 'three-step third store encoding changed')
    call assert_word(artifact%bytes, 221, [147, 8, 208, 5], 'three-step exit encoding changed')
    call assert_word(artifact%bytes, 225, [115, 0, 0, 0], 'three-step ecall encoding changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'three-step storage ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'three-step storage chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'three-step storage qemu command failed')
    call assert_equal(exit_status, 9, 'three-step storage sequence did not return 9')

    input = sequence_four_input('x', 'x', 12, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'four-step storage sequence was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'four-step frame encoding changed')
    call assert_word(artifact%bytes, 193, [147, 5, 16, 0], 'four-step first increment changed')
    call assert_word(artifact%bytes, 209, [147, 5, 16, 0], 'four-step second increment changed')
    call assert_word(artifact%bytes, 225, [147, 5, 16, 0], 'four-step third increment changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'four-step storage ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'four-step storage chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'four-step qemu command failed')
    call assert_equal(exit_status, 10, 'four-step storage sequence did not return 10')

    call compile_mir_v0_riscv_linux(sequence_three_input('x', 'x', 8, .true.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'three-step wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_three_input('y', 'x', 8, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'three-step wrong storage key was accepted')
    call compile_mir_v0_riscv_linux(sequence_three_input('x', 'x', 10, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'three-step wrong result was accepted')

    input = sequence_four_input('x', 'x', 12, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'four-step storage sequence was rejected')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'four-step storage ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'four-step storage chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'four-step storage qemu command failed')
    call assert_equal(exit_status, 10, 'four-step storage sequence did not return 10')

    call compile_mir_v0_riscv_linux(sequence_four_input('x', 'x', 10, .true.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'four-step wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_four_input('y', 'x', 10, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'four-step wrong storage key was accepted')
    call compile_mir_v0_riscv_linux(sequence_four_input('x', 'x', 9, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'four-step wrong result was accepted')
    write (*, '(a)') 'MIR-v0 storage sequence routes: ok'

contains

    subroutine assert_sequence_route(source_rule)
        character(len=*), intent(in) :: source_rule
        character(len=16), parameter :: expected(7) = [character(len=16) :: &
            'addi', 'sd', 'ld', 'addi', 'add', 'sd', 'addi']
        integer :: index

        do index = 0, 6
            call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
                source_rule, int(index, int32))), trim(expected(index + 1)), &
                'execution-part route operation changed')
        end do
    end subroutine assert_sequence_route

    function sequence_input(load_key, store_key, return_id, wrong_order, source_rule) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=*), intent(in) :: source_rule
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
            '(literal 7) (source-rule '//trim(source_rule)//') '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode '//trim(second_opcode)//') (storage-key '//trim(store_key)//') '// &
            '(source-rule '//trim(source_rule)//') (result (id 1) '// &
            '(kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key '//trim(load_key)//') (source-rule '//trim(source_rule)//') '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 3) '// &
            '(opcode const) (literal 1) (source-rule '//trim(source_rule)//') '// &
            '(result (id 3) (kind integer) (type i32))) (instruction (id 4) '// &
            '(opcode '//trim(operation_opcode)//') (source-rule '//trim(source_rule)//') '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 5) '// &
            '(opcode store) (storage-key '//trim(store_key)//') (source-rule '// &
            trim(source_rule)//') (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode return) (source-rule '// &
            trim(source_rule)//') (result (id '//int_text(return_id)//') '// &
            '(kind integer) (type i32)))))'
    end function sequence_input

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text

        write (text, '(i0)') value
    end function int_text

    function sequence_three_input(load_key, store_key, return_id, wrong_order) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=8192) :: value
        character(len=16) :: operation_opcode

        operation_opcode = 'add'
        if (wrong_order) operation_opcode = 'store'
        value = '(mir-function (name main) (entry-block 0) (instruction-count 11) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
            '(source-rule frontend-ast-v1/storage-sequence-3) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key '//trim(store_key)//') (source-rule frontend-ast-v1/storage-sequence-3) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key '//trim(load_key)//') (source-rule frontend-ast-v1/storage-sequence-3) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 3) (opcode const) '// &
            '(literal 1) (source-rule frontend-ast-v1/storage-sequence-3) (result (id 3) '// &
            '(kind integer) (type i32))) (instruction (id 4) (opcode '//trim(operation_opcode)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-3) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 5) (opcode store) (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-3) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode load) (storage-key '//trim(load_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-3) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 7) (opcode const) (literal 1) '// &
            '(source-rule frontend-ast-v1/storage-sequence-3) (result (id 7) (kind integer) '// &
            '(type i32))) (instruction (id 8) (opcode add) (source-rule frontend-ast-v1/storage-sequence-3) '// &
            '(result (id 8) (kind integer) (type i32))) (instruction (id 9) (opcode store) '// &
            '(storage-key '//trim(store_key)//') (source-rule frontend-ast-v1/storage-sequence-3) '// &
            '(result (id 8) (kind integer) (type i32))) (instruction (id 10) (opcode return) '// &
            '(source-rule frontend-ast-v1/storage-sequence-3) (result (id '//int_text(return_id)//') '// &
            '(kind integer) (type i32)))))'
    end function sequence_three_input

    function sequence_four_input(load_key, store_key, return_id, wrong_order) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=16384) :: value
        character(len=16) :: operation_opcode

        operation_opcode = 'add'
        if (wrong_order) operation_opcode = 'store'
        value = '(mir-function (name main) (entry-block 0) (instruction-count 15) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key '//trim(store_key)//') (source-rule frontend-ast-v1/storage-sequence-4) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key '//trim(load_key)//') (source-rule frontend-ast-v1/storage-sequence-4) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 3) (opcode const) '// &
            '(literal 1) (source-rule frontend-ast-v1/storage-sequence-4) (result (id 3) '// &
            '(kind integer) (type i32))) (instruction (id 4) (opcode '//trim(operation_opcode)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 5) (opcode store) (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 4) (kind integer) '// &
            '(type i32))) (instruction (id 6) (opcode load) (storage-key '//trim(load_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 6) (kind integer) '// &
            '(type i32))) (instruction (id 7) (opcode const) (literal 1) '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 7) (kind integer) '// &
            '(type i32))) (instruction (id 8) (opcode add) (source-rule '// &
            'frontend-ast-v1/storage-sequence-4) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 9) (opcode store) (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 8) (kind integer) '// &
            '(type i32))) (instruction (id 10) (opcode load) (storage-key '//trim(load_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 10) (kind integer) '// &
            '(type i32))) (instruction (id 11) (opcode const) (literal 1) '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 11) (kind integer) '// &
            '(type i32))) (instruction (id 12) (opcode add) (source-rule '// &
            'frontend-ast-v1/storage-sequence-4) (result (id 12) (kind integer) '// &
            '(type i32))) (instruction (id 13) (opcode store) (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 12) (kind integer) '// &
            '(type i32))) (instruction (id 14) (opcode return) (source-rule '// &
            'frontend-ast-v1/storage-sequence-4) (result (id '//int_text(return_id)//') '// &
            '(kind integer) (type i32)))))'
    end function sequence_four_input

    function sequence_four_input_legacy(load_key, store_key, return_id) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 15) '// &
            '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 0) '// &
            '(kind integer) (type i32))) (instruction (id 1) (opcode store) '// &
            '(storage-key '//trim(store_key)//') (source-rule frontend-ast-v1/storage-sequence-4) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key '//trim(load_key)//') (source-rule frontend-ast-v1/storage-sequence-4) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 3) (opcode const) '// &
            '(literal 1) (source-rule frontend-ast-v1/storage-sequence-4) (result (id 3) '// &
            '(kind integer) (type i32))) (instruction (id 4) (opcode add) (source-rule '// &
            'frontend-ast-v1/storage-sequence-4) (result (id 4) (kind integer) (type i32))) '// &
            '(instruction (id 5) (opcode store) (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 4) '// &
            '(kind integer) (type i32))) (instruction (id 6) (opcode load) '// &
            '(storage-key '//trim(load_key)//') (source-rule frontend-ast-v1/storage-sequence-4) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 7) (opcode const) '// &
            '(literal 1) (source-rule frontend-ast-v1/storage-sequence-4) (result (id 7) '// &
            '(kind integer) (type i32))) (instruction (id 8) (opcode add) (source-rule '// &
            'frontend-ast-v1/storage-sequence-4) (result (id 8) (kind integer) (type i32))) '// &
            '(instruction (id 9) (opcode store) (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 8) '// &
            '(kind integer) (type i32))) (instruction (id 10) (opcode load) '// &
            '(storage-key '//trim(load_key)//') (source-rule frontend-ast-v1/storage-sequence-4) '// &
            '(result (id 10) (kind integer) (type i32))) (instruction (id 11) (opcode const) '// &
            '(literal 1) (source-rule frontend-ast-v1/storage-sequence-4) (result (id 11) '// &
            '(kind integer) (type i32))) (instruction (id 12) (opcode add) (source-rule '// &
            'frontend-ast-v1/storage-sequence-4) (result (id 10) (kind integer) (type i32))) '// &
            '(instruction (id 13) (opcode store) (storage-key '//trim(store_key)//') '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id 10) '// &
            '(kind integer) (type i32))) (instruction (id 14) (opcode return) '// &
            '(source-rule frontend-ast-v1/storage-sequence-4) (result (id '//int_text(return_id)//') '// &
            '(kind integer) (type i32)))))'
    end function sequence_four_input_legacy

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
