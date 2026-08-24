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
    character(len=32768) :: input
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer :: command_status, exit_status, route_index
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
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
        'frontend-ast-v1/storage-sequence-5'), 19_int32, 'five-step route count changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-5', 14_int32)), 'ld', 'five-step load route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v1/storage-sequence-5', 18_int32)), 'addi', 'five-step return route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v2/execution-part', 14_int32)), 'ld', 'v2 five-step load route changed')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v2/execution-part', 18_int32)), 'addi', 'v2 five-step return route changed')
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
        'frontend-ast-v1/storage-sequence-6'), 23_int32, 'six-step route count changed')
    call assert_sequence_six_route('frontend-ast-v1/storage-sequence-6')
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
        'frontend-ast-v2/execution-part-6'), 23_int32, 'v2 six-step route count changed')
    call assert_sequence_six_route('frontend-ast-v2/execution-part-6')
    call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
        'frontend-ast-v2/execution-part-5', 22_int32)), '', &
        'v2 five-step route collided with six-step route')

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

    input = sequence_five_input('x', 'x', 16, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'five-step storage sequence was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'five-step frame encoding changed')
    call assert_word(artifact%bytes, 193, [147, 5, 16, 0], 'five-step first increment changed')
    call assert_word(artifact%bytes, 241, [147, 5, 16, 0], 'five-step fourth increment changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'five-step storage ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'five-step storage chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'five-step storage qemu command failed')
    call assert_equal(exit_status, 11, 'five-step storage sequence did not return 11')

    input = sequence_five_input_v2('x', 'x', 16, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'v2 five-step storage sequence was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'v2 five-step frame encoding changed')
    call assert_word(artifact%bytes, 193, [147, 5, 16, 0], 'v2 five-step first increment changed')
    call assert_word(artifact%bytes, 241, [147, 5, 16, 0], 'v2 five-step fourth increment changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'v2 five-step storage ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'v2 five-step storage chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'v2 five-step storage qemu command failed')
    call assert_equal(exit_status, 11, 'v2 five-step storage sequence did not return 11')

    input = sequence_six_input('x', 'x', 20, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'six-step storage sequence was rejected')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'six-step storage ELF write failed')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'six-step frame encoding changed')
    call assert_word(artifact%bytes, 185, [35, 48, 161, 0], 'six-step first store encoding changed')
    call assert_word(artifact%bytes, 253, [3, 53, 1, 0], 'six-step fifth load encoding changed')
    call assert_word(artifact%bytes, 265, [35, 48, 161, 0], 'six-step final store encoding changed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'six-step storage chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'six-step storage qemu command failed')
    call assert_equal(exit_status, 12, 'six-step storage sequence did not return 12')

    input = sequence_six_input_v2('x', 'x', 20, .false.)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'v2 six-step storage sequence was rejected')
    call assert_word(artifact%bytes, 177, [19, 1, 1, 255], 'v2 six-step frame encoding changed')
    call assert_word(artifact%bytes, 185, [35, 48, 161, 0], 'v2 six-step first store changed')
    call assert_word(artifact%bytes, 253, [3, 53, 1, 0], 'v2 six-step fifth load changed')
    call assert_word(artifact%bytes, 265, [35, 48, 161, 0], 'v2 six-step final store changed')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'v2 six-step storage ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'v2 six-step storage chmod failed')
    call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_equal(command_status, 0, 'v2 six-step storage qemu command failed')
    call assert_equal(exit_status, 12, 'v2 six-step storage sequence did not return 12')

    do route_index = 7, 10
        input = sequence_generated_input(route_index, 4 * route_index - 4, .false., 'x')
        call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
            'frontend-ast-v1/storage-sequence-'//int_text(route_index)), &
            int(4 * route_index - 1, int32), 'generated route count changed')
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'generated storage sequence was rejected')
        call write_mir_v0_riscv_linux(input, path, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'generated storage ELF write failed')
        call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_equal(command_status, 0, 'generated storage chmod failed')
        call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
            cmdstat=command_status)
        call assert_equal(command_status, 0, 'generated storage qemu command failed')
        call assert_equal(exit_status, route_index + 6, 'generated route returned wrong status')
    end do
    do route_index = 3, 10
        input = sequence_generated_input_v2(route_index, 4 * route_index - 4, .false., 'x')
        call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
            'frontend-ast-v2/execution-part-'//int_text(route_index)), &
            int(4 * route_index - 1, int32), 'v2 generated route count changed')
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, 'v2 generated sequence was rejected')
        if (route_index == 3 .or. route_index == 10) then
            call write_mir_v0_riscv_linux(input, path, status, diagnostic)
            call assert_equal(status, mir_v0_bridge_ok, 'v2 generated ELF write failed')
            call execute_command_line('chmod 755 -- '//path, wait=.true., exitstat=exit_status, &
                cmdstat=command_status)
            call assert_equal(command_status, 0, 'v2 generated chmod failed')
            call execute_command_line('qemu-riscv64 '//path, wait=.true., exitstat=exit_status, &
                cmdstat=command_status)
            call assert_equal(exit_status, route_index + 6, 'v2 generated route returned wrong status')
        end if
    end do
    call compile_mir_v0_riscv_linux(sequence_generated_input(10, 36, .true., 'x'), &
        artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'generated wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_generated_input(10, 36, .false., 'y'), &
        artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'generated wrong storage key was accepted')

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
    call compile_mir_v0_riscv_linux(sequence_five_input('x', 'x', 16, .true.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'five-step wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_five_input('y', 'x', 16, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'five-step wrong storage key was accepted')
    call compile_mir_v0_riscv_linux(sequence_five_input('x', 'x', 15, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'five-step wrong result was accepted')
    call compile_mir_v0_riscv_linux(sequence_six_input('x', 'x', 20, .true.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'six-step wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_six_input('y', 'x', 20, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'six-step wrong storage key was accepted')
    call compile_mir_v0_riscv_linux(sequence_six_input('x', 'x', 19, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'six-step wrong result was accepted')
    call compile_mir_v0_riscv_linux(sequence_six_input_v2('x', 'x', 20, .true.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'v2 six-step wrong order was accepted')
    call compile_mir_v0_riscv_linux(sequence_six_input_v2('y', 'x', 20, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'v2 six-step wrong storage key was accepted')
    call compile_mir_v0_riscv_linux(sequence_six_input_v2('x', 'x', 19, .false.), artifact, &
        status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'v2 six-step wrong result was accepted')
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

    subroutine assert_sequence_six_route(source_rule)
        character(len=*), intent(in) :: source_rule
        character(len=16), parameter :: expected(23) = [character(len=16) :: &
            'addi', 'sd', 'ld', 'addi', 'add', 'sd', 'ld', 'addi', 'add', 'sd', &
            'ld', 'addi', 'add', 'sd', 'ld', 'addi', 'add', 'sd', 'ld', 'addi', &
            'add', 'sd', 'addi']
        integer :: index

        do index = 0, 22
            call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
                source_rule, int(index, int32))), trim(expected(index + 1)), &
                'six-step route operation changed')
        end do
        call assert_equal_text(trim(mir_v0_bridge_policy_route_operation_for( &
            'frontend-ast-v1/storage-sequence-5', 22_int32)), '', &
            'six-step route index collided with five-step route')
    end subroutine assert_sequence_six_route

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

    function sequence_five_input(load_key, store_key, return_id, wrong_order) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=20480) :: value
        character(len=16) :: operation_opcode

        operation_opcode = 'add'
        if (wrong_order) operation_opcode = 'store'
        value = '(mir-function (name main) (entry-block 0) (instruction-count 19) (instructions '// &
            sequence_five_instruction(load_key, store_key, 0, 'const', '7', 'integer-literal-left', '0')// &
            sequence_five_instruction(load_key, store_key, 1, 'store', '', 'integer-sequence-store-literal', '1')// &
            sequence_five_instruction(load_key, store_key, 2, 'load', '', 'integer-sequence-loaded', '2')// &
            sequence_five_instruction(load_key, store_key, 3, 'const', '1', 'integer-sequence-literal-right', '3')// &
            sequence_five_instruction(load_key, store_key, 4, operation_opcode, '', 'integer-sequence-expression', '4')// &
            sequence_five_instruction(load_key, store_key, 5, 'store', '', 'integer-sequence-expression-result', '4')// &
            sequence_five_instruction(load_key, store_key, 6, 'load', '', 'integer-sequence-3-loaded', '6')// &
            sequence_five_instruction(load_key, store_key, 7, 'const', '1', 'integer-sequence-3-literal-right', '7')// &
            sequence_five_instruction(load_key, store_key, 8, 'add', '', 'integer-sequence-3-expression', '8')// &
            sequence_five_instruction(load_key, store_key, 9, 'store', '', 'integer-sequence-3-expression-result', '8')// &
            sequence_five_instruction(load_key, store_key, 10, 'load', '', 'integer-sequence-4-loaded', '10')// &
            sequence_five_instruction(load_key, store_key, 11, 'const', '1', 'integer-sequence-4-literal-right', '11')// &
            sequence_five_instruction(load_key, store_key, 12, 'add', '', 'integer-sequence-4-expression', '12')// &
            sequence_five_instruction(load_key, store_key, 13, 'store', '', 'integer-sequence-4-expression-result', '12')// &
            sequence_five_instruction(load_key, store_key, 14, 'load', '', 'integer-sequence-5-loaded', '14')// &
            sequence_five_instruction(load_key, store_key, 15, 'const', '1', 'integer-sequence-5-literal-right', '15')// &
            sequence_five_instruction(load_key, store_key, 16, 'add', '', 'integer-sequence-5-expression', '16')// &
            sequence_five_instruction(load_key, store_key, 17, 'store', '', 'integer-sequence-5-expression-result', '16')// &
            '(instruction (id 18) (opcode return) '// &
            '(source-rule frontend-ast-v1/storage-sequence-5) (result (id '//int_text(return_id)//') '// &
            '(kind integer) (type i32)))))'

    end function sequence_five_input

    function sequence_six_input(load_key, store_key, return_id, wrong_order) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=24576) :: value
        character(len=16) :: operation_opcode

        operation_opcode = 'add'
        if (wrong_order) operation_opcode = 'store'
        value = '(mir-function (name main) (entry-block 0) (instruction-count 23) (instructions '// &
            sequence_six_instruction(load_key, store_key, 0, 'const', '7', 'integer-literal-left', '0')// &
            sequence_six_instruction(load_key, store_key, 1, 'store', '', 'integer-sequence-store-literal', '1')// &
            sequence_six_instruction(load_key, store_key, 2, 'load', '', 'integer-sequence-loaded', '2')// &
            sequence_six_instruction(load_key, store_key, 3, 'const', '1', 'integer-sequence-literal-right', '3')// &
            sequence_six_instruction(load_key, store_key, 4, operation_opcode, '', 'integer-sequence-expression', '4')// &
            sequence_six_instruction(load_key, store_key, 5, 'store', '', 'integer-sequence-expression-result', '4')// &
            sequence_six_instruction(load_key, store_key, 6, 'load', '', 'integer-sequence-3-loaded', '6')// &
            sequence_six_instruction(load_key, store_key, 7, 'const', '1', 'integer-sequence-3-literal-right', '7')// &
            sequence_six_instruction(load_key, store_key, 8, 'add', '', 'integer-sequence-3-expression', '8')// &
            sequence_six_instruction(load_key, store_key, 9, 'store', '', 'integer-sequence-3-expression-result', '8')// &
            sequence_six_instruction(load_key, store_key, 10, 'load', '', 'integer-sequence-4-loaded', '10')// &
            sequence_six_instruction(load_key, store_key, 11, 'const', '1', 'integer-sequence-4-literal-right', '11')// &
            sequence_six_instruction(load_key, store_key, 12, 'add', '', 'integer-sequence-4-expression', '12')// &
            sequence_six_instruction(load_key, store_key, 13, 'store', '', 'integer-sequence-4-expression-result', '12')// &
            sequence_six_instruction(load_key, store_key, 14, 'load', '', 'integer-sequence-5-loaded', '14')// &
            sequence_six_instruction(load_key, store_key, 15, 'const', '1', 'integer-sequence-5-literal-right', '15')// &
            sequence_six_instruction(load_key, store_key, 16, 'add', '', 'integer-sequence-5-expression', '16')// &
            sequence_six_instruction(load_key, store_key, 17, 'store', '', 'integer-sequence-5-expression-result', '16')// &
            sequence_six_instruction(load_key, store_key, 18, 'load', '', 'integer-sequence-6-loaded', '18')// &
            sequence_six_instruction(load_key, store_key, 19, 'const', '1', 'integer-sequence-6-literal-right', '19')// &
            sequence_six_instruction(load_key, store_key, 20, 'add', '', 'integer-sequence-6-expression', '20')// &
            sequence_six_instruction(load_key, store_key, 21, 'store', '', 'integer-sequence-6-expression-result', '20')// &
            '(instruction (id 22) (opcode return) (source-rule frontend-ast-v1/storage-sequence-6) '// &
            '(result (id '//int_text(return_id)//') (kind integer) (type i32)))))'
    end function sequence_six_input

    function sequence_six_input_v2(load_key, store_key, return_id, wrong_order) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=24576) :: value

        value = sequence_six_input(load_key, store_key, return_id, wrong_order)
        value = replace_text(value, 'frontend-ast-v1/storage-sequence-6', &
            'frontend-ast-v2/execution-part-6')
    end function sequence_six_input_v2

    function sequence_generated_input(sequence_count, return_id, wrong_order, storage_key) result(value)
        integer, intent(in) :: sequence_count, return_id
        logical, intent(in) :: wrong_order
        character(len=*), intent(in) :: storage_key
        character(len=32768) :: value
        character(len=64) :: source_rule, shape
        character(len=16) :: opcode, key
        integer :: index, step, remainder, total

        source_rule = 'frontend-ast-v1/storage-sequence-'//int_text(sequence_count)
        total = 4 * sequence_count - 1
        value = '(mir-function (name main) (entry-block 0) (instruction-count '// &
            trim(int_text(total))//') (instructions '
        do index = 0, total - 1
            opcode = 'const'
            shape = 'integer-literal-left'
            key = ''
            if (index == 1) then
                opcode = 'store'
                shape = 'integer-sequence-store-literal'
                key = storage_key
            else if (index >= 2 .and. index < total - 1) then
                step = 2 + (index - 2) / 4
                remainder = mod(index - 2, 4)
                if (remainder == 0) then
                    opcode = 'load'
                    shape = 'integer-sequence-loaded'
                    if (step > 2) shape = 'integer-sequence-'//int_text(step)//'-loaded'
                    key = storage_key
                else if (remainder == 1) then
                    opcode = 'const'
                    shape = 'integer-sequence-literal-right'
                    if (step > 2) shape = 'integer-sequence-'//int_text(step)//'-literal-right'
                else if (remainder == 2) then
                    opcode = 'add'
                    shape = 'integer-sequence-expression'
                    if (step > 2) shape = 'integer-sequence-'//int_text(step)//'-expression'
                    if (wrong_order .and. step == 2) opcode = 'store'
                else
                    opcode = 'store'
                    shape = 'integer-sequence-expression-result'
                    if (step > 2) shape = 'integer-sequence-'//int_text(step)//'-expression-result'
                    key = storage_key
                end if
            else if (index == total - 1) then
                opcode = 'return'
                shape = 'integer-sequence-'//int_text(sequence_count)//'-expression-result'
            end if
            value = trim(value)//' (instruction (id '//trim(int_text(index))// &
                ') (opcode '//trim(opcode)//') '
            if (index == 0) value = trim(value)//' (literal 7) '
            if (index >= 2 .and. index < total - 1 .and. mod(index - 2, 4) == 1) &
                value = trim(value)//' (literal 1) '
            if (len_trim(key) > 0) value = trim(value)//' (storage-key '//trim(key)//') '
            value = trim(value)//' (source-rule '//trim(source_rule)//') (result (id'
            if (index == total - 1) then
                value = trim(value)//' '//trim(int_text(return_id))
            else if (index >= 5 .and. mod(index - 2, 4) == 3) then
                value = trim(value)//' '//trim(int_text(index - 1))
            else
                value = trim(value)//' '//trim(int_text(index))
            end if
            value = trim(value)//') (kind integer) (type i32))) '
        end do
        value = trim(value)//'))'
    end function sequence_generated_input

    function sequence_generated_input_v2(sequence_count, return_id, wrong_order, storage_key) result(value)
        integer, intent(in) :: sequence_count, return_id
        logical, intent(in) :: wrong_order
        character(len=*), intent(in) :: storage_key
        character(len=32768) :: value

        value = sequence_generated_input(sequence_count, return_id, wrong_order, storage_key)
        value = replace_text(value, 'frontend-ast-v1/storage-sequence-'//int_text(sequence_count), &
            'frontend-ast-v2/execution-part-'//int_text(sequence_count))
    end function sequence_generated_input_v2

    function sequence_six_instruction(load_key, store_key, index, opcode, literal, shape, result_id) &
            result(text)
        character(len=*), intent(in) :: load_key, store_key, opcode, literal, shape, result_id
        integer, intent(in) :: index
        character(len=512) :: text

        text = '(instruction (id '//int_text(index)//') (opcode '//trim(opcode)//') '
        if (len_trim(literal) > 0) text = trim(text)//'(literal '//trim(literal)//') '
        if (index == 1 .or. index == 5 .or. index == 9 .or. index == 13 .or. index == 17 .or. index == 21) then
            text = trim(text)//'(storage-key '//trim(store_key)//') '
        else if (index == 2 .or. index == 6 .or. index == 10 .or. index == 14 .or. index == 18) then
            text = trim(text)//'(storage-key '//trim(load_key)//') '
        end if
        text = trim(text)//'(source-rule frontend-ast-v1/storage-sequence-6) '// &
            '(result (id '//trim(result_id)//') (kind integer) (type i32))) '
    end function sequence_six_instruction

    function sequence_five_input_v2(load_key, store_key, return_id, wrong_order) result(value)
        character(len=*), intent(in) :: load_key, store_key
        integer, intent(in) :: return_id
        logical, intent(in) :: wrong_order
        character(len=20480) :: value

        value = sequence_five_input(load_key, store_key, return_id, wrong_order)
        value = replace_text(value, 'frontend-ast-v1/storage-sequence-5', &
            'frontend-ast-v2/execution-part-5')
    end function sequence_five_input_v2

    function sequence_five_instruction(load_key, store_key, index, opcode, literal, shape, result_id) &
            result(text)
        character(len=*), intent(in) :: load_key, store_key, opcode, literal, shape, result_id
        integer, intent(in) :: index
        character(len=512) :: text

        text = '(instruction (id '//int_text(index)//') (opcode '//trim(opcode)//') '
        if (len_trim(literal) > 0) text = trim(text)//'(literal '//trim(literal)//') '
        if (index == 1 .or. index == 5 .or. index == 9 .or. index == 13 .or. index == 17) then
            text = trim(text)//'(storage-key '//trim(store_key)//') '
        else if (index == 2 .or. index == 6 .or. index == 10 .or. index == 14) then
            text = trim(text)//'(storage-key '//trim(load_key)//') '
        end if
        text = trim(text)//'(source-rule frontend-ast-v1/storage-sequence-5) '// &
            '(result (id '//trim(result_id)//') (kind integer) (type i32))) '
    end function sequence_five_instruction

    function replace_text(value, old, new) result(replaced)
        character(len=*), intent(in) :: value, old, new
        character(len=20480) :: replaced
        integer :: start

        replaced = value
        start = index(replaced, old)
        do while (start > 0)
            replaced = replaced(:start - 1)//new//replaced(start + len(old):)
            start = index(replaced, old)
        end do
    end function replace_text

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
