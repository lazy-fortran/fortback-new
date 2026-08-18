program test_mir_v0_bridge_generated
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, riscv_linux_artifact_t
    use fortback_mir_v0_bridge_metadata, only: mir_v0_source_rule_value, &
        mir_v0_value_kind_value
    use fortback_mir_v0_riscv_linux_bridge_policy, only: &
        mir_v0_bridge_policy_instruction_count, mir_v0_bridge_policy_result_shape_count, &
        mir_v0_bridge_policy_instruction_count_for, &
        mir_v0_bridge_policy_instruction_count_matches
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=2048) :: input, ast_input
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer :: item_count

    call assert_true(mir_v0_source_rule_value('frontend-v0/program') /= 0_int32, &
        'legacy frontend source rule is missing from generated metadata')
    call assert_true(mir_v0_source_rule_value('frontend-ast-v1/program') /= 0_int32, &
        'AST-v1 frontend source rule is missing from generated metadata')
    call assert_true(mir_v0_source_rule_value('unknown/program') == 0_int32, &
        'unknown frontend source rule resolved in generated metadata')
    call assert_true(mir_v0_source_rule_value('frontend-ast-v1/assignment') /= 0_int32, &
        'assignment source rule is missing from generated metadata')
    call assert_true(mir_v0_source_rule_value('frontend-ast-v1/expression') /= 0_int32, &
        'expression source rule is missing from generated metadata')
    call assert_true(mir_v0_source_rule_value('frontend-ast-v2/stop-stmt') /= 0_int32, &
        'STOP source rule is missing from generated metadata')
    call assert_true(mir_v0_source_rule_value('frontend-ast-v2/print-stmt') /= 0_int32, &
        'PRINT source rule is missing from generated metadata')
    call assert_true(mir_v0_source_rule_value('frontend-ast-v2/execution-part') /= 0_int32, &
        'execution-part source rule is missing from generated metadata')
    call assert_true(mir_v0_source_rule_value('frontend-ast-v1/storage-sequence-6') /= 0_int32, &
        'six-step storage source rule is missing from generated metadata')
    call assert_equal(mir_v0_bridge_policy_instruction_count, 10_int32, &
        'generated bridge instruction policy changed')
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
        'frontend-ast-v1/assignment'), 2_int32, 'assignment route count changed')
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('main', &
        'frontend-ast-v1/expression'), 3_int32, 'expression route count changed')
    call assert_equal(mir_v0_bridge_policy_instruction_count_for('p', &
        'frontend-ast-v2/stop-stmt'), 2_int32, 'STOP route count changed')
    call assert_true(mir_v0_bridge_policy_instruction_count_matches('p', &
        'frontend-ast-v2/print-stmt', 7_int32), 'three-item PRINT route count changed')
    do item_count = 11, 60
        call assert_true(mir_v0_bridge_policy_instruction_count_matches('main', &
            'frontend-ast-v2/print-stmt', 2_int32*item_count + 7_int32), &
            'bounded stored-variable PRINT route count missing')
    end do
    call assert_equal(mir_v0_value_kind_value('complex'), 5_int32, &
        'generated complex value kind changed')
    call assert_equal(mir_v0_value_kind_value('logical'), 3_int32, &
        'generated logical value kind changed')
    call assert_equal(mir_v0_value_kind_value('character'), 6_int32, &
        'generated character value kind changed')
    call assert_equal(mir_v0_bridge_policy_result_shape_count, 109_int32, &
        'generated bridge result-shape policy changed')

    input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'generated metadata rejected bridge input')
    call assert_equal(size(artifact%bytes), 400, 'generated bridge ELF size changed')
    call assert_byte(artifact%bytes, 177, 19, 'addi result encoding changed')
    call assert_byte(artifact%bytes, 178, 5, 'addi result encoding changed')
    call assert_byte(artifact%bytes, 179, 0, 'addi result encoding changed')
    call assert_byte(artifact%bytes, 180, 0, 'addi result encoding changed')
    call assert_byte(artifact%bytes, 181, 147, 'addi argument encoding changed')
    call assert_byte(artifact%bytes, 182, 8, 'addi argument encoding changed')
    call assert_byte(artifact%bytes, 183, 208, 'addi argument encoding changed')
    call assert_byte(artifact%bytes, 184, 5, 'addi argument encoding changed')
    call assert_byte(artifact%bytes, 185, 115, 'ecall encoding changed')
    call assert_byte(artifact%bytes, 186, 0, 'ecall encoding changed')
    call assert_byte(artifact%bytes, 187, 0, 'ecall encoding changed')
    call assert_byte(artifact%bytes, 188, 0, 'ecall encoding changed')

    ast_input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(ast_input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'AST-v1 main bridge input rejected')
    write (*, '(a)') 'MIR-v0 generated bridge regression: ok'

contains

    subroutine assert_byte(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected
        character(len=*), intent(in) :: message

        if (iand(int(bytes(index), int32), 255_int32) /= expected) error stop message
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

end program test_mir_v0_bridge_generated
