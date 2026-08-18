program test_mir_v0_bridge_frontend_ast_v1
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, riscv_linux_artifact_t
    implicit none

    type(riscv_linux_artifact_t) :: ast_v1_artifact, legacy_artifact
    character(len=4096) :: ast_v1_input, legacy_input
    character(len=256) :: diagnostic
    integer(int32) :: status

    ast_v1_input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    legacy_input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'

    call compile_mir_v0_riscv_linux(ast_v1_input, ast_v1_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'frontend AST-v1 MIR witness rejected')
    call compile_mir_v0_riscv_linux(legacy_input, legacy_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'legacy MIR-v0 witness rejected')
    call assert_true(size(ast_v1_artifact%bytes) == size(legacy_artifact%bytes), &
        'AST-v1 artifact size changed')
    call assert_true(all(ast_v1_artifact%bytes == legacy_artifact%bytes), &
        'AST-v1 artifact bytes differ from legacy bytes')
    call assert_byte(ast_v1_artifact%bytes, 185, 115, 'AST-v1 ecall encoding changed')
    call assert_byte(ast_v1_artifact%bytes, 186, 0, 'AST-v1 ecall encoding changed')
    call assert_byte(ast_v1_artifact%bytes, 187, 0, 'AST-v1 ecall encoding changed')
    call assert_byte(ast_v1_artifact%bytes, 188, 0, 'AST-v1 ecall encoding changed')
    write (*, '(a)') 'MIR-v0 frontend AST-v1 bridge behavioral checks: ok'

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

end program test_mir_v0_bridge_frontend_ast_v1
