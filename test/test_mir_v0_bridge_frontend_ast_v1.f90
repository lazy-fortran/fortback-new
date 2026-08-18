program test_mir_v0_bridge_frontend_ast_v1
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: ast_v1_artifact, legacy_artifact, real_artifact
    character(len=4096) :: ast_v1_input, legacy_input, real_input
    character(len=4096) :: wrong_type, wrong_kind
    character(len=256) :: diagnostic
    character(len=256) :: path, command
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit

    ast_v1_input = '(mir-function (name p) (entry-block 0) (instruction-count 2) '// &
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
    real_input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind real) '// &
        '(type f32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind real) '// &
        '(type f32)))))'

    call compile_mir_v0_riscv_linux(ast_v1_input, ast_v1_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'frontend AST-v1 MIR witness rejected')
    call compile_mir_v0_riscv_linux(legacy_input, legacy_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'legacy MIR-v0 witness rejected')
    call assert_true(size(ast_v1_artifact%bytes) == size(legacy_artifact%bytes), &
        'source-derived p artifact size changed')
    call assert_true(all(ast_v1_artifact%bytes == legacy_artifact%bytes), &
        'source-derived p artifact bytes differ from legacy bytes')
    call assert_byte(ast_v1_artifact%bytes, 185, 115, 'source-derived p ecall encoding changed')
    call assert_byte(ast_v1_artifact%bytes, 186, 0, 'source-derived p ecall encoding changed')
    call assert_byte(ast_v1_artifact%bytes, 187, 0, 'source-derived p ecall encoding changed')
    call assert_byte(ast_v1_artifact%bytes, 188, 0, 'source-derived p ecall encoding changed')

    call compile_mir_v0_riscv_linux(real_input, real_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'REAL AST-v1 bridge input rejected')
    call assert_true(size(real_artifact%bytes) == size(legacy_artifact%bytes), &
        'REAL bridge ELF size changed')
    call assert_true(all(real_artifact%bytes == legacy_artifact%bytes), &
        'REAL bridge ELF bytes changed')
    path = '/tmp/fortback-mir-v0-real-riscv-linux-test.elf'
    call write_mir_v0_riscv_linux(real_input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'REAL bridge ELF write failed')
    call execute_command_line('chmod 755 -- '//trim(path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'REAL bridge ELF chmod command failed')
    call assert_int(exit_status, 0, 'REAL bridge ELF chmod failed')
    command = 'qemu-riscv64 '//trim(path)
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'REAL bridge qemu could not run artifact')
    call assert_int(exit_status, 0, 'REAL bridge artifact did not return zero')
    open (newunit=unit, file=trim(path), status='old', iostat=io_status)
    call assert_int(io_status, 0, 'REAL bridge ELF was not written')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'REAL bridge ELF cleanup failed')

    wrong_type = real_input
    wrong_type(index(wrong_type, 'type f32'):index(wrong_type, 'type f32') + 7) = &
        'type f64'
    call compile_mir_v0_riscv_linux(wrong_type, real_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong REAL type was accepted')
    wrong_kind = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind logical) '// &
        '(type f32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind logical) '// &
        '(type f32)))))'
    call compile_mir_v0_riscv_linux(wrong_kind, real_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong REAL kind was accepted')
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

    subroutine assert_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_bridge_frontend_ast_v1
