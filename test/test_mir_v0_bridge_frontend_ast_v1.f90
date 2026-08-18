program test_mir_v0_bridge_frontend_ast_v1
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: ast_v1_artifact, legacy_artifact, real_artifact, &
        double_artifact, complex_artifact
    character(len=4096) :: ast_v1_input, legacy_input, real_input, double_input, complex_input
    character(len=4096) :: wrong_type, wrong_kind, complex_wrong_type, complex_wrong_kind
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
    double_input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind real) '// &
        '(type f64))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind real) '// &
        '(type f64)))))'
    complex_input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind complex) '// &
        '(type c32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind complex) '// &
        '(type c32)))))'

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

    call compile_mir_v0_riscv_linux(double_input, double_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'DOUBLE PRECISION bridge input rejected')
    call assert_true(size(double_artifact%bytes) == size(legacy_artifact%bytes), &
        'DOUBLE PRECISION bridge ELF size changed')
    call assert_true(all(double_artifact%bytes == legacy_artifact%bytes), &
        'DOUBLE PRECISION bridge ELF bytes changed')
    path = '/tmp/fortback-mir-v0-double-riscv-linux-test.elf'
    call write_mir_v0_riscv_linux(double_input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'DOUBLE PRECISION bridge ELF write failed')
    call execute_command_line('chmod 755 -- '//trim(path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'DOUBLE PRECISION bridge ELF chmod command failed')
    call assert_int(exit_status, 0, 'DOUBLE PRECISION bridge ELF chmod failed')
    command = 'qemu-riscv64 '//trim(path)
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'DOUBLE PRECISION bridge qemu could not run artifact')
    call assert_int(exit_status, 0, 'DOUBLE PRECISION bridge artifact did not return zero')
    open (newunit=unit, file=trim(path), status='old', iostat=io_status)
    call assert_int(io_status, 0, 'DOUBLE PRECISION bridge ELF was not written')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'DOUBLE PRECISION bridge ELF cleanup failed')

    call compile_mir_v0_riscv_linux(complex_input, complex_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'COMPLEX bridge input rejected')
    call assert_true(size(complex_artifact%bytes) == size(legacy_artifact%bytes), &
        'COMPLEX bridge ELF size changed')
    call assert_true(all(complex_artifact%bytes == legacy_artifact%bytes), &
        'COMPLEX bridge ELF bytes changed')
    call assert_byte(complex_artifact%bytes, 185, 115, 'COMPLEX bridge ecall encoding changed')
    path = '/tmp/fortback-mir-v0-complex-riscv-linux-test.elf'
    call write_mir_v0_riscv_linux(complex_input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'COMPLEX bridge ELF write failed')
    call execute_command_line('chmod 755 -- '//trim(path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'COMPLEX bridge ELF chmod command failed')
    call assert_int(exit_status, 0, 'COMPLEX bridge ELF chmod failed')
    command = 'qemu-riscv64 '//trim(path)
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'COMPLEX bridge qemu could not run artifact')
    call assert_int(exit_status, 0, 'COMPLEX bridge artifact did not return zero')
    open (newunit=unit, file=trim(path), status='old', iostat=io_status)
    call assert_int(io_status, 0, 'COMPLEX bridge ELF was not written')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'COMPLEX bridge ELF cleanup failed')

    wrong_type = double_input
    wrong_type(index(wrong_type, 'type f64'):index(wrong_type, 'type f64') + 7) = &
        'type f16'
    wrong_type(index(wrong_type, 'type f64', back=.true.): &
        index(wrong_type, 'type f64', back=.true.) + 7) = 'type f16'
    call compile_mir_v0_riscv_linux(wrong_type, double_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong DOUBLE PRECISION type was accepted')
    wrong_kind = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind logical) '// &
        '(type f64))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind logical) '// &
        '(type f64)))))'
    call compile_mir_v0_riscv_linux(wrong_kind, double_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong DOUBLE PRECISION kind was accepted')
    complex_wrong_type = complex_input
    complex_wrong_type(index(complex_wrong_type, 'type c32'):index(complex_wrong_type, 'type c32') + 7) = &
        'type c64'
    complex_wrong_type(index(complex_wrong_type, 'type c32', back=.true.): &
        index(complex_wrong_type, 'type c32', back=.true.) + 7) = 'type c64'
    call compile_mir_v0_riscv_linux(complex_wrong_type, complex_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong COMPLEX type was accepted')
    complex_wrong_kind = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind real) '// &
        '(type c32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-ast-v1/program) (result (id 1) (kind real) '// &
        '(type c32)))))'
    call compile_mir_v0_riscv_linux(complex_wrong_kind, complex_artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_out_of_scope, 'wrong COMPLEX kind was accepted')
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
