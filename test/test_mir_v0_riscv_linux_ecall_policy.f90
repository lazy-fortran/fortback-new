program test_mir_v0_riscv_linux_ecall_policy
    use iso_fortran_env, only: int32, int64
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, riscv_linux_artifact_t
    use fortback_mir_v0_riscv_linux_ecall_policy, only: &
        mir_v0_riscv_linux_ecall_encoding, mir_v0_riscv_linux_ecall_operation, &
        mir_v0_riscv_linux_ecall_operands, &
        mir_v0_riscv_linux_operation_supported
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=2048) :: input
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer(int64) :: expected_word

    call assert_true(mir_v0_riscv_linux_operation_supported('ecall'), &
        'generated ecall operation is not supported')
    call assert_true(.not. mir_v0_riscv_linux_operation_supported('exit'), &
        'unsupported operation neighbor was accepted')
    call assert_true(trim(mir_v0_riscv_linux_ecall_operation) == 'ecall', &
        'generated operation name changed')
    call assert_true(all(mir_v0_riscv_linux_ecall_operands == [0_int64, 0_int64, 0_int64]), &
        'generated fixed ecall operands changed')
    call assert_true(trim(mir_v0_riscv_linux_ecall_encoding) == &
        '14..12=0 6..2=0x1C 1..0=3', 'generated ecall encoding policy changed')

    input = '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_true(status == mir_v0_bridge_ok, 'bridge rejected generated ecall policy')

    ! Independent oracle: RISC-V Linux ecall is the fixed I-type word 0x00000073.
    expected_word = int(z'00000073', int64)
    call assert_true(iand(int(artifact%bytes(185), int64), 255_int64) == &
        iand(expected_word, 255_int64), 'ecall low byte changed')
    call assert_true(iand(int(artifact%bytes(186), int64), 255_int64) == &
        iand(shiftr(expected_word, 8), 255_int64), 'ecall byte 1 changed')
    call assert_true(iand(int(artifact%bytes(187), int64), 255_int64) == &
        iand(shiftr(expected_word, 16), 255_int64), 'ecall byte 2 changed')
    call assert_true(iand(int(artifact%bytes(188), int64), 255_int64) == &
        iand(shiftr(expected_word, 24), 255_int64), 'ecall high byte changed')
    write (*, '(a)') 'MIR-v0 RISC-V Linux ecall policy checks: ok'

contains

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_riscv_linux_ecall_policy
