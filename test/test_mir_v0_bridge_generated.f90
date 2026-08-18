program test_mir_v0_bridge_generated
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, riscv_linux_artifact_t
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=2048) :: input
    character(len=256) :: diagnostic
    integer(int32) :: status

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

end program test_mir_v0_bridge_generated
