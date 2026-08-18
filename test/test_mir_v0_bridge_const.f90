program test_mir_v0_bridge_const
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=4096) :: input, malformed
    character(len=256) :: diagnostic, path, command
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit

    input = const_input('(literal 7)')
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'const literal MIR was rejected')
    call assert_byte(artifact%bytes, 177, 19, 'const addi opcode byte changed')
    call assert_byte(artifact%bytes, 178, 5, 'const addi rs1 byte changed')
    call assert_byte(artifact%bytes, 179, 112, 'const addi immediate byte changed')
    call assert_byte(artifact%bytes, 180, 0, 'const addi immediate high byte changed')

    path = '/tmp/fortback-mir-v0-const-riscv-linux-test.elf'
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_ok, 'const ELF write failed')
    call execute_command_line('chmod 755 -- '//trim(path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'const ELF chmod command failed')
    call assert_int(exit_status, 0, 'const ELF chmod failed')
    command = 'qemu-riscv64 '//trim(path)
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status, &
        cmdstat=command_status)
    call assert_int(command_status, 0, 'const qemu could not run artifact')
    call assert_int(exit_status, 7, 'const artifact did not return seven')
    open (newunit=unit, file=trim(path), status='old', iostat=io_status)
    call assert_int(io_status, 0, 'const ELF was not written')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'const ELF cleanup failed')

    malformed = const_input('')
    call compile_mir_v0_riscv_linux(malformed, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_malformed, 'missing literal was accepted')
    malformed = const_input('(literal seven)')
    call compile_mir_v0_riscv_linux(malformed, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_malformed, 'non-integer literal was accepted')
    malformed = const_input('(literal 2147483648)')
    call compile_mir_v0_riscv_linux(malformed, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_malformed, 'out-of-range literal was accepted')
    malformed = input(:len_trim(input) - 1)
    call compile_mir_v0_riscv_linux(malformed, artifact, status, diagnostic)
    call assert_equal(status, mir_v0_bridge_malformed, 'malformed const MIR was accepted')
    write (*, '(a)') 'MIR-v0 const bridge encoding and qemu checks: ok'

contains

    function const_input(literal) result(value)
        character(len=*), intent(in) :: literal
        character(len=4096) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 3) '// &
            '(instructions (instruction (id 0) (opcode const) '// &
            '(source-rule frontend-ast-v1/assignment) '//trim(literal)// &
            ' (result (id 1) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode store) (source-rule frontend-ast-v1/assignment) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) '// &
            '(opcode return) (source-rule frontend-ast-v1/assignment) '// &
            '(result (id 1) (kind integer) (type i32)))))'
    end function const_input

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

end program test_mir_v0_bridge_const
