program test_dispatch_l2_observable_trace
    use iso_fortran_env, only: int8, int32, int64
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_malformed, mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, &
        riscv_linux_artifact_t, write_mir_v0_riscv_linux
    implicit none

    character(len=*), parameter :: witness = &
        '(mir-function (name main) (entry-block 0) (instruction-count 2) '// &
        '(instructions (instruction (id 0) (opcode add) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode return) '// &
        '(source-rule frontend-v0/program) (result (id 1) (kind integer) '// &
        '(type i32)))))'
    character(len=1024) :: malformed, out_of_scope
    character(len=256), parameter :: executable_path = &
        '/tmp/fortback-l2-observable-trace.elf'
    character(len=256), parameter :: malformed_path = &
        '/tmp/fortback-l2-observable-trace-malformed.elf'
    character(len=256), parameter :: out_of_scope_path = &
        '/tmp/fortback-l2-observable-trace-out-of-scope.elf'
    character(len=256) :: diagnostic
    type(riscv_linux_artifact_t) :: first, second
    integer(int32) :: status
    integer :: command_status, exit_status, position

    ! The machine words and ELF fields below are the independent oracle from
    ! tests/golden/l2-first-executable-v0.oracle.toml, not implementation data.
    call compile_mir_v0_riscv_linux(witness, first, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'frozen MIR-v0 witness rejected')
    call assert_equal(trim(first%format), 'ELF64', 'artifact format provenance changed')
    call assert_equal(trim(first%architecture), 'riscv64', &
        'artifact architecture provenance changed')
    call assert_equal(trim(first%platform), 'linux', &
        'artifact platform provenance changed')
    call assert_equal(trim(first%origin), 'DERIVED', &
        'artifact origin provenance changed')
    call assert_equal(trim(first%input_format), 'mir-v0-sx', &
        'artifact input provenance changed')
    if (.not. allocated(first%bytes)) error stop 'ELF artifact bytes are absent'
    call check_elf_oracle(first%bytes)

    call compile_mir_v0_riscv_linux(witness, second, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'second deterministic compile failed')
    if (.not. allocated(second%bytes)) error stop 'second ELF artifact bytes are absent'
    if (size(first%bytes) /= size(second%bytes)) then
        error stop 'ELF artifact size is nondeterministic'
    end if
    if (.not. all(first%bytes == second%bytes)) then
        error stop 'ELF artifact bytes are nondeterministic'
    end if

    call remove_file(executable_path)
    call write_mir_v0_riscv_linux(witness, executable_path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'ELF executable write failed')
    call execute_command_line('chmod 755 -- '//trim(executable_path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'ELF chmod command failed')
    call assert_int(exit_status, 0, 'ELF chmod failed')
    call execute_command_line('qemu-riscv64 '//trim(executable_path), wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'qemu-riscv64 could not run ELF')
    call assert_int(exit_status, 0, 'ELF runtime exit status was not zero')
    call assert_exists(executable_path, 'ELF executable was not written')
    call remove_file(executable_path)

    malformed = witness(:len_trim(witness) - 1)
    call remove_file(malformed_path)
    call compile_mir_v0_riscv_linux(malformed, second, status, diagnostic)
    call assert_status(status, mir_v0_bridge_malformed, 'malformed MIR was accepted')
    if (allocated(second%bytes)) error stop 'malformed MIR produced artifact bytes'
    call write_mir_v0_riscv_linux(malformed, malformed_path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_malformed, 'malformed MIR was accepted')
    call assert_equal(trim(diagnostic), 'mir-v0: unexpected end of SX input', &
        'malformed MIR diagnostic changed')
    call assert_absent(malformed_path, 'malformed MIR produced an artifact')

    out_of_scope = witness
    position = index(out_of_scope, '(name main)')
    if (position == 0) error stop 'frozen witness name field is missing'
    out_of_scope(position:position + 10) = '(name test)'
    call remove_file(out_of_scope_path)
    call compile_mir_v0_riscv_linux(out_of_scope, second, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'out-of-scope MIR was accepted')
    if (allocated(second%bytes)) error stop 'out-of-scope MIR produced artifact bytes'
    call write_mir_v0_riscv_linux(out_of_scope, out_of_scope_path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'out-of-scope MIR was accepted')
    call assert_equal(trim(diagnostic), 'mir-v0: function is out of scope', &
        'out-of-scope MIR diagnostic changed')
    call assert_absent(out_of_scope_path, 'out-of-scope MIR produced an artifact')

    write (*, '(a)') 'L2 fortback observable trace behavioral checks: ok'

contains

    subroutine check_elf_oracle(bytes)
        integer(int8), intent(in) :: bytes(:)

        if (size(bytes) /= 400) error stop 'ELF image size changed'
        call assert_byte(bytes, 1, 127, 'ELF magic byte 1 changed')
        call assert_byte(bytes, 2, 69, 'ELF magic byte 2 changed')
        call assert_byte(bytes, 3, 76, 'ELF magic byte 3 changed')
        call assert_byte(bytes, 4, 70, 'ELF magic byte 4 changed')
        call assert_byte(bytes, 5, 2, 'ELF class changed')
        call assert_byte(bytes, 6, 1, 'ELF data encoding changed')
        call assert_u16(bytes, 17, 2_int64, 'ELF type changed')
        call assert_u16(bytes, 19, 243_int64, 'ELF machine changed')
        call assert_u32(bytes, 21, 1_int64, 'ELF version changed')
        call assert_u64(bytes, 25, int(z'100B0', int64), 'ELF entry changed')
        call assert_u64(bytes, 33, 64_int64, 'ELF program-header offset changed')
        call assert_u64(bytes, 41, 208_int64, 'ELF section-header offset changed')
        call assert_u16(bytes, 53, 64_int64, 'ELF header size changed')
        call assert_u16(bytes, 55, 56_int64, 'ELF program-header size changed')
        call assert_u16(bytes, 57, 2_int64, 'ELF program-header count changed')
        call assert_u16(bytes, 59, 64_int64, 'ELF section-header size changed')
        call assert_u16(bytes, 61, 3_int64, 'ELF section-header count changed')
        call assert_u16(bytes, 63, 2_int64, 'ELF string-table index changed')
        call assert_u32(bytes, 65, int(z'70000003', int64), 'RISC-V flags changed')
        call assert_u32(bytes, 69, 4_int64, 'ELF alignment changed')
        call assert_u64(bytes, 137, int(z'10000', int64), &
            'ELF segment virtual address changed')
        call assert_u64(bytes, 145, int(z'10000', int64), &
            'ELF segment physical address changed')
        call assert_u64(bytes, 153, 188_int64, 'ELF segment file size changed')
        call assert_u64(bytes, 161, 188_int64, 'ELF segment memory size changed')
        call assert_u64(bytes, 169, int(z'1000', int64), &
            'ELF segment alignment changed')
        call assert_u32(bytes, 177, int(z'00000513', int64), 'addi a0 encoding changed')
        call assert_u32(bytes, 181, int(z'05D00893', int64), 'addi a7 encoding changed')
        call assert_u32(bytes, 185, int(z'00000073', int64), 'ecall encoding changed')
    end subroutine check_elf_oracle

    subroutine assert_byte(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index, expected
        character(len=*), intent(in) :: message

        if (iand(int(bytes(index), int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_u16(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index
        integer(int64), intent(in) :: expected
        character(len=*), intent(in) :: message
        integer(int64) :: actual

        actual = int(iand(int(bytes(index), int32), 255_int32), int64)
        actual = actual + ishft(int(iand(int(bytes(index + 1), int32), 255_int32), &
            int64), 8)
        if (actual /= expected) error stop message
    end subroutine assert_u16

    subroutine assert_u32(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index
        integer(int64), intent(in) :: expected
        character(len=*), intent(in) :: message
        integer(int64) :: actual
        integer :: offset

        actual = 0_int64
        do offset = 0, 3
            actual = actual + ishft(int(iand(int(bytes(index + offset), int32), &
                255_int32), int64), 8 * offset)
        end do
        if (actual /= expected) error stop message
    end subroutine assert_u32

    subroutine assert_u64(bytes, index, expected, message)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: index
        integer(int64), intent(in) :: expected
        character(len=*), intent(in) :: message
        integer(int64) :: actual
        integer :: offset

        actual = 0_int64
        do offset = 0, 7
            actual = actual + ishft(int(iand(int(bytes(index + offset), int32), &
                255_int32), int64), 8 * offset)
        end do
        if (actual /= expected) error stop message
    end subroutine assert_u64

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_exists(path, message)
        character(len=*), intent(in) :: path, message
        logical :: exists

        inquire (file=trim(path), exist=exists)
        if (.not. exists) error stop message
    end subroutine assert_exists

    subroutine assert_absent(path, message)
        character(len=*), intent(in) :: path, message
        logical :: exists

        inquire (file=trim(path), exist=exists)
        if (exists) error stop message
    end subroutine assert_absent

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        logical :: exists
        integer :: io_status, unit

        inquire (file=trim(path), exist=exists)
        if (.not. exists) return
        open (newunit=unit, file=trim(path), status='old', action='readwrite', &
            iostat=io_status)
        if (io_status /= 0) error stop 'cannot open stale test artifact'
        close (unit, status='delete', iostat=io_status)
        if (io_status /= 0) error stop 'cannot remove stale test artifact'
    end subroutine remove_file

    subroutine assert_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

    subroutine assert_status(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_status

end program test_dispatch_l2_observable_trace
