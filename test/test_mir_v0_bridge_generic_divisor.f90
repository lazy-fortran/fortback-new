program test_mir_v0_bridge_generic_divisor
    use iso_fortran_env, only: int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        riscv_linux_artifact_provenance_valid, write_mir_v0_riscv_linux
    implicit none

    type(riscv_linux_artifact_t) :: artifact
    character(len=8192) :: input, mutated
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit
    character(len=*), parameter :: elf_path = '/tmp/fortback-mir-v0-generic-divisor.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-mir-v0-generic-divisor.out'

    call assert_qemu(42, 2, '21'//achar(10))
    call assert_qemu(-42, 10, '-4'//achar(10))

    input = initialized_div_input(42, 0)
    call assert_rejected(input, 'divisor literal below the accepted bound was accepted')
    input = initialized_div_input(42, 11)
    call assert_rejected(input, 'divisor literal above the accepted bound was accepted')

    input = initialized_div_input(42, 2)
    mutated = input
    call replace_token(mutated, 'opcode div', 'opcode mul')
    call assert_rejected(mutated, 'unsupported divisor opcode mutation was accepted')
    mutated = input
    call replace_token(mutated, 'opcode store', 'opcode add')
    call assert_rejected(mutated, 'divisor storage opcode mutation was accepted')
    mutated = input
    call replace_token(mutated, 'storage-key x', 'storage-key y')
    call assert_rejected(mutated, 'divisor storage key mutation was accepted')
    mutated = input
    call replace_nth_token(mutated, 'source-rule frontend-ast-v2/print-stmt', &
        'source-rule frontend-ast-v2/unknown', 2)
    call assert_rejected(mutated, 'divisor source-rule mutation was accepted')
    write (*, '(a)') 'MIR-v0 generic initialized divisor QEMU checks: ok'

contains

    subroutine assert_qemu(left, divisor, expected)
        integer, intent(in) :: left, divisor
        character(len=*), intent(in) :: expected
        character(len=16) :: bytes
        integer :: byte_index

        input = initialized_div_input(left, divisor)
        call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, &
            'accepted initialized divisor was rejected')
        call assert_true(len_trim(diagnostic) == 0, &
            'accepted initialized divisor returned a diagnostic')
        call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
            'initialized divisor artifact provenance is invalid')
        call write_mir_v0_riscv_linux(input, elf_path, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_ok, &
            'initialized divisor ELF write failed')
        call execute_command_line('chmod 755 -- '//elf_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized divisor chmod failed')
        call assert_equal(exit_status, 0, 'initialized divisor chmod returned failure')
        call execute_command_line('qemu-riscv64 '//elf_path//' > '//output_path, wait=.true., &
            exitstat=exit_status, cmdstat=command_status)
        call assert_equal(command_status, 0, 'initialized divisor QEMU command failed')
        call assert_equal(exit_status, 0, 'initialized divisor QEMU returned failure')
        open (newunit=unit, file=output_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized divisor output was not written')
        read (unit, iostat=io_status) bytes(1:len(expected))
        call assert_equal(io_status, 0, 'initialized divisor output read failed')
        do byte_index = 1, len(expected)
            call assert_byte(bytes(byte_index:byte_index), expected(byte_index:byte_index), &
                'initialized divisor output changed')
        end do
        read (unit, iostat=io_status) bytes(1:1)
        call assert_true(io_status /= 0, 'initialized divisor wrote extra output')
        close (unit, status='delete', iostat=io_status)
        call assert_equal(io_status, 0, 'initialized divisor output cleanup failed')
    end subroutine assert_qemu

    subroutine assert_rejected(value, message)
        character(len=*), intent(in) :: value, message

        call compile_mir_v0_riscv_linux(value, artifact, status, diagnostic)
        call assert_equal(status, mir_v0_bridge_out_of_scope, message)
        call assert_true(len_trim(diagnostic) > 0, &
            'rejected initialized divisor returned no diagnostic')
    end subroutine assert_rejected

    subroutine replace_token(value, old_token, new_token)
        character(len=*), intent(inout) :: value
        character(len=*), intent(in) :: old_token, new_token
        integer :: offset

        call assert_true(len_trim(new_token) <= len_trim(old_token), &
            'test mutation replacement token is too long')
        offset = index(value, trim(old_token))
        call assert_true(offset > 0, 'test mutation token was not found')
        value(offset:offset + len_trim(old_token) - 1) = new_token
    end subroutine replace_token

    subroutine replace_nth_token(value, old_token, new_token, occurrence)
        character(len=*), intent(inout) :: value
        character(len=*), intent(in) :: old_token, new_token
        integer, intent(in) :: occurrence
        integer :: offset, search_from, found_count

        call assert_true(len_trim(new_token) <= len_trim(old_token), &
            'test nth mutation replacement token is too long')
        search_from = 1
        found_count = 0
        do while (search_from <= len(value))
            offset = index(value(search_from:), trim(old_token))
            if (offset == 0) exit
            offset = search_from + offset - 1
            found_count = found_count + 1
            if (found_count == occurrence) then
                value(offset:offset + len_trim(old_token) - 1) = new_token
                return
            end if
            search_from = offset + len_trim(old_token)
        end do
        call assert_true(.false., 'test nth mutation token was not found')
    end subroutine replace_nth_token

    function initialized_div_input(left, divisor) result(value)
        integer, intent(in) :: left, divisor
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 9) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '//int_text(left)//') '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 0) (kind integer) (type i32))) '// &
            '(instruction (id 1) (opcode store) (storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) (opcode load) '// &
            '(storage-key x) (source-rule frontend-ast-v2/execution-part) (result (id 2) (kind integer) (type i32))) '// &
            '(instruction (id 3) (opcode const) (literal '//int_text(divisor)//') '// &
            '(source-rule frontend-ast-v2/execution-part) (result (id 3) (kind integer) (type i32))) '// &
            '(instruction (id 4) (opcode div) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 5) (opcode store) '// &
            '(storage-key x) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 6) (opcode load) '// &
            '(storage-key x) (source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32))) '// &
            '(instruction (id 7) (opcode output) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 8) (opcode return) '// &
            '(source-rule frontend-ast-v2/print-stmt) (result (id 6) (kind integer) (type i32)))))'
    end function initialized_div_input

    function int_text(number) result(value)
        integer, intent(in) :: number
        character(len=32) :: value

        write (value, '(i0)') number
    end function int_text

    subroutine assert_equal(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_byte(actual, expected, message)
        character(len=1), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_byte

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop message
    end subroutine assert_true

end program test_mir_v0_bridge_generic_divisor
