program test_riscv_generated_table
    use iso_fortran_env, only: int32
    use fortback_riscv_opcode_table, only: riscv_kind_for_mnemonic, &
        riscv_mnemonic_for_kind
    implicit none

    character(len=5), parameter :: names(3) = [character(len=5) :: 'add', 'slti', 'xori']
    integer(int32), parameter :: kinds(3) = [1_int32, 14_int32, 16_int32]
    integer :: i

    do i = 1, size(names)
        call assert_equal(riscv_kind_for_mnemonic(names(i)), kinds(i), 'forward table lookup')
        call assert_equal_text(trim(riscv_mnemonic_for_kind(kinds(i))), trim(names(i)), &
            'reverse table lookup')
    end do
    call assert_equal(riscv_kind_for_mnemonic('not-an-instruction'), 0_int32, &
        'unknown mnemonic accepted')
    call assert_equal_text(trim(riscv_mnemonic_for_kind(99_int32)), '', 'unknown kind accepted')
    write (*, '(a)') 'RISC-V generated opcode table checks: ok'

contains

    subroutine assert_equal(actual, expected, message)
        integer(int32), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_equal_text(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal_text

end program test_riscv_generated_table
