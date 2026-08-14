program test_riscv_source
    use iso_fortran_env, only: int32, int64
    use fortback_riscv_source, only: import_riscv_opcodes, riscv_opcode_record_t, &
        riscv_source_ok
    use fortback_target_ir, only: make_source_ref, source_ref_t
    implicit none

    character(len=*), parameter :: source_text = &
        'add rd rs1 rs2 31..25=0 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'sub rd rs1 rs2 31..25=0x20 14..12=0 6..2=0x0C 1..0=3' // new_line('a') // &
        'or rd rs1 rs2 31..25=0 14..12=6 6..2=0x0C 1..0=3' // new_line('a') // &
        'addi rd rs1 imm12 14..12=0 6..2=0x04 1..0=3' // new_line('a')
    type(source_ref_t) :: source
    type(riscv_opcode_record_t) :: records(4)
    integer(int32) :: count, status

    source = make_source_ref('riscv-opcodes', 'rv_i', &
        '76a20ca8fb1b01c33a31f6ae4104f79914f9d3b90d2ba60f2ca493e9a46928b6', &
        'IMPORTED')
    call import_riscv_opcodes(source_text, source, records, count, status)
    call assert_equal_int(status, riscv_source_ok, 'canonical witness rejected')
    call assert_equal_int(count, 4_int32, 'bounded importer changed record count')
    call assert_equal(trim(records(1)%mnemonic), 'add', 'add was not normalized')
    call assert_equal(trim(records(2)%mnemonic), 'sub', 'sub was not normalized')
    call assert_equal(trim(records(3)%mnemonic), 'or', 'or was not normalized')
    call assert_equal(trim(records(4)%mnemonic), 'addi', 'addi was not normalized')
    call assert_equal64(records(1)%match, int(z'00000033', int64), 'add match changed')
    call assert_equal64(records(2)%match, int(z'40000033', int64), 'sub match changed')
    call assert_equal64(records(3)%match, int(z'00006033', int64), 'or match changed')
    call assert_equal64(records(4)%match, int(z'00000013', int64), 'addi match changed')
    call assert_equal64(records(1)%mask, int(z'FE00707F', int64), 'add mask changed')
    call assert_equal64(records(3)%mask, int(z'FE00707F', int64), 'or mask changed')
    call assert_equal64(records(4)%mask, int(z'0000707F', int64), 'addi mask changed')
    call assert_equal(trim(records(1)%source%artifact), 'riscv-opcodes', &
        'artifact provenance was lost')
    call assert_equal(trim(records(1)%source%object), 'rv_i', 'object provenance was lost')
    call assert_equal(trim(records(1)%source%source_hash), &
        trim(source%source_hash), 'source hash provenance was lost')
    call assert_equal(trim(records(3)%source%source_hash), &
        trim(source%source_hash), 'or source hash provenance was lost')
    call assert_equal(trim(records(1)%source%origin), 'IMPORTED', 'origin was lost')
    write (*, '(a)') 'RISC-V source importer checks: ok'

contains

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_equal_int(value, expected, message)
        integer(int32), intent(in) :: value, expected
        character(len=*), intent(in) :: message

        if (value /= expected) error stop message
    end subroutine assert_equal_int

    subroutine assert_equal64(value, expected, message)
        integer(int64), intent(in) :: value, expected
        character(len=*), intent(in) :: message

        if (value /= expected) error stop message
    end subroutine assert_equal64

end program test_riscv_source
