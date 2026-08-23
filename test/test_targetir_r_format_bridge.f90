program test_targetir_r_format_bridge
    use iso_fortran_env, only: int32, int64
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        write_mir_v0_riscv_linux
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: make_source_ref, make_target_ir, source_ref_t, target_ir_t
    use fortback_targetir_codec, only: targetir_encode_record
    use fortback_targetir_encoding, only: normalize_riscv_r_record, &
        targetir_encoding_invalid_target, targetir_encoding_malformed, targetir_encoding_ok, &
        targetir_encoding_record_t, targetir_encoding_unsupported
    implicit none

    type(source_ref_t) :: source
    type(target_ir_t) :: target, bad_target
    type(riscv_opcode_record_t) :: record, bad_record
    type(targetir_encoding_record_t) :: normalized
    type(riscv_linux_artifact_t) :: artifact
    integer(int64) :: word
    integer(int32) :: status
    character(len=8192) :: input, mutated
    character(len=256) :: diagnostic
    character(len=2) :: output
    integer :: command_status, exit_status, io_status, unit
    character(len=*), parameter :: elf_path = '/tmp/fortback-targetir-r-format.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-targetir-r-format.out'

    source = make_source_ref('riscv-opcodes', 'rv_i', 'r-format-source-hash', 'IMPORTED')
    target = make_target_ir('riscv64', 64_int32, .true., &
        make_source_ref('riscv-isa', 'unprivileged', 'target-source-hash', 'IMPORTED'))
    record = riscv_opcode_record_t('add', 'R', int(z'00000033', int64), &
        int(z'FE00707F', int64), source)

    call normalize_riscv_r_record(target, record, normalized, status)
    call assert_int(status, targetir_encoding_ok, 'R-format normalization failed')
    call assert_field(normalized, 1_int32, 7_int32, 5_int32)
    call assert_field(normalized, 2_int32, 15_int32, 5_int32)
    call assert_field(normalized, 3_int32, 20_int32, 5_int32)
    call assert_equal(trim(normalized%source%source_hash), 'r-format-source-hash', &
        'normalized record provenance changed')
    call assert_equal(trim(normalized%target%source%source_hash), 'target-source-hash', &
        'normalized target provenance changed')
    call targetir_encode_record(target, normalized, [10_int64, 10_int64, 10_int64], word, status)
    call assert_int(status, targetir_encoding_ok, 'R-format TargetIR encode failed')
    call assert64(word, int(z'00A50533', int64), 'add x10,x10,x10 encoding changed')

    bad_record = record
    bad_record%format = 'I'
    call normalize_riscv_r_record(target, bad_record, normalized, status)
    call assert_int(status, targetir_encoding_unsupported, 'wrong R-format accepted')
    call assert_empty(normalized, 'wrong format left output')

    bad_target = target
    bad_target%word_bits = 32_int32
    call normalize_riscv_r_record(bad_target, record, normalized, status)
    call assert_int(status, targetir_encoding_invalid_target, 'wrong target width accepted')
    call assert_empty(normalized, 'wrong target width left output')
    bad_target = target
    bad_target%little_endian = .false.
    call normalize_riscv_r_record(bad_target, record, normalized, status)
    call assert_int(status, targetir_encoding_invalid_target, 'wrong endianness accepted')
    bad_record = record
    bad_record%source = source_ref_t()
    call normalize_riscv_r_record(target, bad_record, normalized, status)
    call assert_int(status, targetir_encoding_malformed, 'missing provenance accepted')
    call assert_empty(normalized, 'missing provenance left output')
    bad_record = record
    bad_record%mask = ior(record%mask, int(z'00000080', int64))
    call normalize_riscv_r_record(target, bad_record, normalized, status)
    call assert_int(status, targetir_encoding_malformed, 'overlapping fixed field accepted')
    call assert_empty(normalized, 'overlapping field left output')

    word = int(z'12345678', int64)
    call targetir_encode_record(target, normalized, [10_int64, 10_int64, 10_int64], word, status)
    call assert_int(status, targetir_encoding_invalid_target, 'cleared record accepted')
    call assert64(word, 0_int64, 'failed TargetIR encode left word')
    call normalize_riscv_r_record(target, record, normalized, status)
    call targetir_encode_record(target, normalized, [10_int64, 10_int64], word, status)
    call assert_int(status, targetir_encoding_malformed, 'wrong operand count accepted')
    call assert64(word, 0_int64, 'wrong operand count left word')
    call targetir_encode_record(target, normalized, [32_int64, 10_int64, 10_int64], word, status)
    call assert_int(status, targetir_encoding_malformed, 'out-of-range register accepted')
    call assert64(word, 0_int64, 'out-of-range register left word')

    input = initialized_variable_z_add_input(3)
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_int(status, mir_v0_bridge_ok, 'z+z compilation failed: '//trim(diagnostic))
    call write_mir_v0_riscv_linux(input, elf_path, status, diagnostic)
    call assert_int(status, mir_v0_bridge_ok, 'z+z ELF write failed: '//trim(diagnostic))
    call execute_command_line('chmod 755 -- '//elf_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0_int32, 'z+z chmod command failed')
    call assert_int(exit_status, 0_int32, 'z+z chmod failed')
    call execute_command_line('qemu-riscv64 '//elf_path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0_int32, 'z+z QEMU command failed')
    call assert_int(exit_status, 0_int32, 'z+z QEMU failed')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0_int32, 'z+z output missing')
    read (unit, iostat=io_status) output
    call assert_int(io_status, 0_int32, 'z+z output read failed')
    call assert_equal(output, '6'//achar(10), 'z+z QEMU output changed')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0_int32, 'z+z output cleanup failed')

    mutated = input
    call replace_token(mutated, '(opcode add)', '(opcode sub)')
    call compile_mir_v0_riscv_linux(mutated, artifact, status, diagnostic)
    call assert_int(status, mir_v0_bridge_out_of_scope, 'mutated z+z operation accepted')
    write (*, '(a)') 'TargetIR R-format bridge and z+z QEMU checks: ok'

contains

    function initialized_variable_z_add_input(initializer) result(value)
        integer, intent(in) :: initializer
        character(len=8192) :: value

        value = '(mir-function (name main) (entry-block 0) (instruction-count 9) '// &
            '(instructions (instruction (id 0) (opcode const) (literal '// &
            int_text(initializer)//') (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 0) (kind integer) (type i32))) (instruction (id 1) '// &
            '(opcode store) (storage-key z) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 1) (kind integer) (type i32))) (instruction (id 2) '// &
            '(opcode load) (storage-key z) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 2) (kind integer) (type i32))) (instruction (id 3) '// &
            '(opcode load) (storage-key z) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 3) (kind integer) (type i32))) (instruction (id 4) '// &
            '(opcode add) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 5) '// &
            '(opcode store) (storage-key z) (source-rule frontend-ast-v2/execution-part) '// &
            '(result (id 4) (kind integer) (type i32))) (instruction (id 6) '// &
            '(opcode load) (storage-key z) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 7) '// &
            '(opcode output) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32))) (instruction (id 8) '// &
            '(opcode return) (source-rule frontend-ast-v2/print-stmt) '// &
            '(result (id 6) (kind integer) (type i32)))))'
    end function initialized_variable_z_add_input

    function int_text(number) result(value)
        integer, intent(in) :: number
        character(len=32) :: value

        write (value, '(i0)') number
    end function int_text

    subroutine replace_token(value, old_token, new_token)
        character(len=*), intent(inout) :: value
        character(len=*), intent(in) :: old_token, new_token
        integer :: offset

        offset = index(value, old_token)
        if (offset == 0) error stop 'test mutation token missing'
        value(offset:offset + len_trim(old_token) - 1) = new_token
    end subroutine replace_token

    subroutine assert_int(actual, expected, message)
        integer, intent(in) :: actual
        integer, intent(in) :: expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert_int

    subroutine assert64(actual, expected, message)
        integer(int64), intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) error stop message
    end subroutine assert64

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message

        if (actual /= expected) error stop message
    end subroutine assert_equal

    subroutine assert_field(record, ordinal, start, width)
        type(targetir_encoding_record_t), intent(in) :: record
        integer(int32), intent(in) :: ordinal, start, width

        if (record%variable_fields(ordinal)%ordinal /= ordinal) error stop 'field ordinal changed'
        if (record%variable_fields(ordinal)%start /= start) error stop 'field start changed'
        if (record%variable_fields(ordinal)%width /= width) error stop 'field width changed'
    end subroutine assert_field

    subroutine assert_empty(record, message)
        type(targetir_encoding_record_t), intent(in) :: record
        character(len=*), intent(in) :: message

        if (len_trim(record%operation_id) /= 0) error stop message
        if (record%word_bits /= 0_int32) error stop message
        if (record%variable_field_count /= 0_int32) error stop message
    end subroutine assert_empty

end program test_targetir_r_format_bridge
