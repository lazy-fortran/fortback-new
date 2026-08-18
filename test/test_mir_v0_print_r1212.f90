program test_mir_v0_print_r1212
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        riscv_linux_artifact_provenance_valid, write_mir_v0_riscv_linux
    implicit none

    character(len=4096) :: input, input_two, input_three, input_four, input_five, input_six, input_seven
    character(len=4096) :: wrong_literal, wrong_shape, wrong_opcode
    character(len=4096) :: wrong_two_literal, wrong_two_shape, wrong_two_opcode
    character(len=4096) :: wrong_three_literal, wrong_three_shape, wrong_three_opcode
    character(len=4096) :: wrong_four_literal, wrong_four_shape, wrong_four_opcode
    character(len=4096) :: wrong_five_literal, wrong_five_shape, wrong_five_opcode
    character(len=4096) :: wrong_six_literal, wrong_six_shape, wrong_six_opcode
    character(len=4096) :: wrong_seven_literal, wrong_seven_shape, wrong_seven_opcode
    character(len=256) :: diagnostic
    character(len=*), parameter :: path = '/tmp/fortback-print-r1212.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-print-r1212.out'
    integer(int8) :: output(2), output_two(4), output_three(6), output_four(9), output_five(12), output_six(15), output_seven(18)
    type(riscv_linux_artifact_t) :: artifact
    integer(int32) :: status
    integer :: command_status, exit_status, io_status, unit

    input = '(mir-function (name p) (entry-block 0) (instruction-count 3) '// &
        '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 2) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'PRINT MIR was rejected')
    call write_mir_v0_riscv_linux(input, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'PRINT ELF write failed')
    call execute_command_line('chmod 755 -- '//path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'PRINT chmod command failed')
    call assert_int(exit_status, 0, 'PRINT chmod failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'PRINT qemu command failed')
    call assert_int(exit_status, 0, 'PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'PRINT output was not written')
    read (unit, iostat=io_status) output
    call assert_int(io_status, 0, 'PRINT output length or bytes changed')
    call assert_byte(output(1), 55, 'PRINT did not write ASCII 7')
    call assert_byte(output(2), 10, 'PRINT did not write exactly one newline')
    read (unit, iostat=io_status) output(1)
    call assert_true(io_status /= 0, 'PRINT wrote bytes beyond 7 and newline')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'PRINT output cleanup failed')

    input_two = '(mir-function (name p) (entry-block 0) (instruction-count 5) '// &
        '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 2) (opcode const) (literal 8) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 3) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 4) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_two, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'two-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_two, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'two-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'two-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'two-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'two-item PRINT output was not written')
    read (unit, iostat=io_status) output_two
    call assert_int(io_status, 0, 'two-item PRINT output length or bytes changed')
    call assert_byte(output_two(1), 55, 'two-item PRINT did not write ASCII 7')
    call assert_byte(output_two(2), 10, 'two-item PRINT did not write newline after 7')
    call assert_byte(output_two(3), 56, 'two-item PRINT did not write ASCII 8')
    call assert_byte(output_two(4), 10, 'two-item PRINT did not write newline after 8')
    read (unit, iostat=io_status) output_two(1)
    call assert_true(io_status /= 0, 'two-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'two-item PRINT output cleanup failed')

    wrong_two_literal = input_two
    wrong_two_literal(index(wrong_two_literal, 'literal 8'):index(wrong_two_literal, 'literal 8') + 8) = &
        'literal 9'
    call compile_mir_v0_riscv_linux(wrong_two_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'two-item PRINT literal mutation was accepted')

    wrong_two_shape = input_two
    wrong_two_shape(index(wrong_two_shape, 'type i32', back=.true.): &
        index(wrong_two_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_two_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'two-item PRINT result-shape mutation was accepted')

    wrong_two_opcode = input_two
    wrong_two_opcode(index(wrong_two_opcode, 'opcode output', back=.true.): &
        index(wrong_two_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_two_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'two-item PRINT opcode mutation was accepted')

    input_three = '(mir-function (name p) (entry-block 0) (instruction-count 7) '// &
        '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 2) (opcode const) (literal 8) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 3) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 4) (opcode const) (literal 9) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 5) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 6) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_three, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'three-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'three-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_three, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'three-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'three-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'three-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'three-item PRINT output was not written')
    read (unit, iostat=io_status) output_three
    call assert_int(io_status, 0, 'three-item PRINT output length or bytes changed')
    call assert_byte(output_three(1), 55, 'three-item PRINT did not write ASCII 7')
    call assert_byte(output_three(2), 10, 'three-item PRINT missed newline after 7')
    call assert_byte(output_three(3), 56, 'three-item PRINT did not write ASCII 8')
    call assert_byte(output_three(4), 10, 'three-item PRINT missed newline after 8')
    call assert_byte(output_three(5), 57, 'three-item PRINT did not write ASCII 9')
    call assert_byte(output_three(6), 10, 'three-item PRINT missed newline after 9')
    read (unit, iostat=io_status) output_three(1)
    call assert_true(io_status /= 0, 'three-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'three-item PRINT output cleanup failed')

    input_four = '(mir-function (name p) (entry-block 0) (instruction-count 9) '// &
        '(instructions (instruction (id 0) (opcode const) (literal 7) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 2) (opcode const) (literal 8) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 3) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 4) (opcode const) (literal 9) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 5) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 6) (opcode const) (literal 10) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 7) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 8) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_four, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'four-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'four-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_four, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'four-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'four-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'four-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'four-item PRINT output was not written')
    read (unit, iostat=io_status) output_four
    call assert_int(io_status, 0, 'four-item PRINT output length or bytes changed')
    call assert_byte(output_four(1), 55, 'four-item PRINT did not write ASCII 7')
    call assert_byte(output_four(2), 10, 'four-item PRINT missed newline after 7')
    call assert_byte(output_four(3), 56, 'four-item PRINT did not write ASCII 8')
    call assert_byte(output_four(4), 10, 'four-item PRINT missed newline after 8')
    call assert_byte(output_four(5), 57, 'four-item PRINT did not write ASCII 9')
    call assert_byte(output_four(6), 10, 'four-item PRINT missed newline after 9')
    call assert_byte(output_four(7), 49, 'four-item PRINT did not write ASCII 1 of 10')
    call assert_byte(output_four(8), 48, 'four-item PRINT did not write ASCII 0 of 10')
    call assert_byte(output_four(9), 10, 'four-item PRINT missed newline after 10')
    read (unit, iostat=io_status) output_four(1)
    call assert_true(io_status /= 0, 'four-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'four-item PRINT output cleanup failed')

    input_five = input_four
    input_five(index(input_five, 'instruction-count 9'): &
        index(input_five, 'instruction-count 9') + 20) = 'instruction-count 11)'
    input_five = input_five(:index(input_five, '(instruction (id 8)') - 1)// &
        '(instruction (id 8) (opcode const) (literal 11) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 9) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 10) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_five, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'five-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'five-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_five, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'five-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'five-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'five-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'five-item PRINT output was not written')
    read (unit, iostat=io_status) output_five
    call assert_int(io_status, 0, 'five-item PRINT output length or bytes changed')
    call assert_byte(output_five(1), 55, 'five-item PRINT did not write ASCII 7')
    call assert_byte(output_five(2), 10, 'five-item PRINT missed newline after 7')
    call assert_byte(output_five(3), 56, 'five-item PRINT did not write ASCII 8')
    call assert_byte(output_five(4), 10, 'five-item PRINT missed newline after 8')
    call assert_byte(output_five(5), 57, 'five-item PRINT did not write ASCII 9')
    call assert_byte(output_five(6), 10, 'five-item PRINT missed newline after 9')
    call assert_byte(output_five(7), 49, 'five-item PRINT did not write ASCII 1 of 10')
    call assert_byte(output_five(8), 48, 'five-item PRINT did not write ASCII 0 of 10')
    call assert_byte(output_five(9), 10, 'five-item PRINT missed newline after 10')
    call assert_byte(output_five(10), 49, 'five-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_five(11), 49, 'five-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_five(12), 10, 'five-item PRINT missed newline after 11')
    read (unit, iostat=io_status) output_five(1)
    call assert_true(io_status /= 0, 'five-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'five-item PRINT output cleanup failed')

    input_six = input_five
    input_six(index(input_six, 'instruction-count 11'): &
        index(input_six, 'instruction-count 11') + 20) = 'instruction-count 13)'
    input_six = input_six(:index(input_six, '(instruction (id 10)') - 1)// &
        '(instruction (id 10) (opcode const) (literal 12) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 11) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 12) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_six, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'six-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'six-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_six, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'six-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'six-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'six-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'six-item PRINT output was not written')
    read (unit, iostat=io_status) output_six
    call assert_int(io_status, 0, 'six-item PRINT output length or bytes changed')
    call assert_byte(output_six(1), 55, 'six-item PRINT did not write ASCII 7')
    call assert_byte(output_six(2), 10, 'six-item PRINT missed newline after 7')
    call assert_byte(output_six(3), 56, 'six-item PRINT did not write ASCII 8')
    call assert_byte(output_six(4), 10, 'six-item PRINT missed newline after 8')
    call assert_byte(output_six(5), 57, 'six-item PRINT did not write ASCII 9')
    call assert_byte(output_six(6), 10, 'six-item PRINT missed newline after 9')
    call assert_byte(output_six(7), 49, 'six-item PRINT did not write ASCII 1 of 10')
    call assert_byte(output_six(8), 48, 'six-item PRINT did not write ASCII 0 of 10')
    call assert_byte(output_six(9), 10, 'six-item PRINT missed newline after 10')
    call assert_byte(output_six(10), 49, 'six-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_six(11), 49, 'six-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_six(12), 10, 'six-item PRINT missed newline after 11')
    call assert_byte(output_six(13), 49, 'six-item PRINT did not write ASCII 1 of 12')
    call assert_byte(output_six(14), 50, 'six-item PRINT did not write ASCII 2 of 12')
    call assert_byte(output_six(15), 10, 'six-item PRINT missed newline after 12')
    read (unit, iostat=io_status) output_six(1)
    call assert_true(io_status /= 0, 'six-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'six-item PRINT output cleanup failed')

    input_seven = input_six
    input_seven(index(input_seven, 'instruction-count 13'): &
        index(input_seven, 'instruction-count 13') + 20) = 'instruction-count 15)'
    input_seven = input_seven(:index(input_seven, '(instruction (id 12)') - 1)// &
        '(instruction (id 12) (opcode const) (literal 13) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 13) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 14) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_seven, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'seven-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'seven-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_seven, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'seven-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'seven-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'seven-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'seven-item PRINT output was not written')
    read (unit, iostat=io_status) output_seven
    call assert_int(io_status, 0, 'seven-item PRINT output length or bytes changed')
    call assert_byte(output_seven(1), 55, 'seven-item PRINT did not write ASCII 7')
    call assert_byte(output_seven(2), 10, 'seven-item PRINT missed newline after 7')
    call assert_byte(output_seven(3), 56, 'seven-item PRINT did not write ASCII 8')
    call assert_byte(output_seven(4), 10, 'seven-item PRINT missed newline after 8')
    call assert_byte(output_seven(5), 57, 'seven-item PRINT did not write ASCII 9')
    call assert_byte(output_seven(6), 10, 'seven-item PRINT missed newline after 9')
    call assert_byte(output_seven(7), 49, 'seven-item PRINT did not write ASCII 1 of 10')
    call assert_byte(output_seven(8), 48, 'seven-item PRINT did not write ASCII 0 of 10')
    call assert_byte(output_seven(9), 10, 'seven-item PRINT missed newline after 10')
    call assert_byte(output_seven(10), 49, 'seven-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_seven(11), 49, 'seven-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_seven(12), 10, 'seven-item PRINT missed newline after 11')
    call assert_byte(output_seven(13), 49, 'seven-item PRINT did not write ASCII 1 of 12')
    call assert_byte(output_seven(14), 50, 'seven-item PRINT did not write ASCII 2 of 12')
    call assert_byte(output_seven(15), 10, 'seven-item PRINT missed newline after 12')
    call assert_byte(output_seven(16), 49, 'seven-item PRINT did not write ASCII 1 of 13')
    call assert_byte(output_seven(17), 51, 'seven-item PRINT did not write ASCII 3 of 13')
    call assert_byte(output_seven(18), 10, 'seven-item PRINT missed newline after 13')
    read (unit, iostat=io_status) output_seven(1)
    call assert_true(io_status /= 0, 'seven-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'seven-item PRINT output cleanup failed')

    wrong_seven_literal = input_seven
    wrong_seven_literal(index(wrong_seven_literal, 'literal 13'): &
        index(wrong_seven_literal, 'literal 13') + 9) = 'literal 14'
    call compile_mir_v0_riscv_linux(wrong_seven_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'seven-item PRINT literal mutation was accepted')

    wrong_seven_shape = input_seven
    wrong_seven_shape(index(wrong_seven_shape, 'type i32', back=.true.): &
        index(wrong_seven_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_seven_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'seven-item PRINT result-shape mutation was accepted')

    wrong_seven_opcode = input_seven
    wrong_seven_opcode(index(wrong_seven_opcode, 'opcode output', back=.true.): &
        index(wrong_seven_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_seven_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'seven-item PRINT opcode mutation was accepted')

    wrong_six_literal = input_six
    wrong_six_literal(index(wrong_six_literal, 'literal 12'): &
        index(wrong_six_literal, 'literal 12') + 9) = 'literal 13'
    call compile_mir_v0_riscv_linux(wrong_six_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'six-item PRINT literal mutation was accepted')

    wrong_six_shape = input_six
    wrong_six_shape(index(wrong_six_shape, 'type i32', back=.true.): &
        index(wrong_six_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_six_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'six-item PRINT result-shape mutation was accepted')

    wrong_six_opcode = input_six
    wrong_six_opcode(index(wrong_six_opcode, 'opcode output', back=.true.): &
        index(wrong_six_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_six_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'six-item PRINT opcode mutation was accepted')

    wrong_five_literal = input_five
    wrong_five_literal(index(wrong_five_literal, 'literal 11'): &
        index(wrong_five_literal, 'literal 11') + 9) = 'literal 12'
    call compile_mir_v0_riscv_linux(wrong_five_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'five-item PRINT literal mutation was accepted')

    wrong_five_shape = input_five
    wrong_five_shape(index(wrong_five_shape, 'type i32', back=.true.): &
        index(wrong_five_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_five_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'five-item PRINT result-shape mutation was accepted')

    wrong_five_opcode = input_five
    wrong_five_opcode(index(wrong_five_opcode, 'opcode output', back=.true.): &
        index(wrong_five_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_five_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'five-item PRINT opcode mutation was accepted')

    wrong_four_literal = input_four
    wrong_four_literal(index(wrong_four_literal, 'literal 10'): &
        index(wrong_four_literal, 'literal 10') + 9) = 'literal 11'
    call compile_mir_v0_riscv_linux(wrong_four_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'four-item PRINT literal mutation was accepted')

    wrong_four_shape = input_four
    wrong_four_shape(index(wrong_four_shape, 'type i32', back=.true.): &
        index(wrong_four_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_four_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'four-item PRINT result-shape mutation was accepted')

    wrong_four_opcode = input_four
    wrong_four_opcode(index(wrong_four_opcode, 'opcode output', back=.true.): &
        index(wrong_four_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_four_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'four-item PRINT opcode mutation was accepted')

    wrong_three_literal = input_three
    wrong_three_literal(index(wrong_three_literal, 'literal 9'): &
        index(wrong_three_literal, 'literal 9') + 8) = 'literal 6'
    call compile_mir_v0_riscv_linux(wrong_three_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'three-item PRINT literal mutation was accepted')

    wrong_three_shape = input_three
    wrong_three_shape(index(wrong_three_shape, 'type i32', back=.true.): &
        index(wrong_three_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_three_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'three-item PRINT result-shape mutation was accepted')

    wrong_three_opcode = input_three
    wrong_three_opcode(index(wrong_three_opcode, 'opcode output', back=.true.): &
        index(wrong_three_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_three_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'three-item PRINT opcode mutation was accepted')

    wrong_literal = input
    wrong_literal(index(wrong_literal, 'literal 7'):index(wrong_literal, 'literal 7') + 8) = &
        'literal 6'
    call compile_mir_v0_riscv_linux(wrong_literal, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'PRINT literal mutation was accepted')

    wrong_shape = input
    wrong_shape(index(wrong_shape, 'type i32'):index(wrong_shape, 'type i32') + 7) = &
        'type real'
    call compile_mir_v0_riscv_linux(wrong_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'PRINT type mutation was accepted')

    wrong_opcode = input
    wrong_opcode(index(wrong_opcode, 'opcode output'):index(wrong_opcode, 'opcode output') + 12) = &
        'opcode return '
    call compile_mir_v0_riscv_linux(wrong_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, 'PRINT output mutation was accepted')
    write (*, '(a)') 'MIR-v0 PRINT R1212 qemu checks: ok'

contains

    subroutine assert_byte(actual, expected, message)
        integer(int8), intent(in) :: actual
        integer, intent(in) :: expected
        character(len=*), intent(in) :: message

        if (iand(int(actual, int32), 255_int32) /= expected) error stop message
    end subroutine assert_byte

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

    subroutine assert_true(value, message)
        logical, intent(in) :: value
        character(len=*), intent(in) :: message

        if (.not. value) error stop message
    end subroutine assert_true

end program test_mir_v0_print_r1212
