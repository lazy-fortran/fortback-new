program test_mir_v0_print_r1212
    use iso_fortran_env, only: int8, int32
    use fortback_mir_v0_riscv_linux, only: compile_mir_v0_riscv_linux, &
        mir_v0_bridge_ok, mir_v0_bridge_out_of_scope, riscv_linux_artifact_t, &
        riscv_linux_artifact_provenance_valid, write_mir_v0_riscv_linux
    implicit none

    character(len=4096) :: input, input_two, input_three, input_novel, input_four, input_five, input_six
    character(len=4096) :: input_seven, input_eight, input_nine, input_ten
    character(len=4096) :: wrong_shape, wrong_opcode
    character(len=4096) :: wrong_two_shape, wrong_two_opcode
    character(len=4096) :: wrong_three_shape, wrong_three_opcode
    character(len=4096) :: wrong_four_shape, wrong_four_opcode
    character(len=4096) :: wrong_five_shape, wrong_five_opcode
    character(len=4096) :: wrong_six_shape, wrong_six_opcode
    character(len=4096) :: wrong_seven_shape, wrong_seven_opcode
    character(len=4096) :: wrong_eight_shape, wrong_eight_opcode
    character(len=4096) :: wrong_nine_shape, wrong_nine_opcode
    character(len=4096) :: wrong_ten_shape, wrong_ten_opcode
    character(len=256) :: diagnostic
    character(len=*), parameter :: path = '/tmp/fortback-print-r1212.elf'
    character(len=*), parameter :: output_path = '/tmp/fortback-print-r1212.out'
    integer(int8) :: output(2), output_two(4), output_three(6), output_novel(9), output_four(9), output_five(12)
    integer(int8) :: output_six(15), output_seven(18), output_eight(21), output_nine(24)
    integer(int8) :: output_ten(27)
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

    input_novel = '(mir-function (name p) (entry-block 0) (instruction-count 7) '// &
        '(instructions (instruction (id 0) (opcode const) (literal 17) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 1) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 2) (opcode const) (literal 18) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 3) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 4) (opcode const) (literal 19) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 5) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 6) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_novel, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'novel PRINT MIR was rejected')
    call write_mir_v0_riscv_linux(input_novel, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'novel PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'novel PRINT qemu command failed')
    call assert_int(exit_status, 0, 'novel PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'novel PRINT output was not written')
    read (unit, iostat=io_status) output_novel
    call assert_int(io_status, 0, 'novel PRINT output length or bytes changed')
    call assert_byte(output_novel(1), 49, 'novel PRINT did not write ASCII 1 of 17')
    call assert_byte(output_novel(2), 55, 'novel PRINT did not write ASCII 7 of 17')
    call assert_byte(output_novel(3), 10, 'novel PRINT missed newline after 17')
    call assert_byte(output_novel(4), 49, 'novel PRINT did not write ASCII 1 of 18')
    call assert_byte(output_novel(5), 56, 'novel PRINT did not write ASCII 8 of 18')
    call assert_byte(output_novel(6), 10, 'novel PRINT missed newline after 18')
    call assert_byte(output_novel(7), 49, 'novel PRINT did not write ASCII 1 of 19')
    call assert_byte(output_novel(8), 57, 'novel PRINT did not write ASCII 9 of 19')
    call assert_byte(output_novel(9), 10, 'novel PRINT missed newline after 19')
    read (unit, iostat=io_status) output_novel(1)
    call assert_true(io_status /= 0, 'novel PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'novel PRINT output cleanup failed')

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

    input_eight = input_seven
    input_eight(index(input_eight, 'instruction-count 15'): &
        index(input_eight, 'instruction-count 15') + 20) = 'instruction-count 17)'
    input_eight = input_eight(:index(input_eight, '(instruction (id 14)') - 1)// &
        '(instruction (id 14) (opcode const) (literal 14) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 15) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 16) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_eight, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'eight-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'eight-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_eight, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'eight-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'eight-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'eight-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'eight-item PRINT output was not written')
    read (unit, iostat=io_status) output_eight
    call assert_int(io_status, 0, 'eight-item PRINT output length or bytes changed')
    call assert_byte(output_eight(1), 55, 'eight-item PRINT did not write ASCII 7')
    call assert_byte(output_eight(2), 10, 'eight-item PRINT missed newline after 7')
    call assert_byte(output_eight(3), 56, 'eight-item PRINT did not write ASCII 8')
    call assert_byte(output_eight(4), 10, 'eight-item PRINT missed newline after 8')
    call assert_byte(output_eight(5), 57, 'eight-item PRINT did not write ASCII 9')
    call assert_byte(output_eight(6), 10, 'eight-item PRINT missed newline after 9')
    call assert_byte(output_eight(7), 49, 'eight-item PRINT did not write ASCII 1 of 10')
    call assert_byte(output_eight(8), 48, 'eight-item PRINT did not write ASCII 0 of 10')
    call assert_byte(output_eight(9), 10, 'eight-item PRINT missed newline after 10')
    call assert_byte(output_eight(10), 49, 'eight-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_eight(11), 49, 'eight-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_eight(12), 10, 'eight-item PRINT missed newline after 11')
    call assert_byte(output_eight(13), 49, 'eight-item PRINT did not write ASCII 1 of 12')
    call assert_byte(output_eight(14), 50, 'eight-item PRINT did not write ASCII 2 of 12')
    call assert_byte(output_eight(15), 10, 'eight-item PRINT missed newline after 12')
    call assert_byte(output_eight(16), 49, 'eight-item PRINT did not write ASCII 1 of 13')
    call assert_byte(output_eight(17), 51, 'eight-item PRINT did not write ASCII 3 of 13')
    call assert_byte(output_eight(18), 10, 'eight-item PRINT missed newline after 13')
    call assert_byte(output_eight(19), 49, 'eight-item PRINT did not write ASCII 1 of 14')
    call assert_byte(output_eight(20), 52, 'eight-item PRINT did not write ASCII 4 of 14')
    call assert_byte(output_eight(21), 10, 'eight-item PRINT missed newline after 14')
    read (unit, iostat=io_status) output_eight(1)
    call assert_true(io_status /= 0, 'eight-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'eight-item PRINT output cleanup failed')

    input_nine = input_eight
    input_nine(index(input_nine, 'instruction-count 17'): &
        index(input_nine, 'instruction-count 17') + 20) = 'instruction-count 19)'
    input_nine = input_nine(:index(input_nine, '(instruction (id 16)') - 1)// &
        '(instruction (id 16) (opcode const) (literal 15) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 17) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 18) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_nine, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'nine-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'nine-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_nine, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'nine-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'nine-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'nine-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'nine-item PRINT output was not written')
    read (unit, iostat=io_status) output_nine
    call assert_int(io_status, 0, 'nine-item PRINT output length or bytes changed')
    call assert_byte(output_nine(1), 55, 'nine-item PRINT did not write ASCII 7')
    call assert_byte(output_nine(2), 10, 'nine-item PRINT missed newline after 7')
    call assert_byte(output_nine(3), 56, 'nine-item PRINT did not write ASCII 8')
    call assert_byte(output_nine(4), 10, 'nine-item PRINT missed newline after 8')
    call assert_byte(output_nine(5), 57, 'nine-item PRINT did not write ASCII 9')
    call assert_byte(output_nine(6), 10, 'nine-item PRINT missed newline after 9')
    call assert_byte(output_nine(7), 49, 'nine-item PRINT did not write ASCII 1 of 10')
    call assert_byte(output_nine(8), 48, 'nine-item PRINT did not write ASCII 0 of 10')
    call assert_byte(output_nine(9), 10, 'nine-item PRINT missed newline after 10')
    call assert_byte(output_nine(10), 49, 'nine-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_nine(11), 49, 'nine-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_nine(12), 10, 'nine-item PRINT missed newline after 11')
    call assert_byte(output_nine(13), 49, 'nine-item PRINT did not write ASCII 1 of 12')
    call assert_byte(output_nine(14), 50, 'nine-item PRINT did not write ASCII 2 of 12')
    call assert_byte(output_nine(15), 10, 'nine-item PRINT missed newline after 12')
    call assert_byte(output_nine(16), 49, 'nine-item PRINT did not write ASCII 1 of 13')
    call assert_byte(output_nine(17), 51, 'nine-item PRINT did not write ASCII 3 of 13')
    call assert_byte(output_nine(18), 10, 'nine-item PRINT missed newline after 13')
    call assert_byte(output_nine(19), 49, 'nine-item PRINT did not write ASCII 1 of 14')
    call assert_byte(output_nine(20), 52, 'nine-item PRINT did not write ASCII 4 of 14')
    call assert_byte(output_nine(21), 10, 'nine-item PRINT missed newline after 14')
    call assert_byte(output_nine(22), 49, 'nine-item PRINT did not write ASCII 1 of 15')
    call assert_byte(output_nine(23), 53, 'nine-item PRINT did not write ASCII 5 of 15')
    call assert_byte(output_nine(24), 10, 'nine-item PRINT missed newline after 15')
    read (unit, iostat=io_status) output_nine(1)
    call assert_true(io_status /= 0, 'nine-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'nine-item PRINT output cleanup failed')

    input_ten = input_nine
    input_ten(index(input_ten, 'instruction-count 19'): &
        index(input_ten, 'instruction-count 19') + 20) = 'instruction-count 21)'
    input_ten = input_ten(:index(input_ten, '(instruction (id 18)') - 1)// &
        '(instruction (id 18) (opcode const) (literal 16) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 19) (opcode output) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32))) (instruction (id 20) (opcode return) '// &
        '(source-rule frontend-ast-v2/print-stmt) (result (id 0) (kind integer) '// &
        '(type i32)))))'
    call compile_mir_v0_riscv_linux(input_ten, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'ten-item PRINT MIR was rejected')
    call assert_true(riscv_linux_artifact_provenance_valid(artifact), &
        'ten-item PRINT artifact provenance was lost')
    call write_mir_v0_riscv_linux(input_ten, path, status, diagnostic)
    call assert_status(status, mir_v0_bridge_ok, 'ten-item PRINT ELF write failed')
    call execute_command_line('qemu-riscv64 '//path//' > '//output_path, wait=.true., &
        exitstat=exit_status, cmdstat=command_status)
    call assert_int(command_status, 0, 'ten-item PRINT qemu command failed')
    call assert_int(exit_status, 0, 'ten-item PRINT artifact did not exit successfully')
    open (newunit=unit, file=output_path, access='stream', form='unformatted', &
        status='old', action='read', iostat=io_status)
    call assert_int(io_status, 0, 'ten-item PRINT output was not written')
    read (unit, iostat=io_status) output_ten
    call assert_int(io_status, 0, 'ten-item PRINT output length or bytes changed')
    call assert_byte(output_ten(1), 55, 'ten-item PRINT did not write ASCII 7')
    call assert_byte(output_ten(2), 10, 'ten-item PRINT missed newline after 7')
    call assert_byte(output_ten(3), 56, 'ten-item PRINT did not write ASCII 8')
    call assert_byte(output_ten(4), 10, 'ten-item PRINT missed newline after 8')
    call assert_byte(output_ten(5), 57, 'ten-item PRINT did not write ASCII 9')
    call assert_byte(output_ten(6), 10, 'ten-item PRINT missed newline after 9')
    call assert_byte(output_ten(7), 49, 'ten-item PRINT did not write ASCII 1 of 10')
    call assert_byte(output_ten(8), 48, 'ten-item PRINT did not write ASCII 0 of 10')
    call assert_byte(output_ten(9), 10, 'ten-item PRINT missed newline after 10')
    call assert_byte(output_ten(10), 49, 'ten-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_ten(11), 49, 'ten-item PRINT did not write ASCII 1 of 11')
    call assert_byte(output_ten(12), 10, 'ten-item PRINT missed newline after 11')
    call assert_byte(output_ten(13), 49, 'ten-item PRINT did not write ASCII 1 of 12')
    call assert_byte(output_ten(14), 50, 'ten-item PRINT did not write ASCII 2 of 12')
    call assert_byte(output_ten(15), 10, 'ten-item PRINT missed newline after 12')
    call assert_byte(output_ten(16), 49, 'ten-item PRINT did not write ASCII 1 of 13')
    call assert_byte(output_ten(17), 51, 'ten-item PRINT did not write ASCII 3 of 13')
    call assert_byte(output_ten(18), 10, 'ten-item PRINT missed newline after 13')
    call assert_byte(output_ten(19), 49, 'ten-item PRINT did not write ASCII 1 of 14')
    call assert_byte(output_ten(20), 52, 'ten-item PRINT did not write ASCII 4 of 14')
    call assert_byte(output_ten(21), 10, 'ten-item PRINT missed newline after 14')
    call assert_byte(output_ten(22), 49, 'ten-item PRINT did not write ASCII 1 of 15')
    call assert_byte(output_ten(23), 53, 'ten-item PRINT did not write ASCII 5 of 15')
    call assert_byte(output_ten(24), 10, 'ten-item PRINT missed newline after 15')
    call assert_byte(output_ten(25), 49, 'ten-item PRINT did not write ASCII 1 of 16')
    call assert_byte(output_ten(26), 54, 'ten-item PRINT did not write ASCII 6 of 16')
    call assert_byte(output_ten(27), 10, 'ten-item PRINT missed newline after 16')
    read (unit, iostat=io_status) output_ten(1)
    call assert_true(io_status /= 0, 'ten-item PRINT wrote extra bytes')
    close (unit, status='delete', iostat=io_status)
    call assert_int(io_status, 0, 'ten-item PRINT output cleanup failed')

    wrong_ten_shape = input_ten
    wrong_ten_shape(index(wrong_ten_shape, 'type i32', back=.true.): &
        index(wrong_ten_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_ten_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'ten-item PRINT result-shape mutation was accepted')

    wrong_ten_opcode = input_ten
    wrong_ten_opcode(index(wrong_ten_opcode, 'opcode output', back=.true.): &
        index(wrong_ten_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_ten_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'ten-item PRINT opcode mutation was accepted')

    wrong_nine_shape = input_nine
    wrong_nine_shape(index(wrong_nine_shape, 'type i32', back=.true.): &
        index(wrong_nine_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_nine_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'nine-item PRINT result-shape mutation was accepted')

    wrong_nine_opcode = input_nine
    wrong_nine_opcode(index(wrong_nine_opcode, 'opcode output', back=.true.): &
        index(wrong_nine_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_nine_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'nine-item PRINT opcode mutation was accepted')

    wrong_eight_shape = input_eight
    wrong_eight_shape(index(wrong_eight_shape, 'type i32', back=.true.): &
        index(wrong_eight_shape, 'type i32', back=.true.) + 7) = 'type real'
    call compile_mir_v0_riscv_linux(wrong_eight_shape, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'eight-item PRINT result-shape mutation was accepted')

    wrong_eight_opcode = input_eight
    wrong_eight_opcode(index(wrong_eight_opcode, 'opcode output', back=.true.): &
        index(wrong_eight_opcode, 'opcode output', back=.true.) + 12) = 'opcode return '
    call compile_mir_v0_riscv_linux(wrong_eight_opcode, artifact, status, diagnostic)
    call assert_status(status, mir_v0_bridge_out_of_scope, &
        'eight-item PRINT opcode mutation was accepted')

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
