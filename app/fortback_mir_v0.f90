program fortback_mir_v0
    use iso_fortran_env, only: error_unit, int32, int64
    use fortback_mir_v0_riscv_linux, only: mir_v0_bridge_ok, &
        mir_v0_bridge_io_error, write_mir_v0_riscv_linux
    implicit none

    character(len=4096) :: input_path, output_path
    character(len=65536) :: input
    character(len=256) :: diagnostic
    integer(int32) :: status
    integer(int64) :: file_size
    integer :: argument_count, chmod_status, io_status, unit

    argument_count = command_argument_count()
    if (argument_count /= 2) then
        write (error_unit, '(a)') &
            'usage: fortback-mir-v0 <mir-v0.sx> <riscv64-linux-elf>'
        error stop 2
    end if
    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)
    inquire (file=trim(input_path), size=file_size, iostat=io_status)
    if (io_status /= 0 .or. file_size <= 0_int64 .or. &
        file_size > int(len(input), int64)) then
        write (error_unit, '(a)') 'mir-v0: input SX cannot be read'
        error stop 2
    end if
    open (newunit=unit, file=trim(input_path), form='unformatted', access='stream', &
        status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
        write (error_unit, '(a)') 'mir-v0: input SX cannot be opened'
        error stop 2
    end if
    read (unit, iostat=io_status) input(:int(file_size))
    close (unit)
    if (io_status /= 0) then
        write (error_unit, '(a)') 'mir-v0: input SX cannot be read'
        error stop 2
    end if

    call write_mir_v0_riscv_linux(input, output_path, status, diagnostic)
    if (status /= mir_v0_bridge_ok) then
        write (error_unit, '(a)') trim(diagnostic)
        if (status == mir_v0_bridge_io_error) error stop 3
        error stop 2
    end if
    call execute_command_line('chmod 755 -- '//trim(output_path), wait=.true., &
        exitstat=io_status, cmdstat=chmod_status)
    if (chmod_status /= 0 .or. io_status /= 0) then
        write (error_unit, '(a)') 'mir-v0: output artifact cannot be made executable'
        error stop 3
    end if
end program fortback_mir_v0
