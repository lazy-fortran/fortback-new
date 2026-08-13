module fortback_elf64
    use iso_fortran_env, only: int8, int16, int32, int64
    use fortback_target_ir, only: source_ref_t, target_ir_t, source_ref_valid
    implicit none
    private

    integer(int32), parameter, public :: elf64_ok = 0_int32
    integer(int32), parameter, public :: elf64_invalid_target = 1_int32
    integer(int32), parameter, public :: elf64_invalid_source = 2_int32
    integer(int32), parameter, public :: elf64_empty_word = 3_int32
    integer(int32), parameter, public :: elf64_capacity = 4_int32
    integer(int32), parameter, public :: elf64_io_error = 5_int32
    integer(int16), parameter, public :: elf64_machine_riscv = 243_int16

    type, public :: elf64_target_t
        type(target_ir_t) :: target
        integer(int16) :: machine = 0_int16
        integer(int8) :: osabi = 0_int8
        integer(int8) :: abi_version = 0_int8
    end type elf64_target_t

    public :: write_elf64_object
    public :: write_elf64_object_to_unit

contains

    subroutine write_elf64_object(metadata, source, word, bytes, status)
        type(elf64_target_t), intent(in) :: metadata
        type(source_ref_t), intent(in) :: source
        integer(int64), intent(in) :: word
        integer(int8), allocatable, intent(out) :: bytes(:)
        integer(int32), intent(out) :: status
        integer(int64) :: section_offset

        if (allocated(bytes)) deallocate (bytes)
        allocate (bytes(0))
        status = validate_input(metadata, source, word)
        if (status /= elf64_ok) return

        deallocate (bytes)
        allocate (bytes(280))
        bytes = 0_int8
        call emit_ident(bytes, metadata)
        call put_u16(bytes, 17, 1_int16)
        call put_u16(bytes, 19, metadata%machine)
        call put_u32(bytes, 21, 1_int32)
        section_offset = 88_int64
        call put_u64(bytes, 41, section_offset)
        call put_u16(bytes, 53, 64_int16)
        call put_u16(bytes, 59, 64_int16)
        call put_u16(bytes, 61, 3_int16)
        call put_u16(bytes, 63, 2_int16)
        bytes(65:68) = word_bytes(word)
        bytes(69:84) = [0_int8, 46_int8, 116_int8, 101_int8, 120_int8, 116_int8, &
            0_int8, 46_int8, 115_int8, 104_int8, 115_int8, 116_int8, 114_int8, &
            116_int8, 97_int8, 98_int8]
        call emit_section_headers(bytes)
    end subroutine write_elf64_object

    subroutine write_elf64_object_to_unit(metadata, source, word, unit, status)
        type(elf64_target_t), intent(in) :: metadata
        type(source_ref_t), intent(in) :: source
        integer(int64), intent(in) :: word
        integer, intent(in) :: unit
        integer(int32), intent(out) :: status
        integer(int8), allocatable :: bytes(:)
        integer :: io_status

        call write_elf64_object(metadata, source, word, bytes, status)
        if (status /= elf64_ok) return

        write (unit, iostat=io_status) bytes
        if (io_status /= 0) status = elf64_io_error
    end subroutine write_elf64_object_to_unit

    integer(int32) function validate_input(metadata, source, word)
        type(elf64_target_t), intent(in) :: metadata
        type(source_ref_t), intent(in) :: source
        integer(int64), intent(in) :: word

        validate_input = elf64_invalid_target
        if (trim(metadata%target%architecture) /= 'riscv64') return
        if (metadata%target%word_bits /= 64_int32) return
        if (.not. metadata%target%little_endian) return
        if (metadata%machine /= elf64_machine_riscv) return
        if (.not. source_ref_valid(source)) then
            validate_input = elf64_invalid_source
            return
        end if
        if (word == 0_int64) then
            validate_input = elf64_empty_word
            return
        end if
        validate_input = elf64_ok
    end function validate_input

    subroutine emit_ident(bytes, metadata)
        integer(int8), intent(inout) :: bytes(:)
        type(elf64_target_t), intent(in) :: metadata
        integer(int8) :: ident(16)

        ident = 0_int8
        ident(1) = 127_int8
        ident(2) = 69_int8
        ident(3) = 76_int8
        ident(4) = 70_int8
        ident(5) = 2_int8
        ident(6) = 1_int8
        ident(7) = 1_int8
        ident(8) = metadata%osabi
        ident(16) = metadata%abi_version
        bytes(1:16) = ident
    end subroutine emit_ident

    subroutine emit_section_headers(bytes)
        integer(int8), intent(inout) :: bytes(:)

        ! The null header occupies the first 64-byte slot.
        call put_u32(bytes, 153, 1_int32)
        call put_u32(bytes, 157, 1_int32)
        call put_u64(bytes, 161, 6_int64)
        call put_u64(bytes, 177, 64_int64)
        call put_u64(bytes, 185, 4_int64)
        call put_u64(bytes, 201, 4_int64)
        call put_u32(bytes, 217, 7_int32)
        call put_u32(bytes, 221, 3_int32)
        call put_u64(bytes, 241, 68_int64)
        call put_u64(bytes, 249, 16_int64)
        call put_u64(bytes, 265, 1_int64)
    end subroutine emit_section_headers

    pure function word_bytes(word) result(bytes)
        integer(int64), intent(in) :: word
        integer(int8) :: bytes(4)

        bytes(1) = int(iand(word, int(z'FF', int64)), int8)
        bytes(2) = int(iand(ishft(word, -8), int(z'FF', int64)), int8)
        bytes(3) = int(iand(ishft(word, -16), int(z'FF', int64)), int8)
        bytes(4) = int(iand(ishft(word, -24), int(z'FF', int64)), int8)
    end function word_bytes

    subroutine put_u16(bytes, offset, value)
        integer(int8), intent(inout) :: bytes(:)
        integer, intent(in) :: offset
        integer(int16), intent(in) :: value
        integer(int64) :: wide

        wide = int(value, int64)
        bytes(offset) = int(iand(wide, 255_int64), int8)
        bytes(offset + 1) = int(iand(ishft(wide, -8), 255_int64), int8)
    end subroutine put_u16

    subroutine put_u32(bytes, offset, value)
        integer(int8), intent(inout) :: bytes(:)
        integer, intent(in) :: offset
        integer(int32), intent(in) :: value
        integer(int64) :: wide
        integer(int8) :: encoded(4)

        wide = int(value, int64)
        encoded(1) = int(iand(wide, 255_int64), int8)
        encoded(2) = int(iand(ishft(wide, -8), 255_int64), int8)
        encoded(3) = int(iand(ishft(wide, -16), 255_int64), int8)
        encoded(4) = int(iand(ishft(wide, -24), 255_int64), int8)
        bytes(offset:offset + 3) = encoded
    end subroutine put_u32

    subroutine put_u64(bytes, offset, value)
        integer(int8), intent(inout) :: bytes(:)
        integer, intent(in) :: offset
        integer(int64), intent(in) :: value
        integer :: i

        do i = 0, 7
            bytes(offset + i) = int(iand(ishft(value, -8 * i), 255_int64), int8)
        end do
    end subroutine put_u64

end module fortback_elf64
