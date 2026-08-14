module fortback_aarch64_record
    use iso_fortran_env, only: int32, int64
    use fortback_aarch64_source, only: aarch64_encoding_record_t
    use fortback_target_ir, only: source_ref_valid, target_ir_t, target_ir_valid
    implicit none
    private

    integer(int32), parameter, public :: aarch64_record_ok = 0_int32
    integer(int32), parameter, public :: aarch64_record_invalid_target = 1_int32
    integer(int32), parameter, public :: aarch64_record_unsupported = 3_int32
    integer(int32), parameter, public :: aarch64_record_malformed = 4_int32
    integer(int64), parameter :: aarch64_word_max = int(z'FFFFFFFF', int64)

    public :: aarch64_find_fixed_record
    public :: aarch64_validate_record

contains

    subroutine aarch64_validate_record(target, record, status)
        type(target_ir_t), intent(in) :: target
        type(aarch64_encoding_record_t), intent(in) :: record
        integer(int32), intent(out) :: status

        status = validate_target(target)
        if (status /= aarch64_record_ok) return
        status = validate_record_target(record%target)
        if (status /= aarch64_record_ok) return
        status = aarch64_record_malformed
        if (.not. source_ref_valid(record%source)) return
        if (.not. source_ref_valid(record%target%source)) return
        if (record%width /= 32_int32) return
        if (record%mask < 0_int64 .or. record%mask > aarch64_word_max) return
        if (record%match < 0_int64 .or. record%match > aarch64_word_max) return
        if (record%mask == 0_int64) return
        if (iand(record%match, not(record%mask)) /= 0_int64) return
        status = aarch64_record_ok
    end subroutine aarch64_validate_record

    subroutine aarch64_find_fixed_record(target, word, records, record_index, status)
        type(target_ir_t), intent(in) :: target
        integer(int64), intent(in) :: word
        type(aarch64_encoding_record_t), intent(in) :: records(:)
        integer(int32), intent(out) :: record_index
        integer(int32), intent(out) :: status
        integer(int32) :: record_status
        integer :: i

        record_index = 0_int32
        status = validate_target(target)
        if (status /= aarch64_record_ok) return
        if (word < 0_int64 .or. word > aarch64_word_max) then
            status = aarch64_record_malformed
            return
        end if
        if (size(records) == 0) then
            status = aarch64_record_unsupported
            return
        end if

        do i = 1, size(records)
            call aarch64_validate_record(target, records(i), record_status)
            if (record_status /= aarch64_record_ok) then
                status = record_status
                return
            end if
            if (iand(word, records(i)%mask) /= records(i)%match) cycle
            record_index = int(i, int32)
            status = aarch64_record_ok
            return
        end do
        status = aarch64_record_unsupported
    end subroutine aarch64_find_fixed_record

    pure integer(int32) function validate_target(target)
        type(target_ir_t), intent(in) :: target

        validate_target = aarch64_record_invalid_target
        if (.not. target_ir_valid(target)) return
        if (trim(target%architecture) /= 'aarch64') return
        if (target%word_bits /= 32_int32) return
        if (.not. target%little_endian) return
        validate_target = aarch64_record_ok
    end function validate_target

    pure integer(int32) function validate_record_target(target)
        type(target_ir_t), intent(in) :: target

        validate_record_target = aarch64_record_invalid_target
        if (len_trim(target%architecture) == 0) return
        if (target%word_bits <= 0_int32) return
        if (mod(target%word_bits, 8_int32) /= 0_int32) return
        if (trim(target%architecture) /= 'aarch64') return
        if (target%word_bits /= 32_int32) return
        if (.not. target%little_endian) return
        validate_record_target = aarch64_record_ok
    end function validate_record_target

end module fortback_aarch64_record
