module fortback_targetir_encoding_sx
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: source_ref_t
    use fortback_targetir_codec, only: targetir_validate_record
    use fortback_targetir_encoding, only: targetir_encoding_capacity, &
        targetir_encoding_malformed, targetir_encoding_ok, targetir_encoding_record_t, &
        targetir_encoding_field_capacity, targetir_variable_field_t
    implicit none
    private

    integer(int32), parameter, public :: targetir_encoding_sx_ok = 0_int32
    integer(int32), parameter, public :: targetir_encoding_sx_malformed = 1_int32
    integer(int32), parameter, public :: targetir_encoding_sx_unsupported = 2_int32
    integer(int32), parameter, public :: targetir_encoding_sx_capacity = 4_int32

    public :: read_targetir_encoding_sx
    public :: write_targetir_encoding_sx

contains

    subroutine write_targetir_encoding_sx(record, text, status)
        type(targetir_encoding_record_t), intent(in) :: record
        character(len=*), intent(out) :: text
        integer(int32), intent(out) :: status
        integer(int32) :: i, position

        text = ''
        status = targetir_encoding_sx_malformed
        if (record%word_bits /= 32_int32) then
            status = targetir_encoding_sx_unsupported
            return
        end if
        if (targetir_validate_record(record%target, record) /= targetir_encoding_ok) return
        position = 1_int32
        call append(text, position, '(targetir-encoding-v0 (target (architecture ')
        call append_atom(text, position, record%target%architecture, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (word-bits ')
        call append_integer(text, position, int(record%target%word_bits, int64), status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (little-endian ')
        call append_boolean(text, position, record%target%little_endian, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') ')
        call append_source(text, position, record%target%source, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (operation-id ')
        call append_atom(text, position, record%operation_id, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (word-bits ')
        call append_integer(text, position, int(record%word_bits, int64), status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (fixed-mask ')
        call append_integer(text, position, record%fixed_mask, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (fixed-match ')
        call append_integer(text, position, record%fixed_match, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (variable-fields')
        do i = 1, record%variable_field_count
            call append(text, position, ' (field (ordinal ')
            call append_integer(text, position, int(record%variable_fields(i)%ordinal, int64), status)
            if (status /= targetir_encoding_sx_ok) return
            call append(text, position, ') (start ')
            call append_integer(text, position, int(record%variable_fields(i)%start, int64), status)
            if (status /= targetir_encoding_sx_ok) return
            call append(text, position, ') (width ')
            call append_integer(text, position, int(record%variable_fields(i)%width, int64), status)
            if (status /= targetir_encoding_sx_ok) return
            call append(text, position, '))')
        end do
        call append(text, position, ') ')
        call append_source(text, position, record%source, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ')')
        if (position > len(text) + 1) then
            text = ''
            status = targetir_encoding_sx_capacity
            return
        end if
        status = targetir_encoding_sx_ok
    end subroutine write_targetir_encoding_sx

    subroutine read_targetir_encoding_sx(text, record, status)
        character(len=*), intent(in) :: text
        type(targetir_encoding_record_t), intent(out) :: record
        integer(int32), intent(out) :: status
        integer(int32) :: position, i, value
        integer(int64) :: integer_value
        logical :: ok, at_end
        character(len=64) :: atom
        type(source_ref_t) :: source

        record = targetir_encoding_record_t()
        status = targetir_encoding_sx_malformed
        if (index(trim(text), '(targetir-encoding-v') == 1 .and. &
            index(trim(text), '(targetir-encoding-v0') /= 1) then
            status = targetir_encoding_sx_unsupported
            return
        end if
        position = 1_int32
        call expect(text, position, '(targetir-encoding-v0 (target (architecture ', ok)
        if (.not. ok) return
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        if (len_trim(atom) > len(record%target%architecture)) return
        record%target%architecture = atom(1:len(record%target%architecture))
        call expect(text, position, ') (word-bits ', ok)
        if (.not. ok) return
        call read_integer(text, position, integer_value, ok)
        if (.not. ok) return
        if (integer_value < 1_int64 .or. integer_value > int(huge(0_int32), int64)) return
        record%target%word_bits = int(integer_value, int32)
        call expect(text, position, ') (little-endian ', ok)
        if (.not. ok) return
        call read_boolean(text, position, record%target%little_endian, ok)
        if (.not. ok) return
        call expect(text, position, ') ', ok)
        if (.not. ok) return
        call read_source(text, position, record%target%source, ok)
        if (.not. ok) return
        call expect(text, position, ') (operation-id ', ok)
        if (.not. ok) return
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        record%operation_id = atom
        call expect(text, position, ') (word-bits ', ok)
        if (.not. ok) return
        call read_integer(text, position, integer_value, ok)
        if (.not. ok) return
        if (integer_value < 1_int64 .or. integer_value > int(huge(0_int32), int64)) return
        record%word_bits = int(integer_value, int32)
        if (record%word_bits /= 32_int32) then
            status = targetir_encoding_sx_unsupported
            record = targetir_encoding_record_t()
            return
        end if
        call expect(text, position, ') (fixed-mask ', ok)
        if (.not. ok) return
        call read_integer(text, position, record%fixed_mask, ok)
        if (.not. ok) return
        call expect(text, position, ') (fixed-match ', ok)
        if (.not. ok) return
        call read_integer(text, position, record%fixed_match, ok)
        if (.not. ok) return
        call expect(text, position, ') (variable-fields', ok)
        if (.not. ok) return
        do
            call peek_close(text, position, at_end, ok)
            if (.not. ok) return
            if (at_end) exit
            if (record%variable_field_count >= targetir_encoding_field_capacity) then
                status = targetir_encoding_sx_capacity
                record = targetir_encoding_record_t()
                return
            end if
            i = record%variable_field_count + 1_int32
            call expect(text, position, '(field (ordinal ', ok)
            if (.not. ok) return
            call read_integer(text, position, integer_value, ok)
            if (.not. ok) return
            if (integer_value < 1_int64 .or. integer_value > int(huge(0_int32), int64)) return
            record%variable_fields(i)%ordinal = int(integer_value, int32)
            call expect(text, position, ') (start ', ok)
            if (.not. ok) return
            call read_integer(text, position, integer_value, ok)
            if (.not. ok) return
            if (integer_value < -1_int64 .or. integer_value > int(huge(0_int32), int64)) return
            record%variable_fields(i)%start = int(integer_value, int32)
            call expect(text, position, ') (width ', ok)
            if (.not. ok) return
            call read_integer(text, position, integer_value, ok)
            if (.not. ok) return
            if (integer_value < -1_int64 .or. integer_value > int(huge(0_int32), int64)) return
            record%variable_fields(i)%width = int(integer_value, int32)
            call expect(text, position, '))', ok)
            if (.not. ok) return
            record%variable_field_count = i
        end do
        call expect(text, position, ') ', ok)
        if (.not. ok) return
        call read_source(text, position, record%source, ok)
        if (.not. ok) return
        call expect(text, position, ')', ok)
        if (.not. ok) return
        call trailing_space_only(text, position, ok)
        if (.not. ok) return
        value = targetir_validate_record(record%target, record)
        if (value /= targetir_encoding_ok) then
            if (value == targetir_encoding_capacity) then
                status = targetir_encoding_sx_capacity
            else
                status = targetir_encoding_sx_malformed
            end if
            record = targetir_encoding_record_t()
            return
        end if
        status = targetir_encoding_sx_ok
    end subroutine read_targetir_encoding_sx

    subroutine append_source(text, position, source, status)
        use fortback_target_ir, only: source_ref_valid
        character(len=*), intent(inout) :: text
        integer(int32), intent(inout) :: position
        type(source_ref_t), intent(in) :: source
        integer(int32), intent(out) :: status

        status = targetir_encoding_sx_malformed
        if (.not. source_ref_valid(source)) return
        call append(text, position, '(source (artifact ')
        call append_atom(text, position, source%artifact, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (object ')
        call append_atom(text, position, source%object, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (source-hash ')
        call append_atom(text, position, source%source_hash, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, ') (origin ')
        call append_atom(text, position, source%origin, status)
        if (status /= targetir_encoding_sx_ok) return
        call append(text, position, '))')
        status = targetir_encoding_sx_ok
    end subroutine append_source

    subroutine read_source(text, position, source, ok)
        character(len=*), intent(in) :: text
        integer(int32), intent(inout) :: position
        type(source_ref_t), intent(out) :: source
        logical, intent(out) :: ok
        character(len=64) :: atom

        source = source_ref_t()
        call expect(text, position, '(source (artifact ', ok)
        if (.not. ok) return
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        source%artifact = atom
        call expect(text, position, ') (object ', ok)
        if (.not. ok) return
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        source%object = atom
        call expect(text, position, ') (source-hash ', ok)
        if (.not. ok) return
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        source%source_hash = atom
        call expect(text, position, ') (origin ', ok)
        if (.not. ok) return
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        if (len_trim(atom) > len(source%origin)) return
        source%origin = atom(1:len(source%origin))
        call expect(text, position, '))', ok)
    end subroutine read_source

    subroutine append_atom(text, position, value, status)
        character(len=*), intent(inout) :: text
        integer(int32), intent(inout) :: position
        character(len=*), intent(in) :: value
        integer(int32), intent(out) :: status
        integer(int32) :: i

        status = targetir_encoding_sx_malformed
        if (len_trim(value) == 0) return
        do i = 1, len_trim(value)
            if (value(i:i) == ' ' .or. value(i:i) == '(' .or. value(i:i) == ')') return
        end do
        call append(text, position, trim(value))
        status = targetir_encoding_sx_ok
    end subroutine append_atom

    subroutine append_integer(text, position, value, status)
        character(len=*), intent(inout) :: text
        integer(int32), intent(inout) :: position
        integer(int64), intent(in) :: value
        integer(int32), intent(out) :: status
        character(len=32) :: buffer
        integer :: ios

        write (buffer, '(i0)', iostat=ios) value
        status = targetir_encoding_sx_malformed
        if (ios /= 0) return
        call append(text, position, trim(buffer))
        status = targetir_encoding_sx_ok
    end subroutine append_integer

    subroutine append_boolean(text, position, value, status)
        character(len=*), intent(inout) :: text
        integer(int32), intent(inout) :: position
        logical, intent(in) :: value
        integer(int32), intent(out) :: status

        if (value) then
            call append(text, position, 'true')
        else
            call append(text, position, 'false')
        end if
        status = targetir_encoding_sx_ok
    end subroutine append_boolean

    subroutine append(text, position, value)
        character(len=*), intent(inout) :: text
        integer(int32), intent(inout) :: position
        character(len=*), intent(in) :: value
        integer(int32) :: finish

        finish = position + len(value) - 1_int32
        if (finish <= len(text)) text(position:finish) = value
        position = finish + 1_int32
    end subroutine append

    subroutine expect(text, position, value, ok)
        character(len=*), intent(in) :: text, value
        integer(int32), intent(inout) :: position
        logical, intent(out) :: ok
        integer(int32) :: finish

        call skip_space(text, position)
        finish = position + len(value) - 1_int32
        ok = .false.
        if (finish > len(text)) return
        if (text(position:finish) /= value) return
        position = finish + 1_int32
        ok = .true.
    end subroutine expect

    subroutine read_atom(text, position, atom, ok)
        character(len=*), intent(in) :: text
        integer(int32), intent(inout) :: position
        character(len=*), intent(out) :: atom
        logical, intent(out) :: ok
        integer(int32) :: start, finish

        atom = ''
        call skip_space(text, position)
        start = position
        do while (position <= len(text))
            if (text(position:position) == ' ' .or. text(position:position) == ')' .or. &
                text(position:position) == '(') exit
            position = position + 1_int32
        end do
        finish = position - 1_int32
        ok = .false.
        if (finish < start) return
        if (finish - start + 1_int32 > len(atom)) return
        atom(1:finish - start + 1) = text(start:finish)
        ok = .true.
    end subroutine read_atom

    subroutine read_integer(text, position, value, ok)
        character(len=*), intent(in) :: text
        integer(int32), intent(inout) :: position
        integer(int64), intent(out) :: value
        logical, intent(out) :: ok
        character(len=64) :: atom
        integer :: ios

        value = 0_int64
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        read (atom, *, iostat=ios) value
        if (ios /= 0) ok = .false.
    end subroutine read_integer

    subroutine read_boolean(text, position, value, ok)
        character(len=*), intent(in) :: text
        integer(int32), intent(inout) :: position
        logical, intent(out) :: value
        logical, intent(out) :: ok
        character(len=64) :: atom

        value = .false.
        call read_atom(text, position, atom, ok)
        if (.not. ok) return
        select case (trim(atom))
        case ('true')
            value = .true.
        case ('false')
            value = .false.
        case default
            ok = .false.
        end select
    end subroutine read_boolean

    subroutine peek_close(text, position, at_end, ok)
        character(len=*), intent(in) :: text
        integer(int32), intent(inout) :: position
        logical, intent(out) :: at_end, ok

        call skip_space(text, position)
        ok = position <= len(text)
        at_end = .false.
        if (.not. ok) return
        at_end = text(position:position) == ')'
    end subroutine peek_close

    subroutine trailing_space_only(text, position, ok)
        character(len=*), intent(in) :: text
        integer(int32), intent(inout) :: position
        logical, intent(out) :: ok

        call skip_space(text, position)
        ok = position > len_trim(text)
    end subroutine trailing_space_only

    subroutine skip_space(text, position)
        character(len=*), intent(in) :: text
        integer(int32), intent(inout) :: position

        do while (position <= len(text))
            if (text(position:position) /= ' ') exit
            position = position + 1_int32
        end do
    end subroutine skip_space

end module fortback_targetir_encoding_sx
