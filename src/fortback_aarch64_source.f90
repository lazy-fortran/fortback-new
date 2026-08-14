module fortback_aarch64_source
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: make_target_ir, source_ref_t, target_ir_t
    implicit none
    private

    integer(int32), parameter, public :: aarch64_source_ok = 0_int32
    integer(int32), parameter, public :: aarch64_source_malformed = 1_int32
    integer(int32), parameter, public :: aarch64_source_unsupported = 2_int32
    integer(int32), parameter, public :: aarch64_source_capacity = 3_int32

    type, public :: aarch64_encoding_record_t
        character(len=32) :: name = ''
        character(len=32) :: operation_id = ''
        integer(int32) :: width = 0_int32
        integer(int64) :: match = 0_int64
        integer(int64) :: mask = 0_int64
        type(target_ir_t) :: target
        type(source_ref_t) :: source
    end type aarch64_encoding_record_t

    public :: import_aarch64_instructions

contains

    ! The caller owns retrieval and extraction.  The boundary is one compact
    ! AARCHMRS Instruction.Instruction JSON object per line.  It retains only
    ! _type, name, operation_id and the nested Encodeset/Range/Values.Value
    ! fields needed here; the full pinned Instructions.json stays external.
    subroutine import_aarch64_instructions(text, source, records, count, status)
        character(len=*), intent(in) :: text
        type(source_ref_t), intent(in) :: source
        type(aarch64_encoding_record_t), intent(out) :: records(:)
        integer(int32), intent(out) :: count, status
        integer :: first, last, next_line
        character(len=8192) :: line

        records = aarch64_encoding_record_t()
        count = 0_int32
        status = aarch64_source_ok
        first = 1
        do while (first <= len(text))
            next_line = index(text(first:), new_line('a'))
            if (next_line == 0) then
                last = len(text)
            else
                last = first + next_line - 2
            end if
            line = ''
            if (last >= first .and. last - first + 1 > len(line)) then
                status = aarch64_source_malformed
                return
            end if
            if (last >= first) line(1:last - first + 1) = text(first:last) ! text-policy: bounded source line
            if (len_trim(line) > 0) then
                if (count >= size(records)) then
                    status = aarch64_source_capacity
                    return
                end if
                call parse_record(line, source, records(count + 1), status)
                if (status /= aarch64_source_ok) return
                count = count + 1_int32
            end if
            if (next_line == 0) exit
            first = last + 2
        end do
    end subroutine import_aarch64_instructions

    subroutine parse_record(line, source, record, status)
        character(len=*), intent(in) :: line
        type(source_ref_t), intent(in) :: source
        type(aarch64_encoding_record_t), intent(out) :: record
        integer(int32), intent(out) :: status
        integer :: cursor, start_bit, width, next, end_quote
        character(len=64) :: value
        logical :: found

        record = aarch64_encoding_record_t()
        record%source = source
        record%target = make_target_ir('aarch64', 32_int32, .true., source)
        status = aarch64_source_malformed
        if (index(line, '"_type":"Instruction.Instruction"') == 0) return
        if (index(line, '"encoding":{"_type":"Instruction.Encodeset.Encodeset"') == 0) return
        if (.not. extract_string(line, '"name":"', record%name)) return
        if (.not. extract_string(line, '"operation_id":"', record%operation_id)) return
        if (.not. supported_name(trim(record%name))) then
            status = aarch64_source_unsupported
            return
        end if
        if (.not. extract_integer(line, '"width":', record%width)) return
        if (record%width /= 32_int32) return

        cursor = index(line, '"values":[')
        if (cursor == 0) return
        cursor = cursor + len('"values":[')
        found = .false.
        do
            next = index(line(cursor:), '"range":{"_type":"Range","start":')
            if (next == 0) exit
            cursor = cursor + next - 1
            if (.not. extract_integer(line(cursor:), '"start":', start_bit)) return
            if (.not. extract_integer(line(cursor:), '"width":', width)) return
            next = index(line(cursor:), ',"value":{"_type":"Values.Value","value":"')
            if (next == 0) return
            cursor = cursor + next + len(',"value":{"_type":"Values.Value","value":"') - 1
            end_quote = index(line(cursor:), '"')
            if (end_quote <= 1 .or. end_quote > len(value)) return
            value = ''
            value(1:end_quote - 1) = line(cursor:cursor + end_quote - 2)
            if (len_trim(value) /= width .or. start_bit < 0 .or. width <= 0 .or. &
                start_bit + width > 32) return
            call add_bits(value(1:len_trim(value)), start_bit, record, found, status)
            if (status /= aarch64_source_ok) return
            cursor = cursor + end_quote
        end do
        if (.not. found) return
        status = aarch64_source_ok
    end subroutine parse_record

    subroutine add_bits(value, start_bit, record, found, status)
        character(len=*), intent(in) :: value
        integer, intent(in) :: start_bit
        type(aarch64_encoding_record_t), intent(inout) :: record
        logical, intent(inout) :: found
        integer(int32), intent(out) :: status
        integer :: i, bit
        integer(int64) :: bit_mask

        status = aarch64_source_malformed
        do i = 1, len(value)
            bit = start_bit + len(value) - i
            bit_mask = ishft(1_int64, bit)
            if (iand(record%mask, bit_mask) /= 0_int64) return
            select case (value(i:i))
            case ('0')
                record%mask = ior(record%mask, bit_mask)
            case ('1')
                record%mask = ior(record%mask, bit_mask)
                record%match = ior(record%match, bit_mask)
            case ('x')
                continue
            case default
                return
            end select
        end do
        found = .true.
        status = aarch64_source_ok
    end subroutine add_bits

    logical function extract_string(text, key, value)
        character(len=*), intent(in) :: text, key
        character(len=*), intent(out) :: value
        integer :: start, finish

        value = ''
        start = index(text, key)
        if (start == 0) then
            extract_string = .false.
            return
        end if
        start = start + len(key)
        finish = index(text(start:), '"')
        extract_string = finish > 1 .and. finish - 1 <= len(value)
        if (extract_string) value(1:finish - 1) = text(start:start + finish - 2)
    end function extract_string

    logical function extract_integer(text, key, value)
        character(len=*), intent(in) :: text, key
        integer, intent(out) :: value
        integer :: start, finish, ios

        value = 0
        start = index(text, key)
        if (start == 0) then
            extract_integer = .false.
            return
        end if
        start = start + len(key)
        finish = start
        do while (finish <= len(text))
            if (text(finish:finish) < '0' .or. text(finish:finish) > '9') exit
            finish = finish + 1
        end do
        if (finish == start) then
            extract_integer = .false.
            return
        end if
        read(text(start:finish - 1), *, iostat=ios) value
        extract_integer = ios == 0
    end function extract_integer

    pure logical function supported_name(name)
        character(len=*), intent(in) :: name

        supported_name = name == 'ADD_64_addsub_imm' .or. name == 'SUB_64_addsub_imm' .or. &
            name == 'NOP_HI_hints' .or. name == 'ADR_only_pcreladdr' .or. &
            name == 'ADRP_only_pcreladdr'
    end function supported_name

end module fortback_aarch64_source
