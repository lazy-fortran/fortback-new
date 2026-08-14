module fortback_riscv_source
    use iso_fortran_env, only: int32, int64
    use fortback_target_ir, only: source_ref_t
    implicit none
    private

    integer(int32), parameter, public :: riscv_source_ok = 0_int32
    integer(int32), parameter, public :: riscv_source_malformed = 1_int32
    integer(int32), parameter, public :: riscv_source_unsupported = 2_int32
    integer(int32), parameter, public :: riscv_source_capacity = 3_int32

    type, public :: riscv_opcode_record_t
        character(len=16) :: mnemonic = ''
        character(len=1) :: format = ''
        integer(int64) :: match = 0_int64
        integer(int64) :: mask = 0_int64
        type(source_ref_t) :: source
    end type riscv_opcode_record_t

    public :: import_riscv_opcodes

contains

    ! The caller owns archive retrieval and extraction; this boundary accepts
    ! the extracted UTF-8 text of one canonical riscv-opcodes object.
    subroutine import_riscv_opcodes(text, source, records, count, status)
        character(len=*), intent(in) :: text
        type(source_ref_t), intent(in) :: source
        type(riscv_opcode_record_t), intent(out) :: records(:)
        integer(int32), intent(out) :: count, status
        integer :: first, last, next_line, words
        character(len=256) :: line
        character(len=32) :: tokens(12)

        records = riscv_opcode_record_t()
        count = 0_int32
        status = riscv_source_ok
        first = 1
        do while (first <= len(text))
            next_line = index(text(first:), new_line('a'))
            if (next_line == 0) then
                last = len(text)
            else
                last = first + next_line - 2
            end if
            line = ''
            if (last >= first) line(1:min(256, last - first + 1)) = &
                text(first:first + min(255, last - first)) ! text-policy: bounded source line
            call parse_line(line, tokens, words, status)
            if (status /= riscv_source_ok) return
            if (words > 0) then
                if (count >= size(records)) then
                    status = riscv_source_capacity
                    return
                end if
                count = count + 1_int32
                call normalize_record(tokens, words, source, records(count), status)
                if (status /= riscv_source_ok) return
            end if
            if (next_line == 0) exit
            first = last + 2
        end do
    end subroutine import_riscv_opcodes

    subroutine parse_line(line, tokens, words, status)
        character(len=*), intent(inout) :: line
        character(len=*), intent(out) :: tokens(:)
        integer, intent(out) :: words
        integer(int32), intent(out) :: status
        integer :: begin, finish, separator

        tokens = ''
        words = 0
        status = riscv_source_ok
        separator = index(line, '#')
        if (separator > 0) line(separator:) = ' '
        begin = 1
        do while (begin <= len_trim(line))
            do while (begin <= len_trim(line))
                if (line(begin:begin) /= ' ') exit
                begin = begin + 1
            end do
            if (begin > len_trim(line)) exit
            finish = begin
            do while (finish <= len_trim(line))
                if (line(finish:finish) == ' ') exit
                finish = finish + 1
            end do
            if (words == size(tokens)) then
                status = riscv_source_malformed
                return
            end if
            words = words + 1
            tokens(words) = line(begin:finish - 1)
            begin = finish + 1
        end do
    end subroutine parse_line

    subroutine normalize_record(tokens, words, source, record, status)
        character(len=*), intent(in) :: tokens(:)
        integer, intent(in) :: words
        type(source_ref_t), intent(in) :: source
        type(riscv_opcode_record_t), intent(out) :: record
        integer(int32), intent(out) :: status
        integer :: operand_count, i, equals, dots, high, low
        integer(int64) :: value, field_mask

        record = riscv_opcode_record_t()
        record%source = source
        status = riscv_source_ok
        if (words < 5) then
            status = riscv_source_malformed
            return
        end if
        if (trim(tokens(1)) /= 'add' .and. trim(tokens(1)) /= 'sub' .and. &
            trim(tokens(1)) /= 'and' .and. &
            trim(tokens(1)) /= 'or' .and. &
            trim(tokens(1)) /= 'xor' .and. &
            trim(tokens(1)) /= 'sll' .and. &
            trim(tokens(1)) /= 'sra' .and. &
            trim(tokens(1)) /= 'addi' .and. &
            trim(tokens(1)) /= 'ori' .and. &
            trim(tokens(1)) /= 'andi') then
            status = riscv_source_unsupported
            return
        end if
        record%mnemonic = tokens(1)
        operand_count = 3
        if (words <= operand_count + 1) then
            status = riscv_source_malformed
            return
        end if
        if (trim(tokens(1)) == 'addi' .or. trim(tokens(1)) == 'ori' .or. &
            trim(tokens(1)) == 'andi') then
            record%format = 'I'
        else
            record%format = 'R'
        end if
        do i = operand_count + 2, words
            equals = index(tokens(i), '=')
            dots = index(tokens(i), '..')
            if (equals <= 1 .or. dots <= 1 .or. dots >= equals) then
                status = riscv_source_malformed
                return
            end if
            read(tokens(i)(1:dots - 1), *, iostat=status) high
            if (status /= 0) then
                status = riscv_source_malformed
                return
            end if
            read(tokens(i)(dots + 2:equals - 1), *, iostat=status) low
            if (status /= 0) then
                status = riscv_source_malformed
                return
            end if
            if (high < low .or. high > 31 .or. low < 0) then
                status = riscv_source_malformed
                return
            end if
            call parse_value(tokens(i)(equals + 1:), value, status)
            if (status /= riscv_source_ok) return
            field_mask = ishft(2_int64**(high - low + 1) - 1_int64, low)
            record%mask = ior(record%mask, field_mask)
            record%match = ior(record%match, iand(ishft(value, low), field_mask))
        end do
    end subroutine normalize_record

    subroutine parse_value(token, value, status)
        character(len=*), intent(in) :: token
        integer(int64), intent(out) :: value
        integer(int32), intent(out) :: status
        character(len=32) :: digits
        integer :: i, digit

        digits = adjustl(token)
        if (len_trim(digits) > 2) then
            if (digits(1:2) == '0x') then
                value = 0_int64
                status = riscv_source_ok
                do i = 3, len_trim(digits)
                    select case (digits(i:i))
                    case ('0':'9')
                        digit = iachar(digits(i:i)) - iachar('0')
                    case ('a':'f')
                        digit = iachar(digits(i:i)) - iachar('a') + 10
                    case ('A':'F')
                        digit = iachar(digits(i:i)) - iachar('A') + 10
                    case default
                        status = riscv_source_malformed
                        return
                    end select
                    value = 16_int64 * value + int(digit, int64)
                end do
            else
                read(digits, *, iostat=status) value
            end if
        else
            read(digits, *, iostat=status) value
        end if
        if (status /= 0) status = riscv_source_malformed
    end subroutine parse_value

end module fortback_riscv_source
