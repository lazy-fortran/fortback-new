module fortback_targetir_sx
    use fortback_target_ir, only: source_ref_t, source_ref_valid
    implicit none
    private

    integer, parameter, public :: targetir_sx_ok = 0
    integer, parameter, public :: targetir_sx_invalid = 1
    integer, parameter, public :: targetir_sx_capacity = 2

    type, public :: targetir_sx_target_t
        character(len=64) :: name = ''
        character(len=64) :: architecture = ''
        type(source_ref_t) :: source
    end type targetir_sx_target_t

    type, public :: targetir_sx_instruction_t
        character(len=64) :: name = ''
        character(len=64) :: feature = ''
        type(source_ref_t) :: source
        character(len=32) :: origin = ''
    end type targetir_sx_instruction_t

    public :: write_targetir_v0
    public :: validate_targetir_v0
    public :: query_targetir_v0_feature

contains

    subroutine write_targetir_v0(target, instructions, count, text, status)
        type(targetir_sx_target_t), intent(in) :: target
        type(targetir_sx_instruction_t), intent(in) :: instructions(:)
        integer, intent(in) :: count
        character(len=*), intent(out) :: text
        integer, intent(out) :: status
        integer :: i, position

        text = ''
        status = targetir_sx_invalid
        if (.not. valid_target(target)) return
        if (count <= 0 .or. count > size(instructions)) return
        do i = 1, count
            if (.not. valid_instruction(instructions(i))) return
        end do

        position = 1
        call append(text, position, '(targetir-v0 (target (name ')
        call append(text, position, trim(target%name))
        call append(text, position, ') (architecture ')
        call append(text, position, trim(target%architecture))
        call append(text, position, ') (source (artifact ')
        call append(text, position, trim(target%source%artifact))
        call append(text, position, ') (object ')
        call append(text, position, trim(target%source%object))
        call append(text, position, ') (source-hash ')
        call append(text, position, trim(target%source%source_hash))
        call append(text, position, ')))')
        do i = 1, count
            call append(text, position, ' (instruction (name ')
            call append(text, position, trim(instructions(i)%name))
            call append(text, position, ') (feature ')
            call append(text, position, trim(instructions(i)%feature))
            call append(text, position, ') (source (artifact ')
            call append(text, position, trim(instructions(i)%source%artifact))
            call append(text, position, ') (object ')
            call append(text, position, trim(instructions(i)%source%object))
            call append(text, position, ') (source-hash ')
            call append(text, position, trim(instructions(i)%source%source_hash))
            call append(text, position, ')) (origin ')
            call append(text, position, trim(instructions(i)%origin))
            call append(text, position, '))')
        end do
        call append(text, position, ')')
        if (position > len(text) + 1) then
            text = ''
            status = targetir_sx_capacity
            return
        end if
        status = targetir_sx_ok
    end subroutine write_targetir_v0

    subroutine query_targetir_v0_feature(instructions, count, feature, indices, &
            match_count, status)
        type(targetir_sx_instruction_t), intent(in) :: instructions(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: feature
        integer, intent(out) :: indices(:)
        integer, intent(out) :: match_count, status
        integer :: i, selected_count

        indices = 0
        match_count = 0
        status = targetir_sx_invalid
        if (count <= 0) return
        if (count > size(instructions)) return
        if (.not. nonempty_atom(feature)) return
        do i = 1, count
            if (.not. valid_instruction(instructions(i))) return
        end do

        selected_count = 0
        do i = 1, count
            if (trim(instructions(i)%feature) == trim(feature)) then
                selected_count = selected_count + 1
            end if
        end do
        if (selected_count == 0) return
        if (selected_count > size(indices)) then
            status = targetir_sx_capacity
            return
        end if
        do i = 1, count
            if (trim(instructions(i)%feature) == trim(feature)) then
                match_count = match_count + 1
                indices(match_count) = i
            end if
        end do
        status = targetir_sx_ok
    end subroutine query_targetir_v0_feature

    pure logical function validate_targetir_v0(text)
        character(len=*), intent(in) :: text
        integer :: depth, i

        validate_targetir_v0 = .false.
        if (len_trim(text) == 0) return
        if (text(1:1) /= '(' .or. text(len_trim(text):len_trim(text)) /= ')') return
        if (index(text, '(targetir-v0 ') /= 1) return
        if (.not. has_all_fields(text)) return
        if (.not. known_atoms(text)) return

        depth = 0
        do i = 1, len_trim(text)
            if (text(i:i) == '(') depth = depth + 1
            if (text(i:i) == ')') then
                depth = depth - 1
                if (depth < 0) return
            end if
        end do
        if (depth /= 0) return
        validate_targetir_v0 = .true.
    end function validate_targetir_v0

    pure logical function valid_target(target)
        type(targetir_sx_target_t), intent(in) :: target

        valid_target = nonempty_atom(target%name)
        if (.not. nonempty_atom(target%architecture)) valid_target = .false.
        if (.not. source_ref_valid(target%source)) valid_target = .false.
        if (.not. valid_atoms(target%source)) valid_target = .false.
    end function valid_target

    pure logical function valid_instruction(instruction)
        type(targetir_sx_instruction_t), intent(in) :: instruction

        valid_instruction = nonempty_atom(instruction%name)
        if (.not. nonempty_atom(instruction%feature)) valid_instruction = .false.
        if (.not. source_ref_valid(instruction%source)) valid_instruction = .false.
        if (.not. valid_atoms(instruction%source)) valid_instruction = .false.
        if (.not. known_origin(instruction%origin)) valid_instruction = .false.
    end function valid_instruction

    pure logical function valid_atoms(source)
        type(source_ref_t), intent(in) :: source

        valid_atoms = nonempty_atom(source%artifact)
        if (.not. nonempty_atom(source%object)) valid_atoms = .false.
        if (.not. nonempty_atom(source%source_hash)) valid_atoms = .false.
    end function valid_atoms

    pure logical function nonempty_atom(value)
        character(len=*), intent(in) :: value
        integer :: i

        nonempty_atom = len_trim(value) > 0
        do i = 1, len_trim(value)
            if (value(i:i) == ' ' .or. value(i:i) == '(' .or. value(i:i) == ')') &
                nonempty_atom = .false.
        end do
    end function nonempty_atom

    pure logical function known_origin(origin)
        character(len=*), intent(in) :: origin

        select case (trim(origin))
        case ('authoritative-machine-readable', 'authoritative-prose', 'derived', &
                'comparison', 'differential')
            known_origin = .true.
        case default
            known_origin = .false.
        end select
    end function known_origin

    pure logical function has_all_fields(text)
        character(len=*), intent(in) :: text

        has_all_fields = index(text, '(target (name ') > 0
        if (index(text, '(architecture ') == 0) has_all_fields = .false.
        if (index(text, '(instruction (name ') == 0) has_all_fields = .false.
        if (index(text, '(feature ') == 0) has_all_fields = .false.
        if (index(text, '(origin ') == 0) has_all_fields = .false.
        if (index(text, '(source (artifact ') == 0) has_all_fields = .false.
        if (index(text, '(object ') == 0) has_all_fields = .false.
        if (index(text, '(source-hash ') == 0) has_all_fields = .false.
        if (index(text, '(origin authoritative-machine-readable)') == 0 .and. &
            index(text, '(origin authoritative-prose)') == 0 .and. &
            index(text, '(origin derived)') == 0 .and. &
            index(text, '(origin comparison)') == 0 .and. &
            index(text, '(origin differential)') == 0) has_all_fields = .false.
    end function has_all_fields

    pure logical function known_atoms(text)
        character(len=*), intent(in) :: text
        integer :: close_pos, cursor, open_pos, space_pos
        character(len=64) :: atom, value

        known_atoms = .true.
        cursor = 1
        do
            open_pos = index(text(cursor:), '(')
            if (open_pos == 0) exit
            open_pos = cursor + open_pos - 1
            space_pos = index(text(open_pos:), ' ')
            if (space_pos == 0) exit
            space_pos = open_pos + space_pos - 1
            atom = ''
            if (space_pos - open_pos - 1 > len(atom)) then
                known_atoms = .false.
                return
            end if
            atom(1:space_pos - open_pos - 1) = text(open_pos + 1:space_pos - 1)
            select case (trim(atom))
            case ('targetir-v0', 'target', 'instruction', 'name', 'architecture', &
                    'source', 'artifact', 'object', 'source-hash', 'feature', 'origin')
                if (trim(atom) == 'name' .or. trim(atom) == 'architecture' .or. &
                    trim(atom) == 'artifact' .or. trim(atom) == 'object' .or. &
                    trim(atom) == 'source-hash' .or. trim(atom) == 'feature' .or. &
                    trim(atom) == 'origin') then
                    close_pos = index(text(space_pos + 1:), ')')
                    if (close_pos == 0) then
                        known_atoms = .false.
                        return
                    end if
                    close_pos = space_pos + close_pos
                    value = ''
                    if (close_pos - space_pos - 1 > len(value)) then
                        known_atoms = .false.
                        return
                    end if
                    value(1:close_pos - space_pos - 1) = &
                        text(space_pos + 1:close_pos - 1)
                    if (len_trim(value) == 0) then
                        known_atoms = .false.
                        return
                    end if
                    if (.not. known_origin(value)) then
                        if (trim(atom) == 'origin') then
                            known_atoms = .false.
                            return
                        end if
                    end if
                end if
            case default
                known_atoms = .false.
                return
            end select
            cursor = open_pos + 1
        end do
    end function known_atoms

    subroutine append(text, position, value)
        character(len=*), intent(inout) :: text
        integer, intent(inout) :: position
        character(len=*), intent(in) :: value
        integer :: finish

        finish = position + len(value) - 1
        if (finish <= len(text)) text(position:finish) = value
        position = finish + 1
    end subroutine append

end module fortback_targetir_sx
