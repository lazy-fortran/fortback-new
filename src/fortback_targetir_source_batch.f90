module fortback_targetir_source_batch
    use iso_fortran_env, only: int32
    use fortback_aarch64_source, only: aarch64_encoding_record_t
    use fortback_riscv_source, only: riscv_opcode_record_t
    use fortback_target_ir, only: target_ir_t
    use fortback_targetir_encoding, only: normalize_aarch64_record, &
        normalize_riscv_i_record, &
        targetir_encoding_invalid_target, targetir_encoding_malformed, &
        targetir_encoding_ok, targetir_encoding_record_t, targetir_encoding_unsupported
    use fortback_targetir_table, only: targetir_encoding_table_append, &
        targetir_encoding_table_clear, targetir_encoding_table_t, targetir_table_capacity, &
        targetir_table_duplicate
    implicit none
    private

    integer(int32), parameter, public :: targetir_source_batch_ok = targetir_encoding_ok
    integer(int32), parameter, public :: targetir_source_batch_malformed = &
        targetir_encoding_malformed
    integer(int32), parameter, public :: targetir_source_batch_unsupported = &
        targetir_encoding_unsupported
    integer(int32), parameter, public :: targetir_source_batch_invalid_target = &
        targetir_encoding_invalid_target
    integer(int32), parameter, public :: targetir_source_batch_capacity = targetir_table_capacity
    integer(int32), parameter, public :: targetir_source_batch_duplicate = &
        targetir_table_duplicate

    integer(int32), parameter, public :: targetir_source_batch_riscv_i = 1_int32
    integer(int32), parameter, public :: targetir_source_batch_aarch64 = 2_int32

    type, public :: targetir_source_batch_item_t
        integer(int32) :: kind = 0_int32
        type(target_ir_t) :: target
        type(riscv_opcode_record_t) :: riscv_i
        type(aarch64_encoding_record_t) :: aarch64
    end type targetir_source_batch_item_t

    public :: make_aarch64_source_batch_item
    public :: make_riscv_i_source_batch_item
    public :: normalize_targetir_source_batch

contains

    pure function make_riscv_i_source_batch_item(target, record) result(item)
        type(target_ir_t), intent(in) :: target
        type(riscv_opcode_record_t), intent(in) :: record
        type(targetir_source_batch_item_t) :: item

        item = targetir_source_batch_item_t()
        item%kind = targetir_source_batch_riscv_i
        item%target = target
        item%riscv_i = record
    end function make_riscv_i_source_batch_item

    pure function make_aarch64_source_batch_item(target, record) result(item)
        type(target_ir_t), intent(in) :: target
        type(aarch64_encoding_record_t), intent(in) :: record
        type(targetir_source_batch_item_t) :: item

        item = targetir_source_batch_item_t()
        item%kind = targetir_source_batch_aarch64
        item%target = target
        item%aarch64 = record
    end function make_aarch64_source_batch_item

    subroutine normalize_targetir_source_batch(items, table, status)
        type(targetir_source_batch_item_t), intent(in) :: items(:)
        type(targetir_encoding_table_t), intent(inout) :: table
        integer(int32), intent(out) :: status
        type(targetir_encoding_record_t) :: normalized
        integer :: i

        call targetir_encoding_table_clear(table)
        status = targetir_source_batch_ok
        if (size(items) > targetir_table_capacity) then
            status = targetir_source_batch_capacity
            return
        end if

        do i = 1, size(items)
            call normalize_source_item(items(i), normalized, status)
            if (status /= targetir_source_batch_ok) then
                call targetir_encoding_table_clear(table)
                return
            end if
            call targetir_encoding_table_append(table, items(i)%target, normalized, status)
            if (status /= targetir_source_batch_ok) then
                call targetir_encoding_table_clear(table)
                return
            end if
        end do
    end subroutine normalize_targetir_source_batch

    subroutine normalize_source_item(item, normalized, status)
        type(targetir_source_batch_item_t), intent(in) :: item
        type(targetir_encoding_record_t), intent(out) :: normalized
        integer(int32), intent(out) :: status

        normalized = targetir_encoding_record_t()
        select case (item%kind)
        case (targetir_source_batch_riscv_i)
            call normalize_riscv_i_record(item%target, item%riscv_i, normalized, status)
        case (targetir_source_batch_aarch64)
            call normalize_aarch64_record(item%target, item%aarch64, normalized, status)
        case default
            status = targetir_source_batch_unsupported
        end select
    end subroutine normalize_source_item

end module fortback_targetir_source_batch
