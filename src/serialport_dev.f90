module serialport_dev

    use, intrinsic :: iso_c_binding, only : c_ptr
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : C_NULL_PTR 
    use, intrinsic :: iso_c_binding, only : C_NULL_CHAR 
    use, intrinsic :: iso_c_binding, only : c_associated

    use serialport_types, only            : SPU_OK
    use serialport_utils, only            : spu_free_port
    use serialport_utils, only            : spu_get_port_by_name
    use serialport_utils, only            : spu_get_port_by_number
    use serialport_info,  only            : serialport_info_t

    implicit none

    private

    type, public      :: serialport_t
        type(c_ptr)   :: spu_port_ptr = C_NULL_PTR 
        logical       :: initialized  = .false.
    contains
        procedure :: get_info => get_serialport_info
        final     :: del_serialport
    end type serialport_t

    interface serialport_t
        procedure :: new_serialport_by_name 
        procedure :: new_serialport_by_number
    end interface serialport_t

contains

    function new_serialport_by_name(port_name,ok) result(port)
        implicit none
        character(len=*), intent(in) :: port_name
        logical, intent(out)         :: ok
        type(serialport_t)           :: port
        integer(c_int)               :: err_flag

        call spu_get_port_by_name(port_name//C_NULL_CHAR, port%spu_port_ptr, err_flag)
        if (err_flag /= SPU_OK) then 
            port%initialized = .false.
            ok = .false.
            return
        end if
        port%initialized = .true.
        ok = .true.
    end function new_serialport_by_name

    function new_serialport_by_number(port_num, ok) result(port)
        implicit none
        integer, intent(in)         :: port_num
        logical, intent(out)        :: ok
        type(serialport_t)          :: port
        integer(c_int)              :: err_flag

        call spu_get_port_by_number(port_num, port%spu_port_ptr, err_flag)
        if (err_flag /= SPU_OK) then
            port%initialized = .false.
            ok = .false.
            return
        end if
        port%initialized = .true.
        ok = .true.
    end function new_serialport_by_number

    function get_serialport_info(this) result(info)
        implicit none
        class(serialport_t), intent(in) :: this
        type(serialport_info_t)         :: info
        if ((this%initialized) .and. (c_associated(this%spu_port_ptr))) then
            info = serialport_info_t(this%spu_port_ptr)
        else
            info = serialport_info_t()
        end if
    end function get_serialport_info

    subroutine del_serialport(this) 
        implicit none
        type(serialport_t), intent(inout) :: this
        if (c_associated(this%spu_port_ptr)) then 
            print *, 'freeing ptr'
            call spu_free_port(this%spu_port_ptr)
        end if
    end subroutine del_serialport

end module serialport_dev
