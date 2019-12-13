module serialport_dev

    use, intrinsic :: iso_c_binding, only : c_ptr
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : C_NULL_PTR 
    use, intrinsic :: iso_c_binding, only : C_NULL_CHAR 
    use, intrinsic :: iso_c_binding, only : c_associated

    use serialport_types 
    use serialport_utils, only            : spu_open_port
    use serialport_utils, only            : spu_free_port
    use serialport_utils, only            : spu_get_port_by_name
    use serialport_utils, only            : spu_get_port_by_number
    use serialport_info,  only            : serialport_info_t

    implicit none

    private

    type, public      :: serialport_t
        type(c_ptr)   :: spu_port_ptr = C_NULL_PTR 
        logical       :: initialized  = .false.
        logical       :: is_open_flag = .false.
    contains
        procedure :: get_info   => get_serialport_info
        procedure :: open_conn  => open_serialport   
        !procedure :: close_conn => close_serialport 
        final     :: del_serialport
    end type serialport_t

    interface serialport_t
        procedure :: new_serialport_by_name 
        procedure :: new_serialport_by_number
    end interface serialport_t

contains

    ! Class contructors
    ! -----------------------------------------------------------------

    function new_serialport_by_name(port_name,ok) result(port)
        implicit none
        ! Arguments
        character(len=*), intent(in) :: port_name
        logical, intent(out)         :: ok
        ! Return
        type(serialport_t)           :: port
        ! Local variables
        integer(c_int)               :: err_flag

        ok = .false.
        if (port%initialized) return 

        port%initialized = .false.

        call spu_get_port_by_name(port_name//C_NULL_CHAR, port%spu_port_ptr, err_flag)
        if (err_flag == SPU_OK) then 
            if (c_associated(port%spu_port_ptr)) then
                port%initialized = .true.
                ok = .true.
            end if
        end if
    end function new_serialport_by_name

    function new_serialport_by_number(port_num, ok) result(port)
        implicit none
        ! Arguments
        integer, intent(in)         :: port_num
        logical, intent(out)        :: ok
        ! Return
        type(serialport_t)          :: port
        ! Local variables
        integer(c_int)              :: err_flag

        ok = .false.
        if (port%initialized) return 

        port%initialized = .false.

        call spu_get_port_by_number(port_num, port%spu_port_ptr, err_flag)
        if (err_flag == SPU_OK) then
            if (c_associated(port%spu_port_ptr)) then
                port%initialized = .true.
                ok = .true.
            end if
        end if
    end function new_serialport_by_number

    ! Class Methods
    ! -----------------------------------------------------------------

    subroutine open_serialport(this, mode, ok) 
        implicit none

        ! Arguments
        class(serialport_t), intent(inout) :: this
        character(len=*), intent(in)       :: mode
        logical, intent(out), optional     :: ok

        ! Local variables
        logical                            :: ok_mode 
        integer(c_int)                     :: err_flag
        integer(c_int)                     :: mode_flag

        if (present(ok)) then
            ok = .false.
        end if

        if (.not. this%initialized) return 

        call get_mode_flag(mode, mode_flag, ok_mode)
        if (.not. ok_mode) return 

        call spu_open_port(this%spu_port_ptr, mode_flag, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) then
                ok = .true.
            end if
        end if

    end subroutine open_serialport


    function get_serialport_info(this) result(info)
        implicit none
        class(serialport_t), intent(in) :: this
        type(serialport_info_t)         :: info
        if (this%initialized) then 
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

    ! Utility procedures 
    ! -----------------------------------------------------------------

    subroutine get_mode_flag(mode, mode_flag, ok)
        implicit none
        character(len=*), intent(in) :: mode
        integer(c_int), intent(out)  :: mode_flag
        logical, intent(out)         :: ok

        ok = .true. 
        select case (trim(mode)) 
            case ('r')
                mode_flag = SP_MODE_READ
            case ('w') 
                mode_flag = SP_MODE_WRITE
            case ('rw', 'wr') 
                mode_flag = SP_MODE_READ_WRITE 
            case default 
                mode_flag = 0
                ok = .false.
        end select
    end subroutine get_mode_flag


end module serialport_dev
