module serialport_config

    use, intrinsic :: iso_c_binding, only : c_ptr
    use, intrinsic :: iso_c_binding, only : C_NULL_PTR 
    use, intrinsic :: iso_c_binding, only : c_associated
    use serialport_types 

    implicit none
    private

    type, public     :: serialport_config_t
        type(c_ptr)  :: spu_config_ptr = C_NULL_PTR
    contains
        final :: del_serialport_config
        
    end type serialport_config_t

contains

    subroutine del_serialport_config(this)
        implicit none
        type(serialport_config_t), intent(inout) :: this
        if (c_associated(this%spu_config_ptr)) then 
            !call spu_free_config(this%spu_config_ptr)
        end if
    end subroutine del_serialport_config

end module serialport_config
