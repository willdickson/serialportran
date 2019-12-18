module serialport_config

    use, intrinsic :: iso_c_binding, only : c_ptr
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : C_NULL_PTR 
    use, intrinsic :: iso_c_binding, only : c_associated
    use serialport_utils, only : spu_new_config
    use serialport_utils, only : spu_free_config
    use serialport_utils, only : spu_get_config
    use serialport_utils, only : spu_get_config_baudrate
    use serialport_utils, only : spu_set_config_baudrate
    use serialport_utils, only : spu_get_config_bits
    use serialport_utils, only : spu_set_config_bits
    use serialport_utils, only : spu_get_config_parity
    use serialport_utils, only : spu_set_config_parity
    use serialport_utils, only : spu_get_config_stopbits
    use serialport_utils, only : spu_set_config_stopbits
    use serialport_utils, only : get_parity_string
    use serialport_utils, only : get_parity_enum
    use serialport_types 

    implicit none
    private

    type, public     :: serialport_config_t
        type(c_ptr)  :: spu_config_ptr = C_NULL_PTR
        logical      :: ok_flag = .false.
    contains
        procedure :: get_baudrate => get_config_baudrate
        procedure :: set_baudrate => set_config_baudrate
        procedure :: get_bytesize => get_config_bytesize
        procedure :: set_bytesize => set_config_bytesize
        procedure :: get_parity   => get_config_parity
        procedure :: set_parity   => set_config_parity
        procedure :: get_stopbits => get_config_stopbits
        procedure :: set_stopbits => set_config_stopbits
        procedure :: ok           => get_config_ok
        final     :: del_config
    end type serialport_config_t

    interface serialport_config_t
        procedure :: new_serialport_config
        procedure :: new_serialport_config_from_ptr
    end interface serialport_config_t

contains

    ! Constructors
    ! -------------------------------------------------------------------------

    function new_serialport_config() result(config)
        implicit none
        type(serialport_config_t)      :: config
        integer(c_int)                 :: err_flag
        config%ok_flag = .false.
        call spu_new_config(config%spu_config_ptr,err_flag)
        if (err_flag == SPU_OK) config%ok_flag = .true.
    end function new_serialport_config

    function new_serialport_config_from_ptr(spu_port_ptr) result(config)
        implicit none
        type(c_ptr), intent(in), value :: spu_port_ptr
        type(serialport_config_t)      :: config
        integer(c_int)                 :: err_flag

        ! Allocate new config struct
        config%ok_flag = .false.
        call spu_new_config(config%spu_config_ptr,err_flag)
        if ((err_flag /= SPU_OK)) return 

        ! Get configuration from device
        call spu_get_config(spu_port_ptr, config%spu_config_ptr, err_flag)
        if (err_flag == SPU_OK) config%ok_flag = .true. 
    end function new_serialport_config_from_ptr

    ! Methods
    ! ------------------------------------------------------------------------

    function get_config_ok(this) result(val)
        implicit none
        class(serialport_config_t), intent(in) :: this
        logical                                :: val 
        val = this%ok_flag
    end function get_config_ok


    subroutine get_config_baudrate(this,baudrate,ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        integer, intent(out)                   :: baudrate
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: baudrate_tmp
        integer(c_int)                         :: err_flag 

        baudrate = 0
        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return 

        call spu_get_config_baudrate(this%spu_config_ptr, baudrate_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            baudrate = int(baudrate_tmp,kind(baudrate))
        end if
    end subroutine get_config_baudrate


    subroutine set_config_baudrate(this,baudrate,ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        integer, intent(in)                    :: baudrate
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: baudrate_tmp
        integer(c_int)                         :: err_flag 

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return 

        baudrate_tmp = int(baudrate,kind(c_int))
        call spu_set_config_baudrate(this%spu_config_ptr, baudrate, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine set_config_baudrate


    subroutine get_config_bytesize(this,bytesize,ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        integer, intent(out)                   :: bytesize
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: bytesize_tmp
        integer(c_int)                         :: err_flag

        bytesize = 0
        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return 

        call spu_get_config_bits(this%spu_config_ptr, bytesize_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            bytesize = int(bytesize_tmp, kind(bytesize))
        end if
    end subroutine get_config_bytesize


    subroutine set_config_bytesize(this,bytesize,ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        integer, intent(in)                    :: bytesize
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: bytesize_tmp
        integer(c_int)                         :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return 

        bytesize_tmp = int(bytesize,kind(c_int))
        call spu_set_config_bits(this%spu_config_ptr, bytesize_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine set_config_bytesize


    subroutine get_config_parity(this, parity_mode, ok)
        implicit none
        class(serialport_config_t), intent(in)     :: this
        character(len=:), allocatable, intent(out) :: parity_mode
        logical, optional, intent(out)             :: ok
        integer(c_int)                             :: parity_enum
        integer(c_int)                             :: err_flag

        parity_mode = ''
        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_get_config_parity(this%spu_config_ptr, parity_enum, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            parity_mode = get_parity_string(parity_enum)
        end if
    end subroutine get_config_parity


    subroutine set_config_parity(this, parity_mode, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        character(len=*), intent(in)           :: parity_mode
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: parity_enum
        integer(c_int)                         :: err_flag
        logical                                :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_parity_enum(parity_mode, parity_enum, enum_ok)
        if (enum_ok) then
            call spu_set_config_parity(this%spu_config_ptr, parity_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_config_parity


    subroutine get_config_stopbits(this, stopbits, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        integer, intent(out)                   :: stopbits
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: stopbits_tmp
        integer(c_int)                         :: err_flag

        stopbits = 0
        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_get_config_stopbits(this%spu_config_ptr, stopbits_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            stopbits = int(stopbits_tmp, kind(stopbits))
        end if
    end subroutine get_config_stopbits


    subroutine set_config_stopbits(this, stopbits, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        integer, intent(in)                    :: stopbits
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: stopbits_tmp
        integer(c_int)                         :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        stopbits_tmp = int(stopbits,kind(c_int))
        call spu_set_config_stopbits(this%spu_config_ptr, stopbits_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine set_config_stopbits


    ! Destructor
    ! -------------------------------------------------------------------------
    subroutine del_config(this)
        implicit none
        type(serialport_config_t), intent(inout) :: this
        integer(c_int)                           :: err_flag
        if (c_associated(this%spu_config_ptr)) then 
            call spu_free_config(this%spu_config_ptr,err_flag)
        end if
    end subroutine del_config

end module serialport_config
