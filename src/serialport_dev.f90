module serialport_dev

    use, intrinsic :: iso_c_binding, only : c_ptr
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_size_t
    use, intrinsic :: iso_c_binding, only : c_null_ptr 
    use, intrinsic :: iso_c_binding, only : c_null_char 
    use, intrinsic :: iso_c_binding, only : c_new_line
    use, intrinsic :: iso_c_binding, only : c_associated

    use serialport_types 
    use serialport_kinds
    use serialport_info,   only : serialport_info_t
    use serialport_config, only : serialport_config_t
    use serialport_utils,  only : spu_open_port
    use serialport_utils,  only : spu_close_port
    use serialport_utils,  only : spu_free_port
    use serialport_utils,  only : spu_get_port_by_name
    use serialport_utils,  only : spu_get_port_by_number
    use serialport_utils,  only : spu_set_config
    use serialport_utils,  only : get_mode_enum 
    use serialport_utils,  only : spu_set_baudrate
    use serialport_utils,  only : spu_set_bits
    use serialport_utils,  only : spu_set_parity
    use serialport_utils,  only : spu_set_stopbits
    use serialport_utils,  only : spu_set_rts
    use serialport_utils,  only : spu_set_cts
    use serialport_utils,  only : spu_set_dtr
    use serialport_utils,  only : spu_set_dsr
    use serialport_utils,  only : spu_set_xon_xoff
    use serialport_utils,  only : spu_set_flowcontrol
    use serialport_utils,  only : spu_blocking_read
    use serialport_utils,  only : spu_blocking_read_next
    use serialport_utils,  only : spu_nonblocking_read
    use serialport_utils,  only : spu_blocking_write
    use serialport_utils,  only : spu_nonblocking_write
    use serialport_utils,  only : spu_input_waiting
    use serialport_utils,  only : spu_output_waiting
    use serialport_utils,  only : spu_flush
    use serialport_utils,  only : spu_get_time_ms
    use serialport_utils,  only : get_parity_enum
    use serialport_utils,  only : get_rts_enum
    use serialport_utils,  only : get_cts_enum
    use serialport_utils,  only : get_dtr_enum
    use serialport_utils,  only : get_dsr_enum
    use serialport_utils,  only : get_xon_xoff_enum
    use serialport_utils,  only : get_flowcontrol_enum
    use serialport_utils,  only : get_buffer_enum
    use serialport_utils,  only : get_time_ms


    implicit none
    private

    character(c_char), parameter :: default_readline_eol = c_new_line


    type, public      :: serialport_t
        type(c_ptr)   :: spu_port_ptr = c_null_ptr 
        logical       :: is_open_flag = .false.
        logical       :: ok_flag      = .false.
    contains
        procedure :: ok                 => get_serialport_ok
        procedure :: get_info           => get_serialport_info
        procedure :: open_conn          => open_serialport   
        procedure :: is_open            => get_serialport_is_open
        procedure :: close_conn         => close_serialport 
        procedure :: get_config         => get_serialport_config
        procedure :: set_config         => set_serialport_config
        procedure :: set_baudrate       => set_serialport_baudrate
        procedure :: set_bytesize       => set_serialport_bytesize
        procedure :: set_parity         => set_serialport_parity
        procedure :: set_stopbits       => set_serialport_stopbits
        procedure :: set_rts            => set_serialport_rts
        procedure :: set_cts            => set_serialport_cts
        procedure :: set_dtr            => set_serialport_dtr
        procedure :: set_dsr            => set_serialport_dsr
        procedure :: set_xon_xoff       => set_serialport_xon_xoff
        procedure :: set_flowcontrol    => set_serialport_flowcontrol
        procedure :: readline           => serialport_readline
        procedure :: blocking_read      => serialport_blocking_read
        procedure :: blocking_read_next => serialport_blocking_read_next
        procedure :: nonblocking_read   => serialport_nonblocking_read
        procedure :: blocking_write     => serialport_blocking_write
        procedure :: nonblocking_write  => serialport_nonblocking_write
        procedure :: in_waiting         => get_serialport_in_waiting
        procedure :: out_waiting        => get_serialport_out_waiting
        procedure :: flush_buffer       => flush_serialport_buffer 
        final     :: del_serialport
    end type serialport_t

    interface serialport_t
        procedure :: new_serialport_by_name 
        procedure :: new_serialport_by_number
    end interface serialport_t

contains

    ! Class contructors
    ! -----------------------------------------------------------------

    function new_serialport_by_name(port_name) result(port)
        implicit none
        ! Arguments
        character(len=*), intent(in) :: port_name
        ! Return
        type(serialport_t)           :: port
        ! Local variables
        integer(c_int)               :: err_flag

        port%ok_flag = .false.
        call spu_get_port_by_name(port_name//c_null_char, port%spu_port_ptr, err_flag)
        if (err_flag == SPU_OK) then 
            if (c_associated(port%spu_port_ptr)) then
                port%ok_flag = .true.
            end if
        end if
    end function new_serialport_by_name


    function new_serialport_by_number(port_num) result(port)
        implicit none
        ! Arguments
        integer, intent(in)         :: port_num
        ! Return
        type(serialport_t)          :: port
        ! Local variables
        integer(c_int)              :: err_flag

        port%ok_flag = .false.
        call spu_get_port_by_number(port_num, port%spu_port_ptr, err_flag)
        if (err_flag == SPU_OK) then
            if (c_associated(port%spu_port_ptr)) then
                port%ok_flag = .true.
            end if
        end if
    end function new_serialport_by_number


    ! Class Methods
    ! -----------------------------------------------------------------

    function get_serialport_ok(this) result(val)
        implicit none
        ! Argument
        class(serialport_t), intent(in) :: this
        ! Return
        logical                         :: val
        val = this%ok_flag
    end function get_serialport_ok


    function get_serialport_info(this) result(info)
        implicit none
        ! Argument
        class(serialport_t), intent(in) :: this
        ! Return
        type(serialport_info_t)         :: info
        if (this%ok_flag) then 
            info = serialport_info_t(this%spu_port_ptr)
        else
            info = serialport_info_t()
        end if
    end function get_serialport_info


    subroutine open_serialport(this, mode, ok) 
        implicit none
        ! Arguments
        class(serialport_t), intent(inout) :: this
        character(len=*), intent(in)       :: mode
        logical, intent(out), optional     :: ok
        ! Local variables
        logical                            :: ok_mode 
        integer(c_int)                     :: err_flag
        integer(c_int)                     :: mode_enum

        if (present(ok)) then
            ok = .false.
        end if
        if (.not.this%ok_flag) return

        call get_mode_enum(mode, mode_enum, ok_mode)
        if (.not. ok_mode) return 

        call spu_open_port(this%spu_port_ptr, mode_enum, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) then
                ok = .true.
            end if
            this%is_open_flag = .true.
        end if
    end subroutine open_serialport


    subroutine close_serialport(this, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(inout) ::this
        logical, intent(out), optional     :: ok
        ! Local variables
        integer(c_int)                     :: err_flag

        if (present(ok)) then
            ok = .false.
        end if
        if (.not. this%ok_flag)  return
        if (.not. this%is_open_flag) return

        call spu_close_port(this%spu_port_ptr, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) then
                ok = .true.
            end if
            this%is_open_flag = .false.
        end if
    end subroutine close_serialport

    
    function get_serialport_is_open(this)  result(val)
        implicit none
        ! Argument
        class(serialport_t), intent(in) :: this
        ! Return
        logical                         :: val
        val = this%is_open_flag
    end function get_serialport_is_open

    
    function get_serialport_config(this) result(config)
        implicit none
        ! Argument
        class(serialport_t), intent(in) :: this
        ! Return
        type(serialport_config_t)       :: config
        config = serialport_config_t(this%spu_port_ptr)
    end function get_serialport_config


    subroutine set_serialport_config(this, config, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)       :: this
        type(serialport_config_t), intent(in) :: config
        logical, optional, intent(out)        :: ok
        ! Local variables
        integer(c_int)                        :: err_flag

        if (present(ok)) ok = .false.
        if (.not. config%ok()) return
        if (.not. this%ok_flag) return

        call spu_set_config(this%spu_port_ptr, config%spu_config_ptr, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine set_serialport_config


    subroutine set_serialport_baudrate(this, baudrate, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)    :: this
        integer, intent(in)                :: baudrate
        logical, optional, intent(out)     :: ok
        ! Local variables
        integer(c_int)                     :: baudrate_tmp
        integer(c_int)                     :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        baudrate_tmp = int(baudrate, kind(c_int))
        call spu_set_baudrate(this%spu_port_ptr, baudrate_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine set_serialport_baudrate


    subroutine set_serialport_bytesize(this, bytesize, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)    :: this
        integer, intent(in)                :: bytesize
        logical, optional, intent(out)     :: ok
        ! Local varialbes
        integer(c_int)                     :: bytesize_tmp
        integer(c_int)                     :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return
        
        bytesize_tmp = int(bytesize, kind(c_int))
        call spu_set_bits(this%spu_port_ptr, bytesize_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine set_serialport_bytesize


    subroutine set_serialport_parity(this, parity_mode, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)     :: this
        character(len=*), intent(in)        :: parity_mode
        logical, optional, intent(out)      :: ok
        ! Local variables
        integer(c_int)                      :: parity_enum
        integer(c_int)                      :: err_flag
        logical                             :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_parity_enum(parity_mode, parity_enum, enum_ok)
        if (enum_ok) then
            call spu_set_parity(this%spu_port_ptr, parity_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_serialport_parity


    subroutine set_serialport_stopbits(this, stopbits, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)     :: this
        integer, intent(in)                 :: stopbits
        logical, optional, intent(out)      :: ok
        ! Local variables
        integer(c_int)                      :: stopbits_tmp
        integer(c_int)                      :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        stopbits_tmp = int(stopbits,kind(c_int))
        call spu_set_stopbits(this%spu_port_ptr, stopbits_tmp, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine set_serialport_stopbits


    subroutine set_serialport_rts(this, rts, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)    :: this
        character(len=*), intent(in)       :: rts
        logical, optional, intent(out)     :: ok
        ! Local variables
        integer(c_int)                     :: rts_enum
        integer(c_int)                     :: err_flag
        logical                            :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_rts_enum(rts, rts_enum, enum_ok)
        if (enum_ok) then
            call spu_set_rts(this%spu_port_ptr, rts_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_serialport_rts


    subroutine set_serialport_cts(this, cts, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        character(len=*), intent(in)      :: cts
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: cts_enum
        integer(c_int)                    :: err_flag
        logical                           :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_cts_enum(cts, cts_enum, enum_ok)
        if (enum_ok) then
            call spu_set_cts(this%spu_port_ptr, cts_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_serialport_cts


    subroutine set_serialport_dtr(this, dtr, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        character(len=*), intent(in)      :: dtr
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: dtr_enum
        integer(c_int)                    :: err_flag
        logical                           :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_dtr_enum(dtr, dtr_enum, enum_ok)
        if (enum_ok) then
            call spu_set_dtr(this%spu_port_ptr, dtr_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_serialport_dtr


    subroutine set_serialport_dsr(this, dsr, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        character(len=*), intent(in)      :: dsr
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: dsr_enum
        integer(c_int)                    :: err_flag
        logical                           :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_dsr_enum(dsr, dsr_enum, enum_ok)
        if (enum_ok) then
            call spu_set_dsr(this%spu_port_ptr, dsr_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_serialport_dsr


    subroutine set_serialport_xon_xoff(this, xon_xoff, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        character(len=*), intent(in)      :: xon_xoff
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: xon_xoff_enum
        integer(c_int)                    :: err_flag
        logical                           :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_xon_xoff_enum(xon_xoff, xon_xoff_enum, enum_ok)
        if (enum_ok) then
            call spu_set_xon_xoff(this%spu_port_ptr, xon_xoff_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_serialport_xon_xoff


    subroutine set_serialport_flowcontrol(this, flowcontrol, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        character(len=*), intent(in)      :: flowcontrol
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: flowcontrol_enum
        integer(c_int)                    :: err_flag
        logical                           :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_flowcontrol_enum(flowcontrol, flowcontrol_enum, enum_ok)
        if (enum_ok) then
            call spu_set_flowcontrol(this%spu_port_ptr, flowcontrol_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_serialport_flowcontrol


    subroutine serialport_readline(this, bytes, timeout_ms, ok, eol)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)               :: this
        character(:,c_char), allocatable, intent(out) :: bytes
        integer, intent(in)                           :: timeout_ms 
        logical, optional, intent(out)                :: ok
        character(c_char), optional, intent(in)       :: eol
        ! Local variables
        character(c_char)                             :: eol_char
        integer                                       :: alloc_stat
        integer(c_size_t)                             :: read_num
        character(1,c_char)                           :: read_val
        logical                                       :: read_ok
        real(dp)                                      :: t_start
        real(dp)                                      :: dt
        integer                                       :: n

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        if (allocated(bytes)) then
            deallocate(bytes, stat=alloc_stat)
            if (alloc_stat /= 0) return
        end if

        if (present(eol)) then
            eol_char = eol
        else
            eol_char = default_readline_eol
        end if

        allocate(character(0,c_char)::bytes, stat=alloc_stat)
        if (alloc_stat /= 0) return

        t_start = get_time_ms()
        do 
            ! Read new byte value from serial port
            read_num = 1
            call this%nonblocking_read(read_num,read_val,read_ok)
            if (.not. read_ok) return
            if (read_num == 1) then 
                bytes = bytes//read_val
                ! Check for termination criteria
                n = len(bytes)
                if (bytes(n:n) == eol_char) then
                    if (present(ok)) ok = .true.
                    exit
                end if
            end if

            ! Elapsed time for timeout condition and exit if necessary
            dt = get_time_ms() - t_start
            if (dt > timeout_ms) then
                if (present(ok)) ok = .false.
                exit
            end if
        end do
    end subroutine serialport_readline


    subroutine serialport_blocking_read(this, num_bytes, bytes, timeout_ms,  ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)           :: this
        integer(c_size_t), intent(inout)          :: num_bytes
        character(num_bytes,c_char), intent(out)  :: bytes   
        integer, intent(in)                       :: timeout_ms 
        logical, optional, intent(out)            :: ok
        ! Local variables
        character(1,c_char)                       :: bytes_tmp(num_bytes)
        integer(c_size_t)                         :: num_bytes_req
        integer(c_size_t)                         :: num_bytes_tru
        integer(c_int)                            :: err_flag
        integer(c_size_t)                         :: i

        num_bytes_req = num_bytes
        num_bytes_tru = num_bytes
        num_bytes = 0

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_blocking_read(this%spu_port_ptr, bytes_tmp, num_bytes_tru, timeout_ms, err_flag)  
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            do i=1,num_bytes_req
                if (i <= num_bytes_tru) then
                    bytes(i:i) = bytes_tmp(i)
                else
                    bytes(i:i) = ' '
                end if
            end do 
            num_bytes = num_bytes_tru
        end if
    end subroutine serialport_blocking_read


    subroutine serialport_blocking_read_next(this, num_bytes, bytes, timeout_ms,  ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)           :: this
        integer(c_size_t), intent(inout)          :: num_bytes
        character(num_bytes,c_char), intent(out)  :: bytes   
        integer, intent(in)                       :: timeout_ms 
        logical, optional, intent(out)            :: ok
        ! Local variables
        character(1,c_char)                       :: bytes_tmp(num_bytes)
        integer(c_size_t)                         :: num_bytes_req
        integer(c_size_t)                         :: num_bytes_tru
        integer(c_int)                            :: err_flag
        integer(c_size_t)                         :: i

        num_bytes_req = num_bytes
        num_bytes_tru = num_bytes
        num_bytes = 0

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_blocking_read_next(this%spu_port_ptr, bytes_tmp, num_bytes_tru, timeout_ms, err_flag)  
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            do i=1,num_bytes_req
                if (i <= num_bytes_tru) then
                    bytes(i:i) = bytes_tmp(i)
                else
                    bytes(i:i) = ' '
                end if
            end do 
            num_bytes = num_bytes_tru
        end if
    end subroutine serialport_blocking_read_next


    subroutine serialport_nonblocking_read(this, num_bytes, bytes, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)           :: this
        integer(c_size_t), intent(inout)          :: num_bytes
        character(num_bytes,c_char), intent(out)  :: bytes   
        logical, optional, intent(out)            :: ok
        ! Local variables
        character(1,c_char)                       :: bytes_tmp(num_bytes)
        integer(c_size_t)                         :: num_bytes_req
        integer(c_size_t)                         :: num_bytes_tru
        integer(c_int)                            :: err_flag
        integer(c_size_t)                         :: i

        num_bytes_req = num_bytes
        num_bytes_tru = num_bytes
        num_bytes = 0

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_nonblocking_read(this%spu_port_ptr, bytes_tmp, num_bytes_tru, err_flag)  
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            do i=1,num_bytes_req
                if (i <= num_bytes_tru) then
                    bytes(i:i) = bytes_tmp(i)
                else
                    bytes(i:i) = ' '
                end if
            end do 
            num_bytes = num_bytes_tru
        end if
    end subroutine serialport_nonblocking_read


    subroutine serialport_blocking_write(this, num_bytes, bytes, timeout_ms, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)          :: this
        integer(c_size_t), intent(inout)         :: num_bytes
        character(num_bytes,c_char), intent(in)  :: bytes  
        integer, intent(in)                      :: timeout_ms 
        logical, optional, intent(out)           :: ok
        ! Local variables
        character(1,c_char)                      :: bytes_tmp(num_bytes)
        integer(c_size_t)                        :: num_bytes_tmp
        integer(c_int)                           :: err_flag
        integer(c_size_t)                        :: i

        num_bytes_tmp = num_bytes
        num_bytes = 0

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        do i=1,num_bytes_tmp
            bytes_tmp(i) = bytes(i:i)
        end do

        call spu_blocking_write(this%spu_port_ptr, bytes_tmp, num_bytes_tmp, timeout_ms, err_flag)  
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            num_bytes = num_bytes_tmp
        end if
    end subroutine serialport_blocking_write


    subroutine serialport_nonblocking_write(this, num_bytes, bytes, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)          :: this
        integer(c_size_t), intent(inout)         :: num_bytes
        character(num_bytes,c_char), intent(in)  :: bytes  
        logical, optional, intent(out)           :: ok
        ! Local variables
        character(1,c_char)                      :: bytes_tmp(num_bytes)
        integer(c_size_t)                        :: num_bytes_tmp
        integer(c_int)                           :: err_flag
        integer(c_size_t)                        :: i

        num_bytes_tmp = num_bytes
        num_bytes = 0

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        do i=1,num_bytes_tmp
            bytes_tmp(i) = bytes(i:i)
        end do

        call spu_nonblocking_write(this%spu_port_ptr, bytes_tmp, num_bytes_tmp, err_flag)  
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            num_bytes = num_bytes_tmp
        end if
    end subroutine serialport_nonblocking_write


    subroutine get_serialport_in_waiting(this, num_bytes, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        integer(c_size_t), intent(out)    :: num_bytes
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: err_flag

        num_bytes = 0
        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_input_waiting(this%spu_port_ptr, num_bytes, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine get_serialport_in_waiting


    subroutine get_serialport_out_waiting(this, num_bytes, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        integer(c_size_t), intent(out)    :: num_bytes
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: err_flag

        num_bytes = 0
        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_output_waiting(this%spu_port_ptr, num_bytes, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
        end if
    end subroutine get_serialport_out_waiting


    subroutine flush_serialport_buffer(this, buffer, ok)
        implicit none
        ! Arguments
        class(serialport_t), intent(in)   :: this
        character(len=*), intent(in)      :: buffer
        logical, optional, intent(out)    :: ok
        ! Local variables
        integer(c_int)                    :: buffer_enum
        logical                           :: enum_ok
        integer(c_int)                    :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_buffer_enum(buffer, buffer_enum, enum_ok)
        if (enum_ok) then
            call spu_flush(this%spu_port_ptr, buffer_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine flush_serialport_buffer


    subroutine del_serialport(this) 
        implicit none
        type(serialport_t), intent(inout) :: this
        if (c_associated(this%spu_port_ptr)) then 
            call spu_free_port(this%spu_port_ptr)
        end if
    end subroutine del_serialport


end module serialport_dev
