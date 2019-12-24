module serialport_config

    use, intrinsic :: iso_c_binding, only : c_ptr
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : C_NULL_PTR 
    use, intrinsic :: iso_c_binding, only : c_associated
    use, intrinsic :: iso_fortran_env, only : stdout => output_unit
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
    use serialport_utils, only : spu_get_config_rts
    use serialport_utils, only : spu_set_config_rts
    use serialport_utils, only : spu_get_config_cts
    use serialport_utils, only : spu_set_config_cts
    use serialport_utils, only : spu_get_config_dtr
    use serialport_utils, only : spu_set_config_dtr
    use serialport_utils, only : spu_get_config_dsr
    use serialport_utils, only : spu_set_config_dsr
    use serialport_utils, only : spu_get_config_xon_xoff
    use serialport_utils, only : spu_set_config_xon_xoff
    use serialport_utils, only : spu_set_config_flowcontrol
    use serialport_utils, only : get_parity_string
    use serialport_utils, only : get_parity_enum
    use serialport_utils, only : get_rts_string
    use serialport_utils, only : get_rts_enum
    use serialport_utils, only : get_cts_string
    use serialport_utils, only : get_cts_enum
    use serialport_utils, only : get_dtr_string
    use serialport_utils, only : get_dtr_enum
    use serialport_utils, only : get_dsr_string
    use serialport_utils, only : get_dsr_enum
    use serialport_utils, only : get_xon_xoff_string
    use serialport_utils, only : get_xon_xoff_enum
    use serialport_utils, only : get_flowcontrol_string
    use serialport_utils, only : get_flowcontrol_enum
    use serialport_types 

    implicit none
    private

    type, public     :: serialport_config_t
        type(c_ptr)  :: spu_config_ptr = C_NULL_PTR
        logical      :: ok_flag = .false.
    contains
        procedure :: ok              => get_config_ok
        procedure :: get_baudrate    => get_config_baudrate
        procedure :: set_baudrate    => set_config_baudrate
        procedure :: get_bytesize    => get_config_bytesize
        procedure :: set_bytesize    => set_config_bytesize
        procedure :: get_parity      => get_config_parity
        procedure :: set_parity      => set_config_parity
        procedure :: get_stopbits    => get_config_stopbits
        procedure :: set_stopbits    => set_config_stopbits
        procedure :: get_rts         => get_config_rts
        procedure :: set_rts         => set_config_rts
        procedure :: get_cts         => get_config_cts
        procedure :: set_cts         => set_config_cts
        procedure :: get_dtr         => get_config_dtr
        procedure :: set_dtr         => set_config_dtr
        procedure :: get_dsr         => get_config_dsr
        procedure :: set_dsr         => set_config_dsr
        procedure :: get_xon_xoff    => get_config_xon_xoff
        procedure :: set_xon_xoff    => set_config_xon_xoff
        procedure :: set_flowcontrol => set_config_flowcontrol
        procedure :: print_verbose   => print_config_verbose
        procedure :: print_concise   => print_config_concise
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


    subroutine get_config_rts(this, rts, ok)
        implicit none
        class(serialport_config_t), intent(in)     :: this
        character(len=:), allocatable, intent(out) :: rts
        logical, optional, intent(out)             :: ok
        integer(c_int)                             :: rts_enum
        integer(c_int)                             :: err_flag

        rts = ''
        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_get_config_rts(this%spu_config_ptr, rts_enum, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            rts = get_rts_string(rts_enum)
        end if
    end subroutine get_config_rts


    subroutine set_config_rts(this, rts, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        character(len=*), intent(in)           :: rts
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: rts_enum
        integer(c_int)                         :: err_flag
        logical                                :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_rts_enum(rts, rts_enum, enum_ok)
        if (enum_ok) then
            call spu_set_config_rts(this%spu_config_ptr, rts_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_config_rts


    subroutine get_config_cts(this, cts, ok)
        implicit none
        class(serialport_config_t), intent(in)     :: this
        character(len=:), allocatable, intent(out) :: cts
        logical, optional, intent(out)             :: ok
        integer(c_int)                             :: cts_enum
        integer(c_int)                             :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_get_config_cts(this%spu_config_ptr, cts_enum, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            cts = get_cts_string(cts_enum)
        end if
    end subroutine get_config_cts


    subroutine set_config_cts(this, cts, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        character(len=*), intent(in)           :: cts
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: cts_enum
        integer(c_int)                         :: err_flag
        logical                                :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_cts_enum(cts, cts_enum, enum_ok)
        if (enum_ok) then
            call spu_set_config_cts(this%spu_config_ptr, cts_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_config_cts


    subroutine get_config_dtr(this, dtr, ok)
        implicit none
        class(serialport_config_t), intent(in)     :: this
        character(len=:), allocatable, intent(out) :: dtr
        logical, optional, intent(out)             :: ok
        integer(c_int)                             :: dtr_enum
        integer(c_int)                             :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_get_config_dtr(this%spu_config_ptr, dtr_enum, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            dtr = get_dtr_string(dtr_enum)
        end if
    end subroutine get_config_dtr


    subroutine set_config_dtr(this, dtr, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        character(len=*), intent(in)           :: dtr
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: dtr_enum
        integer(c_int)                         :: err_flag
        logical                                :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_dtr_enum(dtr, dtr_enum, enum_ok)
        if (enum_ok) then
            call spu_set_config_dtr(this%spu_config_ptr, dtr_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_config_dtr


    subroutine get_config_dsr(this, dsr, ok)
        implicit none
        class(serialport_config_t), intent(in)     :: this
        character(len=:), allocatable, intent(out) :: dsr
        logical, optional, intent(out)             :: ok
        integer(c_int)                             :: dsr_enum
        integer(c_int)                             :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_get_config_dsr(this%spu_config_ptr, dsr_enum, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            dsr = get_dsr_string(dsr_enum)
        end if
    end subroutine get_config_dsr


    subroutine set_config_dsr(this, dsr, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        character(len=*), intent(in)           :: dsr
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: dsr_enum
        integer(c_int)                         :: err_flag
        logical                                :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_dsr_enum(dsr, dsr_enum, enum_ok)
        if (enum_ok) then
            call spu_set_config_dsr(this%spu_config_ptr, dsr_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_config_dsr


    subroutine get_config_xon_xoff(this, xon_xoff, ok)
        implicit none
        class(serialport_config_t), intent(in)     :: this
        character(len=:), allocatable, intent(out) :: xon_xoff
        logical, optional, intent(out)             :: ok
        integer(c_int)                             :: xon_xoff_enum
        integer(c_int)                             :: err_flag

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call spu_get_config_xon_xoff(this%spu_config_ptr, xon_xoff_enum, err_flag)
        if (err_flag == SPU_OK) then
            if (present(ok)) ok = .true.
            xon_xoff = get_xon_xoff_string(xon_xoff_enum)
        end if
    end subroutine get_config_xon_xoff


    subroutine set_config_xon_xoff(this, xon_xoff, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        character(len=*), intent(in)           :: xon_xoff
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: xon_xoff_enum
        integer(c_int)                         :: err_flag
        logical                                :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_xon_xoff_enum(xon_xoff, xon_xoff_enum, enum_ok)
        if (enum_ok) then
            call spu_set_config_xon_xoff(this%spu_config_ptr, xon_xoff_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_config_xon_xoff


    subroutine set_config_flowcontrol(this, flowcontrol, ok)
        implicit none
        class(serialport_config_t), intent(in) :: this
        character(len=*), intent(in)           :: flowcontrol
        logical, optional, intent(out)         :: ok
        integer(c_int)                         :: flowcontrol_enum
        integer(c_int)                         :: err_flag
        logical                                :: enum_ok

        if (present(ok)) ok = .false.
        if (.not. this%ok_flag) return

        call get_flowcontrol_enum(flowcontrol, flowcontrol_enum, enum_ok)
        if (enum_ok) then
            call spu_set_config_flowcontrol(this%spu_config_ptr, flowcontrol_enum, err_flag)
            if (err_flag == SPU_OK) then
                if (present(ok)) ok = .true.
            end if
        end if
    end subroutine set_config_flowcontrol


    subroutine print_config_verbose(this)
        implicit none
        ! Argument
        class(serialport_config_t), intent(in) :: this

        ! Local variables
        integer                       :: baudrate
        logical                       :: baudrate_ok

        integer                       :: bytesize 
        logical                       :: bytesize_ok

        integer                       :: stopbits
        logical                       :: stopbits_ok

        character(len=:), allocatable :: parity_mode
        logical                       :: parity_mode_ok

        character(len=:), allocatable :: rts
        logical                       :: rts_ok

        character(len=:), allocatable :: cts
        logical                       :: cts_ok

        character(len=:), allocatable :: dtr 
        logical                       :: dtr_ok

        character(len=:), allocatable :: dsr 
        logical                       :: dsr_ok 

        character(len=:), allocatable :: xon_xoff 
        logical                       :: xon_xoff_ok

        print '(1X,A,T15,L)',  'ok:', this%ok()       

        call this%get_baudrate(baudrate, baudrate_ok)
        if (baudrate_ok) then
            print '(1X,A,T15,I0)', 'baudrate:', baudrate
        else
            print '(1X,A,T15,I0,T25,A,L)', 'baudrate:', baudrate, 'ok = ', baudrate_ok
        end if

        call this%get_bytesize(bytesize, bytesize_ok)
        if (bytesize_ok) then
            print '(1X,A,T15,I0)', 'bytesize:', bytesize
        else
            print '(1X,A,T15,I0,T25,A,L)', 'bytesize:', bytesize, 'ok = ', bytesize_ok
        end if

        call this%get_stopbits(stopbits,stopbits_ok)
        if (stopbits_ok) then
            print '(1X,A,T15,I0)', 'stopbits:', stopbits
        else
            print '(1X,A,T15,I0,T25,A,L)', 'stopbits:', stopbits, 'ok = ', stopbits_ok
        end if

        call this%get_parity(parity_mode, parity_mode_ok)
        if (parity_mode_ok) then
            print '(1X,A,T15,A)', 'parity:', parity_mode 
        else
            print '(1X,A,T15,A,T25,A,L)', 'parity:', parity_mode, 'ok = ', parity_mode_ok
        end if

        call this%get_rts(rts, rts_ok)
        if (rts_ok) then
            print '(1X,A,T15,A)', 'rts:', rts 
        else
            print '(1X,A,T15,A,T25,A,L)', 'rts:', rts, 'ok = ', rts_ok
        end if

        call this%get_cts(cts, cts_ok)
        if (cts_ok) then
            print '(1X,A,T15,A)', 'cts:', cts 
        else
            print '(1X,A,T15,A,T25,A,L)', 'cts:', cts, 'ok = ', cts_ok
        end if

        call this%get_dtr(dtr, dtr_ok)
        if (dtr_ok) then
            print '(1X,A,T15,A)', 'dtr:', dtr 
        else
            print '(1X,A,T15,A,T25,A,L)', 'dtr:', dtr, 'ok = ', dtr_ok
        end if

        call this%get_dsr(dsr, dsr_ok)
        if (dsr_ok) then
            print '(1X,A,T15,A)', 'dsr:', dsr 
        else
            print '(1X,A,T15,A,T25,A,L)', 'dsr:', dsr, 'ok = ', dsr_ok
        end if

        call this%get_xon_xoff(xon_xoff, xon_xoff_ok)
        if (xon_xoff_ok) then
            print '(1X,A,T15,A)', 'xon_xoff:', xon_xoff 
        else
            print '(1X,A,T15,A,T25,A,L)', 'xon_xoff:', xon_xoff, 'ok = ', xon_xoff_ok
        end if
    end subroutine print_config_verbose


    subroutine print_config_concise(this)
        implicit none
        ! Argument
        class(serialport_config_t), intent(in) :: this
        ! Local variables
        integer                       :: baudrate
        integer                       :: bytesize 
        integer                       :: stopbits
        character(len=:), allocatable :: parity_mode
        character(len=:), allocatable :: rts
        character(len=:), allocatable :: cts
        character(len=:), allocatable :: dtr 
        character(len=:), allocatable :: dsr 
        character(len=:), allocatable :: xon_xoff 

        call this%get_baudrate(baudrate)
        call this%get_bytesize(bytesize)
        call this%get_stopbits(stopbits)
        call this%get_parity(parity_mode)
        call this%get_rts(rts)
        call this%get_cts(cts)
        call this%get_dtr(dtr)
        call this%get_dsr(dsr)
        call this%get_xon_xoff(xon_xoff)

        write (stdout,'(1X)', advance='no') 
        write (stdout,'(A,I0,A)', advance='no') 'Bd: ', baudrate, ', ' 
        write (stdout,'(A,I0,A)', advance='no') 'data: ', bytesize, ', ' 
        write (stdout,'(A,I0,A)', advance='no') 'stop: ', stopbits, ', ' 
        write (stdout,'(A,A,A)',  advance='no') 'parity: ', parity_mode, ', ' 
        write (stdout,'(A,A,A)',  advance='no') 'rts: ', rts, ', ' 
        write (stdout,'(A,A,A)',  advance='no') 'cts: ', cts, ', ' 
        write (stdout,'(A,A,A)',  advance='no') 'dtr: ', dtr, ', ' 
        write (stdout,'(A,A,A)',  advance='no') 'dsr: ', dsr, ', ' 
        write (stdout,'(A,A)',    advance='no') 'xon_xoff: ', xon_xoff 
        write (stdout,'(A)')  '' 

    end subroutine print_config_concise

    ! Destructor
    ! -------------------------------------------------------------------------
    subroutine del_config(this)
        implicit none
        type(serialport_config_t), intent(inout) :: this
        integer(c_int)                           :: err_flag
        if (c_associated(this%spu_config_ptr)) then 
            call spu_free_config(this%spu_config_ptr, err_flag)
        end if
    end subroutine del_config

end module serialport_config
