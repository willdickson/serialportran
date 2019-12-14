module serialport_list

    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : c_ptr
    use serialport_types, only            : SPU_OK 
    use serialport_types, only            : spu_port_info_t
    use serialport_utils, only            : spu_get_num_ports
    use serialport_utils, only            : spu_get_port_info
    use serialport_utils, only            : spu_get_port_by_number
    use serialport_info,  only            : serialport_info_t

    implicit none
    private

    type, public :: serialport_list_t
        type(serialport_info_t), allocatable, dimension(:) :: info
    contains
        procedure :: update        => update_serialport_list
        procedure :: length        => length_serialport_list
        procedure :: get_info      => get_serialport_info
        procedure :: print_verbose => print_info_verbose
        procedure :: print_concise => print_info_concise
    end type serialport_list_t

contains

    subroutine update_serialport_list(this)
        implicit none
        class(serialport_list_t), intent(inout) :: this
        integer(c_int)                          :: num_ports
        integer(c_int)                          :: err_flag
        integer(c_int)                          :: i
        type(c_ptr)                             :: spu_port_ptr
        type(spu_port_info_t)                   :: spu_info

        if (allocated(this%info)) deallocate(this%info)
        
        call spu_get_num_ports(num_ports, err_flag)  
        if (err_flag /= SPU_OK) return

        allocate(this%info(num_ports))

        do i=1,num_ports
            this%info(i) = serialport_info_t()
            call spu_get_port_by_number(i-1, spu_port_ptr, err_flag)
            if (err_flag /=SPU_OK) cycle 

            call spu_get_port_info(spu_port_ptr, spu_info, err_flag)
            if (err_flag /= SPU_OK) cycle

            this%info(i) = serialport_info_t(spu_info)

        end do
    end subroutine update_serialport_list

    function length_serialport_list(this) result(length)
        implicit none
        class(serialport_list_t), intent(in)    :: this
        integer                                 :: length
        if (allocated(this%info)) then
            length = size(this%info)
        else
            length =  0
        end if
    end function length_serialport_list

    function get_serialport_info(this,port_num) result(info)
        implicit none
        class(serialport_list_t), intent(in)    :: this
        integer, intent(in)                     :: port_num
        type(serialport_info_t)                 :: info
        info = serialport_info_t()
        if (this%length() > 0) then
            if ((port_num > 0) .and. (port_num <= this%length())) then
                info = this%info(port_num)
            end if
        end if
    end function get_serialport_info

    subroutine print_info_verbose(this)
        implicit none
        class(serialport_list_t), intent(in)   :: this
        type(serialport_info_t)                :: info
        integer                                :: i
        do i=1,this%length()
            print '(/)'
            print '(A,I4)', 'port: ', i
            print '(A)', repeat('-',50)
            info = this%get_info(i)
            call info%print_verbose()
        end do
        print '(/)'
    end subroutine print_info_verbose

    subroutine print_info_concise(this)
        implicit none
        class(serialport_list_t), intent(in)   :: this
        type(serialport_info_t)                :: info
        integer                                :: i
        print '(/)'
        print '(A,T5,A,T20,A,T30,A,T40,A)', '#', 'port', 'vid', 'pid ', 'serial'
        print '(A)', repeat('-',60)
        do i=1,this%length()
            info = this%get_info(i)
            call info%print_concise(i)
        end do
        print '(/)'
    end subroutine print_info_concise

end module serialport_list
