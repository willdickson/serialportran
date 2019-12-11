module serialport_info

    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : c_ptr
    use  serialport_types, only           : SPU_OK
    use  serialport_types, only           : buf_len
    use  serialport_types, only           : spu_port_info_t
    use  serialport_utils, only           : get_transport_string
    use  serialport_utils, only           : c_char_vector_to_string
    use  serialport_utils, only           : spu_get_port_info

    implicit none
    private

    type, public                       :: serialport_info_t
        character(len=:), allocatable  :: port_name
        character(len=:), allocatable  :: description
        character(len=:), allocatable  :: usb_manufacturer
        character(len=:), allocatable  :: usb_product
        character(len=:), allocatable  :: usb_serial
        character(len=:), allocatable  :: bluetooth_address
        integer                        :: usb_bus
        integer                        :: usb_address
        integer                        :: usb_vendor_id
        integer                        :: usb_product_id
        integer                        :: transport
    contains
        procedure :: print_verbose => print_info_verbose
        procedure :: print_concise => print_info_concise
    end type serialport_info_t

    interface serialport_info_t
        procedure :: new_serialport_info_empty
        procedure :: new_serialport_info_from_spu
        procedure :: new_serialport_info_from_ptr
    end interface serialport_info_t

contains

    function new_serialport_info_empty() result(info)
        implicit none
        type(serialport_info_t) :: info
        info%port_name = ''
        info%description = ''
        info%usb_manufacturer = ''
        info%usb_product = ''
        info%usb_serial = ''
        info%bluetooth_address = ''
        info%usb_bus = 0 
        info%usb_address = 0 
        info%usb_vendor_id = 0 
        info%usb_product_id =  0 
        info%transport = 0 
    end function new_serialport_info_empty

    function new_serialport_info_from_spu(spu_info) result(info)
        implicit none
        type(spu_port_info_t), intent(in)   :: spu_info
        type(serialport_info_t)             :: info
        character(len=buf_len, kind=c_char) :: tmp  

        call c_char_vector_to_string(spu_info%port_name, tmp)
        info%port_name = trim(tmp)

        call c_char_vector_to_string(spu_info%description, tmp)
        info%description = trim(tmp)

        call c_char_vector_to_string(spu_info%usb_manufacturer, tmp)
        info%usb_manufacturer = trim(tmp)

        call c_char_vector_to_string(spu_info%usb_product, tmp)
        info%usb_product = trim(tmp)

        call c_char_vector_to_string(spu_info%usb_serial, tmp)
        info%usb_serial = trim(tmp)

        call c_char_vector_to_string(spu_info%bluetooth_address, tmp)
        info%bluetooth_address = tmp

        info%usb_bus         = spu_info%usb_bus
        info%usb_address     = spu_info%usb_address
        info%usb_vendor_id   = spu_info%usb_vendor_id
        info%usb_product_id  = spu_info%usb_product_id
        info%transport       = spu_info%transport


    end function new_serialport_info_from_spu

    function new_serialport_info_from_ptr(spu_port_ptr,ok) result(info)
        implicit none
        type(c_ptr), intent(in), value  :: spu_port_ptr
        logical, optional, intent(out)  :: ok
        type(serialport_info_t)         :: info
        type(spu_port_info_t)           :: spu_info 
        integer(c_int)                  :: err_flag

        info = serialport_info_t()
        call spu_get_port_info(spu_port_ptr, spu_info, err_flag)
        if (err_flag /= SPU_OK) then
            if (present(ok)) ok = .false.
        else
            info = serialport_info_t(spu_info)
            if (present(ok)) ok = .true.
        end if
    end function  new_serialport_info_from_ptr

    subroutine print_info_verbose(this)
        implicit none
        class(serialport_info_t), intent(in) :: this
        character(len=:), allocatable        :: transport_string

        allocate(character(0)::transport_string)
        transport_string = get_transport_string(this%transport)
        print '(1X,A,A)',       'port_name:           ', this%port_name
        print '(1X,A,A)',       'description:         ', this%description
        print '(1X,A,A)',       'usb manufacturer:    ', this%usb_manufacturer
        print '(1X,A,A)',       'usb product:         ', this%usb_product
        print '(1X,A,A)',       'usb serial:          ', this%usb_serial
        print '(1X,A,A)',       'bluetooth_address:   ', this%bluetooth_address
        print '(1x,A,I0)',      'usb_bus:             ', this%usb_bus
        print '(1x,A,I0)',      'usb_address:         ', this%usb_address
        print '(1x,A,A,Z0.4)',  'usb_vendor_id:       ', '0x', this%usb_vendor_id
        print '(1x,A,A,Z0.4)',  'usb_product_id:      ', '0x', this%usb_product_id
        print '(1x,A,A)',       'transport:           ', transport_string
    end subroutine print_info_verbose

    subroutine print_info_concise(this,ind)
        implicit none
        class(serialport_info_t), intent(in) :: this
        integer, optional, intent(in)        :: ind
        if (present(ind)) then 
            print '(I0,A,A,A,Z0.4,A,Z0.4,A,A)', ind, '   ', this%port_name, '   0x', & 
                this%usb_vendor_id, '   0x', this%usb_product_id, '   ',  this%usb_serial
        else
            print '(A,A,Z0.4,A,Z0.4,A,A)', this%port_name, '   0x', this%usb_vendor_id, & 
                '   0x', this%usb_product_id, '   ',  this%usb_serial
        end if
    end subroutine print_info_concise

end module serialport_info
