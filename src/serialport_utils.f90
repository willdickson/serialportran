module serialport_utils

    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : c_ptr
    use serialport_types, only            : buf_len 
    use serialport_types, only            : spu_port_info_t
    use serialport_types, only            : SPU_OK
    use serialport_types, only            : SPU_ERR
    use serialport_types, only            : SP_TRANSPORT_NATIVE
    use serialport_types, only            : SP_TRANSPORT_USB
    use serialport_types, only            : SP_TRANSPORT_BLUETOOTH

    implicit none
    private

    public spu_get_num_ports 
    public spu_get_port_name 
    public spu_get_port_desc
    public spu_get_port_by_name
    public spu_get_port_by_number
    public spu_get_port_info
    public spu_free_port
    public spu_usleep
    public spu_msleep
    public c_char_vector_to_string
    public c_char_string_to_vector
    public print_spu_port_info
    public get_transport_string

    interface

        !void spu_get_num_ports(int *num_ports, int *err_flag);
        subroutine spu_get_num_ports(num_ports, err_flag) & 
            bind(c,name="spu_get_num_ports") 
            import c_int
            implicit none
            integer(c_int), intent(out)  :: num_ports
            integer(c_int), intent(out)  :: err_flag
        end subroutine spu_get_num_ports

        !void spu_get_port_name(int port_num, char port_name[], int max_len, int *err_flag);
        subroutine spu_get_port_name(port_num, port_name, max_len, err_flag) & 
            bind(c,name="spu_get_port_name")
            import c_int
            import c_char
            import c_ptr
            implicit none
            integer(c_int), intent(in), value      :: port_num
            integer(c_int), intent(in), value      :: max_len
            character(kind=c_char), intent(inout)  :: port_name(*)
            integer(c_int), intent(out)            :: err_flag
        end subroutine spu_get_port_name

        !void spu_get_port_desc(int port_num, char port_desc[], int max_len, int *err_flag);
        subroutine spu_get_port_desc(port_num, port_desc, max_len, err_flag) & 
            bind(c,name="spu_get_port_desc")
            import c_int
            import c_char
            import c_ptr
            implicit none
            integer(c_int), intent(in), value      :: port_num
            integer(c_int), intent(in), value      :: max_len
            character(kind=c_char), intent(inout)  :: port_desc(*)
            integer(c_int), intent(out)            :: err_flag
        end subroutine spu_get_port_desc

        !void spu_get_port_by_name(char name[], struct sp_port **port, int *err_flag);
        subroutine spu_get_port_by_name(port_name, port, err_flag) & 
            bind(c,name="spu_get_port_by_name")
            import c_int
            import c_char
            import c_ptr
            implicit none
            character(kind=c_char), intent(in)  :: port_name(*)
            type(c_ptr), intent(out)            :: port
            integer(c_int), intent(out)         :: err_flag
        end subroutine

        !void spu_get_port_by_number(int number, struct sp_port **port, int *err_flag);
        subroutine spu_get_port_by_number(port_num, port, err_flag) &
            bind(c,name="spu_get_port_by_number")
            import c_int
            import c_ptr
            implicit none
            integer(c_int), intent(in), value   :: port_num
            type(c_ptr), intent(out)            :: port
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_get_port_by_number

        !void spu_get_port_info(struct sp_port *port, struct spu_port_info *info, int *err_flag);
        subroutine spu_get_port_info(port,info,err_flag) &
            bind(c,name="spu_get_port_info")
            import c_int
            import c_ptr
            import spu_port_info_t
            implicit none
            type(c_ptr), intent(in), value      :: port
            type(spu_port_info_t), intent(out)  :: info
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_get_port_info

        !void spu_free_port(struct sp_port *port);
        subroutine spu_free_port(port) &
            bind(c,name="spu_free_port")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: port
        end subroutine spu_free_port

        !void spu_usleep(int usecs);
        subroutine spu_usleep(usec) &
            bind(c,name="spu_usleep")
            import c_int
            implicit none
            integer(c_int), intent(in), value  :: usec
        end subroutine spu_usleep 

        !void spu_msleep(int msecs);
        subroutine spu_msleep(msec) &
            bind(c,name="spu_msleep")
            import c_int
            implicit none
            integer(c_int), intent(in), value  :: msec
        end subroutine spu_msleep 

    end interface

contains

    subroutine c_char_string_to_vector(string, vector)
        implicit none
        character(len=*, kind=c_char), intent(in)  :: string
        character(len=1, kind=c_char), intent(out) :: vector(len(string))
        integer                                    :: i
        vector = [('', i=1,size(vector))] 
        do i=1,len(string)
            vector(i) = string(i:i)
        end do 
    end subroutine  c_char_string_to_vector


    subroutine c_char_vector_to_string(vector, string)
        implicit none
        character(len=*, kind=c_char), intent(out) :: string
        character(len=1, kind=c_char), intent(in)  :: vector(len(string))
        integer                                    :: i
        string = ''
        do i=1,len(string)
            string(i:i) = vector(i)
        end do 
    end subroutine  c_char_vector_to_string

    subroutine print_spu_port_info(info)
        implicit none
        type(spu_port_info_t)                :: info
        character(len=buf_len, kind=c_char)  :: port_name
        character(len=buf_len, kind=c_char)  :: description 
        character(len=buf_len, kind=c_char)  :: usb_manufacturer 
        character(len=buf_len, kind=c_char)  :: usb_product
        character(len=buf_len, kind=c_char)  :: usb_serial
        character(len=buf_len, kind=c_char)  :: bluetooth_address 
        character(len=buf_len)               :: transport_string

        call c_char_vector_to_string(info%port_name, port_name)
        call c_char_vector_to_string(info%description, description)
        call c_char_vector_to_string(info%usb_manufacturer, usb_manufacturer)
        call c_char_vector_to_string(info%usb_product, usb_product)
        call c_char_vector_to_string(info%usb_serial, usb_serial)
        call c_char_vector_to_string(info%bluetooth_address, bluetooth_address)
        transport_string = get_transport_string(info%transport)

        print '(/)'
        print '(A)', 'port_info'
        print '(A)', repeat('-',60)
        print '(1X,A,A)',       'port_name:           ', trim(port_name)
        print '(1X,A,A)',       'description:         ', trim(description)
        print '(1X,A,A)',       'usb manufacturer:    ', trim(usb_manufacturer)
        print '(1X,A,A)',       'usb product:         ', trim(usb_product)
        print '(1X,A,A)',       'usb serial:          ', trim(usb_serial)
        print '(1X,A,A)',       'bluetooth_address:   ', trim(bluetooth_address)
        print '(1x,A,I0)',      'usb_bus:             ', info%usb_bus
        print '(1x,A,I0)',      'usb_address:         ', info%usb_address
        print '(1x,A,A,Z0.4)',  'usb_vendor_id:       ', '0x', info%usb_vendor_id
        print '(1x,A,A,Z0.4)',  'usb_product_id:      ', '0x', info%usb_product_id
        print '(1x,A,A)',       'transport:           ', trim(transport_string)
        print '(/)'
    end subroutine print_spu_port_info

    function get_transport_string(transport_int) result(transport_string)
        implicit none
        integer(c_int), intent(in)     :: transport_int
        character(len=:), allocatable  :: transport_string
        select case (transport_int)
            case (SP_TRANSPORT_NATIVE)
                transport_string = 'native'
            case (SP_TRANSPORT_USB)
                transport_string = 'usb'
            case (SP_TRANSPORT_BLUETOOTH)
                transport_string = 'bluetooth'
            case default
                transport_string = 'unknown'
        end select
    end function get_transport_string

end module serialport_utils
