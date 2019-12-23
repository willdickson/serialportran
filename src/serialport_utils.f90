module serialport_utils

    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : c_ptr
    use serialport_types 

    implicit none
    private

    public spu_open_port
    public spu_close_port
    public spu_new_config
    public spu_free_config
    public spu_get_config
    public spu_get_config_baudrate
    public spu_set_config_baudrate
    public spu_get_config_bits
    public spu_set_config_bits
    public spu_get_config_parity
    public spu_set_config_parity
    public spu_get_config_stopbits
    public spu_set_config_stopbits
    public spu_get_config_rts
    public spu_set_config_rts
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
    public get_mode_enum
    public get_parity_string
    public get_parity_enum
    public get_rts_string
    public get_rts_enum

    interface

        !void spu_open_port(struct sp_port *port, enum sp_mode mode_enum, int *err_flag)
        subroutine spu_open_port(port, mode_enum, err_flag) &
            bind(c, name="spu_open_port")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: mode_enum
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_open_port


        !void spu_close_port(struct sp_port *port, int *err_flag)
        subroutine spu_close_port(port, err_flag) &
            bind(c, name="spu_close_port")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_close_port


        !void spu_new_config(struct sp_port_config **config, int *err_flag)
        subroutine spu_new_config(config, err_flag) &
            bind(c, name="spu_new_config")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(out)          :: config 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_new_config


        !void spu_free_config(struct sp_port_config *config, int *err_flag)
        subroutine spu_free_config(config, err_flag) &
            bind(c, name="spu_new_config")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value :: config 
            integer(c_int), intent(out)    :: err_flag
        end subroutine spu_free_config


        !void spu_get_config(struct sp_port *port, struct sp_port_config *config, int *err_flag);
        subroutine spu_get_config(port, config, err_flag) &
            bind(c, name="spu_get_config")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config


        !void spu_get_config_baudrate(const struct sp_port_config *config, int *baudrate, int *err_flag)
        subroutine spu_get_config_baudrate(config, baudrate, err_flag) &
            bind(c, name="spu_get_config_baudrate")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: baudrate
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_baudrate


        !void spu_set_config_baudrate(struct sp_port_config *config, int baudrate, int *err_flag)
        subroutine spu_set_config_baudrate(config, baudrate, err_flag) &
            bind(c, name="spu_set_config_baudrate")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: baudrate
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_baudrate


        !void spu_get_config_bits(const struct sp_port_config *config, int *bits, int *err_flag)
        subroutine spu_get_config_bits(config, bits, err_flag) &
            bind(c, name="spu_get_config_bits")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: bits
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_bits


        !void spu_set_config_bits(struct sp_port_config *config, int bits, int *err_flag)
        subroutine spu_set_config_bits(config, bits, err_flag) &
            bind(c, name="spu_set_config_bits")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: bits
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_bits
        

        !void spu_get_config_parity(const struct sp_port_config *config, enum sp_parity *parity, int *err_flag)
        subroutine spu_get_config_parity(config, parity_enum, err_flag) &
            bind(c, name="spu_get_config_parity")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: parity_enum
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_parity


        !void spu_set_config_parity(struct sp_port_config *config, enum sp_parity  parity, int *err_flag)
        subroutine spu_set_config_parity(config, parity_enum, err_flag) &
            bind(c, name="spu_set_config_parity")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: parity_enum
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_parity

        
        !void spu_get_config_stopbits(const struct sp_port_config *config, int *stopbits, int *err_flag)
        subroutine spu_get_config_stopbits(config, stopbits, err_flag) &
            bind(c, name="spu_get_config_stopbits")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: stopbits
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_stopbits


        !void spu_set_config_stopbits(struct sp_port_config *config, int stopbits, int *err_flag)
        subroutine spu_set_config_stopbits(config, stopbits, err_flag) &
            bind(c, name="spu_set_config_stopbits")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: stopbits
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_stopbits


        !void spu_get_config_rts(const struct sp_port_config *config, enum sp_rts *rts, int *err_flag)
        subroutine spu_get_config_rts(config, rts, err_flag) &
            bind(c, name="spu_get_config_rts")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: rts
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_rts


        !void spu_set_config_rts(struct sp_port_config *config, enum sp_rts rts, int *err_flag)
        subroutine spu_set_config_rts(config, rts, err_flag) &
            bind(c, name="spu_set_config_rts")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: rts
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_rts
        

        !void spu_get_num_ports(int *num_ports, int *err_flag)
        subroutine spu_get_num_ports(num_ports, err_flag) & 
            bind(c, name="spu_get_num_ports") 
            import c_int
            implicit none
            integer(c_int), intent(out)  :: num_ports
            integer(c_int), intent(out)  :: err_flag
        end subroutine spu_get_num_ports


        !void spu_get_port_name(int port_num, char port_name[], int max_len, int *err_flag)
        subroutine spu_get_port_name(port_num, port_name, max_len, err_flag) & 
            bind(c, name="spu_get_port_name")
            import c_int
            import c_char
            import c_ptr
            implicit none
            integer(c_int), intent(in), value      :: port_num
            integer(c_int), intent(in), value      :: max_len
            character(kind=c_char), intent(inout)  :: port_name(*)
            integer(c_int), intent(out)            :: err_flag
        end subroutine spu_get_port_name


        !void spu_get_port_desc(int port_num, char port_desc[], int max_len, int *err_flag)
        subroutine spu_get_port_desc(port_num, port_desc, max_len, err_flag) & 
            bind(c, name="spu_get_port_desc")
            import c_int
            import c_char
            import c_ptr
            implicit none
            integer(c_int), intent(in), value      :: port_num
            integer(c_int), intent(in), value      :: max_len
            character(kind=c_char), intent(inout)  :: port_desc(*)
            integer(c_int), intent(out)            :: err_flag
        end subroutine spu_get_port_desc


        !void spu_get_port_by_name(char name[], struct sp_port **port, int *err_flag)
        subroutine spu_get_port_by_name(port_name, port, err_flag) & 
            bind(c, name="spu_get_port_by_name")
            import c_int
            import c_char
            import c_ptr
            implicit none
            character(kind=c_char), intent(in)  :: port_name(*)
            type(c_ptr), intent(out)            :: port
            integer(c_int), intent(out)         :: err_flag
        end subroutine


        !void spu_get_port_by_number(int number, struct sp_port **port, int *err_flag)
        subroutine spu_get_port_by_number(port_num, port, err_flag) &
            bind(c, name="spu_get_port_by_number")
            import c_int
            import c_ptr
            implicit none
            integer(c_int), intent(in), value   :: port_num
            type(c_ptr), intent(out)            :: port
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_get_port_by_number


        !void spu_get_port_info(struct sp_port *port, struct spu_port_info *info, int *err_flag)
        subroutine spu_get_port_info(port,info,err_flag) &
            bind(c, name="spu_get_port_info")
            import c_int
            import c_ptr
            import spu_port_info_t
            implicit none
            type(c_ptr), intent(in), value      :: port
            type(spu_port_info_t), intent(out)  :: info
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_get_port_info


        !void spu_free_port(struct sp_port *port)
        subroutine spu_free_port(port) &
            bind(c, name="spu_free_port")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: port
        end subroutine spu_free_port


        !void spu_usleep(int usecs);
        subroutine spu_usleep(usec) &
            bind(c, name="spu_usleep")
            import c_int
            implicit none
            integer(c_int), intent(in), value  :: usec
        end subroutine spu_usleep 

        
        !void spu_msleep(int msecs)
        subroutine spu_msleep(msec) &
            bind(c, name="spu_msleep")
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


    function get_transport_string(transport_enum) result(transport_string)
        implicit none
        integer(c_int), intent(in)     :: transport_enum
        character(len=:), allocatable  :: transport_string
        select case (transport_enum)
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


    subroutine get_mode_enum(mode_string, mode_enum, ok)
        implicit none
        character(len=*), intent(in) :: mode_string
        integer(c_int), intent(out)  :: mode_enum
        logical, intent(out)         :: ok

        ok = .true. 
        select case (trim(mode_string)) 
            case ('r')
                mode_enum = SP_MODE_READ
            case ('w') 
                mode_enum = SP_MODE_WRITE
            case ('rw', 'wr') 
                mode_enum = SP_MODE_READ_WRITE 
            case default 
                mode_enum = 0
                ok = .false.
        end select
    end subroutine get_mode_enum


    function get_parity_string(parity_enum) result(parity_string)
        implicit none
        integer(c_int), intent(in)    :: parity_enum
        character(len=:), allocatable :: parity_string
        select case (parity_enum)
            case (SP_PARITY_INVALID)
                parity_string = 'invalid'
            case (SP_PARITY_NONE)
                parity_string = 'none'
            case (SP_PARITY_ODD)     
                parity_string = 'odd'
            case (SP_PARITY_EVEN) 
                parity_string = 'even'
            case (SP_PARITY_MARK)
                parity_string = 'mark'
            case (SP_PARITY_SPACE)
                parity_string = 'space'
            case default
                parity_string = 'unknown'
        end select
    end function get_parity_string


    subroutine get_parity_enum(parity_string, parity_enum, ok)
        implicit none
        character(len=*), intent(in)   :: parity_string
        integer(c_int), intent(out)    :: parity_enum
        logical, intent(out)           :: ok
        ok = .true.
        select case (parity_string)
            case ('invalid')
                parity_enum = SP_PARITY_INVALID 
            case ('none') 
                parity_enum = SP_PARITY_NONE    
            case ('odd') 
                parity_enum = SP_PARITY_ODD     
            case ('even')
                parity_enum = SP_PARITY_EVEN    
            case ('mark')
                parity_enum = SP_PARITY_MARK    
            case ('space')
                parity_enum = SP_PARITY_SPACE   
            case default
                ok = .false.
                parity_enum = SP_PARITY_INVALID 
        end select
    end subroutine get_parity_enum


    function get_rts_string(rts_enum) result(rts_string)
        implicit none
        integer(c_int), intent(in)    :: rts_enum
        character(len=:), allocatable :: rts_string
        select case (rts_enum)
            case (SP_RTS_INVALID)
                rts_string = 'invalid'
            case (SP_RTS_OFF)
                rts_string = 'off'
            case (SP_RTS_ON) 
                rts_string = 'on'
            case (SP_RTS_FLOW_CONTROL)
                rts_string = 'flow_control'
            case default
                rts_string = 'unknown'
        end select
    end function get_rts_string


    subroutine get_rts_enum(rts_string, rts_enum, ok)
        implicit none
        character(len=*), intent(in)  :: rts_string
        integer(c_int), intent(out)   :: rts_enum
        logical, intent(out)          :: ok
        ok = .true.
        select case (trim(rts_string))
            case ('invalid')
                rts_enum = SP_RTS_INVALID
            case ('off')
                rts_enum = SP_RTS_OFF
            case ('on')
                rts_enum = SP_RTS_ON
            case ('flow_control')
                rts_enum = SP_RTS_FLOW_CONTROL
            case default
                ok = .false. 
                rts_enum = SP_RTS_INVALID
        end select
    end subroutine get_rts_enum


end module serialport_utils
