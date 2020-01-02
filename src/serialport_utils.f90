module serialport_utils

    use, intrinsic :: iso_c_binding, only : c_ptr
    use, intrinsic :: iso_c_binding, only : c_int
    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_size_t
    use, intrinsic :: iso_c_binding, only : c_double
    use serialport_types 
    use serialport_kinds

    implicit none
    private

    public spu_open_port
    public spu_close_port
    public spu_set_baudrate
    public spu_set_bits
    public spu_set_parity
    public spu_set_stopbits
    public spu_set_rts
    public spu_set_cts
    public spu_set_dtr
    public spu_set_dsr
    public spu_set_xon_xoff
    public spu_set_flowcontrol
    public spu_new_config
    public spu_free_config
    public spu_get_config
    public spu_set_config
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
    public spu_get_config_cts
    public spu_set_config_cts
    public spu_get_config_dtr
    public spu_set_config_dtr
    public spu_get_config_dsr
    public spu_set_config_dsr
    public spu_get_config_xon_xoff
    public spu_set_config_xon_xoff
    public spu_set_config_flowcontrol
    public spu_get_num_ports 
    public spu_get_port_name 
    public spu_get_port_desc
    public spu_get_port_by_name
    public spu_get_port_by_number
    public spu_get_port_info
    public spu_free_port
    public spu_blocking_read
    public spu_blocking_read_next
    public spu_nonblocking_read
    public spu_blocking_write
    public spu_nonblocking_write
    public spu_input_waiting
    public spu_output_waiting
    public spu_usleep
    public spu_msleep
    public spu_get_time_ms

    public c_char_vector_to_string
    public c_char_string_to_vector
    public print_spu_port_info
    public get_transport_string
    public get_mode_enum
    public get_parity_string
    public get_parity_enum
    public get_rts_string
    public get_rts_enum
    public get_cts_string
    public get_cts_enum
    public get_dtr_string
    public get_dtr_enum
    public get_dsr_string
    public get_dsr_enum
    public get_xon_xoff_string
    public get_xon_xoff_enum
    public get_flowcontrol_string
    public get_flowcontrol_enum
    public get_time_ms

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

        !void spu_set_baudrate(struct sp_port *port, int baudrate, int *err_flag)
        subroutine spu_set_baudrate(port, baudrate, err_flag) &
            bind(c, name="spu_set_baudrate")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: baudrate 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_baudrate


        !void spu_set_bits(struct sp_port *port, int bits, int *err_flag)
        subroutine spu_set_bits(port, bits, err_flag) &
            bind(c, name="spu_set_bits")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: bits 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_bits


        !void spu_set_parity(struct sp_port *port, enum sp_parity parity, int *err_flag)
        subroutine spu_set_parity(port, parity_mode, err_flag) &
            bind(c, name="spu_set_parity")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: parity_mode 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_parity


        !void spu_set_stopbits(struct sp_port *port, int stopbits, int *err_flag)
        subroutine spu_set_stopbits(port, stopbits, err_flag) &
            bind(c, name="spu_set_stopbits")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: stopbits 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_stopbits


        !void spu_set_rts(struct sp_port *port, enum sp_rts rts, int *err_flag)
        subroutine spu_set_rts(port, rts, err_flag) &
            bind(c, name="spu_set_rts")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: rts 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_rts


        !void spu_set_cts(struct sp_port *port, enum sp_cts cts, int *err_flag)
        subroutine spu_set_cts(port, cts, err_flag) &
            bind(c, name="spu_set_cts")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: cts 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_cts


        !void spu_set_dtr(struct sp_port *port, enum sp_dtr dtr, int *err_flag)
        subroutine spu_set_dtr(port, dtr, err_flag) &
            bind(c, name="spu_set_dtr")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: dtr 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_dtr


        !void spu_set_dsr(struct sp_port *port, enum sp_dsr dsr, int *err_flag)
        subroutine spu_set_dsr(port, dsr, err_flag) &
            bind(c, name="spu_set_dsr")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: dsr 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_dsr


        !void spu_set_xon_xoff(struct sp_port *port, enum sp_xonxoff xon_xoff, int *err_flag)
        subroutine spu_set_xon_xoff(port, xon_xoff, err_flag) &
            bind(c, name="spu_set_xon_xoff")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: xon_xoff 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_xon_xoff


        !void spu_set_flowcontrol(struct sp_port *port, enum sp_flowcontrol flowcontrol, int *err_flag)
        subroutine spu_set_flowcontrol(port, flowcontrol, err_flag) &
            bind(c, name="spu_set_flowcontrol")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            integer(c_int), intent(in), value :: flowcontrol 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_flowcontrol


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


        !void spu_set_config(struct sp_port *port, const struct sp_port_config *config, int *err_flag)
        subroutine spu_set_config(port, config, err_flag) &
            bind(c, name="spu_set_config")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: port
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config


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
        

        !void spu_get_config_cts(const struct sp_port_config *config, enum sp_cts *cts, int *err_flag)
        subroutine spu_get_config_cts(config, cts, err_flag) &
            bind(c, name="spu_get_config_cts")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: cts
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_cts


        !void spu_set_config_cts(struct sp_port_config *config, enum sp_cts cts, int *err_flag)
        subroutine spu_set_config_cts(config, cts, err_flag) &
            bind(c, name="spu_set_config_cts")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: cts
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_cts


        !void spu_get_config_dtr(const struct sp_port_config *config, enum sp_dtr *dtr, int *err_flag)
        subroutine spu_get_config_dtr(config, dtr, err_flag) &
            bind(c, name="spu_get_config_dtr")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: dtr 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_dtr


        !void spu_set_config_dtr(struct sp_port_config *config, enum sp_dtr dtr, int *err_flag)
        subroutine spu_set_config_dtr(config, dtr, err_flag) &
            bind(c, name="spu_set_config_dtr")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: dtr
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_dtr


        !void spu_get_config_dsr(const struct sp_port_config *config, enum sp_dsr *dsr, int *err_flag)
        subroutine spu_get_config_dsr(config, dsr, err_flag) &
            bind(c, name="spu_get_config_dsr")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: dsr 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_dsr


        !void spu_set_config_dsr(struct sp_port_config *config, enum sp_dsr dsr, int *err_flag)
        subroutine spu_set_config_dsr(config, dsr, err_flag) &
            bind(c, name="spu_set_config_dsr")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: dsr
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_dsr


        !void spu_get_config_xon_xoff(const struct sp_port_config *config, enum sp_xon_xoff *xon_xoff, int *err_flag)
        subroutine spu_get_config_xon_xoff(config, xon_xoff, err_flag) &
            bind(c, name="spu_get_config_xon_xoff")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(out)       :: xon_xoff 
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_get_config_xon_xoff


        !void spu_set_config_xon_xoff(struct sp_port_config *config, enum sp_xon_xoff xon_xoff, int *err_flag)
        subroutine spu_set_config_xon_xoff(config, xon_xoff, err_flag) &
            bind(c, name="spu_set_config_xon_xoff")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: xon_xoff
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_xon_xoff


        !void spu_set_config_flowcontrol(struct sp_port_config *config, enum sp_flowcontrol flowcontrol, int *err_flag);
        subroutine spu_set_config_flowcontrol(config, flowcontrol, err_flag) &
            bind(c, name="spu_set_config_flowcontrol")
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value    :: config
            integer(c_int), intent(in), value :: flowcontrol
            integer(c_int), intent(out)       :: err_flag
        end subroutine spu_set_config_flowcontrol


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


        !void spu_blocking_read(struct sp_port *port, void *buf, int *count, int timeout_ms, int *err_flag)
        subroutine spu_blocking_read(port, buf, num_bytes, timeout_ms, err_flag) & 
            bind(c, name="spu_blocking_read")
            import c_ptr
            import c_int
            import c_char 
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value      :: port
            character(kind=c_char), intent(in)  :: buf(*)
            integer(c_size_t), intent(inout)    :: num_bytes
            integer(c_int), intent(in), value   :: timeout_ms
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_blocking_read


        !void spu_blocking_read_next(struct sp_port *port, char buf[], size_t *count, int timeout_ms, int *err_flag)
        subroutine spu_blocking_read_next(port, buf, num_bytes, timeout_ms, err_flag) &
            bind(c, name="spu_blocking_read_next")
            import c_ptr
            import c_int
            import c_char 
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value      :: port
            character(kind=c_char), intent(in)  :: buf(*)
            integer(c_size_t), intent(inout)    :: num_bytes
            integer(c_int), intent(in), value   :: timeout_ms
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_blocking_read_next


        !void spu_nonblocking_read(struct sp_port *port, char buf[],  size_t *count, int *err_flag)
        subroutine spu_nonblocking_read(port, buf, num_bytes, err_flag) &
            bind(c, name="spu_nonblocking_read")
            import c_ptr
            import c_int
            import c_char
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value      :: port
            character(kind=c_char), intent(in)  :: buf(*)
            integer(c_size_t), intent(inout)    :: num_bytes
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_nonblocking_read


        !void spu_blocking_write(struct sp_port *port, const char buf[], int *count, int timeout_ms, int *err_flag)
        subroutine spu_blocking_write(port, buf, num_bytes, timeout_ms, err_flag) &
            bind(c, name="spu_blocking_write")
            import c_ptr
            import c_int
            import c_char 
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value      :: port
            character(kind=c_char), intent(in)  :: buf(*)
            integer(c_size_t), intent(inout)    :: num_bytes
            integer(c_int), intent(in), value   :: timeout_ms
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_blocking_write


        !void spu_nonblocking_write(struct sp_port *port, const char buf[], size_t *count, int *err_flag)
        subroutine spu_nonblocking_write(port, buf, num_bytes, err_flag) &
            bind(c, name="spu_nonblocking_write")
            import c_ptr
            import c_int
            import c_char 
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value      :: port
            character(kind=c_char), intent(in)  :: buf(*)
            integer(c_size_t), intent(inout)    :: num_bytes
            integer(c_int), intent(out)         :: err_flag
        end subroutine spu_nonblocking_write


        !void spu_input_waiting(struct sp_port *port, int *count, int *err_flag)
        subroutine spu_input_waiting(port, num_bytes, err_flag) &
            bind(c, name="spu_input_waiting")
            import c_ptr
            import c_int
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value :: port
            integer(c_size_t), intent(out) :: num_bytes
            integer(c_int), intent(out)    :: err_flag
        end subroutine spu_input_waiting


        !void spu_output_waiting(struct sp_port *port, int *count, int *err_flag)
        subroutine spu_output_waiting(port, num_bytes, err_flag) &
            bind(c, name="spu_output_waiting")
            import c_ptr
            import c_int
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value :: port
            integer(c_size_t), intent(out) :: num_bytes
            integer(c_int), intent(out)    :: err_flag
        end subroutine spu_output_waiting


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


        !void spu_get_time_ms(double *t)
        subroutine spu_get_time_ms(t) & 
            bind(c, name="spu_get_time_ms")
            import c_double
            implicit none
            real(c_double), intent(out) :: t
        end subroutine spu_get_time_ms


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
        character(len=*), intent(in)    :: mode_string
        integer(c_int), intent(out)     :: mode_enum
        logical, optional, intent(out)  :: ok

        if (present(ok)) ok = .true. 
        select case (trim(mode_string)) 
            case ('r')
                mode_enum = SP_MODE_READ
            case ('w') 
                mode_enum = SP_MODE_WRITE
            case ('rw', 'wr') 
                mode_enum = SP_MODE_READ_WRITE 
            case default 
                mode_enum = 0
                if (present(ok)) ok = .false.
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
        logical, optional, intent(out) :: ok
        if (present(ok)) ok = .true.
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
                if (present(ok)) ok = .false.
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
        character(len=*), intent(in)   :: rts_string
        integer(c_int), intent(out)    :: rts_enum
        logical, optional, intent(out) :: ok
        if (present(ok)) ok = .true.
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
                if (present(ok)) ok = .false. 
                rts_enum = SP_RTS_INVALID
        end select
    end subroutine get_rts_enum


    function get_cts_string(cts_enum) result(cts_string)
        implicit none
        integer(c_int), intent(in)     :: cts_enum
        character(len=:), allocatable  :: cts_string
        select case (cts_enum)
            case (SP_CTS_INVALID)
                cts_string = 'invalid'
            case (SP_CTS_IGNORE)
                cts_string = 'ignore'
            case (SP_CTS_FLOW_CONTROL)
                cts_string = 'flow_control'
            case default
                cts_string = 'unknown'
        end select
    end function get_cts_string


    subroutine get_cts_enum(cts_string, cts_enum, ok)
        implicit none
        character(len=*), intent(in)   :: cts_string
        integer(c_int), intent(out)    :: cts_enum
        logical, optional, intent(out) :: ok 
        if (present(ok)) ok = .true.
        select case (trim(cts_string))
            case ('invalid')
                cts_enum = SP_CTS_INVALID
            case ('ignore')
                cts_enum = SP_CTS_IGNORE
            case ('flow_control')
                cts_enum = SP_CTS_FLOW_CONTROL
            case default
                if (present(ok)) ok = .false. 
                cts_enum = SP_CTS_INVALID
        end select
    end subroutine get_cts_enum


    function get_dtr_string(dtr_enum) result(dtr_string)
        implicit none
        integer(c_int), intent(in)     :: dtr_enum
        character(len=:), allocatable  :: dtr_string
        select case (dtr_enum)
            case (SP_DTR_INVALID)
                dtr_string = 'invalid'
            case (SP_DTR_OFF)
                dtr_string = 'off'
            case (SP_DTR_ON)
                dtr_string = 'on'
            case (SP_DTR_FLOW_CONTROL)
                dtr_string = 'flow_control'
            case default
                dtr_string = 'unknown'
        end select
    end function get_dtr_string


    subroutine get_dtr_enum(dtr_string, dtr_enum, ok)
        implicit none
        character(len=*), intent(in)   :: dtr_string
        integer(c_int), intent(out)    :: dtr_enum
        logical, optional, intent(out) :: ok 
        if (present(ok)) ok = .true.
        select case (trim(dtr_string))
            case ('invalid')
                dtr_enum = SP_DTR_INVALID
            case ('off')
                dtr_enum = SP_DTR_OFF
            case ('on')
                dtr_enum = SP_DTR_ON
            case ('flow_control')
                dtr_enum = SP_DTR_FLOW_CONTROL
            case default
                if (present(ok)) ok = .false. 
                dtr_enum = SP_DTR_INVALID
        end select
    end subroutine get_dtr_enum


    function get_dsr_string(dsr_enum) result(dsr_string)
        implicit none
        integer(c_int), intent(in)     :: dsr_enum
        character(len=:), allocatable  :: dsr_string
        select case (dsr_enum)
            case (SP_DSR_INVALID)
                dsr_string = 'invalid'
            case (SP_DSR_IGNORE)
                dsr_string = 'ignore'
            case (SP_DSR_FLOW_CONTROL)
                dsr_string = 'flow_control'
            case default
                dsr_string = 'unknown'
        end select
    end function get_dsr_string


    subroutine get_dsr_enum(dsr_string, dsr_enum, ok)
        implicit none
        character(len=*), intent(in)   :: dsr_string
        integer(c_int), intent(out)    :: dsr_enum
        logical, optional, intent(out) :: ok 
        if (present(ok)) ok = .true.
        select case (trim(dsr_string))
            case ('invalid')
                dsr_enum = SP_DSR_INVALID
            case ('ignore')
                dsr_enum = SP_DSR_IGNORE
            case ('flow_control')
                dsr_enum = SP_DSR_FLOW_CONTROL
            case default
                if (present(ok)) ok = .false. 
                dsr_enum = SP_DSR_INVALID
        end select
    end subroutine get_dsr_enum


    function get_xon_xoff_string(xon_xoff_enum) result(xon_xoff_string)
        implicit none
        integer(c_int), intent(in)     :: xon_xoff_enum
        character(len=:), allocatable  :: xon_xoff_string
        select case (xon_xoff_enum)
            case (SP_XONXOFF_INVALID)
                xon_xoff_string = 'invalid'
            case (SP_XONXOFF_DISABLED)
                xon_xoff_string = 'disabled'
            case (SP_XONXOFF_IN)
                xon_xoff_string = 'in'
            case (SP_XONXOFF_OUT)
                xon_xoff_string = 'out'
            case (SP_XONXOFF_INOUT)
                xon_xoff_string = 'inout'
            case default
                xon_xoff_string = 'unknown'
        end select
    end function get_xon_xoff_string


    subroutine get_xon_xoff_enum(xon_xoff_string, xon_xoff_enum, ok)
        implicit none
        character(len=*), intent(in)   :: xon_xoff_string
        integer(c_int), intent(out)    :: xon_xoff_enum
        logical, optional, intent(out) :: ok 
        if (present(ok)) ok = .true.
        select case (trim(xon_xoff_string))
            case ('invalid')
                xon_xoff_enum = SP_XONXOFF_INVALID
            case ('disabled')
                xon_xoff_enum = SP_XONXOFF_DISABLED
            case ('in')
                xon_xoff_enum = SP_XONXOFF_IN
            case ('out')
                xon_xoff_enum = SP_XONXOFF_OUT
            case ('inout')
                xon_xoff_enum = SP_XONXOFF_INOUT
            case default
                if (present(ok)) ok = .false. 
                xon_xoff_enum = SP_XONXOFF_INVALID
        end select
    end subroutine get_xon_xoff_enum


    function get_flowcontrol_string(flowcontrol_enum) result(flowcontrol_string)
        implicit none
        integer(c_int), intent(in)     :: flowcontrol_enum
        character(len=:), allocatable  :: flowcontrol_string
        select case (flowcontrol_enum)
            case (SP_FLOWCONTROL_NONE)
                flowcontrol_string = 'none'
            case (SP_FLOWCONTROL_XONXOFF)
                flowcontrol_string = 'xon_xoff'
            case (SP_FLOWCONTROL_RTSCTS)
                flowcontrol_string = 'rts_cts'
            case (SP_FLOWCONTROL_DTRDSR)
                flowcontrol_string = 'dtr_dsr'
            case default
                flowcontrol_string = 'unknown'
        end select
    end function get_flowcontrol_string


    subroutine get_flowcontrol_enum(flowcontrol_string, flowcontrol_enum, ok)
        implicit none
        character(len=*), intent(in)   :: flowcontrol_string
        integer(c_int), intent(out)    :: flowcontrol_enum
        logical, optional, intent(out) :: ok 
        if (present(ok)) ok = .true.
        select case (trim(flowcontrol_string))
            case ('none')
                flowcontrol_enum = SP_FLOWCONTROL_NONE
            case ('xon_xoff')
                flowcontrol_enum = SP_FLOWCONTROL_XONXOFF
            case ('rts_cts')
                flowcontrol_enum = SP_FLOWCONTROL_RTSCTS
            case ('dtr_dsr')
                flowcontrol_enum = SP_FLOWCONTROL_DTRDSR
            case default
                if (present(ok)) ok = .false. 
                flowcontrol_enum = SP_FLOWCONTROL_NONE
        end select
    end subroutine get_flowcontrol_enum


    function get_time_ms() result(t)
        implicit none
        ! Result
        real(dp) :: t
        ! Local variables
        real(c_double) :: t_tmp
        call spu_get_time_ms(t_tmp)
        t = real(t_tmp, kind(t))
    end function get_time_ms


end module serialport_utils
