program serial_test

    use serialport_list,   only : serialport_list_t
    use serialport_info,   only : serialport_info_t
    use serialport_config, only : serialport_config_t
    use serialport_dev,    only : serialport_t

    implicit none

    type(serialport_list_t)       :: port_list
    type(serialport_t)            :: port
    type(serialport_info_t)       :: info
    type(serialport_config_t)     :: config
    logical                       :: ok

    call port_list%update()
    call port_list%print_concise()

    port = serialport_t('/dev/ttyACM0')
    print '(A,L)', 'ok = ', ok
    if ( .not. port%ok() )  then
        print '(A)', 'port creation failed'
        stop
    else 
        print '(A)', 'port creation successful'
    end if
    print *, ''

    info = port%get_info()
    print *, 'info.ok() = ', info%ok()
    print *, ''

    call info%print_verbose()
    print *, ''


    call port%open_conn('rw',ok)
    print *, 'open_conn ok = ', ok
    print *, 'port%is_open() = ', port%is_open()
    print *, ''

    call port%set_baudrate(9600,ok)
    print *, 'set_baudrate ok = ', ok

    call port%set_bytesize(8,ok)
    print *, 'set_bytesize ok = ', ok

    call port%set_parity('none',ok)
    print *, 'set_parity ok = ', ok

    call port%set_stopbits(1,ok)
    print *, 'set_stopbits ok = ', ok

    call port%set_rts('on',ok)
    print *, 'set_rts ok = ', ok

    call port%set_cts('ignore',ok)
    print *, 'set_cts ok = ', ok

    call port%set_dtr('off',ok)
    print *, 'set_dtr ok = ', ok

    call port%set_dsr('ignore',ok)
    print *, 'set_dsr ok = ', ok

    call port%set_xon_xoff('in',ok)
    print *, 'set_xon_xoff ok = ', ok

    call port%set_flowcontrol('none',ok)
    print *, 'set_flowcontrol ok = ', ok

    config = port%get_config()
    print *, 'config.ok() = ', ok
    print *, ''

    !config = serialport_config_t(baudrate=115200) 

    call config%print_verbose()
    print *, ''

    !call config%print_concise()
    !print *, ''

    !call port%set_config(config, ok)
    !print *, 'set_config ok = ', ok


    !config = port%get_config()
    !print *, 'config ok = ', config%ok() 
    !print *, ''

    !call config%print_verbose()
    !print *, ''

    call port%close_conn(ok)
    print *, 'close_conn ok = ', ok

contains

end program serial_test

