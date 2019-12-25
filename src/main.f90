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
    print *, 'ok = ', ok
    print *, 'port%is_open() = ', port%is_open()
    print *, ''

    config = serialport_config_t(baudrate=115200) 

    call config%print_verbose()
    print *, ''

    call config%print_concise()
    print *, ''

    call port%set_config(config, ok)
    print *, 'set_config ok = ', ok


    config = port%get_config()
    print *, 'config ok = ', config%ok() 
    print *, ''

    call config%print_verbose()
    print *, ''

    call port%close_conn(ok)
    print *, 'ok = ', ok

contains

end program serial_test

