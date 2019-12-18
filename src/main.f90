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
    integer                       :: baudrate
    integer                       :: bytesize
    integer                       :: stopbits
    character(len=:), allocatable :: parity_mode


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

    config = port%get_config()
    print *, 'config ok = ', config%ok() 
    print *, ''

    call config%get_baudrate(baudrate, ok)
    print *, 'get_baudrate ok = ', ok
    print *, 'baudrate = ', baudrate
    print *, ''

    call config%set_baudrate(115200,ok)
    call config%get_baudrate(baudrate, ok)
    print *, 'get_baudrate ok = ', ok
    print *, 'baudrate = ', baudrate
    print *, ''

    call config%get_bytesize(bytesize, ok)
    print *, 'get bytesize ok = ', ok
    print *, 'bytesize = ',  bytesize

    call config%set_bytesize(7, ok)
    call config%get_bytesize(bytesize, ok)
    print *, 'get bytesize ok = ', ok
    print *, 'bytesize = ',  bytesize
    print *, ''

    call config%get_parity(parity_mode, ok)
    print *, 'get_parity ok = ', ok
    print *, 'parity_mode = ', parity_mode

    call config%set_parity('none', ok)
    call config%get_parity(parity_mode, ok)
    print *, 'get_parity ok = ', ok
    print *, 'parity_mode = ', parity_mode
    print *, ''

    call config%get_stopbits(stopbits, ok)
    print *, 'get_stopbits ok = ', ok
    print *, 'stopbits = ', stopbits

    call config%set_stopbits(2, ok)
    call config%get_stopbits(stopbits, ok)
    print *, 'get_stopbits ok = ', ok
    print *, 'stopbits = ', stopbits
    print *, ''

    call port%close_conn(ok)
    print *, 'ok = ', ok


contains

end program serial_test

