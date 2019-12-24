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
    character(len=:), allocatable :: rts
    character(len=:), allocatable :: cts
    character(len=:), allocatable :: dtr 
    character(len=:), allocatable :: dsr 
    character(len=:), allocatable :: xon_xoff 


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

    !call config%get_baudrate(baudrate, ok)
    !print *, 'get_baudrate ok = ', ok
    !print *, 'baudrate = ', baudrate
    !print *, ''

    !call config%set_baudrate(115200,ok)
    !call config%get_baudrate(baudrate, ok)
    !print *, 'get_baudrate ok = ', ok
    !print *, 'baudrate = ', baudrate
    !print *, ''

    !call config%get_bytesize(bytesize, ok)
    !print *, 'get bytesize ok = ', ok
    !print *, 'bytesize = ',  bytesize

    !call config%set_bytesize(7, ok)
    !call config%get_bytesize(bytesize, ok)
    !print *, 'get bytesize ok = ', ok
    !print *, 'bytesize = ',  bytesize
    !print *, ''

    !call config%get_parity(parity_mode, ok)
    !print *, 'get_parity ok = ', ok
    !print *, 'parity_mode = ', parity_mode

    !call config%set_parity('none', ok)
    !call config%get_parity(parity_mode, ok)
    !print *, 'get_parity ok = ', ok
    !print *, 'parity_mode = ', parity_mode
    !print *, ''

    !call config%get_stopbits(stopbits, ok)
    !print *, 'get_stopbits ok = ', ok
    !print *, 'stopbits = ', stopbits

    !call config%set_stopbits(2, ok)
    !print *, 'set_stopbits ok = ', ok
    !call config%get_stopbits(stopbits, ok)
    !print *, 'get_stopbits ok = ', ok
    !print *, 'stopbits = ', stopbits
    !print *, ''

    !call config%get_rts(rts, ok)
    !print *, 'get_rts ok = ', ok
    !print *, 'rts = ', rts 
    !print *, ''

    !call config%set_rts('off', ok)
    !print *, 'set_rts ok = ', ok
    !call config%get_rts(rts, ok)
    !print *, 'get_rts ok = ', ok
    !print *, 'rts = ', rts 
    !print *, ''

    !call config%get_cts(cts, ok)
    !print *, 'get_cts ok = ', ok
    !print *, 'cts = ', cts 
    !print *, ''

    !call config%set_cts('flow_control', ok)
    !print *, 'set_cts ok = ', ok
    !call config%get_cts(cts, ok)
    !print *, 'get_cts ok = ', ok
    !print *, 'cts = ', cts 
    !print *, ''

    !call config%get_dtr(dtr, ok)
    !print *, 'get_dtr ok = ', ok
    !print *, 'dtr = ', dtr 
    !print *, ''

    !call config%set_dtr('flow_control', ok)
    !print *, 'set_dtr ok = ', ok
    !call config%get_dtr(dtr, ok)
    !print *, 'get_dtr ok = ', ok
    !print *, 'dtr = ', dtr 
    !print *, ''

    !call config%get_dsr(dsr, ok)
    !print *, 'get_dsr ok = ', ok
    !print *, 'dsr = ', dsr 
    !print *, ''

    !call config%set_dsr('flow_control', ok)
    !print *, 'set_dsr ok = ', ok
    !call config%get_dsr(dsr, ok)
    !print *, 'get_dsr ok = ', ok
    !print *, 'dsr = ', dsr 
    !print *, ''

    !call config%get_xon_xoff(xon_xoff, ok)
    !print *, 'get_xon_xoff ok = ', ok
    !print *, 'xon_xoff = ', xon_xoff 
    !print *, ''

    !call config%set_xon_xoff('out', ok)
    !print *, 'set_xon_xoff ok = ', ok
    !call config%get_xon_xoff(xon_xoff, ok)
    !print *, 'get_xon_xoff ok = ', ok
    !print *, 'xon_xoff = ', xon_xoff 
    !print *, ''

    !call config%set_flowcontrol('xon_xoff', ok)
    !print *, 'set_flowcontrol ok = ', ok

    !print *, ''
    !print *, ''

    call config%print_verbose()
    print *, ''
    print *, ''

    call config%print_concise()
    print *, ''
    print *, ''

    call port%close_conn(ok)
    print *, 'ok = ', ok



contains

end program serial_test

