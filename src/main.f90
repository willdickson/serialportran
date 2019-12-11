program serial_test

    use serialport_list, only: serialport_list_t
    use serialport_info, only: serialport_info_t
    use serialport_dev,  only: serialport_t

    implicit none

    type(serialport_list_t) :: port_list
    type(serialport_t)      :: port
    type(serialport_info_t) :: info
    logical                 :: ok
    

    call port_list%update()
    call port_list%print_concise()

    port = serialport_t('/dev/ttyACM0',ok)
    print '(A,L)', 'ok = ', ok
    if ( .not. ok )  then
        print '(A)', 'port creation failed'
        stop
    else 
        print '(A)', 'port creation successful'
    end if
    print '(/)'

    info = port%get_info()
    call info%print_verbose()

    print '(/)'

contains

end program serial_test

