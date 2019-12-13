program serial_test

    use serialport_list, only: serialport_list_t
    use serialport_info, only: serialport_info_t
    use serialport_dev,  only: serialport_t

    implicit none

    type(serialport_list_t) :: port_list
    type(serialport_t)      :: port
    type(serialport_info_t) :: info
    logical                 :: ok

    character(len=100), allocatable      :: tmp

    tmp  = 'wr'


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

    call port%open_conn('rw',ok)

    call port%close_conn(ok)

contains

end program serial_test

