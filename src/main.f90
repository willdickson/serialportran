program serial_test

    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_size_t
    use serialport_utils,  only : spu_msleep
    use serialport_list,   only : serialport_list_t
    use serialport_info,   only : serialport_info_t
    use serialport_config, only : serialport_config_t
    use serialport_dev,    only : serialport_t

    implicit none

    type(serialport_list_t)          :: port_list
    type(serialport_t)               :: port
    type(serialport_info_t)          :: info
    type(serialport_config_t)        :: config
    logical                          :: ok
    integer(c_size_t)                :: num_bytes
    character(:,c_char), allocatable :: bytes_send
    character(:,c_char), allocatable :: bytes_recv
    integer                          :: i

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

    config = serialport_config_t(baudrate=115200)

    call port%set_config(config, ok)
    !print *, 'set_config ok = ', ok


    config = port%get_config()
    print *, 'config.ok() = ', ok
    print *, ''

    call config%print_verbose()
    print *, ''

    num_bytes = 1
    allocate(character(1)::bytes_send)
    bytes_send(1:1) = '2'
    call port%blocking_write(num_bytes, bytes_send, 10, ok)
    print *, 'blocking_write ok = ', ok
    print *, 'num_bytes = ', num_bytes
    print *, 'bytes_send = ', bytes_send
    print *, ''

    call spu_msleep(10)
    call port%in_waiting(num_bytes, ok)
    print *, 'in_waiting ok = ', ok
    print *, 'num_bytes = ', num_bytes
    print *, ''

    num_bytes = 10
    allocate(character(num_bytes)::bytes_recv)
    call port%blocking_read(num_bytes, bytes_recv, 10, ok)
    print *, 'blocking_read ok = ', ok
    print *, 'num_bytes = ', num_bytes
    print *, 'bytes_recv = ', bytes_recv
    print *, ''

    do i = 1,int(num_bytes)
        print *, i, iachar(bytes_recv(i:i))
    end do


    !call port%in_waiting(num_bytes, ok)
    !print *, 'in_waiting ok = ', ok
    !print *, 'num_bytes = ', num_bytes 

    !call port%out_waiting(num_bytes, ok)
    !print *, 'out_waiting ok = ', ok
    !print *, 'num_bytes = ', num_bytes 


    call port%close_conn(ok)
    print *, 'close_conn ok = ', ok

contains

end program serial_test

