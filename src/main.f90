program serial_test

    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_size_t
    use, intrinsic :: iso_c_binding, only : c_double
    use serialport_utils,  only : spu_msleep
    use serialport_utils,  only : get_time_ms
    use serialport_list,   only : serialport_list_t
    use serialport_info,   only : serialport_info_t
    use serialport_config, only : serialport_config_t
    use serialport_dev,    only : serialport_t
    use serialport_kinds

    implicit none

    !type(serialport_list_t)          :: port_list
    type(serialport_t)               :: port
    type(serialport_config_t)        :: config
    logical                          :: ok
    integer(c_size_t)                :: num_bytes
    character(:,c_char), allocatable :: bytes_send
    character(:,c_char), allocatable :: bytes_recv
    integer                          :: i
    integer                          :: n
    integer                          :: num_send
    integer                          :: fail_count
    real(dp)                         :: t1
    real(dp)                         :: t2 
    real(dp)                         :: dt
    real(dp)                         :: freq

    !call port_list%update()
    !call port_list%print_concise()

    print *, ''

    port = serialport_t('/dev/ttyACM0')
    if (.not. port%ok() )  then
        print *, 'port creation failed'
        stop
    end if

    call port%open_conn('rw',ok)
    if (.not. ok) then
        print *, 'open failed'
        stop
    end if

    config = serialport_config_t(baudrate=115200)
    if (.not. config%ok()) then
        print *, 'create config failed'
        stop
    end if

    call port%set_config(config, ok)
    if (.not. ok) then
        print *, 'set config failed'
        stop
    end if

    allocate(character(1)::bytes_send)
    
    t1 = get_time_ms()

    num_send = 100000
    fail_count = 0

    do i=1,num_send

        num_bytes = 1
        bytes_send(1:1) = '2'
        call port%blocking_write(num_bytes, bytes_send, 10, ok)
        if (.not. ok) then 
            print *, 'write failed'
            stop
        end if

        call port%readline(bytes_recv,10,ok)
        if (.not. ok) then
            print *, 'readline failed'
            fail_count = fail_count + 1
            cycle
        end if
        n = len(bytes_recv)
        !print *, bytes_recv(1:n-1)

    end do

    t2 = get_time_ms()
    dt = t2 - t1
    freq = num_send/(1.0e-3*dt)

    print *, 'num_send   = ', num_send
    print *, 'dt         = ', 1.0e-3*dt
    print *, 'freq       = ', freq
    print *, 'fail_count = ', fail_count
    print *, ''

    call port%close_conn(ok)
    if (.not. ok) then
        print *, 'close_conn failed'
        stop
    end if

end program serial_test

