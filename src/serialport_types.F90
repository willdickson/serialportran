module serialport_types

#include "serialport_defs.inc"

    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_int


    implicit none
    private 

    public buf_len
    public SPU_OK
    public SPU_ERR
    public SP_TRANSPORT_NATIVE
    public SP_TRANSPORT_USB
    public SP_TRANSPORT_BLUETOOTH
    public spu_port_info_t

    integer,  parameter ::  buf_len = SPU_BUF_LEN 

    enum, bind(c)
        enumerator :: SPU_OK  = 0
        enumerator :: SPU_ERR = 1
    end enum

    enum, bind(c)
        enumerator :: SP_TRANSPORT_NATIVE    = 0
        enumerator :: SP_TRANSPORT_USB       = 1
        enumerator :: SP_TRANSPORT_BLUETOOTH = 2
    end enum

    type, bind(c)                            :: spu_port_info_t
        character(len=1, kind=c_char)        :: port_name(buf_len) 
        character(len=1, kind=c_char)        :: description(buf_len) 
        character(len=1, kind=c_char)        :: usb_manufacturer(buf_len) 
        character(len=1, kind=c_char)        :: usb_product(buf_len)
        character(len=1, kind=c_char)        :: usb_serial(buf_len)
        character(len=1, kind=c_char)        :: bluetooth_address(buf_len)
        integer(c_int)                       :: usb_bus
        integer(c_int)                       :: usb_address
        integer(c_int)                       :: usb_vendor_id
        integer(c_int)                       :: usb_product_id
        integer(c_int)                       :: transport
    end type spu_port_info_t


end module serialport_types
