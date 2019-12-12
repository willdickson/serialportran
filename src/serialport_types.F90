module serialport_types

#include "serialport_defs.inc"

    use, intrinsic :: iso_c_binding, only : c_char
    use, intrinsic :: iso_c_binding, only : c_int


    implicit none
    public

    integer,  parameter ::  buf_len = SPU_BUF_LEN 

    enum, bind(c)
        enumerator :: SPU_OK  = 0
        enumerator :: SPU_ERR = 1
    end enum

    ! Port access modes
    enum, bind(c) 
        enumerator :: SP_MODE_READ       = 1
        enumerator :: SP_MODE_WRITE      = 2
        enumerator :: SP_MODE_READ_WRITE = 3
    end enum

    ! Port events
    enum, bind(c) 
        enumerator :: SP_EVENT_RX_READY = 1
        enumerator :: SP_EVENT_TX_READY = 2
        enumerator :: SP_EVENT_ERROR    = 4
    end enum

    ! Buffer selection
    enum, bind(c) 
        enumerator :: SP_BUF_INPUT  = 1
        enumerator :: SP_BUF_OUTPUT = 2
        enumerator :: SP_BUF_BOTH   = 3
    end enum

    ! Parity settings
    enum, bind(c) 
        enumerator :: SP_PARITY_INVALID = -1
        enumerator :: SP_PARITY_NONE    =  0
        enumerator :: SP_PARITY_ODD     =  1
        enumerator :: SP_PARITY_EVEN    =  2
        enumerator :: SP_PARITY_MARK    =  3
        enumerator :: SP_PARITY_SPACE   =  4
    end enum

    ! RTS pin behaviour
    enum, bind(c)
        enumerator :: SP_RTS_INVALID      = -1
        enumerator :: SP_RTS_OFF          =  0
        enumerator :: SP_RTS_ON           =  1
        enumerator :: SP_RTS_FLOW_CONTROL =  2
    end enum

    ! CTS pin behaviour
    enum, bind(c)
        enumerator :: SP_CTS_INVALID      = -1
        enumerator :: SP_CTS_IGNORE       =  0
        enumerator :: SP_CTS_FLOW_CONTROL =  1
    end enum

    ! DTR pin behaviour
    enum, bind(c) 
        enumerator :: SP_DTR_INVALID      = -1
        enumerator :: SP_DTR_OFF          =  0
        enumerator :: SP_DTR_ON           =  1
        enumerator :: SP_DTR_FLOW_CONTROL =  2
    end enum

    ! DSR pin behaviour. 
    enum, bind(c)
        enumerator :: SP_DSR_INVALID      = -1
        enumerator :: SP_DSR_IGNORE       =  0
        enumerator :: SP_DSR_FLOW_CONTROL =  1
    end enum
    
    ! XON/XOFF flow control behaviour. 
    enum, bind(c)
        enumerator :: SP_XONXOFF_INVALID  = -1
        enumerator :: SP_XONXOFF_DISABLED =  0
        enumerator :: SP_XONXOFF_IN       =  1
        enumerator :: SP_XONXOFF_OUT      =  2
        enumerator :: SP_XONXOFF_INOUT    =  3
    end enum
    
    ! Standard flow control combinations. 
    enum, bind(c)
        enumerator :: SP_FLOWCONTROL_NONE    = 0
        enumerator :: SP_FLOWCONTROL_XONXOFF = 1
        enumerator :: SP_FLOWCONTROL_RTSCTS  = 2
        enumerator :: SP_FLOWCONTROL_DTRDSR  = 3
    end enum

    ! Input signals
    enum, bind(c) 
        enumerator :: SP_SIG_CTS = 1
        enumerator :: SP_SIG_DSR = 2
        enumerator :: SP_SIG_DCD = 4
        enumerator :: SP_SIG_RI  = 8
    end enum

    ! Transport types.
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
