#ifndef SERIALPORT_UTILS_H
#define SERIALPORT_UTILS_H

#include <libserialport.h>
#include "serialport_defs.inc"

enum spu_return {
    SPU_OK = 0,
    SPU_ERR = -1
};

struct spu_port_info {
    char port_name[SPU_BUF_LEN];
    char description[SPU_BUF_LEN];
    char usb_manufacturer[SPU_BUF_LEN];
    char usb_product[SPU_BUF_LEN];
    char usb_serial[SPU_BUF_LEN];
    char bluetooth_address[SPU_BUF_LEN];
    int  usb_bus;
    int  usb_address;
    int  usb_vendor_id;
    int  usb_product_id;
    int  transport;
};

void spu_usleep(int usecs);
void spu_msleep(int msecs);

void spu_open_port(struct sp_port *port, enum sp_mode mode_flag, int *err_flag);
void spu_close_port(struct sp_port *port, int *err_flag);

void spu_set_baudrate(struct sp_port *port, int baudrate, int *err_flag);
void spu_set_bits(struct sp_port *port, int bits, int *err_flag);
void spu_set_parity(struct sp_port *port, enum sp_parity parity, int *err_flag);
void spu_set_stopbits(struct sp_port *port, int stopbits, int *err_flag);
void spu_set_rts(struct sp_port *port, enum sp_rts rts, int *err_flag);
void spu_set_cts(struct sp_port *port, enum sp_cts cts, int *err_flag);
void spu_set_dtr(struct sp_port *port, enum sp_dtr dtr, int *err_flag);
void spu_set_dsr(struct sp_port *port, enum sp_dsr dsr, int *err_flag);
void spu_set_xon_xoff(struct sp_port *port, enum sp_xonxoff xon_xoff, int *err_flag);
void spu_set_flowcontrol(struct sp_port *port, enum sp_flowcontrol flowcontrol, int *err_flag);

void spu_new_config(struct sp_port_config **config, int *err_flag);
void spu_free_config(struct sp_port_config *config, int *err_flag);

void spu_get_config(struct sp_port *port, struct sp_port_config *config, int *err_flag);
void spu_set_config(struct sp_port *port, const struct sp_port_config *config, int *err_flag);

void spu_get_config_baudrate(const struct sp_port_config *config, int *baudrate, int *err_flag);
void spu_set_config_baudrate(struct sp_port_config *config, int baudrate, int *err_flag);

void spu_get_config_bits(const struct sp_port_config *config, int *bits, int *err_flag);
void spu_set_config_bits(struct sp_port_config *config, int bits, int *err_flag);

void spu_get_config_parity(const struct sp_port_config *config, enum sp_parity *parity, int *err_flag);
void spu_set_config_parity(struct sp_port_config *config, enum sp_parity  parity, int *err_flag);

void spu_get_config_stopbits(const struct sp_port_config *config, int *stopbits, int *err_flag);
void spu_set_config_stopbits(struct sp_port_config *config, int stopbits, int *err_flag);

void spu_get_config_rts(const struct sp_port_config *config, enum sp_rts *rts, int *err_flag);
void spu_set_config_rts(struct sp_port_config *config, enum sp_rts rts, int *err_flag);

void spu_get_config_cts(const struct sp_port_config *config, enum sp_cts *cts, int *err_flag);
void spu_set_config_cts(struct sp_port_config *config, enum sp_cts cts, int *err_flag);

void spu_get_config_dtr(const struct sp_port_config *config, enum sp_dtr *dtr, int *err_flag);
void spu_set_config_dtr(struct sp_port_config *config, enum sp_dtr dtr, int *err_flag);

void spu_get_config_dsr(const struct sp_port_config *config, enum sp_dsr *dsr, int *err_flag);
void spu_set_config_dsr(struct sp_port_config *config, enum sp_dsr dsr, int *err_flag);

void spu_get_config_xon_xoff(const struct sp_port_config *config, enum sp_xonxoff *xon_xoff, int *err_flag);
void spu_set_config_xon_xoff(struct sp_port_config *config, enum sp_xonxoff xon_xoff, int *err_flag);

void spu_set_config_flowcontrol(struct sp_port_config *config, enum sp_flowcontrol flowcontrol, int *err_flag);

void spu_get_num_ports(int *num_ports, int *err_flag);

void spu_get_port_name(int port_num, char port_name[], int max_len, int *err_flag);
void spu_get_port_desc(int port_num, char port_desc[], int max_len, int *err_flag);

void spu_get_port_info(struct sp_port *port, struct spu_port_info *info, int *err_flag);
void spu_get_port_by_name(char name[], struct sp_port **port, int *err_flag);
void spu_get_port_by_number(int number, struct sp_port **port, int *err_flag);
void spu_free_port(struct sp_port *port);

// TODO
// --------------------------------------------------------------------------------------------------------------
void spu_blocking_read(struct sp_port *port, void *buf, int *count, int timeout_ms, int *err_flag);
//void spu_blocking_read_next(struct sp_port *port, void *buf, int *count, int timeout_ms,  int *err_flag);
//void spu_nonblocking_read(struct sp_port *port, void *buf,  int *count, int *err_flag);

//void spu_blocking_write(struct sp_port *port, const void *buf, int *count, int timeout_ms, int *err_flag);
//void spu_nonblocking_write(struct sp_port *port, const void *buf, int *count, int *err_flag);

void spu_input_waiting(struct sp_port *port, int *count, int *err_flag);
void spu_output_waiting(struct sp_port *port, int *count, int *err_flag);

//void spu_flush(struct sp_port *port, enum sp_buffer buffers, int *err_flag);
//void spu_drain(struct sp_port *port, int *err_flag);

//-----------------------------------------------------------------------------------------------------------------

//void spu_get_port_by_serial(char serial[], struct sp_port **port);

#endif 
