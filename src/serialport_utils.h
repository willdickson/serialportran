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

void spu_get_num_ports(int *num_ports, int *err_flag);
void spu_get_port_name(int port_num, char port_name[], int max_len, int *err_flag);
void spu_get_port_desc(int port_num, char port_desc[], int max_len, int *err_flag);


void spu_get_port_info(struct sp_port *port, struct spu_port_info *info, int *err_flag);
void spu_get_port_by_name(char name[], struct sp_port **port, int *err_flag);
void spu_get_port_by_number(int number, struct sp_port **port, int *err_flag);
void spu_free_port(struct sp_port *port);
//void spu_get_port_by_serial(char serial[], struct sp_port **port);

#endif 
