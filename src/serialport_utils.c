#include "serialport_utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef _WIN32
#error "windows time includes not yet implmented "
#else
#define ERR_MSG oh nooo!
#include <time.h>
#endif



void spu_usleep(int usecs) {
    int abs_usecs = abs(usecs);
    usleep((useconds_t)(abs_usecs));
}


void spu_msleep(int msecs) {
    int abs_usecs = 1000*abs(msecs);
    usleep((useconds_t)(abs_usecs));
}


#ifdef _WIN32
#error "windows verion of get_time_ms not yet implemented "
#else
void spu_get_time_ms(double *t) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC,&ts);
    *t = 1000.0*(double)(ts.tv_sec) + 1.0e-6*(double)(ts.tv_nsec);
}
#endif


void spu_open_port(struct sp_port *port, enum sp_mode mode_flag, int *err_flag) {
    *err_flag = SPU_ERR;
    if (port != NULL) {
        if (sp_open(port,mode_flag) == SP_OK) {
            *err_flag = SPU_OK;
        }
    }
}


void spu_close_port(struct sp_port *port, int *err_flag) {
    *err_flag = SPU_ERR;
    if (port != NULL) {
        if (sp_close(port) == SP_OK) {
            *err_flag = SPU_OK;
        }
    }
}


void spu_set_baudrate(struct sp_port *port, int baudrate, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_baudrate(port, baudrate) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_bits(struct sp_port *port, int bits, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_bits(port, bits) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_parity(struct sp_port *port, enum sp_parity parity, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_parity(port, parity) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_stopbits(struct sp_port *port, int stopbits, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_stopbits(port, stopbits) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_rts(struct sp_port *port, enum sp_rts rts, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_rts(port, rts) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_cts(struct sp_port *port, enum sp_cts cts, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_cts(port, cts) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_dtr(struct sp_port *port, enum sp_dtr dtr, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_dtr(port, dtr) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_dsr(struct sp_port *port, enum sp_dsr dsr, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_dsr(port, dsr) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_xon_xoff(struct sp_port *port, enum sp_xonxoff xon_xoff, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_xon_xoff(port, xon_xoff) == SP_OK) {
        *err_flag = SPU_OK;
    }
}

void spu_set_flowcontrol(struct sp_port *port, enum sp_flowcontrol flowcontrol, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_flowcontrol(port, flowcontrol) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_new_config(struct sp_port_config **config, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_new_config(config) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_free_config(struct sp_port_config *config, int *err_flag) {
    *err_flag = SPU_ERR;
    if (config != NULL) {
        sp_free_config(config);
        *err_flag = SPU_OK;
    }
}


void spu_get_config(struct sp_port *port, struct sp_port_config *config, int *err_flag)
{
    *err_flag = SPU_ERR;
    if (sp_get_config(port,config) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config(struct sp_port *port, const struct sp_port_config *config, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config(port,config) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_baudrate(const struct sp_port_config *config, int *baudrate, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_baudrate(config,baudrate) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_baudrate(struct sp_port_config *config, int baudrate, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_baudrate(config,baudrate) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_bits(const struct sp_port_config *config, int *bits, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_bits(config,bits) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_bits(struct sp_port_config *config, int bits, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_bits(config,bits) == SP_OK) {
        *err_flag = SPU_OK;
    }
}



void spu_get_config_parity(const struct sp_port_config *config, enum sp_parity *parity, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_parity(config, parity) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_parity(struct sp_port_config *config, enum sp_parity  parity, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_parity(config, parity) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_stopbits(const struct sp_port_config *config, int *stopbits, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_stopbits(config, stopbits) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_stopbits(struct sp_port_config *config, int stopbits, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_stopbits(config, stopbits) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_rts(const struct sp_port_config *config, enum sp_rts *rts, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_rts(config, rts) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_rts(struct sp_port_config *config, enum sp_rts rts, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_rts(config, rts) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_cts(const struct sp_port_config *config, enum sp_cts *cts, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_cts(config, cts) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_cts(struct sp_port_config *config, enum sp_cts cts, int *err_flag) { 
    *err_flag = SPU_ERR;
    if (sp_set_config_cts(config, cts) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_dtr(const struct sp_port_config *config, enum sp_dtr *dtr, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_dtr(config, dtr) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_dtr(struct sp_port_config *config, enum sp_dtr dtr, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_dtr(config, dtr) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_dsr(const struct sp_port_config *config, enum sp_dsr *dsr, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_dsr(config, dsr) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_dsr(struct sp_port_config *config, enum sp_dsr dsr, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_dsr(config, dsr) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_config_xon_xoff(const struct sp_port_config *config, enum sp_xonxoff *xon_xoff, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_get_config_xon_xoff(config, xon_xoff) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_xon_xoff(struct sp_port_config *config, enum sp_xonxoff xon_xoff, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_set_config_xon_xoff(config, xon_xoff) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_set_config_flowcontrol(struct sp_port_config *config, enum sp_flowcontrol flowcontrol, int *err_flag)
{
    *err_flag = SPU_ERR;
    if (sp_set_config_flowcontrol(config, flowcontrol) == SP_OK) {
        *err_flag = SPU_OK;
    }
}


void spu_get_num_ports(int *num_ports, int *err_flag) {
    struct sp_port **port_list;
    int i=0;
    *num_ports = 0;
    *err_flag = SPU_ERR;
    if (sp_list_ports(&port_list) == SP_OK) {
        for (i=0; port_list[i]!=NULL; i++) {} 
        *num_ports = i;
        *err_flag = SPU_OK;
        sp_free_port_list(port_list);
    }
    return;
}


void spu_get_port_name(int port_num, char port_name[], int max_len, int *err_flag) {
    struct sp_port **port_list;
    char *port_name_tmp = NULL;

    *err_flag = SPU_ERR;
    if (sp_list_ports(&port_list) == SP_OK) {
        for (int i=0; port_list[i]!=NULL; i++) {
            if (i == port_num) {
                port_name_tmp = sp_get_port_name(port_list[i]);
                break;
            }
        }
        if (port_name_tmp != NULL) {
            *err_flag  = SPU_OK;
            strncpy(port_name,port_name_tmp,max_len);
        }
        sp_free_port_list(port_list);
    }
    return;
}


void spu_get_port_desc(int port_num, char port_desc[], int max_len, int *err_flag) {
    struct sp_port **port_list;
    char *port_desc_tmp = NULL;

    *err_flag = SPU_ERR;
    if (sp_list_ports(&port_list) == SP_OK) {
        for (int i=0; port_list[i]!=NULL; i++) {
            if (i == port_num) {
                port_desc_tmp = sp_get_port_description(port_list[i]);
                break;
            }
        }
        if (port_desc_tmp != NULL) {
            *err_flag  = SPU_OK;
            strncpy(port_desc,port_desc_tmp,max_len);
        }
        sp_free_port_list(port_list);
    }
    return;
}


void spu_get_port_info(struct sp_port *port, struct spu_port_info *info, int *err_flag) {

    struct sp_port **port_list;
    int rval = SP_OK;

    char *buf_ptr;

    *err_flag = SPU_ERR;
    if (sp_list_ports(&port_list) == SP_OK) {


        buf_ptr = sp_get_port_name(port);
        if (buf_ptr != NULL) {
            strncpy(info -> port_name, buf_ptr, SPU_BUF_LEN);
        }
        else {
            strncpy(info -> port_name, "NA",SPU_BUF_LEN);
        }

        buf_ptr = sp_get_port_description(port);
        if (buf_ptr != NULL) {
            strncpy(info -> description, buf_ptr, SPU_BUF_LEN);
        }
        else {
            strncpy(info -> description, "NA",SPU_BUF_LEN);
        }

        buf_ptr = sp_get_port_usb_manufacturer(port);
        if (buf_ptr != NULL) {
            strncpy(info -> usb_manufacturer, buf_ptr, SPU_BUF_LEN);
        }
        else {
            strncpy(info -> usb_manufacturer, "NA",SPU_BUF_LEN);
        }

        buf_ptr = sp_get_port_usb_product(port);
        if (buf_ptr != NULL) {
            strncpy(info -> usb_product, buf_ptr, SPU_BUF_LEN);
        }
        else {
            strncpy(info -> usb_product, "NA",SPU_BUF_LEN);
        }

        buf_ptr = sp_get_port_usb_serial(port);
        if (buf_ptr != NULL) {
            strncpy(info -> usb_serial, buf_ptr, SPU_BUF_LEN);
        }
        else {
            strncpy(info -> usb_serial, "NA",SPU_BUF_LEN);
        }

        buf_ptr = sp_get_port_bluetooth_address(port);
        if (buf_ptr != NULL) {
            strncpy(info -> bluetooth_address, buf_ptr, SPU_BUF_LEN);
        } 
        else {
            strncpy(info -> bluetooth_address, "NA",SPU_BUF_LEN);
        }

        rval = sp_get_port_usb_bus_address(port, &(info->usb_bus), &(info->usb_address));
        if (rval != SP_OK) {
            info -> usb_bus = -1;
            info -> usb_address = -1;
        }

        rval = sp_get_port_usb_vid_pid(port, &(info->usb_vendor_id), &(info->usb_product_id));
        if (rval != SP_OK) {
            info -> usb_vendor_id = 0;
            info -> usb_product_id = 0;
        }

        info-> transport = sp_get_port_transport(port); 

        *err_flag = SPU_OK;
        sp_free_port_list(port_list);
    }
    return;
}


void spu_get_port_by_name(char name[], struct sp_port **port, int *err_flag) {
    if (sp_get_port_by_name(name,port) == SP_OK) {
        *err_flag = SPU_OK;
    }
    else {
        *err_flag = SPU_ERR;
    }
    return;
}


void spu_get_port_by_number(int number, struct sp_port **port, int *err_flag) {
    struct sp_port **port_list;
    *err_flag = SPU_ERR;
    if (sp_list_ports(&port_list) == SP_OK) {
        for (int i=0; port_list[i]!=NULL; i++) {
            if (i==number) {
                *port = port_list[i];
                *err_flag = SPU_OK;
            }
        }
    }
    return;
}


void spu_free_port(struct sp_port *port) {
    sp_free_port(port);
    return;
}


void spu_blocking_read(struct sp_port *port, char buf[], size_t *count, int timeout_ms, int *err_flag) {
    size_t count_req = *count;
    int rval = sp_blocking_read(port, (void *)(buf), count_req, (unsigned int)(timeout_ms));
    if (rval >=0) {
        if (rval > 0) {
        }
        *err_flag = SPU_OK;
        *count = (size_t)(rval);
    }
    else {
        *err_flag = SPU_ERR;
        *count = 0;
    }
}


void spu_nonblocking_read(struct sp_port *port, char buf[],  size_t *count, int *err_flag) {
    size_t count_req = *count;
    int rval = sp_nonblocking_read(port, (void *)(buf), count_req);
    if (rval >=0) {
        *err_flag = SPU_OK;
        *count = (size_t)(rval);
    }
    else {
        *err_flag = SPU_ERR;
        *count = 0;
    }
}


void spu_blocking_read_next(struct sp_port *port, char buf[], size_t *count, int timeout_ms,  int *err_flag) {
    size_t count_req = *count;
    int rval = sp_blocking_read_next(port, (void *)(buf), count_req, (unsigned int)(timeout_ms));
    if (rval >=0) {
        if (rval > 0) {
        }
        *err_flag = SPU_OK;
        *count = (size_t)(rval);
    }
    else {
        *err_flag = SPU_ERR;
        *count = 0;
    }
}


void spu_blocking_write(struct sp_port *port, const char buf[], size_t *count, int timeout_ms, int *err_flag) {
    size_t count_req = *count;
    int rval = sp_blocking_write(port, (void *)(buf), count_req, (unsigned int)(timeout_ms));
    if (rval >=0) {
        *err_flag = SPU_OK;
        *count = (size_t)(rval);
    }
    else {
        *err_flag = SPU_ERR;
        *count = 0;
    }
}


void spu_nonblocking_write(struct sp_port *port, const char buf[], size_t *count, int *err_flag) {
    size_t count_req = *count;
    int rval = sp_nonblocking_write(port, (void *)(buf), count_req);
    if (rval >=0) {
        *err_flag = SPU_OK;
        *count = (size_t)(rval);
    }
    else {
        *err_flag = SPU_ERR;
        *count = 0;
    }
}


void spu_input_waiting(struct sp_port *port, size_t *count, int *err_flag) {
    int rval = sp_input_waiting(port);
    if (rval >= 0) {
        *err_flag = SPU_OK;
        *count = (size_t)(rval);
    }
    else {
        *err_flag = SPU_ERR;
        *count = 0;
    }
}


void spu_output_waiting(struct sp_port *port, size_t *count, int *err_flag) {
    int rval = sp_output_waiting(port);
    if (rval >= 0) {
        *err_flag = SPU_OK;
        *count = (size_t)(rval);
    }
    else {
        *err_flag = SPU_ERR;
        *count = 0;
    }
}


void spu_flush(struct sp_port *port, enum sp_buffer buffers, int *err_flag) {
    *err_flag = SPU_ERR;
    if (sp_flush(port, buffers) == SP_OK) {
        *err_flag = SPU_OK;
    }
}
