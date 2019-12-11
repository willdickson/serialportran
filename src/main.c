#include <stdio.h>
#include <stdlib.h>
#include "serialport_utils.h"

int main(int argc, char* argv[])
{
    int num_ports;
    int err_flag;
    int max_len = 100;
    char port_name[max_len];
    char port_desc[max_len];
    struct sp_port *port = NULL;
    struct spu_port_info info;

    spu_get_port_by_name("/dev/ttyACM0", &port, &err_flag);
    printf("err_flag: %d\n", err_flag);

    spu_get_port_by_number(0, &port, &err_flag);

    spu_get_port_info(port, &info, &err_flag);
    printf("err_flag: %d\n", err_flag);
    printf("info.name        = %s\n", info.port_name);
    printf("info.description = %s\n", info.description); 
    printf("\n");

    spu_get_num_ports(&num_ports, &err_flag);
    printf("num_ports: %d, err_flag: %d\n", num_ports, err_flag);

    for (int i=0; i<num_ports; i++) {
        spu_get_port_name(i, port_name, max_len, &err_flag);
        spu_get_port_desc(i, port_desc, max_len, &err_flag);
        printf(" %d: %s, %s\n", i, port_name, port_desc);
    }

    printf("sizeof(int): %d\n", (int) sizeof(int));

    return 0;
}
