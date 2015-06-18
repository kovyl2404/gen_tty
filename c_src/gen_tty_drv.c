#include <errno.h>
#include <erl_driver.h>
#include <stdio.h>
#include <assert.h>
#include <strings.h>
#include <ei.h>
#include <string.h>
#include "gen_tty.h"
#include "utils.h"
#include "gen_tty_proto.h"

char __driver_name[] = "gen_tty_drv";

typedef struct {
    ErlDrvPort port;
    GtFileP file;
} GtPortData;

ErlDrvData drv_start(ErlDrvPort port, char* command) {
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    char* const file_name = command + sizeof(__driver_name);
    GtPortData* port_data = (GtPortData*)driver_alloc(sizeof(GtPortData));
    port_data->file = NULL;
    port_data->port = port;
    int ret = gt_open(file_name, &port_data->file);
    if (ret != 0) {
        driver_free(port_data);
        errno = ret;
        return ERL_DRV_ERROR_ERRNO;
    }
    return (ErlDrvData)port_data;
}

void drv_stop(ErlDrvData drv) {
    GtPortData* port_data = (GtPortData*)drv;
    int ret = gt_close(port_data->file);
    assert(ret == 0);
}

void drv_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) {

}

void drv_ready_input(ErlDrvData drv_data, ErlDrvEvent event) {

}

void drv_ready_output(ErlDrvData drv_data, ErlDrvEvent event) {

}

ErlDrvSSizeT drv_control(
    ErlDrvData drv, unsigned int command,
    char *buf, ErlDrvSizeT len, char **ret_buf,
    ErlDrvSizeT rlen
) {
    GtPortData* port_data = (GtPortData*)drv;
    GtOpts opts;
    bzero(&opts, sizeof(GtOpts));
    int ret = 0;
    ei_x_buff ext_buf;
    ErlDrvBinary* binary_buf;
    switch (command) {
        case GT_ERL_CTL_GETOPTS:
            ret = gt_getopts(port_data->file, &opts);
            if (ret != 0) {
                return encode_error(*ret_buf, rlen, ret);
            }
            if (ei_x_new_with_version(&ext_buf) != 0) {
                return encode_error(*ret_buf, rlen, ENOMEM);
            }
            if ( encode_opts(&ext_buf, &opts) != 0
                || (binary_buf = driver_alloc_binary(ext_buf.index)) == NULL ) {
                ei_x_free(&ext_buf);
                return encode_error(*ret_buf, rlen, ENOMEM);
            }
            memcpy(binary_buf->orig_bytes, ext_buf.buff, ext_buf.index);
            ei_x_free(&ext_buf);
            *ret_buf = (char*)binary_buf;
            return ext_buf.index;
        case GT_ERL_CTL_SETOPTS:
            ret = gt_getopts(port_data->file, &opts);
            if (ret != 0) {
                return encode_error(*ret_buf, rlen, ret);
            }
            if ( decode_opts(buf, len, &opts) != 0 ) {
                return encode_error(*ret_buf, rlen, EINVAL);
            }
            ret = gt_setopts(port_data->file, &opts);
            if ( ret != 0 ) {
                return encode_error(*ret_buf, rlen, ret);
            }
            return encode_ok(*ret_buf, rlen);
    }
    return 0;
}


ErlDrvEntry driver_entry = {
    NULL,
    drv_start,
    drv_stop,
    drv_output,
    drv_ready_input,
    drv_ready_output,
    __driver_name,
    NULL,
    NULL,
    drv_control,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(gen_tty_drv) {
    return &driver_entry;
}


