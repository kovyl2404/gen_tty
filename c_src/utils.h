#ifndef GEN_TTY_DRV_UTILS_H
#define GEN_TTY_DRV_UTILS_H

#include "gen_tty.h"

int encode_ok(char* buf, unsigned int buf_len);
int encode_error(char* buf, unsigned int buf_len, int code);
int encode_opts(ei_x_buff* buf, GtOpts* const opts);
int decode_opts(char* buf, size_t buf_len, GtOpts *opts);

#endif
