#include <ei.h>
#include <assert.h>
#include <erl_driver.h>
#include <stdbool.h>
#include "utils.h"
#include "gen_tty.h"
#include "gen_tty_proto.h"


#define CHECK_RES(x) \
    {if ((x) != 0) { return EINVAL; }}
#define CHECK_RES_AND_INDEX(x, idx, len) { \
    if (((x) != 0) || (idx > len) ) { return EINVAL; } \
}



int encode_error(char* buf, unsigned int buf_len, int code) {
    int index = 0;
    ei_encode_version(buf, &index);
    ei_encode_tuple_header(buf, &index, 2);
    ei_encode_atom(buf, &index, "error");
    ei_encode_atom(buf, &index, erl_errno_id(code));
    assert( (unsigned int)index < buf_len );
    return index;
}

int encode_ok(char* buf, unsigned int buf_len) {
    int index = 0;
    ei_encode_version(buf, &index);
    ei_encode_atom(buf, &index, "ok");
    assert( (unsigned int)index < buf_len );
    return index;
}

int encode_opts(ei_x_buff* buf, GtOpts* const opts) {

    CHECK_RES( ei_x_encode_list_header(buf, 3) );
    CHECK_RES( ei_x_encode_tuple_header(buf, 2) );
    CHECK_RES( ei_x_encode_ulong(buf, GT_ERL_OPT_SPEED) );
    CHECK_RES( ei_x_encode_ulong(buf, opts->baud_rate) );
    CHECK_RES( ei_x_encode_tuple_header(buf, 2) );
    CHECK_RES( ei_x_encode_ulong(buf, GT_ERL_OPT_CSIZE) );
    CHECK_RES( ei_x_encode_ulong(buf, opts->char_size) );
    CHECK_RES( ei_x_encode_tuple_header(buf, 2) );
    CHECK_RES( ei_x_encode_ulong(buf, GT_ERL_OPT_PARITY) );

    unsigned long parity_val = 0;
    switch (opts->parity) {
        case parNone:
            parity_val = GT_ERL_PARITY_VAL_NONE;
            break;
        case parEven:
            parity_val = GT_ERL_PARITY_VAL_EVEN;
            break;
        case parOdd:
            parity_val = GT_ERL_PARITY_VAL_ODD;
            break;
        case parMark:
            parity_val = GT_ERL_PARITY_VAL_MARK;
            break;
        case parSpace:
            parity_val = GT_ERL_PARITY_VAL_SPACE;
            break;
        default:
            assert(false);
    }

    CHECK_RES( ei_x_encode_ulong(buf, parity_val));
    CHECK_RES( ei_x_encode_empty_list(buf) );
    return 0;
}

int do_apply_option(unsigned long opt, unsigned long opt_val, GtOpts* opts) {
    GtParity parity = 0;
    switch (opt) {
        case GT_ERL_OPT_SPEED:
            opts->baud_rate = opt_val;
            break;
        case GT_ERL_OPT_CSIZE:
            if ( opt_val < 5 || opt_val > 8) {
                return 1;
            }
            opts->char_size = opt_val;
            break;
        case GT_ERL_OPT_PARITY:
            switch (opt_val) {
                case GT_ERL_PARITY_VAL_NONE:
                    parity = parNone;
                    break;
                case GT_ERL_PARITY_VAL_EVEN:
                    parity = parEven;
                    break;
                case GT_ERL_PARITY_VAL_ODD:
                    parity = parOdd;
                    break;
                case GT_ERL_PARITY_VAL_MARK:
                    parity = parMark;
                    break;
                case GT_ERL_PARITY_VAL_SPACE:
                    parity = parSpace;
                    break;
                default:
                    return 1;
            }
            opts->parity = parity;
            break;
        default:
            return 1;
    }
    return 0;
}

int do_decode_opts_list(char* buf, int* index, size_t buf_len, GtOpts *opts) {
    int arity = 0;
    CHECK_RES_AND_INDEX(ei_decode_list_header(buf, index, &arity), *index, buf_len);
    if (arity == 0) return 0;
    for (int i = 0; i < arity; i++) {
        int tuple_arity = 0;
        CHECK_RES_AND_INDEX(ei_decode_tuple_header(buf, index, &tuple_arity), *index, buf_len);
        if ( tuple_arity != 2) {
            return EINVAL;
        }
        unsigned long opt_key = 0, opt_val = 0;
        CHECK_RES_AND_INDEX( ei_decode_ulong(buf, index, &opt_key), *index, buf_len );
        CHECK_RES_AND_INDEX( ei_decode_ulong(buf, index, &opt_val), *index, buf_len );
        CHECK_RES( do_apply_option(opt_key, opt_val, opts) );
    }
    return do_decode_opts_list(buf, index, buf_len, opts);
}

int decode_opts(char* buf, size_t buf_len, GtOpts *opts) {
    int index = 0, arity = 0;
    CHECK_RES_AND_INDEX( ei_decode_version(buf, &index, NULL), index, buf_len );
    return do_decode_opts_list(buf, &index, buf_len, opts);
}