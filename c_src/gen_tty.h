#ifndef GEN_TTY_DRV_GEN_TTY_H
#define GEN_TTY_DRV_GEN_TTY_H

typedef struct __gt_file* GtFileP;

typedef enum {
    parEven     = 0,
    parOdd      = 1,
    parSpace    = 2,
    parMark     = 3,
    parNone     = 4
} GtParity;

typedef enum {
    cs5 = 5,
    cs6 = 6,
    cs7 = 7,
    cs8 = 8
} GtCharSize;

typedef struct {
    unsigned int baud_rate;
    GtCharSize char_size;
    GtParity parity;
} GtOpts;

int gt_open(char* const file_name, GtFileP* output);
int gt_close(GtFileP file);

int gt_setopts(GtFileP file, GtOpts* const opts);
int gt_getopts(GtFileP file, GtOpts* opts);

#endif
