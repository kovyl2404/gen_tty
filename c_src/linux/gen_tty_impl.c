
#include <erl_driver.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#define __USE_MISC
#include <termios.h>
#include <strings.h>
#include <stdio.h>

#include "../gen_tty.h"
#include "baud_rate.h"

GtCharSize flag_to_char_size(unsigned int flag);
unsigned int char_size_to_flag(GtCharSize size);
GtParity flag_to_parity(unsigned int flag);
unsigned int parity_to_flag(GtParity parity);

typedef struct __gt_file {
    int fd;
    struct termios oldtio;
} GtSerialFile;

int gt_open(char* const file_name, GtFileP* output) {
    int fd = open(file_name, O_RDWR | O_NOCTTY);
    if (fd < 0) {
        return errno;
    }
    if (! isatty(fd)) {
        close(fd);
        return ENOTTY;
    }
    GtFileP file = (GtFileP)driver_alloc(sizeof(GtSerialFile));
    file->fd=fd;
    tcgetattr(fd, &file->oldtio);
    *output = file;
    return 0;
}

int gt_close(GtFileP file) {
    tcsetattr(file->fd, TCSADRAIN, &file->oldtio);
    int ret = close(file->fd);
    driver_free(file);
    return ret;
}

int gt_getopts(GtFileP file, GtOpts* opts) {
    struct termios curtio;
    tcgetattr(file->fd, &curtio);
    unsigned int real_speed = flag_to_speed(cfgetispeed(&curtio));
    if (real_speed == 0) {
        return EINVAL;
    }
    opts->baud_rate = real_speed;
    opts->char_size = flag_to_char_size( curtio.c_cflag );
    opts->parity = flag_to_parity( curtio.c_cflag );
    return 0;
}

int gt_setopts(GtFileP file, GtOpts* opts) {
    struct termios newtio;
    bzero(&newtio, sizeof(struct termios));
    newtio.c_cflag |= CREAD;
    newtio.c_cflag |= parity_to_flag(opts->parity);
    newtio.c_cflag |= char_size_to_flag(opts->char_size);
    unsigned int speed_flag = speed_to_flag(opts->baud_rate);
    if ( speed_flag == 0 ) {
        return EINVAL;
    }
    cfsetispeed(&newtio, speed_flag);
    cfsetospeed(&newtio, speed_flag);
    return tcsetattr(file->fd, TCSANOW, &newtio);
}

GtCharSize flag_to_char_size(unsigned int flag) {
    switch (flag & CSIZE) {
        case CS5:
            return cs5;
        case CS6:
            return cs6;
        case CS7:
            return cs7;
        case CS8:
            return cs8;
    }
    assert(false);
}

unsigned int char_size_to_flag(GtCharSize parity) {
    switch (parity) {
        case cs5: return CS5;
        case cs6: return CS6;
        case cs7: return CS7;
        case cs8: return CS8;
    }
    assert(false);
}

GtParity flag_to_parity(unsigned int flag) {
    if ( (flag & PARENB) == 0 ) {
        return parNone;
    }
    GtParity ret = 0;
    if ( flag & PARODD ) ret |= parOdd;
    if ( flag & CMSPAR ) ret |= parSpace;
    return ret;
}

unsigned int parity_to_flag(GtParity parity) {
    switch (parity) {
        case parNone:
            return 0;
        case parEven:
            return PARENB;
        case parOdd:
            return PARENB | PARODD;
        case parMark:
            return PARENB | CMSPAR;
        case parSpace:
            return PARENB | CMSPAR | PARODD;
    }
    assert(false);
}
