#ifndef GEN_TTY_DRV_BAUD_RATE_H
#define GEN_TTY_DRV_BAUD_RATE_H

unsigned int speed_to_flag(unsigned int real_speed);
unsigned int flag_to_speed(unsigned int speed_flag);

#endif
