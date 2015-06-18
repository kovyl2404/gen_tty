#include <stddef.h>
#include <termios.h>
#include <stdlib.h>
#include "baud_rate.h"


struct _speed {
    unsigned int real_speed;
    unsigned int flag;
} static _baud_rates[] = {
        {50,   B50},    {75,    B75},     {110,   B110},
        {134,  B134},   {150,   B150},    {200,   B200},
        {300,  B300},   {600,   B600},    {1200,  B1200},
        {1800, B1800},  {2400,  B2400},   {4800,  B4800},
        {9600, B9600},  {19200, B19200},  {38400, B38400}
};

#define SPEED_ENTRY_SIZE (sizeof(_baud_rates[0]))
#define SPEED_ENTRIES_COUNT (sizeof(_baud_rates)/sizeof(_baud_rates[0]))

static int speed_comparator(const void* e1, const void* e2) {
    struct _speed* entry1 = (struct _speed*)e1;
    struct _speed* entry2 = (struct _speed*)e2;
    return (int)(entry1->real_speed) - (int)(entry2->real_speed);
}

unsigned int speed_to_flag(unsigned int real_speed) {
    void* res = bsearch(&real_speed, _baud_rates, SPEED_ENTRIES_COUNT, SPEED_ENTRY_SIZE, speed_comparator );
    if ( res != NULL ) {
        return ((struct _speed*)res)->flag;
    }
    return 0;
}

unsigned int flag_to_speed(unsigned int flag) {
    for ( int i=0; i<SPEED_ENTRIES_COUNT; i++) {
        if ( _baud_rates[i].flag == flag ) {
            return _baud_rates[i].real_speed;
        }
    }
    return 0;
}
