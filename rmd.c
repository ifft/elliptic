#include <stdio.h>
#include <stdint.h>
#include "rmd160.h"

int main()
{
    uint32_t MDbuf[5];
    uint8_t  data[64] = {0,};
    MDinit(MDbuf);
    data[0] = 'a';
    MDfinish(MDbuf, data, 1, 0);
    for (unsigned outer = 0; outer < 64 / 4; ++outer)
    {
        for (unsigned i = 0; i < 4; ++i)
        {
            printf("%02x ", data[outer * 4 + i]);
        }

        printf("\n");
    }
    printf("end of main data[0] = %02x\n", data[0]);
    printf("end of main data[1] = %02x\n", data[1]);
    printf("result:\n");
    for (unsigned outer = 0; outer < 5; ++outer)
    {
        printf("%x ", MDbuf[outer]);

        printf("\n");
    }

    return 0;
}    

