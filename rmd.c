#include <stdint.h>
#include <stdio.h>
#include "rmd160.h"

int main()
{
    byte data[32] = {0,};
    dword mdbuf[5] = {0,};;
    unsigned i, j;
    data[0] = 'a';

    MDinit(mdbuf);
    MDfinish(mdbuf, data, 1, 0);

    for (i = 0; i < 5; ++i)
    {
        for (j = 0; j < 4; ++j)
        {
            printf("%02x ", *(((byte*)&mdbuf[i]) + j));
        }
        printf("\n");
    }

    return 0;
}

