#include<stdio.h>
#include<cstring>

extern "C" char *decrypt_char( char *temp)
    {
    int i=0,len=0;

    len = strlen(temp);

    for (i = 0; i<len; i++)
        {
        if ( temp[i] == 1 )
            temp[i] = 32;

        if (i == 0 )
            temp[i] = temp[i] + 29;
        if (i == 1 )
            temp[i] = temp[i] + 5;
        if (i == 2 )
            temp[i] = temp[i] + 8;
        if (i == 3 )
            temp[i] = temp[i] + 12;
        if (i == 4)
            temp[i] = temp[i] + 3;
        if (i == 5)
            temp[i] = temp[i] + 7;
        if (i == 6)
            temp[i] = temp[i] + 10;
        if (i == 7)
            temp[i] = temp[i] + 1;
        if (i == 8)
            temp[i] = temp[i] +9;
        if (i == 9)
            temp[i] = temp[i] + 20;
        if (i == 10)
            temp[i] = temp[i] + 4;
        if (i == 11)
            temp[i] = temp[i] + 6;
        if (i == 12)
            temp[i] = temp[i] + 14;
        if (i == 13)
            temp[i] = temp[i] + 28;
        if (i == 14)
            temp[i] = temp[i] + 2;
        if (i == 15)
            temp[i] = temp[i] + 11;
        if (i == 16)
            temp[i] = temp[i] + 13;
        if (i == 17)
            temp[i] = temp[i] + 16;
        if (i == 18)
            temp[i] = temp[i] + 6;
        if (i == 19)
            temp[i] = temp[i] + 19;
        if (i == 20)
            temp[i] = temp[i] + 5;
        if (i == 21)
            temp[i] = temp[i] + 27;
        if (i == 22)
            temp[i] = temp[i] + 8;
        if (i == 23)
            temp[i] = temp[i] + 15;
        if (i == 24)
            temp[i] = temp[i] + 1;
        if (i == 25)
            temp[i] = temp[i] + 30;
        if (i == 26)
            temp[i] = temp[i] + 10;
        if (i == 27)
            temp[i] = temp[i] + 2;
        if (i == 28)
            temp[i] = temp[i] + 16;
        if (i == 29)
            temp[i] = temp[i] + 11;
        if (i == 30)
            temp[i] = temp[i] + 21;
        }
    temp[i] = '\0';

    return temp;
    }
