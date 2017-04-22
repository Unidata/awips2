#include<stdio.h>
#include<cstring>

extern "C" char *encrypt_char( char *temp)
{
   int i=0,len=0;

   len = strlen(temp);
   for (i = 0; i<len; i++)
    {
       if (i == 0 )
          if ( (temp[i]=temp[i]-29) == 32 )
                temp[i] = 1;
       if (i == 1 )
          if ( (temp[i]=temp[i]-5) == 32 )
                temp[i] = 1;
       if (i == 2 )
          if ( (temp[i]=temp[i]-8) == 32 )
                temp[i] = 1;
       if (i == 3 )
          if ( (temp[i]=temp[i]-12) == 32 )
                temp[i] = 1;
       if (i == 4)
          if ( (temp[i]=temp[i]-3) == 32 )
                temp[i] = 1;
       if (i == 5)
          if ( (temp[i]=temp[i]-7) == 32 )
                temp[i] = 1;
       if (i == 6)
          if ( (temp[i]=temp[i]-10) == 32 )
                temp[i] = 1;
       if (i == 7)
          if ( (temp[i]=temp[i]-1) == 32 )
                temp[i] = 1;
       if (i == 8)
          if ( (temp[i]=temp[i]-9) == 32 )
                temp[i] = 1;
       if (i == 9)
          if ( (temp[i]=temp[i]-20) == 32 )
                temp[i] = 1;
       if (i == 10)
          if ( (temp[i]=temp[i]-4) == 32 )
                temp[i] = 1;
       if (i == 11)
          if ( (temp[i]=temp[i]-6) == 32 )
                temp[i] = 1;
       if (i == 12)
          if ( (temp[i]=temp[i]-14) == 32 )
                temp[i] = 1;
       if (i == 13)
          if ( (temp[i]=temp[i]-28) == 32 )
                temp[i] = 1;
       if (i == 14)
          if ( (temp[i]=temp[i]-2) == 32 )
                temp[i] = 1;
       if (i == 15)
          if ( (temp[i]=temp[i]-11) == 32 )
                temp[i] = 1;
       if (i == 16)
          if ( (temp[i]=temp[i]-13) == 32 )
                temp[i] = 1;
       if (i == 17)
          if ( (temp[i]=temp[i]-16) == 32 )
                temp[i] = 1;
       if (i == 18)
          if ( (temp[i]=temp[i]-6) == 32 )
                temp[i] = 1;
       if (i == 19)
          if ( (temp[i]=temp[i]-19) == 32 )
                temp[i] = 1;
       if (i == 20)
          if ( (temp[i]=temp[i]-5) == 32 )
                temp[i] = 1;
       if (i == 21)
          if ( (temp[i]=temp[i]-27) == 32 )
                temp[i] = 1;
       if (i == 22)
          if ( (temp[i]=temp[i]-8) == 32 )
                temp[i] = 1;
       if (i == 23)
          if ( (temp[i]=temp[i]-15) == 32 )
                temp[i] = 1;
       if (i == 24)
          if ( (temp[i]=temp[i]-1) == 32 )
                temp[i] = 1;
       if (i == 25)
          if ( (temp[i]=temp[i]-30) == 32 )
                temp[i] = 1;
       if (i == 26)
          if ( (temp[i]=temp[i]-10) == 32 )
                temp[i] = 1;
       if (i == 27)
          if ( (temp[i]=temp[i]-2) == 32 )
                temp[i] = 1;
       if (i == 28)
          if ( (temp[i]=temp[i]-16) == 32 )
                temp[i] = 1;
       if (i == 29)
          if ( (temp[i]=temp[i]-11) == 32 )
                temp[i] = 1;
       if (i == 30)
          if ( (temp[i]=temp[i]-21) == 32 )
                temp[i] = 1;
     }
     temp[i] = '\0';
return temp;
}
