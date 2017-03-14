#include <string.h>   
#include "decodedpa.h"

int getadapt(int *numparm, float params[37], char param38[2])
{

/*
    function to read adaptable parameters from raw DPA products

    the adaptable parameters are found by searching for the header of
     the form "ADAP(nn)" where nn = 38 for Build 4,5 
                                  = 32 for Build 8

    ORPG Build 4 and 5 have the same number of adaptable parameters
    parameters numbers 1 - 10 and 15 have different definitions in Bld 4/5

    ORPG Build 8 has dropped the following parameters:
       max storm speed
       threshold max time difference
       minimum area time continuity test
       time continuity parameter #1
       time continuity parameter #2
       max rate of echo area change

    calling function: decodeDPA

*/

   int n;
   char ch, prevchar;
   char head[4],str4[4],str3[3],str9[9],blank8[9],Tstring[9],Fstring[9];

   head[0]='A';
   head[1]='P';
   head[2]='(';
   head[3]='\0';
   prevchar = ' ';
   blank8[8]='\0';
   Tstring[8]='\0';
   Fstring[8]='\0';
   param38[1] = '\0';
   strcpy(blank8,"        ");
   strcpy(Tstring,"       T");
   strcpy(Fstring,"       F");

/*----------------------------*/
/*  search for header         */
/*----------------------------*/

   for (;;)
   {
     n = fscanf(dpafile,"%c",&ch);
     if (n == EOF)
     {
       return 1;
     }

     if (ch == 'D' && prevchar == 'A')
     {
       fgets(str4, 4, dpafile);
       if (strcmp(str4,head) == 0) break;
     }
     prevchar=ch;

    }

/*-----------------------------*/
/*  read number of parameters  */
/*-----------------------------*/

    fgets(str3, 3, dpafile);
    *numparm = atoi(str3);

/*-----------------------------*/
/*  read past ')'              */
/*-----------------------------*/

    fseek(dpafile, 1L, SEEK_CUR);

/*---------------------------------------------*/
/*  read parameters                            */
/*                                             */
/*  all parameters are floats except for last  */
/*  parameter which is char[1]                 */
/*---------------------------------------------*/

    for(n=0; n < *numparm; n++)
    {
      if(fgets(str9, 9, dpafile) == NULL)
      {
        printf("EOF encountered before completion of read of adaptable parameters");
        printf(" -- last parameter read was number %d\n",n);
        return 1;
      }
      if(strcmp(blank8,str9) == 0)
         params[n] = -99.;
      else if(strcmp(Tstring,str9) == 0)
         strcpy(param38,"T");
      else if(strcmp(Fstring,str9) == 0)
         strcpy(param38,"F");
      else
         params[n] = atof(str9);
    }

    return 0;

}
