#include <string.h>
#include "decodedpa.h"

/**************************************************************************
 *							
 *  get_build_num()
 *		
 *  PURPOSE
 *  Determine if DPA product is Build 8, Build 5 or Build 4.  Since the version number of
 *  the DPA product did not change between the builds, the only way to differentiate
 *  between the three is to search for a string which is unique to one of them.
 *  To distinguish between Build 8 and the other formats, the string "ADAP(32" is
 *  searched for.  If the string is found, then the product is Build 8 and the routine
 *  returns 8.  If the string is not found, then the string "BLOCK" is searched for.
 *  If the string is found, then the product is Build 5 and the routine returns 5.
 *  Else the product is Build 4 and the routine returns 4.
 *
 *  At the end of this routine, the file pointer is reset to where it was at the        
 *  beginning of the routine.
 *
 *  NOTES
 *  The formats differ in the supplemental data portion after the rate scan info
 *  as follows:
 *
 *  Build 4 product :                     
 *  (1) hourly accumulation end date
 *  (2) hourly accumulation end time
 *  (3) total no. of isolated bins
 *  (4) total no. of outliers interpolated
 *  (5) total no. of outliers replaced
 *  (6) mean percent area reduction
 *  (7) mean bi-scan ratio
 *  (8) number of bad scans in hour 
 *
 *  
 *  Build 5/Build 8 product :                     
 *  (1) hourly accumulation end date
 *  (2) hourly accumulation end time
 *  (3) total no. of blockage bins rejected
 *  (4) total no. of clutter bins rejected
 *  (5) number of bins smoothed
 *  (6) percent of hybrid scan bins filled
 *  (7) highest elev. angle used in hybscan
 *  (8) total hybrid scan rain area
 *  (9) number of bad scans in hour
 *

 *  The Build 8 product has 32 adaptable parameters while the Build 4 and Build 5
 *  products have 38 parameters.

    calling function: decodeDPA

 ************************************************************************/

short int get_build_num()
             
{

   int n, build_num, numparm;
   long begin_position;
   char ch, prevchar;
   char head[4],str3[3],str4[4];
   
/*--------------------------------------------------*/
/*   store current position of file pointer         */
/*--------------------------------------------------*/

   begin_position = ftell(dpafile);

/*---------------------------------------------*/
/*  search for the string ADAP(32              */
/*  if found, then product is Build 8          */
/*  else product is Build 4 or Build 5         */
/*---------------------------------------------*/

   head[0]='A';
   head[1]='P';
   head[2]='(';
   head[3]='\0';
   prevchar = ' ';

/*----------------------------*/
/*  search for header         */
/*----------------------------*/

   for (;;)
   {
       n = fscanf(dpafile,"%c",&ch);
       if (n == EOF)
       {
         break;
       }

       if (ch == 'D' && prevchar == 'A')
       {
         fgets(str4, 4, dpafile);
         if (strcmp(str4,head) == 0) break;
       }
       prevchar=ch;

    }

/*---------------------------------------------------------*/
/*  if number of parameters = 32, then product is Build 8  */
/*  else continue                                          */
/*---------------------------------------------------------*/

    fgets(str3, 3, dpafile);
    numparm = atoi(str3);

    if(numparm == 32)
    {
          build_num = 8;
    }
    else
    {

       /*------------------------------------------------------------*/
       /*  search for the string "BLOCK" which is part of the string */
       /*    "TOTAL NO. OF BLOCKAGE BINS REJECTED"                   */
       /*------------------------------------------------------------*/

          head[0]='O';
          head[1]='C';
          head[2]='K';
          head[3]='\0';
          prevchar = ' ';

          for (;;)
          {
            n = fscanf(dpafile,"%c",&ch);
            if (n == EOF)
            {
               build_num = 4;
               break;
            }

            if (ch == 'L' && prevchar == 'B')
            {
              fgets(str4, 4, dpafile);
              if (strcmp(str4,head) == 0)
              {
                 build_num = 5;
                 break;
              }
            }
            prevchar=ch;

          }

     }

/*--------------------------------------------------------------------*/
/*  return file pointer to where it was at beginning of this routine  */
/*--------------------------------------------------------------------*/

    fseek(dpafile, begin_position, SEEK_SET);

    return (build_num);
}
