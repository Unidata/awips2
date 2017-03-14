#include <string.h>
#include "DPARadar.h"
#include "decodedpa.h"
#include "time_convert.h"
#include "GeneralUtil.h"

/*------------------------------------------------------*/
/*   routine for checking if product is within window   */
/*        around the top of the hour  AND  if current   */
/*        product is closer than a previous product     */
/*        deemed to be top-of-hour                      */
/*                                                      */
/*   if current product and previous product are same   */
/*    number of minutes off the top-of-hour, then       */
/*    current product is considered closer              */
/*                                                      */
/*   if window is set to 10 (minutes), then products    */
/*    with minutes 00 - 10 (inclusive) and 50 - 00 (inclusive)  */
/*    are considered to be "within the window"          */
/*                                                      */
/*   this routine is executed for decoding only         */
/*                                                      */
/*   calling function: decodeDPA                        */
/*------------------------------------------------------*/

void top_hour_check(int window, char strdt[22], short minoff, int *ret_code)
{

   int i, ihr, ihre, ihrl, ib, aminoff;
   char where[100], str1[22], strdte[22], strdtl[22], chr[3];

   time_t jtime;

   DPARadar *formHead;

   *ret_code = 0;

/*------------------------------------------------------*/
/*  check minoff against window                         */
/*  aminoff = abs value of minoff                       */
/*------------------------------------------------------*/

   aminoff = minoff;
   if(aminoff < 0) aminoff = -1 * aminoff;

   if(aminoff <= window)
   {

      /*----------------------------------------------------------*/
      /*  calculate hours of interest for search                  */
      /*  ihre = earlier hour of interest                         */
      /*  strdte = datetime for earlier hour                      */
      /*  ihrl = later   hour of interest                         */
      /*  strdtl = datetime for later hour                        */
      /*  ihr  = top-of-hour hour                                 */
      /*                                                          */
      /*  if hour = 0, then date of earlier hour is previous day  */
      /*  if hour = 23, then date of later hour is next day       */
      /*----------------------------------------------------------*/

         sprintf(chr,"%c%c",strdt[11],strdt[12]);
         ihr = atoi(chr);
         strcpy(strdtl,strdt);
         strcpy(strdte,strdt);
         
         if(minoff >= 0) 
         {
            ihrl = ihr;
            ihre = ihrl - 1;
            if(ihre == -1)
            {
               ihre = 23;

               yearsec_ansi_to_timet(strdt,&jtime);
               jtime = jtime - 86400;
               timet_to_yearsec_ansi(jtime,strdte);
            }
         }
         else
         {
            ihre = ihr;
            ihrl = ihre + 1;
            if(ihrl == 24)
            {
               ihrl = 0;

               yearsec_ansi_to_timet(strdt,&jtime);
               jtime = jtime + 86400;
               timet_to_yearsec_ansi(jtime,strdtl);
            }
         }

      /*----------------------------------------------------------------------*/
      /*   check for previous product at top-of-hour                          */
      /*   for 2350z, 2351z, 2352z etc products, must check for 00z products  */
      /*    with tomorrow's date                                              */
      /*----------------------------------------------------------------------*/

         if(ihrl == 0)
         {
            sprintf(str1,"%c%c%c%c-%c%c-%c%c 00:00:00",
             strdtl[0],strdtl[1],strdtl[2],strdtl[3],strdtl[5],strdtl[6],strdtl[8],strdtl[9]);
         }
         else
         {
            sprintf(str1,"%c%c%c%c-%c%c-%c%c %02d:00:00",
             strdt[0],strdt[1],strdt[2],strdt[3],strdt[5],strdt[6],strdt[8],strdt[9],
             ihrl);
         }

         sprintf(where,"WHERE radid = '%s' AND obstime = '%s'",radid,str1);
         if((formHead = GetDPARadar(where)))
         {

            *ret_code = 3;
            FreeDPARadar(formHead);
            return;
         }

         FreeDPARadar(formHead);

      /*------------------------------------------------------*/
      /*   check for previous product within window around    */
      /*     top-of-hour                                      */
      /*------------------------------------------------------*/

         for(i=1; i<aminoff+1; i++)
         {
            sprintf(str1,"%c%c%c%c-%c%c-%c%c %02d:%02d:00",
             strdtl[0],strdtl[1],strdtl[2],strdtl[3],strdtl[5],strdtl[6],strdtl[8],strdtl[9],
             ihrl,i);

            sprintf(where,"WHERE radid = '%s' AND obstime = '%s'",radid,str1);
            if((formHead = GetDPARadar(where)))
            {

               *ret_code = 2;
               if(i == aminoff) *ret_code = 0;   /* curr product and prev prod have same minoff  */
               FreeDPARadar(formHead);
               return;

            }


            ib = 60 - i;
            sprintf(str1,"%c%c%c%c-%c%c-%c%c %02d:%02d:00",
             strdte[0],strdte[1],strdte[2],strdte[3],strdte[5],strdte[6],strdte[8],strdte[9],
             ihre,ib);

            sprintf(where,"WHERE radid = '%s' AND obstime = '%s'",radid,str1);
            if((formHead = GetDPARadar(where)))
            {

               *ret_code = 2;
               if(i == aminoff) *ret_code = 0;   /* curr product and prev prod have same minoff  */
               FreeDPARadar(formHead);
               return;

            }
           
         }  /* end for(i= ...) */

      /*------------------------------------------------------*/
      /*   no previous product found within window around     */
      /*     top-of-hour                                      */
      /*------------------------------------------------------*/

               *ret_code = 0;

    }
    
    else
    {
       *ret_code = 1;
    }

}
