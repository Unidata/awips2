#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "mtr.h"
#include "global_external.h"
#define  SIZE_1 100

/* --------------------------------------------------------------------
  
       FUNCTION
         extract_smsection

       PURPOSE  
         Parse off the 5 sections of an SM report and place them ib
         buffers.

      VERSION and UPDATES
         1.0    JAN 96   David G. Brandon
                Original Version
         1.1    FEB 25 96 DGB
                Allow for phantom character in function 'strpat'.  
		
         1.2    JAN 16 00 DGB
	        Change argument list for calls to the new strpat.
         1.3    FEB 01 2006 RAE
	         Added function prototype for strpat().
*--------------------------------------------------------------------- */

extern int    VERBOSE, DEBUG;
extern int    AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, METAR, REMARKS; 
extern int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len);


int extract_smsection()
{
   int s111s, s222s,  s333s, s444s, s555s;
   int ptr, len, buff_end;

   memset(sm.s000,0,sizeof(sm.s000));
   memset(sm.s111,0,sizeof(sm.s111));
   memset(sm.s222,0,sizeof(sm.s222));
   memset(sm.s333,0,sizeof(sm.s333));
   memset(sm.s444,0,sizeof(sm.s444));
   memset(sm.s555,0,sizeof(sm.s555));

   s111s = -1;
   s222s = -1;
   s333s = -1;
   s444s = -1;
   s555s = -1;

   buff_end = strlen(databuf_.sdata);

   /* section 1 begins with the 2nd group of numbers */
   if ( (ptr = strpat(databuf_.sdata,"$$$$$ $",'$','%','/',(int)NULL,strlen(databuf_.sdata) )) < 0 ) /*dgb:01/16/00 */
      {
      fprintf(stdout,"\nFATAL ERROR: section 1 group not found in sm report");
      return (-1);
      }

   s111s = ptr + 6;

   /* look for other sections in SM - and starting postion in buffer */
   if ( (ptr = strpat(databuf_.sdata," 222 ",'$','%',(int)NULL,(int)NULL,strlen(databuf_.sdata) )) >= 0 ) /*dgb:01/16/00 */
         s222s = ptr;
   if ( (ptr = strpat(databuf_.sdata," 333 ",'$','%',(int)NULL,(int)NULL,strlen(databuf_.sdata) )) >= 0 ) /*dgb:01/16/00 */
         s333s = ptr;
   if ( (ptr = strpat(databuf_.sdata," 444 ",'$','%',(int)NULL,(int)NULL,strlen(databuf_.sdata) )) >= 0 ) /*dgb:01/16/00 */
         s444s = ptr;
   if ( (ptr = strpat(databuf_.sdata," 555 ",'$','%',(int)NULL,(int)NULL,strlen(databuf_.sdata) )) >= 0 ) /*dgb:01/16/00 */
         s555s = ptr;
   

   /* store section 1 */
   if ( s222s > 0 )
        {
        len = s222s - s111s;
        strncpy(sm.s111,&databuf_.sdata[s111s],len);
        }
   else
   if ( s333s > 0 )
        {
        len = s333s - s111s;
        strncpy(sm.s111,&databuf_.sdata[s111s],len);
        }   
   else
   if ( s444s > 0 )
        {
        len = s444s - s111s;
        strncpy(sm.s111,&databuf_.sdata[s111s],len);
        }   
   else
   if ( s555s > 0 )
        {
        len = s555s - s111s;
        strncpy(sm.s111,&databuf_.sdata[s111s],len);
        }   
   else
        {
        len = buff_end - s111s;
        strncpy(sm.s111,&databuf_.sdata[s111s],len);
        }   

   /* store section 2 */
   if ( s333s > 0 )
        {
        len = s333s - s222s;
        strncpy(sm.s222,&databuf_.sdata[s222s],len);
        }
   else
   if ( s444s > 0 )
        {
        len = s444s - s222s;
        strncpy(sm.s222,&databuf_.sdata[s222s],len);
        }   
   else
   if ( s555s > 0 )
        {
        len = s555s - s222s;
        strncpy(sm.s222,&databuf_.sdata[s222s],len);
        }   
   else
        {
        len = buff_end - s222s;
        strncpy(sm.s222,&databuf_.sdata[s222s],len);
        }   


   /* store section 3 */
   if ( s444s > 0 )
        {
        len = s444s - s333s;
        strncpy(sm.s333,&databuf_.sdata[s333s],len);
        }   
   else
   if ( s555s > 0 )
        {
        len = s555s - s333s;
        strncpy(sm.s333,&databuf_.sdata[s333s],len);
        }   
   else
        {
        len = buff_end - s333s;
        strncpy(sm.s333,&databuf_.sdata[s333s],len);
        }   


   /* store section 4 */
   if ( s555s > 0 )
        {
        len = s555s - s444s;
        strncpy(sm.s444,&databuf_.sdata[s444s],len);
        }   
   else
        {
        len = buff_end - s444s;
        strncpy(sm.s444,&databuf_.sdata[s444s],len);
        }   


   /* store section 5 */
   if ( s555s > 0 )
        {
        len = buff_end - s555s;
        strncpy(sm.s555,&databuf_.sdata[s555s],len);
        }   


return(0);
}


