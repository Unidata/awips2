#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "mtr.h"
#include "global_external.h"
/* --------------------------------------------------------------------
  
       FUNCTION
         check_dtg

       PURPOSE  
         To check is a date/time override is set on with the
	 -x switch.  This is used only in the test mode.
	 
       Jan 10 00 DGB
          Original version.
*/
void check_dtg()
{
char buff[15], buff1[5];

      if ( test_.century_flag != 0 && test_.test_flag == 1 )
      {

        memset(buff,0,sizeof(buff));
        if ( strlen(test_.century_string) == 4 )           /* dgb:01/18/98 */
        {
          memset(buff1,0,sizeof(buff1));
          strncpy(buff1,&test_.century_string[0],4);
          buffer_.idate[0] = atoi(buff1);                   /* dgb:01/02/98 */
          printf("\n***NOTE: system year  reset to: %d\n",buffer_.idate[0]);

        }
        else
        if ( strlen(test_.century_string) == 6 )           /* dgb:01/18/98 */
        {
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[0],4);       /* dgb:01/18/98 */
          buffer_.idate[0] = atoi(buff1);                          /* dgb:01/18/98 */   
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[4],2);       /* dgb:01/18/98 */
          buffer_.idate[1] = atoi(buff1);                          /* dgb:01/18/98 */ 
          printf("\n***NOTE: system year  reset to: %d\n",buffer_.idate[0]);
          printf("\n***NOTE: system month reset to: %d\n",buffer_.idate[1]);
         }
        else                                               /* dgb:01/18/98 */
        if ( strlen(test_.century_string) == 8 )           /* dgb:01/18/98 */
        {
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[0],4);       /* dgb:01/18/98 */
          buffer_.idate[0] = atoi(buff1);                          /* dgb:01/18/98 */  
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[4],2);       /* dgb:01/18/98 */
          buffer_.idate[1] = atoi(buff1);                          /* dgb:01/18/98 */ 
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[6],2);       /* dgb:01/18/98 */
          buffer_.idate[2] = atoi(buff1);                          /* dgb:01/18/98 */  
          printf("\n***NOTE: system year  reset to: %d\n",buffer_.idate[0]);
          printf("\n***NOTE: system month reset to: %d\n",buffer_.idate[1]);
          printf("\n***NOTE: system day   reset to: %d\n",buffer_.idate[2]);
        }
        else                                               /* dgb:01/18/98 */
        if ( strlen(test_.century_string) == 10)           /* dgb:01/18/98 */
        {
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[0],4);       /* dgb:01/18/98 */
          buffer_.idate[0] = atoi(buff1);                          /* dgb:01/18/98 */  
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[4],2);       /* dgb:01/18/98 */
          buffer_.idate[1] = atoi(buff1);                          /* dgb:01/18/98 */ 
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[6],2);       /* dgb:01/18/98 */
          buffer_.idate[2] = atoi(buff1);                          /* dgb:01/18/98 */  
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[8],2);       /* dgb:01/18/98 */
          buffer_.idate[1] = atoi(buff1);                          /* dgb:01/18/98 */  
          printf("\n***NOTE: system year  reset to: %d\n",buffer_.idate[0]);
          printf("\n***NOTE: system month reset to: %d\n",buffer_.idate[1]);
          printf("\n***NOTE: system day   reset to: %d\n",buffer_.idate[2]);
          printf("\n***NOTE: system hour  reset to: %d\n",buffer_.idate[3]);
        }
        else                                               /* dgb:01/18/98 */
        if ( strlen(test_.century_string) == 12)           /* dgb:01/18/98 */
        {
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[0],4);       /* dgb:01/18/98 */
          buffer_.idate[0] = atoi(buff1);                          /* dgb:01/18/98 */  
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[4],2);       /* dgb:01/18/98 */
          buffer_.idate[1] = atoi(buff1);                          /* dgb:01/18/98 */ 
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[6],2);       /* dgb:01/18/98 */
          buffer_.idate[2] = atoi(buff1);                          /* dgb:01/18/98 */  
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[8],2);       /* dgb:01/18/98 */
          buffer_.idate[3] = atoi(buff1);                          /* dgb:01/18/98 */  
          memset(buff1,0,sizeof(buff1));                   /* dgb:01/18/98 */
          strncpy(buff1,&test_.century_string[10],2);      /* dgb:01/18/98 */
          buffer_.idate[4] = atoi(buff1);                          /* dgb:01/18/98 */  
          printf("\n***NOTE: system year  reset to: %d\n",buffer_.idate[0]);
          printf("\n***NOTE: system month reset to: %d\n",buffer_.idate[1]);
          printf("\n***NOTE: system day   reset to: %d\n",buffer_.idate[2]);
          printf("\n***NOTE: system hour  reset to: %d\n",buffer_.idate[3]);
          printf("\n***NOTE: system min   reset to: %d\n",buffer_.idate[4]);
       }
       }
 }
