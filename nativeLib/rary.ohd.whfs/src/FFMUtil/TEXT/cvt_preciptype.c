#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"

#include "ArealProductSettings.h"


/*******************************************************************
   cvt_preciptype.c	
   
   PURPOSE
   Converts the preciptype enumerated variable into a
   the designated typesource string corresponding
   to the class of data.
   
   *****************************************************************/

int cvt_preciptype_to_ts(PrecipType	precipType,
                         char		*ts)
{

   int status;
   
   
   /* initialize */
   
   memset(ts, 0, SHEF_TS_LEN + 1);
   status = 1;
   
   
   /* now set the value */
   
   if (precipType == STAGE1_PRECIP)
      strcpy(ts, "PR");
      
   else if (precipType == STAGE2_GAGE_ONLY_PRECIP)
      strcpy(ts, "PG");
       
   else if (precipType == STAGE2_GAGE_RADAR_PRECIP)
      strcpy(ts, "PS");
      
   else
      status = -1;
   
   return(status);
}
