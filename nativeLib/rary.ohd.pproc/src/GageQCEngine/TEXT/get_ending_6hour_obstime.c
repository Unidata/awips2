#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"

/*******************************************************************************
* MODULE NAME:  getEnding6HourObsTime
* PURPOSE:      Returns the value of token dqc_ending_6hour_obstime.
*               For the instantaneous observed data elements
*               (6 hour temperature and 6 hour freezing level)
*               this token indicates the time of the last observed
*               data element in a DailyQC day.  The DailyQC
*               day spans a 24 hour period from 12z-12z.
*               The time of the last observed 6 hour
*               temperature or freezing level value may either
*               be 06z or 12z.  By default it is 06z, meaning that
*               the four 6 hour values are 12z, 18z, 00z, 06z.
*               It can be set to 12z, meaning that the four
*               6 hour values are 18z, 00z, 06z, and 12z.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   None
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         Ending 6 hour obs time.
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*   Requires a value for the token dqc_ending_6hour_obstime.  This may
*   be set as an environment variable or it may be set in an
*   .Apps_defaults file.
*
* ERROR HANDLING:
*  DESCRIPTION
*  Errors resulting from an undefined or misdefined token
*  will cause the ending obs value to default to 6.
********************************************************************************
*/

int getEnding6HourObsTime()
{
   /* Static Variables */
   static const char * tokenName = "dqc_ending_6hour_obstime";
   static int value = DEFAULT_ENDING_6HOUR_OBS_TIME;
   static int first = 1;

   /* Automatic Variables */
   char tokenValue[20];
   char * endptr = NULL;
   int length;

   if ( first == 1 )
   {
      length = strlen(tokenName);
      get_apps_defaults( (char * ) tokenName,
                         &length,
			 tokenValue,
			 &length );

      /* If the token is defined, attempt
         to process its value. */
      if ( length > 0 )
      {
         errno = 0;
         value = (int) strtol(tokenValue, &endptr, 10);

         if ( ( errno != 0 ) || ( ( value != 6 ) &&
	      ( value != 12 ) ) )
	 {
	     value = DEFAULT_ENDING_6HOUR_OBS_TIME;
	 }
      }

      first = 0;
   }

   return value;
}
