/*******************************************************************************
* FILENAME:           get_day_of_12hr_obs.c
* DESCRIPTION:        Contains the getDayOf12HrObs function.
*                     Using the specified times of the 6 hour
*                     observed temperatures (12z, 18z, 00z, 06z or
*                     18z, 00z, 06z, 12z)  this function returns
*                     the index of the DQC day the 12 hour observation
*                     belongs to.
*
* ORIGINAL AUTHOR:   Bryon Lawrence
* CREATION DATE:     June 11, 2008
* ORGANIZATION:      OHD/HSEB
* MACHINE:           IBM Linux
* MODIFICATION HISTORY:
* DATE         PROGRAMMER        DESCRIPTION/REASON
********************************************************************************
*/
#include "dqc_preproc.h"

int getDayOf12HrObs ( const char * strCurrDate,
                      const dqcInitStruct * pInitStruct,
		      int numDays )
{
   int compareResult;
   int dateIndex;
   int i;

   /**
    * determine the candidate date index value
    */
   dateIndex = -1 ;

   for ( i = 0; i < numDays; i++ )
   {
      /* Compare the YYYY-MM-DD portion of the dates. */
      compareResult = strncmp(strCurrDate, pInitStruct->strDate[i], 10);

      if ( compareResult == 0 )
      {
         dateIndex = i;
         break;
      }
   }

   if ( dateIndex != -1 )
   {

      if ( pInitStruct->dqc_ending_6hour_obstime_flag ==
	     DQC_PREPROCESSOR_ENDING_OBSTIME_06Z )
      {
         dateIndex++;

	 if ( dateIndex == numDays )
	 {
	    dateIndex = -1;
	 }
      }

   }

   return dateIndex;
}
