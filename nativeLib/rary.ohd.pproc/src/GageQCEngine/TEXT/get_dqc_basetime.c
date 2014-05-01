/*******************************************************************************
* FILENAME:           getDqcBasetime
* DESCRIPTION:        Retrieves the value of the dqc_preprocessor_basetime
*                     token.
*
* ORIGINAL AUTHOR:    Bryon Lawrence
* CREATION DATE:      January 3, 2008
* ORGANIZATION:       OHD/HSEB
* MACHINE:            Linux
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     1/3/2008     Bryon Lawrence    First Coding
********************************************************************************
*/
#include <string.h>

#include "GeneralUtil.h"

/*******************************************************************************
* MODULE NAME:  getDqcBasetime
* PURPOSE:      Returns the base hour of the DailyQC day.  This is the beginning
*               hour of the 24 hours which define the DailyQC day.
*
* ARGUMENTS:
*   None. 
*   
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         baseTime                    This is a value which is either 0,6,12, or 18.
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*   getAppsDefaults                         GeneralUtils.h
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   char *     dqcPreprocessorBasetime      Contains the name of the
*                                           dqc_preprocessor_basetime token.
*   char       tokenValue []                Contains the value of the
*                                           dqc_preprocessor_basetime token.
*   int        baseTime                     The basetime. A value of 0,6,12,
*                                           or 18.
*   int        tokenValueLength             The length of the value of the
*                                           dqc_preprocessor_basetime token.
*   int        returnStatus                 The successes/failure code 
*                                           returned by get_apps_defaults.
*   int        tokenLength                  The length of the 
*                                           dqc_preprocessor_basetime token.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*    If the value of the token dqc_preprocessor_basetime is not in the set
*    {0,6,12,18}, then the return value of this function defaults to 12. 
*    
********************************************************************************
*/
int getDqcBasetime ( )
{
   static char * dqcPreprocessorBasetime = "dqc_preprocessor_basetime";
   static char tokenValue [50];
   static int baseTime = 12;
   static int first = 1;
   int tokenValueLength;
   int returnStatus;
   int tokenLength;

   if ( first == 1 )
   {
      memset ( tokenValue, '\0', 50 );
      tokenLength = strlen ( dqcPreprocessorBasetime );

      returnStatus = get_apps_defaults ( dqcPreprocessorBasetime, 
                                         &tokenLength,
                                         tokenValue,
                                         &tokenValueLength );

      if ( ( returnStatus == 0 ) && ( tokenValueLength > 0 ) )
      {
         if ((strcasecmp (tokenValue, "18Z")) == 0)
         {
            baseTime = 18;
         }
         else if ((strcasecmp (tokenValue, "06Z")) == 0)
         {
            baseTime = 6;
         }
         else if ((strcasecmp (tokenValue, "00Z")) == 0)
         {
            baseTime = 0;
         }
      }

      first = 0;
   }

   return baseTime;
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
