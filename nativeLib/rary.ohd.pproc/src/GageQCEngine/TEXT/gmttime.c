#include <time.h>

/*******************************************************************************
* FILENAME:             gmttime.c
* GENERAL INFORMATION:
* DESCRIPTION:          Returns the broken down GMT time of a given ticks
*                       value.  Ticks are the number of seconds that 
*                       have elapsed since  Midnight, Jan 1, 1970.
*
* ORIGINAL AUTHOR:     Craig Peterson
* CREATION DATE:       ????
* ORGANIZATION:        CBRFC
* MACHINE:             Linux
* MODIFICATION HISTORY: 
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        10/3/2006    B. Lawrence       Added documentation.
*                                                  Incorporated into
*                                                  MPE Editor.
********************************************************************************
*/

/*******************************************************************************
* MODULE NAME:   gmttime
* PURPOSE:       Converts a Unix ticks time into a broken down GMT time.
*                The result is stored in a tm structure.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  time_t *    secs                 Time expressed in UNIX ticks.  This
*                                           is the number of seconds which
*                                           have elapsed since 
*                                           Midnight, Jan 1, 1970.
*    
* RETURNS:
*   DATA TYPE                               DESCRIPTION
*   struct tm *                             The broken down GMT time.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME                         DESCRIPTION
*   struct tm *   gmtim                        Points to broken down time
*                                              returned by gmtime function.
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   Returns a NULL pointer if an error is encountered during the time
*   conversion.
********************************************************************************
*/
struct tm* gmttime(time_t *secs)

{
   struct tm *gmtim = NULL;

   gmtim=gmtime(secs);
   return gmtim;
}
     
