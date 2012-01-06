/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        12/17/2004   Bryon Lawrence    Initial Coding
*          2        12/22/2004   Bryon Lawrence    Fixed module testing bug.
*          1        10/03/2007   Bryon Lawrence    Updated to initialize the
*                                                  sixhrqc and sixhroffset
*                                                  fields in the HourlyPP
*                                                  structure.
********************************************************************************
*/
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "gage_pp_write_rec.h"
#include "time_convert.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
int gage_pp_init ( HourlyPP * pHourlyPP,
                   const char datetime [ ANSI_YEARSEC_TIME_LEN + 1 ],
                   const char lid [ LOC_ID_LEN + 1 ],
                   const char ts [ SHEF_TS_LEN + 1 ],
                   short int value,
                   char obsdate [ OBSYTD + 1 ],
                   char minute_offset ,
                   char qc )
{
   char hour [ HOURSIZ + 1 ];
   char datetime_temp [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char * pChar = NULL;
   int hour_slot;
   int i;
   int status;
   short int null_value;
   time_t obstime;

   /* Make a local copy of the datetime variable. */
   strcpy ( datetime_temp, datetime );

   /* Prepare an HourlyPP structure for writing to the HourlyPP */
   memset ( pHourlyPP , 0 , sizeof ( HourlyPP ) ) ;
   memset ( pHourlyPP->hourly_qc , '-' , NUM_HOURLY_SLOTS ) ;
   pHourlyPP->hourly_qc [ NUM_HOURLY_SLOTS ] = '\0' ;
   memset ( pHourlyPP->minute_offset , '-' , NUM_HOURLY_SLOTS ) ;
   pHourlyPP->minute_offset [ NUM_HOURLY_SLOTS ] = '\0' ;
   memset ( pHourlyPP->sixhrqc , '-' , NUM_6HOURLY_SLOTS ) ;
   pHourlyPP->sixhrqc [ NUM_6HOURLY_SLOTS ] = '\0' ;
   memset ( pHourlyPP->sixhroffset , '-' , NUM_6HOURLY_SLOTS ) ;
   pHourlyPP->sixhroffset [ NUM_6HOURLY_SLOTS ] = '\0' ;

   /* Initialize the lid and ts members. */
   memset ( pHourlyPP->lid, '\0', LOC_ID_LEN + 1 );
   strncpy ( pHourlyPP->lid, lid, LOC_ID_LEN ); 
   memset ( pHourlyPP->ts, '\0', SHEF_TS_LEN + 1 );
   strncpy ( pHourlyPP->ts, ts, SHEF_TS_LEN );

   SetNull ( SHORT, & null_value );

   for ( i = 0 ; i < NUM_HOURLY_SLOTS ; ++ i )
   {
      set_hour_slot_value ( pHourlyPP, i + 1, null_value );
   }

   for ( i = 0; i < NUM_6HOURLY_SLOTS; ++i )
   {
      set_6hour_slot_value ( pHourlyPP, i + 1, null_value );
   }

   pChar = strchr ( datetime_temp, ' ' );
   ++ pChar ;
   memset ( hour, '\0', HOURSIZ + 1 );
   strncpy ( hour, pChar, HOURSIZ );
   hour_slot = atoi ( hour ) ;

   if ( hour_slot == 0 )
   {
       hour_slot = 24 ;
       status = yearsec_ansi_to_timet ( datetime_temp, & obstime ) ;
       obstime -= SECONDS_PER_HOUR ;
       status = timet_to_yearsec_ansi ( obstime, datetime_temp ) ;
   }

   /* Top of the hour. */
   pHourlyPP->minute_offset [ hour_slot - 1 ] = minute_offset ;
   pHourlyPP->hourly_qc [ hour_slot - 1 ] = qc ;

   memset ( obsdate, '\0', OBSYTD + 1 );
   strncpy ( obsdate , datetime_temp , OBSYTD ) ;
   status = ansi_date_to_date_t ( obsdate , & ( pHourlyPP->obsdate ) ) ;

   /* Set value. */
   set_hour_slot_value ( pHourlyPP, hour_slot, value ) ;

   return hour_slot;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
