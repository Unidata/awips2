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
*          1        1/20/2006    Bryon Lawrence    Fixed to manipulate
*                                                  the edit bias flag
*                                                  correctly.
*          1        3/24/2006    Bryon Lawrence    Fixed to correctly 
*                                                  manipulate the radar
*                                                  availability flag.
********************************************************************************
*/

#include <string.h>

#include "DbmsDefs.h"
#include "RWRadarResult.h"
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

/*
   this subroutine writes information about each radar and hour of execution
      to the RWRadarResult table
   if a record already exists, then an update is done

   calling subroutine: runERMosaic
*/

void writeRadarResult(const char * rad, 
                      const char * dt,
                      const int ngag, 
                      const int irad,
                      const double bias_used, 
                      const double mem_span_bias,
                      long int *irc)
{
   char datetime [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char where [ 100 ];
   RWRadarResult radar_struct;
   RWRadarResult * pRWRadarResult = NULL;
       
   /* Initialize the radar id. */
   radar_struct.radid [ RADAR_ID_LEN ]='\0';
   strncpy(radar_struct.radid,rad,RADAR_ID_LEN);

   /* Initialize the obstime. */
   memset(datetime, '\0', ANSI_YEARSEC_TIME_LEN + 1);
   strncpy(datetime,dt,ANSI_YEARSEC_TIME_LEN);
   yearsec_ansi_to_dt(datetime, &radar_struct.obstime); 

   /* Initialize the number of gages used, the bias value used
      and the memory span of this bias. */
   /* Initialize the RWRadarResult structure for update/insertion. */
   radar_struct.num_gages = ngag;
   radar_struct.rw_bias_val_used = bias_used;
   radar_struct.mem_span_used = mem_span_bias;

   /* Initialize the radar availability flag. */
   radar_struct.rad_avail[BOOL_LEN]='\0';
   radar_struct.rad_avail[0]='n';
   if(irad == 1) radar_struct.rad_avail[0]='y';
   if(irad == 2) radar_struct.rad_avail[0]='z';
      
   /* Initialize the ignore radar flag. */
   radar_struct.ignore_radar[BOOL_LEN]='\0';
   radar_struct.ignore_radar[0]='n';

   /* Initialize the edit bias flag. */
   radar_struct.edit_bias[BOOL_LEN]='\0';
   radar_struct.edit_bias[0]='n';

   /* Determine if the record already exists in the RWRadarResult
      table. The primary key is the radar id and the obstime. */
   sprintf ( where, "WHERE radid='%s' AND obstime='%s'", radar_struct.radid, 
                     datetime ); 
   pRWRadarResult = GetRWRadarResult ( where );

   if ( pRWRadarResult != NULL )
   {
      /* A row exists in the RWRadarResult table for the radid
         and obstime. Perform an Update.*/
      radar_struct.edit_bias[0] = pRWRadarResult->edit_bias[0];
      radar_struct.ignore_radar[0] = pRWRadarResult->ignore_radar[0];
      radar_struct.rad_avail[0] = pRWRadarResult->rad_avail[0];
      FreeRWRadarResult ( pRWRadarResult );
      pRWRadarResult = NULL;
      * irc = UpdateRWRadarResult ( & radar_struct, where );
   }
   else
   {
      /* A row does not exist in the RWRadarResult table for the
         radid and obstime. Perform an Insert. */
      * irc = PutRWRadarResult ( & radar_struct ) ;
   }

   return ;
}
