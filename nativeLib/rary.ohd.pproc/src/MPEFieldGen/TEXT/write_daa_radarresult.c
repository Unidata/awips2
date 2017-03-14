#include <string.h>

#include "DbmsDefs.h"
#include "DAARadarResult.h"
#include "time_convert.h"

/*
   this routine writes information about each radar and hour of execution
      to the DAARadarResult table
   if a record already exists, then an update is done

   calling subroutine: runRDMosaic
*/

void writeDAARadarResult(const char * rad, 
                      const char * dt,
                      const int ngag, 
                      const int irad,
                      const double bias_used, 
                      const double mem_span_bias,
                      long int *irc)
{
   char datetime [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char where [ 100 ];
   DAARadarResult radar_struct;
   DAARadarResult * pDAARadarResult = NULL;
       
   /* Initialize the radar id. */
   radar_struct.radid [ RADAR_ID_LEN ]='\0';
   strncpy(radar_struct.radid,rad,RADAR_ID_LEN);

   /* Initialize the obstime. */
   memset(datetime, '\0', ANSI_YEARSEC_TIME_LEN + 1);
   strncpy(datetime,dt,ANSI_YEARSEC_TIME_LEN);
   yearsec_ansi_to_dt(datetime, &radar_struct.obstime); 

   /* Initialize the number of gages used, the bias value used
      and the memory span of this bias. */
   /* Initialize the DAARadarResult structure for update/insertion. */
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

   /* Determine if the record already exists in the DAARadarResult
      table. The primary key is the radar id and the obstime. */
   sprintf ( where, "WHERE radid='%s' AND obstime='%s'", radar_struct.radid, 
                     datetime ); 
   pDAARadarResult = GetDAARadarResult ( where );

   if ( pDAARadarResult != NULL )
   {
      /* A row exists in the DAARadarResult table for the radid
         and obstime. Perform an Update.*/
      radar_struct.edit_bias[0] = pDAARadarResult->edit_bias[0];
      radar_struct.ignore_radar[0] = pDAARadarResult->ignore_radar[0];
      /*radar_struct.rad_avail[0] = pDAARadarResult->rad_avail[0]; */
      FreeDAARadarResult ( pDAARadarResult );
      pDAARadarResult = NULL;
      * irc = UpdateDAARadarResult ( & radar_struct, where );
   }
   else
   {
      /* A row does not exist in the DAARadarResult table for the
         radid and obstime. Perform an Insert. */
      * irc = PutDAARadarResult ( & radar_struct ) ;
   }

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/write_daa_radarresult.c,v $";
 static char rcs_id2[] = "$Id: write_daa_radarresult.c,v 1.1 2012/04/25 16:04:15 pst Exp $";}
/*  ===================================================  */

}
