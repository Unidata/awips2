#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "gage_pp_init.h"
#include "gage_pp_write_rec.h"
#include "GeneralUtil.h"
#include "QualityCode.h"
#include "ToolDefs.h"

/**********************************************************
   This subroutine writes QCed gage data to the database

   Developed by Feng Ding (Sept. 2003)

   calling subroutine: read_gage_data(rfcfieldgen)
                       mlts_qc.f(rfcfieldgen)
		       
   return values: 
     irc: sql execution return value
     messageid:
       0 - normal
       4 - Error: Could not update row 
   
*******************************************************/    
void MPEFieldGen_writeGageQC ( const char * lid, 
					const double value,
					const char * ts,
                    const int dur, 
                    const char * dt, 
                    const int qctype, 
                    long int *irc, 
                    int *messageid )
{
   char clid [ LOC_ID_LEN + 1 ];
   char cts [ SHEF_TS_LEN + 1 ];
   char datetime [ ANSI_YEARSEC_TIME_LEN + 1 ];
   char msgstr [ 512 ];
   char obsdate [ OBSYTD + 1 ];
   char offset = '0' ;
   char qc = 'Z';
   double converted_gage_value;
   GagePPoptions options;
   int status ;
   short int gage_value ;

   HourlyPP hourlyPP;
   int hour_slot;
   short int revision [ HOURS_PER_DAY ];
   short int revision6 [ NUM_6HOURLY_SLOTS ];
   WriteInfo write_info;
      
   * messageid = 0;

   /* Set the options structure.  Note that a revision always
      overwrites an existing value in the HourlyPP table. */
   options.shef_duplicate = USE_REVCODE;

   /* Prepare the location id as a C string. */
   memset ( clid, '\0', LOC_ID_LEN + 1 );
   strncpy ( clid, lid, LOC_ID_LEN );
   strip_tblanks( clid );

   /* Prepare the TypeSource as a C string. */
   memset ( cts, '\0', SHEF_TS_LEN + 1 );
   strncpy ( cts, ts, SHEF_TS_LEN );

   /* Prepare the datetime as a C string. */
   memset ( datetime, '\0', ANSI_YEARSEC_TIME_LEN + 1 );
   strncpy ( datetime, dt, ANSI_YEARSEC_TIME_LEN );

   /* Convert the gage value to the expected units... inches * 100. */
   converted_gage_value = round ( ( value / 25.4 ) * 100.0 );
   gage_value = ( short ) converted_gage_value;

   /* Test the QC type. A type of 1 means that the gage value 
      failed the SCC check. A value of 2 means that the gage value
      failed the MSC check. Any other value is ignored. */
   if ( ( qctype  == 1 ) || ( qctype == 2 ) )
   {
      if ( qctype == 1 )
      {
         /* QC_SCC_FAILED */
         qc = 'C';
      }
      else if ( qctype == 2 )
      {
         /* QC_MSC_FAILED */
         qc = 'L';
      }

      sprintf ( msgstr, "In write_gageqc: "
		        "%s %s %d %c\n", clid, cts, gage_value, qc ) ;

      /* Initialize the HourlyPP structure. */
      hour_slot = gage_pp_init ( & hourlyPP,
                                 datetime,
                                 clid,
                                 cts,
                                 gage_value,
                                 obsdate,
                                 offset,
                                 qc );

      /* Set the revision flag. */
      revision [ hour_slot - 1 ] = 1;

      /* Insert or update to the HourlyPP table. */
      status = gage_pp_write_rec ( & hourlyPP, 
                                   & write_info, 
                                   msgstr,
                                   "PP", 
                                   obsdate, 
                                   & options,
                                   revision, 
                                   revision6,
                                   1 );

      if ( status != GPP_OK )
      {
         * messageid = 4;
      }
   }

   return ;

} /* end MPEFieldGen_writeGageQC */
