#include <string.h>
#include <float.h>

#include "DbmsDefs.h"
#include "DAABiasDyn.h"
#include "time_convert.h"
#include "mpe_fieldgen.h"

void write_daabiasdyn(const char *rad,
                     const char *site_id,
					const char * dt,
					const int num_span,
					double * num_pairs,
					double * sumgag, 
					double * sumrad,
					double * bb,
                    long int *irc)

{
   /*
      This function writes the state variables to the DAABiasDyn table
      calling function: calculateMeanBiasDP
   */
      
   char strdt[ANSI_YEARSEC_TIME_LEN + 1 ];
   int i;
   DAABiasDyn bias_struct;
      
   *irc = 0;

   /* Initialize the DAABiasDyn structure for insert/update. */

   /* Initialize the radar identifier. */
   bias_struct.radid[RADAR_ID_LEN]='\0';
   strncpy ( bias_struct.radid, rad, RADAR_ID_LEN );

   /* Initialize the office identifier. */
   bias_struct.office_id[WFO_LEN]='\0';
   strncpy ( bias_struct.office_id, site_id, WFO_LEN );

   /* Initialize the obstime. */
   memset ( strdt, '\0', ANSI_YEARSEC_TIME_LEN + 1 );
   strncpy ( strdt, dt, ANSI_YEARSEC_TIME_LEN );
   yearsec_ansi_to_dt ( strdt, & bias_struct.obstime );

   /* Begin the loop over the memory span indexes. */
   for ( i = 0; i < num_span; ++ i )
   {
      bias_struct.memspan_ind = (short)i;
      bias_struct.numpairs = num_pairs[ i ];
      bias_struct.sumgag = (float)sumgag[ i ];
      bias_struct.sumrad = (float)sumrad[ i ];
      bias_struct.bias = (float)bb[ i ];

      if(bias_struct.numpairs <= DBL_MIN)
      {
         bias_struct.numpairs = 0.0;
	  }

      InsertOrUpdateDAABiasDyn(& bias_struct);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/write_daabiasdyn.c,v $";
 static char rcs_id2[] = "$Id: write_daabiasdyn.c,v 1.1 2012/04/25 16:04:05 pst Exp $";}
/*  ===================================================  */

}  
