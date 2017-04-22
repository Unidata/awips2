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
*
********************************************************************************
*/
#include <string.h>
#include <float.h>

#include "DbmsDefs.h"
#include "RWBiasDyn.h"
#include "time_convert.h"
#include "mpe_fieldgen.h"

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
void MPEFieldGen_write_rwbiasdyn(const char *rad,
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
      This function writes the state variables to the RWBiasDyn table
      calling function: calculateMeanBias
   */
      
   char strdt[ANSI_YEARSEC_TIME_LEN + 1 ];
   int i;
   RWBiasDyn bias_struct;
      
   *irc = 0;

   /* Initialize the RWBiasDyn structure for insert/update. */

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

      InsertOrUpdateRWBiasDyn(& bias_struct);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/write_rwbiasdyn.c,v $";
 static char rcs_id2[] = "$Id: write_rwbiasdyn.c,v 1.1 2007/10/15 12:19:17 dsa Exp $";}
/*  ===================================================  */

}  
