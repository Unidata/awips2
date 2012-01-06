
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

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"
#include "dbmserrs.h"
#include "QualityCode.h"
#include "mpe_log_utils.h"
#include "RawPP.h"
#include "time_convert.h"
#include "update_rawPP.h"

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

void update_rawPP ( const char * obstime,
                    const char * lid,
                    const char * ts,
                    char shef_qual_code,
                    int dur,
                    float value )
{
   const char extremum = 'Z';
   static const char * pe = "PP";
   static const char * product_id = "MPEUPDATE";
   int db_status;
   const int revision = 1;
   char where [ 200 ];
   RawPP * pRawPP = NULL;
   RawPP rawPP;
   time_t current_time ;

   /* Build the select where clause. */
   sprintf ( where , "WHERE lid='%s' AND pe='%s' AND dur=%d "
                     "AND ts='%s' AND extremum='%c' AND obstime='%s'",
                     lid, pe, dur, ts, extremum, obstime );

   /* Check if this PP record already exists. */
   pRawPP = GetRawPP ( where );

   if ( pRawPP == NULL )
   {
      /* The record does not exist. Insert it into the database. */

      /* Initialize the lid. */
      memset ( rawPP.lid , '\0', LOC_ID_LEN + 1 );
      strncpy ( rawPP.lid , lid, LOC_ID_LEN );

      /* Initialize the physical element. */
      memset ( rawPP.pe , '\0', SHEF_PE_LEN + 1 );
      strncpy ( rawPP.pe, pe, SHEF_PE_LEN );

      /* Initialize the duration. */
      rawPP.dur = dur ;

      /* Initialize the type source. */
      memset ( rawPP.ts , '\0', SHEF_TS_LEN + 1 );
      strncpy ( rawPP.ts , ts, SHEF_TS_LEN );

      /* Initialize the extremum. */
      memset ( rawPP.extremum, '\0', SHEF_EX_LEN + 1 );
      rawPP.extremum[0] = extremum;

      /* Initialize the obstime, product time, and posting times. */
      yearsec_ansi_to_dt ( ( char * ) obstime, &rawPP.obstime );

      current_time = time ( NULL );
      timet_to_yearsec_dt ( current_time, & rawPP.producttime );
      rawPP.postingtime = rawPP.producttime;

      /* Initialize the value. */
      rawPP.value = value;

      /* Initialize the shef quality code. */
      memset ( rawPP.shef_qual_code, '\0', SHEF_QC_LEN + 1 ); 
      rawPP.shef_qual_code [ 0 ] = shef_qual_code;

      /* Initialize the revision flag. */
      rawPP.revision = revision;

      /* Initialize the product id. */
      memset ( rawPP.product_id, '\0', PRODUCT_LEN + 1 );
      strncpy ( rawPP.product_id, product_id, PRODUCT_LEN );
   
      /* Initialize the quality code. */
      rawPP.quality_code = 0;
      set_qccode ( QC_MANUAL_PASSED, &rawPP.quality_code );

      db_status = PutRawPP ( & rawPP );

      if ( db_status != ERR_OK )
      {
         flogMessage ( stderr, "Insert into RawPP for lid: %s pe: %s dur: %d "
                           "ts: %s extremum: %c obstime: %s\n"
                           "value: %6.2f shef_qual_code: %c failed. "
                           "SQLCODE %d\n", lid, pe, dur, ts, 
                           extremum, obstime, value, shef_qual_code,
                           db_status ); 
      }
      
   }
   else
   {
      /* The record already exists.  Update it. */
     
      /* Update the value. */
      pRawPP->value = value;

      /* Update the shef quality code. */
      memset ( pRawPP->shef_qual_code, '\0', SHEF_QC_LEN + 1 );
      pRawPP->shef_qual_code[0] = shef_qual_code; 

      /* Update the revision flag. */
      pRawPP->revision = revision;

      /* Update the quality code. */
      set_qccode ( QC_MANUAL_PASSED, & pRawPP->quality_code );

      /* Update the product and posting times. */
      current_time = time ( NULL );
      timet_to_yearsec_dt ( current_time, & pRawPP->producttime );
      pRawPP->postingtime = pRawPP->producttime;

      db_status = UpdateRawPP ( pRawPP, where );

      if ( db_status != ERR_OK )
      {
         flogMessage ( stderr, "Update in RawPP for value: %6.2f and "
                           "shes_qual_code: %c using where clause:\n"
                           "'%s' failed.  SQLCODE: %d,\n", value,
                           shef_qual_code, where, db_status);
      }

      if ( pRawPP != NULL )
      {
         FreeRawPP ( pRawPP );
         pRawPP = NULL;
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

} 
