/*******************************************************************************
* FILENAME:            run_satpre.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          runsatpre
* DESCRIPTION:         If the SATPRE product has been chosen as the Best QPE, 
*                      then apply any polygons to the satellite precipitation
*                      product and save it as the Best QPE.
*
*
* ORIGINAL AUTHOR:     Bryon Lawrence 
* CREATION DATE:       March 24, 2008
* ORGANIZATION:        HSEB/OHD
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE            PROGRAMMER        DESCRIPTION/REASON
*          1        March 24, 2008  Bryon Lawrence    Original Coding
*
********************************************************************************
*/
#include "mpe_fieldgen.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    runSatpre
* PURPOSE:        Writes the satellite precipitation field as the Best QPE.  
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  run_date_struct * pRunDate,
*   Input  geo_data_struct * pGeoData,
*   Input  mpe_params_struct * pMPEParams,
*   Output double ** QPEMosaic
*
* RETURNS:
*   None.
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME              DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

void runSatpre  ( const run_date_struct * pRunDate,
                  const geo_data_struct * pGeoData,
                  mpe_params_struct * pMPEParams,
                  double ** QPEMosaic )
{
  static char dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'};
  double ** satpre = NULL;
  int isat_avail;
  int i, j ;
  int year;
  int month;
  int day;
  int hour;

  static int first = 1 ;

  const int rowSize = pGeoData->num_rows ;
  const int colSize = pGeoData->num_cols ;

  struct tm * pRunTime = NULL ;

  getCurrentTime(currTime);
  sprintf ( message, "%s = time begin SATPRE calc\n", currTime );
  printMessage ( message, logFile );

  /* Only perform the following if the Satellite Precipitation
     field is to be stored as the Best QPE. */ 
  if ( strcmp ( pMPEParams->qpe_fieldtype, "satpre") == 0)
  {
     /**
      * dateYMD string should be in format: yyyymmddhh
      **/
     pRunTime = gmtime(&(pRunDate->tRunTime)) ;
     strftime(dateYMD, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pRunTime);

     year = pRunTime->tm_year + 1900;
     month = pRunTime->tm_mon + 1;
     day = pRunTime->tm_mday;
     hour = pRunTime->tm_hour;

     /* Read the satellite data:
        isat_avail = satellite availability flag
                   = 0 - satellite data not available
                   = 1 - satellite data available */
     
     satpre = MPEFieldGen_read_satellite ( pRunDate,
                               pGeoData,
                               hour,
                               & isat_avail );
      
     if ( isat_avail == 0 )
     {
        sprintf ( message, "  satellite data not available for current "
                              "hour.\n" );
        printMessage (message, logFile );
     }

     /* Store the satellite precipitation field as the Best QPE. */ 
     for ( i = 0; i < rowSize; ++i )
     {
        for ( j = 0; j < colSize; ++ j )
        {
           QPEMosaic[i][j] = satpre [i][j];
        }
     }

     /* Satellite data are not freed.  They are saved in case they are need in
        subsequent MPE fields and mosaics. */

   } /* if ( strcmp ( pMPEParams->qpe_fieldtype, "satpre") == 0) */

   getCurrentTime ( currTime );
   sprintf ( message, "%s = time end   SATPRE calc\n", currTime );
   printMessage ( message, logFile ); 

   return ;

} /* end runSatpre */
