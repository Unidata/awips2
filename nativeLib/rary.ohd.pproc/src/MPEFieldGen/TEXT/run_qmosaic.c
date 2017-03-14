/*******************************************************************************
* FILENAME:            run_qmosaic.c
*
* Purpose:
*
* input variables
*
* pRunDate - date/time.
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain.
*
* pMPEParams - static parameters.
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2008  P Tilles          initial version
*********************************************************************************/

#include <sys/stat.h>
#include <sys/types.h>

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

void runQMosaic(const run_date_struct * pRunDate ,
                  const geo_data_struct * pGeoData ,
                  const mpe_params_struct * pMPEParams ,
                  double ** QMosaic,
                  double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    static char    mosaicDir[PATH_LEN] = "" ;
    static int    first = 1 ;

    int i, j ;
    int length;
    int errFlag ;

    static char dateYMD[YYYYMMDDHH_LEN + 1] ;
    static char fileName[PATH_LEN] = "" ;
    struct stat statInfo;
    struct tm * pRunTime = NULL ;
    long int irc ;

    getCurrentTime (currTime);
    sprintf ( message, "%s = time begin processing QMOSAIC.", currTime );
    printMessage ( message, logFile );

       /**
        * dateYMD string should be in format: yyyymmddhh
        **/
       memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
       pRunTime = gmtime(&(pRunDate->tRunTime)) ;
       strftime(dateYMD, YYYYMMDDHH_LEN + 1,
           "%Y%m%d%H", pRunTime);

       /**
        * Initialize array
        **/
       for(i = 0; i < rowSize; i ++)
       {
           for(j = 0; j < colSize; j ++)
           {
               QMosaic[i][j] = MOSAIC_DEFAULT ;
           }
       }

       if (first == 1)
       {
           if(getAppsDefaults("mpe_qmosaic_dir", mosaicDir) == -1)
           {
               sprintf ( message , "ERROR: Invalid token value"
                   " for token \"mpe_qmosaic_dir\"."
                   "\n\tProgram exit.") ;
               shutDownMPE( message, logFile );
           }

           first = 0 ;
       }

       /* Build the path and filename for the 1 hour
          qmosaic product. */
       sprintf ( fileName, "%s/QMOSAIC%sz", mosaicDir, dateYMD );

       /* Check if the QMOSAIC file exists. */
       errFlag = stat ( fileName, & statInfo );

       if ( ( errFlag == 0 ) && ( statInfo.st_mode & S_IFREG ) )
       {
          /* Read the QMosaic product file for this hour. */
          length = strlen ( fileName );

          MPEFieldGen_readxmrg ( ptrMPEParams->os,
                     rowSize,
                     colSize,
                     fileName,
                     length,
                     FACTOR_PRECIP,
                     QMosaic,
                     & errFlag );

          if ( errFlag != 0 )
          {
             sprintf ( message, "Error reading QMOSAIC file = '%s' -- "
                                "missing values subsituted.", fileName );

             for(i = 0; i < rowSize; i ++)
             {
                for(j = 0; j < colSize; j ++)
                {
                   QMosaic[i][j] = MOSAIC_DEFAULT ;
                }
             }
          }
       }
       else
       {
          sprintf ( message, "QMOSAIC file = '%s' not found "
                             " -- missing values substituted.", fileName );
          printMessage ( message, logFile );
       }

       /**
        * fill in the "best estimate" mosaic
        **/
       if(strcmp(pMPEParams->qpe_fieldtype, "qmosaic") == 0)
       {
          for(i = 0; i < rowSize; i ++)
          {
             for(j = 0; j < colSize; j ++)
             {
                QPEMosaic[i][j] = QMosaic[i][j] ;
             }
          }
       }

   getCurrentTime ( currTime );
   sprintf ( message, "%s = time   Done processing QMOSAIC", currTime );
   printMessage ( message, logFile );

} /* end runQMosaic */
