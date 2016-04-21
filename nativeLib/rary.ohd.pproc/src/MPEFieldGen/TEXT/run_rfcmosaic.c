/*******************************************************************************
* FILENAME:            run_rfcmosaic.c
*
* Purpose:
*
* If the RFC Mosaic has been chosen as the Best QPE, then read the 1 hour
* RFC Mosaic product file and save it as the Best QPE.
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
*   March 2008   Bryon Lawrence    Created. 
*********************************************************************************/

#include <sys/stat.h>
#include <sys/types.h>

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

static double ** RfcMosaic = NULL ;

static void allocRfcMosaicMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    RfcMosaic = (double **)malloc(rowSize * sizeof(double *)); 

    if(RfcMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocRfcMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    for(i = 0; i < rowSize; i++)
    {
        RfcMosaic[i] = (double *)malloc(colSize * sizeof(double)); 
        if(RfcMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocRfcMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    
}

static void releaseRfcMosaicMemory(const geo_data_struct * pGeoData) 
{
    int    i ;
    const int rowSize = pGeoData->num_rows;

    if(RfcMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(RfcMosaic[i] != NULL)
            {
                free(RfcMosaic[i]);
                RfcMosaic[i] = NULL;
            }
        }
        free(RfcMosaic);
        RfcMosaic = NULL;
    }
}

void runRfcMosaic(const run_date_struct * pRunDate ,
                  const geo_data_struct * pGeoData ,
                  const mpe_params_struct * pMPEParams ,
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

    RfcMosaic = NULL ;

    getCurrentTime (currTime);
    sprintf ( message, "%s = time begin processing RFCMOSAIC.", currTime );
    printMessage ( message, logFile );

    /* Only do this if the RFCMOSAIC has been chosen
       as the BEST QPE */
    if(strcmp(pMPEParams->qpe_fieldtype, "rfcmosaic") == 0)
    {

       /**      
        * Allocates memory for data arrays
        * based on the geo grid size data.
        **/
       allocRfcMosaicMemory(pGeoData) ;

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
               RfcMosaic[i][j] = MOSAIC_DEFAULT ;
           }
       }

       if (first == 1)
       {
           if(getAppsDefaults("gaq_xmrg_1hr_dir", mosaicDir) == -1)
           {
               sprintf ( message , "ERROR: Invalid token value"
                   " for token \"gaq_xmrg_1hr_dir\"."
                   "\n\tProgram exit.") ;
               shutDownMPE( message, logFile );
           }

           first = 0 ;
       }

       /* Build the path and filename for the 1 hour
          RFC mosaic product. */
       sprintf ( fileName, "%s/RFCMOSAIC01%sz", mosaicDir, dateYMD );

       /* Check if the RFCMOSAIC file exists. */
       errFlag = stat ( fileName, & statInfo );

       if ( ( errFlag == 0 ) && ( statInfo.st_mode & S_IFREG ) ) 
       {
          /* Read the RFCMosaic product file for this hour. */
          length = strlen ( fileName );

          MPEFieldGen_readxmrg ( ptrMPEParams->os,
                     rowSize,
                     colSize,
                     fileName,
                     length,
                     FACTOR_PRECIP,
                     RfcMosaic,
                     & errFlag );

          if ( errFlag != 0 )
          {
             sprintf ( message, "Error reading RFCMOSAIC file = '%s' -- "
                                "missing values subsituted.", fileName ); 

             for(i = 0; i < rowSize; i ++)
             {
                for(j = 0; j < colSize; j ++)
                {
                   RfcMosaic[i][j] = MOSAIC_DEFAULT ;
                }
             }
          }
       }
       else
       {
          sprintf ( message, "RFCMOSAIC file = '%s' not found "
                             " -- missing values substituted.", fileName );
          printMessage ( message, logFile );
       }

       /**      
        * fill in the "best estimate" mosaic
        **/
       for(i = 0; i < rowSize; i ++)
       {
          for(j = 0; j < colSize; j ++)
          {
             QPEMosaic[i][j] = RfcMosaic[i][j] ;
          }
       }

       releaseRfcMosaicMemory(pGeoData) ;
   }

   getCurrentTime ( currTime );
   sprintf ( message, "%s = time   Done processing RFCMOSAIC", currTime );
   printMessage ( message, logFile );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
