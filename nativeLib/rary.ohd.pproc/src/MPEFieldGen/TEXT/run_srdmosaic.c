/*******************************************************************************
* FILENAME:  run_srdmosaic.c
*
* Purpose:
* This function computes the srdmosaic data.
*
* calling function: main_mpe_fieldgen
* functions called: writeArray.
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
* LSatpre - the LSATPRE array
*
* LDMosaic - the ldmosaic array.
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

/* BAD  BAD  BAD  BAD!!! GLOBAL VARIABLE! */
double ** SRDMosaic ;


/* These function prototypes belong in a .h file */
void allocSRDMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseSRDMosaicMemory(const geo_data_struct * pGeoData) ;

void runSRDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                int    ** ID,
                double ** LSatpre ,
                double ** LDMosaic ,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char mosaicDir[PATH_LEN] = "" ;
    static int first = 1 ;

    int i, j , ii, jj, iw;
    int errFlag ;
    float w;

    static char dateYMD[YYYYMMDDHH_LEN + 1] = "" ; 
    static char fileName[PATH_LEN] = "" ;
    char    tokenvalue[TOKEN_VALUE_LEN];
    struct tm * pRunTime = NULL ;
    long int irc ;
    int srg_smoothing = 0;

    SRDMosaic = NULL ;

    /**      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocSRDMosaicMemory(pGeoData) ;

    /**      
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin srdmosaic calculation." , 
                     currTime) ;
    printMessage( message, logFile );        

    if ( pMPEParams->locbias_1hr_rerun == 0 )
    {
       sprintf ( message, "SRDMosaic recalculation on rerun = OFF" );
       printMessage ( message, logFile );
    }

    if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
         ( pRunDate->hourNum > 1 ) ) 
    {

    /**
     * read mpe_srg_smoothing = on/off switch for srg smoothing.
     * if token is not found, then 0 is assumed.
    **/
    srg_smoothing = 0;    
    if((getAppsDefaults("mpe_srg_smoothing", tokenvalue) != -1)
     && (strcmp(toLowerCase(tokenvalue), "on") == 0))
    {
        srg_smoothing = 1;
    }

    /**      
     * Initialize array
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            SRDMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    /**      
     * Create SRDMosaic field 
     **/

        /**      
         * generate SRDMOSAIC field = with no smoothing or satellite data not available
         **/

if (srg_smoothing == 0 || pMPEParams->sat_avail == 0)
{
        sprintf( message , "SRDMOSAIC field generated without smoothing" );
        printMessage( message, logFile );        

        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                if(LDMosaic[i][j] < 0.0) 
                   SRDMosaic[i][j] = LSatpre[i][j];
                else
                   SRDMosaic[i][j] = LDMosaic[i][j];
            }
        }
} /* (srg_smoothing != 0 && pMPEParams->sat_avail != 0) */
else /* (srg_smoothing != 0 && pMPEParams->sat_avail != 0) */
{

        /**      
         * generate SRDMOSAIC field with WBA smoothing
         **/

        sprintf( message , "SRDMOSAIC field generated with WBA smoothing" );
        printMessage( message, logFile );        

        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {

                if(LDMosaic[i][j] < 0.0)
                {
                   SRDMosaic[i][j] = LSatpre[i][j];
                }
                else
                {

                   iw = 0;

                   for(ii = i-12; ii <= i+12; ii++)
                   {
                      for(jj = j-12; jj <= j+12; jj++)
                      {
                         if(ii >= 0 && jj >= 0 && ii < rowSize && jj < colSize)
                         {
                            if(ID[ii][jj] > 0) iw++;
                         }
                      }
                   } 

                   w = iw/625.;

                   SRDMosaic[i][j] = (LDMosaic[i][j] * w) + (LSatpre[i][j] * (1-w));
                } /* (LDMosaic[i][j] >= 0.0) */

            } /* for j */
        } /* for i */

} /* (srg_smoothing != 0 && pMPEParams->sat_avail != 0) */

} /* if (locbias_1hr_rerun == 1 || pRunDate->hourNum > 1 ) */

        getCurrentTime(currTime) ;
        sprintf( message , "%s = time   end srdmosaic calculation." , 
                        currTime) ;
        printMessage( message, logFile );

    /**
    * write srdmosaic based on input options
    **/

if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
         ( pRunDate->hourNum > 1 ) ) 
    {

        if(first == 1)
        {
            if(getAppsDefaults("mpe_srdmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_srdmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            first = 0 ;
        }

        sprintf(fileName, "SRDMOSAIC%sz", dateYMD ); 
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, SRDMosaic, &irc) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number =%ld "
                "attempting to write file: %s/%s." , 
                irc, mosaicDir, fileName) ;
            printMessage( message, logFile );
        }
        else
        {
            sprintf( message , "STATUS: file written to: %s/%s." , 
            mosaicDir, fileName) ;
            printMessage( message, logFile );
        }

    /**      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is srdmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "srdmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = SRDMosaic[i][j] ;
            }
        }
    }

} /* if (locbias_1hr_rerun == 1 || pRunDate->hourNum > 1 ) */

} /* end runSRDMosaic */


void allocSRDMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**      
     * allocate memory for SRDMosaic (double data type) variable
     **/
    SRDMosaic = (double **)malloc(rowSize * sizeof(double *)); 
    if(SRDMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocSRDMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        SRDMosaic[i] = (double *)malloc(colSize * sizeof(double)); 
        if(SRDMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocSRDMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    
} /* end allocSRDMosaicMemory */

void releaseSRDMosaicMemory(const geo_data_struct * pGeoData) 
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(SRDMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(SRDMosaic[i] != NULL)
            {
                free(SRDMosaic[i]);
                SRDMosaic[i] = NULL;
            }
        }
        free(SRDMosaic);
        SRDMosaic = NULL;
    }

} /* end releaseSRDMosaicMemory */
