/*******************************************************************************
* FILENAME:  run_srmosaic.c
*
* Purpose:
* This function computes the srmosaic data.
*
* calling function: main_mpe_fieldgen
* functions called: writeArray.
*
* input variables
*
* pRunDate - date/time.
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*            and dimension of the RFC estimation domain.
*
* pMPEParams - static parameters.
*
* LSatpre - the LSATPRE array
*
* LMosaic - the lmosaic array.
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2007   P Tilles          Initial Version
*   April 2008   P Tilles          Added Weighted Boundary Adjustment
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

double ** SRMosaic ;

void allocSRMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseSRMosaicMemory(const geo_data_struct * pGeoData) ;

void runSRMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                //loubnaint ** MPEFieldGen_ID,
                int ** ID,
                double ** LSatpre ,
                double ** LMosaic ,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char mosaicDir[PATH_LEN] = "" ;
    static int first = 1 ;

    int i, j , ii, jj, iw;
    float w;
    int errFlag ;

    static char dateYMD[YYYYMMDDHH_LEN + 1] = "" ;
    static char fileName[PATH_LEN] = "" ;
    char    tokenvalue[TOKEN_VALUE_LEN];
    struct tm * pRunTime = NULL ;
    long int irc;
    int srg_smoothing = 0;


    SRMosaic = NULL ;

    /**
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocSRMosaicMemory(pGeoData) ;

    /**
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);
    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin srmosaic calculation." ,
                     currTime) ;
    printMessage( message, logFile );

    if ( pMPEParams->locbias_1hr_rerun == 0 )
    {
        sprintf ( message, "SRMosaic recalculation on rerun = OFF" );
    }
    printMessage ( message, logFile );

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
            SRMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    /**
     * Create SRMosaic field
     **/

    /**
     * generate SRMOSAIC field = with no smoothing or satellite data not available
     **/
    if (srg_smoothing == 0 || pMPEParams->sat_avail == 0)
    {
        sprintf( message , "SRMOSAIC field generated without smoothing");
        printMessage( message, logFile );

       for(i = 0; i < rowSize; i ++)
       {
            for(j = 0; j < colSize; j ++)
            {
                if(LMosaic[i][j] < 0.0) /*previously was if(LMosaic[i][j] == MOSAIC_DEFAULT) */
                   SRMosaic[i][j] = LSatpre[i][j];
                else
                   SRMosaic[i][j] = LMosaic[i][j];
            }
        }
   }
   else
   {

   /**
    * generate SRMOSAIC field with WBA smoothing
    **/

   sprintf( message , "SRMOSAIC field generated with WBA smoothing" );
   printMessage( message, logFile );

   for(i = 0; i < rowSize; i ++)
   {
       for(j = 0; j < colSize; j ++)
       {

           if(LMosaic[i][j] < 0.0)
           {
              SRMosaic[i][j] = LSatpre[i][j];
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

              SRMosaic[i][j] = (LMosaic[i][j] * w) + (LSatpre[i][j] * (1-w));
           }

       }
   }

}

}
        getCurrentTime(currTime) ;
        sprintf( message , "%s = time   end srmosaic calculation." ,
                        currTime) ;
        printMessage( message, logFile );

    /**
    * write srmosaic based on input options
    **/

     if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
            ( pRunDate->hourNum > 1 ) )
     {
        if(first == 1)
        {
            if(getAppsDefaults("mpe_srmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_srmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            first = 0 ;
        }

        sprintf(fileName, "SRMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, SRMosaic, &irc) ;

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
     * if the qpe_fieldtype is srmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "srmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = SRMosaic[i][j] ;
            }
        }
    }
}
}

void allocSRMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**
     * allocate memory for SRMosaic (double data type) variable
     **/
    SRMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(SRMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocSRMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        SRMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(SRMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocSRMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
}

void releaseSRMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(SRMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(SRMosaic[i] != NULL)
            {
                free(SRMosaic[i]);
                SRMosaic[i] = NULL;
            }
        }
        free(SRMosaic);
        SRMosaic = NULL;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/run_srmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_srmosaic.c,v 1.1 2007/10/15 12:19:16 dsa Exp lawrence $";}
/*  ===================================================  */

}
