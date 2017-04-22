/*******************************************************************************
* FILENAME:  run_rfcbmosaic.c
*
* Purpose:
* This function is converted from FORTRAN code: run_bmosaic.f.
* This function computes the bmosaic data.
*
* calling function: main_mpe_fieldgen
* functions called: apply_mfb, writeArray
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
* meanFieldBias    - the mean field bias value computed for each radar.
*
* ID - the mosaic id array of radars.
*
* RMosaic - the raw mosaic of radars.
*
* output variables
*
* BMosaic - the bmosaic array.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*   May   2007   Bryon Lawrence    wrote to produce the RFC mean field bias 
*                                  corrected mosaic.
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "RWBiasDyn.h"

static void apply_rfc_mfb(const double * local_mfbias ,
                          const double * rfc_mfbias,
	                  const int rowSize ,
	                  const int colSize ,
       	                  int ** ID ,
		          double ** RMosaic ,
		          double ** BMosaic)
{
	int i, j, k ;
	for(i = 0; i < rowSize;  ++i)
	{
		for(j = 0; j < colSize; ++j )
		{
			k = ID[i][j] ;

			if(k > 0)
			{
				/**
				 * Add a check for RMosaic.
				 * Apply mean bias value to RMosaic to make BMosaic
				 * only when the RMosaic is not default missing value.
				 * Otherwise the BMosaic is default missing value.
				 * Added by guoxian zhou May 2005
				 **/
				if(RMosaic[i][j] != MOSAIC_DEFAULT)
                                {
                                        if ( rfc_mfbias[k-1] >= 0 )
                                        {
					    BMosaic[i][j] = rfc_mfbias[k-1] * RMosaic[i][j] ;
                                        }
                                        else
                                        {
					    BMosaic[i][j] = local_mfbias[k-1] * RMosaic[i][j] ;
                                        }
                                }
			}
		}
	}
}


static void getRfcMeanFieldBias ( double rfcMeanFieldBias [ ],
                                  const mpe_params_struct * pMPEParams,
                                  const radarLoc_table_struct * ptrRadarLocTable,
                                  const char * datetime )
{
   char where[200];
   int i;
   int status;
   RWBiasDyn * pRWBiasDynHead = NULL;
   RWBiasDyn * pRWBiasDynNode = NULL;

   for ( i = 0; i < ptrRadarLocTable->radarNum; ++i )
   {
      rfcMeanFieldBias[i] = -1;

      status = strcmp ( ptrRadarLocTable->ptrRadarLocRecords[i].officeID, pMPEParams->fxa_local_site );

      if (status != 0 )
      {
         /* For each office_id which is not the local WFO, attempt to 
            get a bias value from the RWBiasDyn table. */
         sprintf ( where, "WHERE radid = '%s' and office_id = '%s' AND obstime = '%s' "
                          "AND numpairs >= %ld ORDER BY memspan_ind ASC", 
                           ptrRadarLocTable->ptrRadarLocRecords[i].radarID,
                           ptrRadarLocTable->ptrRadarLocRecords[i].officeID,
                           datetime, pMPEParams->ptrRWBiasStat->npair_bias_select );
         pRWBiasDynHead = GetRWBiasDyn ( where );
     
         if ( pRWBiasDynHead != NULL )
         {
            pRWBiasDynNode = (RWBiasDyn * ) ListFirst ( &pRWBiasDynHead->list );

            if ( pRWBiasDynNode != NULL )
            {
               rfcMeanFieldBias[i] = pRWBiasDynNode->bias;
            }
         }
      }
   }
}

void runRfcBMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                const radarLoc_table_struct * ptrRadarLocTable,
                double * meanFieldBias ,
                int ** ID ,
                double ** RMosaic ,
                double ** RfcBMosaic,
                double ** QPEMosaic)
{
    double rfcMeanFieldBias [ ptrRadarLocTable->radarNum ];
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char    dateYMD[YYYYMMDDHH_LEN + 1] ; 

    static char    fileName[PATH_LEN] = "" ;
    static char    mosaicDir[PATH_LEN] = "" ;
    static int    first = 1 ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    register int i, j ;    

    /**      
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);
    strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y-%m-%d %H:00:00", pRunTime ) ;

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin RfcBMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );

    /**      
     * Initialize arrays
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            RfcBMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    /**
    * Retrieve the RFC bias information.
    *
    **/
    getRfcMeanFieldBias(rfcMeanFieldBias, 
                        pMPEParams,
                        ptrRadarLocTable,
                        datetime );

    /**
    * apply mean field bias values to RMOSAIC field
    * to create RfcBMOSAIC field.  First try to use an RFC
    * bias value for a given radar.  If an RFC bias is not
    * available, default to the WFO bias value.
    * 
    **/
    apply_rfc_mfb(meanFieldBias, rfcMeanFieldBias, rowSize, colSize, ID, RMosaic, RfcBMosaic) ;

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end RfcBMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );        

    /**
    * write out gridded data in xmrg format to flat files
    **/
    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin writing fields to flat files." , 
                    currTime) ;
    printMessage( message, logFile );

    if(first == 1)
    {
        if(getAppsDefaults("mpe_rfcbmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"mpe_bmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        first = 0 ;
    }
    
    sprintf(fileName, "RFCBMOSAIC%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, RfcBMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s." , 
            irc, mosaicDir, fileName) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , mosaicDir,
                           fileName) ;
        printMessage( message, logFile );
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end writing fields to flat files." , 
                    currTime) ;
    printMessage( message, logFile );
    
    /**      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is rfcbmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "rfcbmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = RfcBMosaic[i][j] ;
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/run_rfcbmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_rfcbmosaic.c,v 1.1 2007/10/15 12:25:39 dsa Exp lawrence $";}
/*  ===================================================  */

}
