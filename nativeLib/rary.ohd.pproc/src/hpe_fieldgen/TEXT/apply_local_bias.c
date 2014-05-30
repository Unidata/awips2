/***********************************************************************
* Filename: apply_local_bias.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 08/08/2007
*
* Development Group: OHD/HSEB
*
* Description:
* applying the local bias values
* to the ERMOSAIC field to generate the EBMOSAIC field.
* 
* Modules:
* applyLocalBias
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include <sys/stat.h>
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: applyLocalBias
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 08/08/2007
* 
* Description:
* This function applies the local bias values
* to the ERMOSAIC field to generate the EBMOSAIC field.
*  
* Calling Arguments:
* Name   input/Output    Type               Description
*
* tRunTime      Input    time_t             current run time 
* pGeoData      Input    geo_data_struct*   global HRAP lowerleft-corner
*                                           bin and dimension and dimension
*                                           of the RFC estimation domain
* pMPEParams    Input    mpe_params_struct* static parameters
* ERMosaic      Input    double **          the raw dsp mosaic.
* EBMosaic     Output    double **          the local biased dsp mosaic.
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* getAppsDefaults, init2DDoubleArray, readxmrg,
* convertDoubleArray, free2DDoubleArray
*
* Return Value:
* Type          Description
* int           status for applying local bias to build EBMosaic
*               0 -- failure
*               1 -- success 
*
* Error Codes/Exceptions:
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer         Action
* 08/16/2007  Guoxian Zhou      first version 
* 07/2013     JingtaoD          dual pol
***********************************************************************/

static double  ** origLocBias = NULL;
static double  ** locBias = NULL ;

extern int dualpol_used;

static void initArrays(const geo_data_struct * pGeoData,
                       const int hrap_grid_factor) ;

int applyLocalBias(const time_t tRunTime ,
                   const geo_data_struct * pGeoData ,
                   const empe_params_struct * pEMPEParams ,
                   double ** ERMosaic ,
                   double ** EBMosaic)
{
    const char * LOCBIAS_DIR_TOKEN  = "mpe_locbias_dir";
    const char * LOCBIASDP_DIR_TOKEN ="mpe_locbiasdp_dir";
    const int LOCBIAS_HOURS = 3;

    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;
    const int origRowSize = rowSize / pEMPEParams->hrap_grid_factor;
    const int origColSize = colSize / pEMPEParams->hrap_grid_factor;

    static char localBiasDir[PATH_LEN] = {'\0'} ;
    char fileName[PATH_LEN] = {'\0'} ;
    char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    struct tm * pRunTime = NULL ;

    static int first = 0;
    char prefix[10] = {'\0'};

    int i, j ;

    double factor = FACTOR_PRECIP ;
    int irc = 0 ;

    struct stat statInfo;

    time_t currTime = tRunTime ;

    if (dualpol_used == 1)
    {
        if(hpe_fieldgen_getAppsDefaults(LOCBIASDP_DIR_TOKEN, localBiasDir) == -1)
        {
            sprintf ( message , "WARNING: token \"%s\" not available"
                      " - using mean field bias.", LOCBIASDP_DIR_TOKEN) ;
            hpe_fieldgen_printMessage( message );

            return 0;

            }
	    else
	    {
	      strcpy(prefix, "LOCBIASDP");
	      sprintf ( message , "STATUS: using dual pol local bias.") ;
              printMessage( message );
            }		
	
	}
	else
	{
	    if(getAppsDefaults(LOCBIAS_DIR_TOKEN, localBiasDir) == -1)
            {
        	sprintf ( message , "WARNING: token \"%s\" not available"
                	  " - using mean field bias.", LOCBIAS_DIR_TOKEN) ;
        	printMessage( message );

        	return 0;

            }
	    else
	    {
	       strcpy(prefix, "LOCBIAS");
	       sprintf ( message , "STATUS: using single pol local bias.") ;
              printMessage( message );
	    }
	}

        i = 0;
        int status = 0;    
    
        while (i < LOCBIAS_HOURS)
        {
            pRunTime = gmtime(&currTime) ;
            strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
                     "%Y%m%d%H", pRunTime);
        
            sprintf(fileName, "%s/%s%sz", localBiasDir, prefix, strDateTime ); 
	        sprintf(message, "STATUS: local bias file name is %s.", fileName);
	        hpe_fieldgen_printMessage( message );
	    
    
            /*
             * Check to determine if the local bias file exists
             * and is readable.
             */
    
            status = stat ( fileName, & statInfo );
    
            if ( ( status != 0) || !( statInfo.st_mode & S_IFREG ) )  
            {
                /*
                 * the local bias file does not exist,
                 * check the local bias file of previous hour.   
                 */
    
                currTime -= SECONDS_PER_HOUR;
            }
            else
            {
                 irc = 1;
                 break;
            }
            
            i++ ;
        }
        
        if(irc == 0)
        {
            sprintf ( message , "WARNING: local bias file not found"
                      " - using mean field bias.") ;
            hpe_fieldgen_printMessage( message );
    
            return 0;
        }

        initArrays(pGeoData, pEMPEParams->hrap_grid_factor);

        int lenfn = strlen(fileName);
    
        readxmrg(pEMPEParams->os, origRowSize, origColSize, fileName,
                 lenfn, factor, origLocBias, &irc) ;

        if(irc != 0)
        {
            sprintf ( message , "WARNING: failure to load file -- %s\n"
                      "\t using mean field bias.", fileName) ;
            hpe_fieldgen_printMessage( message );

            return 0;
        }

        convertDoubleArray(origRowSize , origColSize ,
                           origLocBias ,
                           BIAS_DEFAULT ,
                           rowSize , colSize ,
                           locBias );
  /*  }*/

    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            /*
             * Apply local bias value to ERMosaic to make EBMosaic
             * only when both of the ERMosaic and the locBias are not
             * default missing values.
             * Otherwise the EBMosaic is default to missing value.
             */

            if( ( ERMosaic[i][j] >= 0.0 ) &&
                ( fabs(locBias[i][j] - BIAS_DEFAULT) > 0.1) )
            {
                EBMosaic[i][j] = locBias[i][j] * ERMosaic[i][j] ;
            }
        }
    }

    first = 1;

    return 1;

}


static void initArrays(const geo_data_struct * pGeoData,
                       const int hrap_grid_factor) 
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int origRowSize = rowSize / hrap_grid_factor;
    const int origColSize = colSize / hrap_grid_factor;

    origLocBias = init2DDoubleArray(BIAS_DEFAULT, origRowSize, origColSize);

    locBias = init2DDoubleArray(BIAS_DEFAULT, rowSize, colSize);

}

void free_locbias_memory(const geo_data_struct * pGeoData,
                         const int hrap_grid_factor) 
{
    const int rowSize = pGeoData->num_rows ;
    const int origRowSize = rowSize / hrap_grid_factor;

    free2DDoubleArray(origLocBias, origRowSize );
                      
    free2DDoubleArray(locBias, rowSize );
}
