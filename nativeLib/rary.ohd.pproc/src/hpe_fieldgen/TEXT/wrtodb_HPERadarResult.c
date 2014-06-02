/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION: This table includes information related to radar source,  bias source etc used 
*              in HPE fieldgen
*
* ORIGINAL AUTHOR:  Jingtao Deng
* CREATION DATE:    09/23/2013
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*         
********************************************************************************
*/

#include <string.h>

#include "DbmsDefs.h"
#include "HPERadarResult.h"
#include "time_convert.h"
#include "empe_fieldgen.h"

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

/*
   calling subroutine: runERMosaic, runEBMosaic, runDHRMosaic,  runBDHRMosaic 
*/

void wrtodb_HPERadarResult(const char               * hpe_productname,
		                   const char               * producttime,
                           const empe_params_struct * pEMPEParams,
			               const int                radar_data_source)
{
  
   HPERadarResult hperadarresult_struct;
 
   const char * HPE_BIAS_SOURCE_TOKEN = "hpe_bias_source";
   static int first = 1 ;
   static char bias_source[6] = "RFC";  // RFC or LOCAL, default to RFC
   char strTokenValue[6] = {'\0'} ; 

   /* retrieve token HPE_BIAS_SOURCE_TOKEN,  it can be LOCAL or RFC */

   if(first == 1)
   {
      if(hpe_fieldgen_getAppsDefaults(HPE_BIAS_SOURCE_TOKEN, strTokenValue) != -1)
        {
    	  chgupper(strTokenValue);
    	  if (strcmp(strTokenValue, "LOCAL") == 0)
            {
                strcpy(bias_source, "LOCAL");

                sprintf ( message , "STATUS: in wrtodb_HPERadarResult: token value for \"%s\" is: %s.",
                          HPE_BIAS_SOURCE_TOKEN, bias_source) ;
                hpe_fieldgen_printMessage( message );
            }

    	    else if(strcmp(strTokenValue, "RFC") == 0)
            {
                strcpy(bias_source, "RFC");

                sprintf ( message , "STATUS: in wrtodb_HPERadarResult: token value for \"%s\" is: %s.",
                          HPE_BIAS_SOURCE_TOKEN, bias_source) ;
                hpe_fieldgen_printMessage( message );            	
            }
        }
        else
        {        
            sprintf ( message , "ERROR: in wrtodb_HPERadarResult: Invalid token value"
                     " for token \"%s\".", HPE_BIAS_SOURCE_TOKEN) ;
            hpe_fieldgen_printMessage( message );
        }

        first = 0;
    } 
    
   /* Initialize the product name. and assign value */
   /*hperadarresult_struct.hpe_productname [ PRESET_DESCR_LEN + 1 ] = '\0';*/
   strncpy(hperadarresult_struct.hpe_productname, hpe_productname, PRESET_DESCR_LEN);

   /* assign producttime */
   yearsec_ansi_to_dt(producttime, &hperadarresult_struct.producttime);

   /* Initialize the number of available radar. and assign value */
   hperadarresult_struct.num_radar_avail = 0;
   hperadarresult_struct.num_radar_avail = pEMPEParams->radar_avail_num;

   /* Initialize the bias source . and assign value */
   hperadarresult_struct.bias_source[BIAS_LEN + 1] = '\0';
   
   if (pEMPEParams->blnMeanFieldBias == 0 && pEMPEParams->blnDHRMeanFieldBias == 0)
	   strcpy(hperadarresult_struct.bias_source, "NO BIAS");
   else   //EBMOSAIC or BDHRMOSAIC 
   {
       if (pEMPEParams->blnUseLocalBias == 1)
           strcpy(hperadarresult_struct.bias_source, "SITE LOCAL BIAS");
       else
       {
           if (strcmp(bias_source, "RFC") == 0)
	          strcpy(hperadarresult_struct.bias_source, "RFC MEAN BIAS");
	   else if (strcmp(bias_source, "LOCAL") == 0)
	    	  strcpy(hperadarresult_struct.bias_source, "SITE MEAN BIAS" );
       }
   }
              
   /* Initialize the radar data source and assign value */
   hperadarresult_struct.radar_data_source[BOOL_LEN + 1]='\0';
   hperadarresult_struct.radar_data_source[0]='S';   //default to single pol radar source
   
   if (radar_data_source == 0)   //single pol
      strcpy(hperadarresult_struct.radar_data_source, "S");
   else
      strcpy(hperadarresult_struct.radar_data_source, "D");   
   
   /* insert or update HPERadarResult table */
   InsertOrUpdateHPERadarResult(&hperadarresult_struct);
   

   return ;
}
