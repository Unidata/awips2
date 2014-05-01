/***********************************************************************
* Filename: read_mean_bias.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: October 2006
*
* Development Group: OHD
*
* Description:
* Contains routine for reading the mean bias value from the database.
* 
* Modules:
* readMeanBias
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: readMeanBias
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: October 2006
*
* Description:
* This function is modified from the get_mean_bias.c.
* 
* Calling Arguments:
* Name            Input/Output Type          Description
*
* pRunDate        Input     const run_date_struct *
*                                            date/time 
* pRadarLocRecord Input     const radarLoc_table_struct *
*                                            info from radarLoc table
* pMPEParams      Input     const mpe_params_struct *
*                                            static parameters
* meanFieldBias   Output    double[][]       the mean field bias value
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* readRadarResult, binary_search
*
* Return Value:
* Type          Description
* void
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
* 10/24/2006  Guoxian Zhou      modification for empe version 
*
***********************************************************************/

void readMeanBias(const run_date_struct * pRunDate,
                  const radarLoc_table_struct * pRadarLocTable ,
                  const empe_params_struct * pMPEParams ,
                  double * meanFieldBias )
{
    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char radarID[ RADAR_ID_LEN + 1] = {'\0'} ;

    int i;
    short radar_count;
    int blnEditBias,  blnIgnoreRadar ;

    double editBiasValue = 0.0 ;
    double memSpanBias ;
    double meanBias = 0.0 ;

    struct tm * pRunTime = NULL;
    long int    irc ;

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y-%m-%d %H:00:00", pRunTime ) ;

    radar_result_struct * pRadarResult = NULL;

    pRadarResult = (radar_result_struct *) 
        malloc(pRadarLocTable->radarNum * sizeof(radar_result_struct)); 
    if(pRadarResult == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRMosaic function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        memset(pRadarResult[i].radID, '\0', RADAR_ID_LEN + 1) ;
        pRadarResult[i].edit_bias = 0;
        pRadarResult[i].ignore_radar = 0;
        pRadarResult[i].bias = 0.0 ;
    }

    /*      
     * Read the edit bias flag, edited bias, ignore radar flag
     * from RWRadarResult table for the top hour
     */

    readRadarResult (datetime, pRadarResult, &radar_count, &irc) ;
    if (irc < 0)
    {
        sprintf ( message , "ERROR: Database error #%ld attempting to "
            "select record from RWRadarResult table.", irc) ;
        hpe_fieldgen_printMessage( message );        
    }

    /*      
     * begin loop on radars
     */

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        meanFieldBias[i] = 0.0 ;

        strcpy(radarID, pRadarLocTable->ptrRadarLocRecords[i].radarID) ;

        /*
         * initialize edit bias flag, ignore radar flag
         */

        blnEditBias    = 0 ;
        blnIgnoreRadar = 0 ;
        editBiasValue  = 0.0 ;

        radar_result_struct * pRadarInfo = NULL ;

        pRadarInfo = (radar_result_struct *)
            binary_search ( pRadarResult, radarID, radar_count,
                sizeof ( radar_result_struct), compare_radar_id );

        /*
         * load in the radar info 
         */

        if ( pRadarInfo != NULL )
        {
            editBiasValue  = pRadarInfo->bias;
            blnEditBias    = pRadarInfo->edit_bias;
            blnIgnoreRadar = pRadarInfo->ignore_radar;
        }

        /*      
         * load the mean field bias value 
         * and save it to meanFieldBias[i]
         */

       retrieveMeanBias(radarID, datetime, pMPEParams ,
                 &meanBias, &memSpanBias );

        meanFieldBias[i] = meanBias ;

        /*
         * if blnEditBias = 1, then use edited bias value
         */

        if(blnEditBias == 1)
        {
            meanFieldBias[i] = editBiasValue ;

            sprintf ( message , "Edited bias value = %4.2f used.",
                editBiasValue);
            hpe_fieldgen_printMessage( message );        
        }
    }

    if(pRadarResult != NULL)
    {
        free(pRadarResult);
        pRadarResult = NULL;
    }

}
