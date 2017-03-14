/*******************************************************************************
* FILENAME:            get_mean_bias.c
*
* Purpose:
* This function is converted from FORTRAN code: get_bias.f.
* it calculates the mean field bias value for each radar.
*
* calling function: runRMosaic
* functions called: pairGageRadar, calculateMeanBias
*
* input variables
*
* pRadarLocRecord - data record from radarLoc table
*
* datetime - date and time of current run in Informix datetime format
*          - minutes and seconds set to 00
*
* radar - two-dimensional radar data
*
* radarMiscBins - misc bin data
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain 
*
* pGageArray - array of gage data
*
* pMPEParams - static parameters
*
* output variables
*
* gageRadarPairNum - gage/radar pair number
*
* meanBias - mean bias data
*
* memSpanBias - memory span bias data
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2005   Guoxian Zhou      finish conversion to C Language 
*
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include "read_stage1_decoded.h"

void MPEFieldGen_getMeanBias(const radarLoc_record_struct * pRadarLocRecord,
                const char * datetime ,
                short int radarMiscBins [ ] [NUM_DPA_COLS] , 
                float radar [ ] [ NUM_DPA_COLS ] ,
                const geo_data_struct * pGeoData ,
                const gage_table_struct * pGageArray ,
                const mpe_params_struct * pMPEParams ,
                double * meanBias,
                double * memSpanBias,
                int *  gageRadarPairNum)
{
    gage_radar_pair_table_struct * pGageRadarPairTable = NULL ;

    *gageRadarPairNum = 0 ;
    *meanBias = 0 ;
	*memSpanBias = 0 ;

    /**      
     * allocate memory for gage radar pair struct data
     **/
    const int pairSize 
        = pGageArray->totalGageNum - pGageArray->pseudoGageNum ; 

//    if(pairSize > 0)
    {
        pGageRadarPairTable = 
        (gage_radar_pair_table_struct *)malloc(sizeof(gage_radar_pair_table_struct)); 
        if(pGageRadarPairTable == NULL)
        {
            sprintf ( message , "ERROR: Memory allocation failure"
                " in get_mean_bias function."
                "\n\tProgram exit");
            shutDownMPE( message, logFile );
        }

        pGageRadarPairTable->ptrGageRadarPair = 
            (gage_radar_pair_struct *)malloc(pairSize * sizeof(gage_radar_pair_struct)); 
        if(pGageRadarPairTable->ptrGageRadarPair == NULL)
        {
            sprintf ( message , "ERROR: Memory allocation failure"
                " in get_mean_bias function."
                "\n\tProgram exit");
            shutDownMPE( message, logFile );
        }

        MPEFieldGen_pairGageRadar(pRadarLocRecord, radarMiscBins , radar ,
                    pGeoData , pGageArray , pMPEParams ,
                    pGageRadarPairTable) ;

        *gageRadarPairNum = pGageRadarPairTable->pairNum ;
    
        /**      
         * qc the gage/radar pairs and calculate mean field bias.
         **/
        MPEFieldGen_calculateMeanBias(pRadarLocRecord->radarID, datetime ,
                    pMPEParams, pGageArray, pGageRadarPairTable ,
                    meanBias, memSpanBias ) ;

        /**      
         * release memory for gage radar pair struct data
         **/
        if(pGageRadarPairTable != NULL)
        {
            if(pGageRadarPairTable->ptrGageRadarPair != NULL)
            {
                free(pGageRadarPairTable->ptrGageRadarPair) ;
                pGageRadarPairTable->ptrGageRadarPair = NULL ;
            }

            free(pGageRadarPairTable) ;
            pGageRadarPairTable = NULL ;
        }

    } /* if(pairSize > 0) */

} /* end MPEFieldGen_getMeanBias */
