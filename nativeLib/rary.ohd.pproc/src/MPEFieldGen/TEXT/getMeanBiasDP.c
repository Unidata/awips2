/*******************************************************************************
* FILENAME:            getMeanBiasDP.c
*
* Purpose:
* calculates the mean field bias value for each DP radar.
*
* calling function: runRDMosaic
* functions called: pairGageRadar, calculateMeanBiasDP
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
*
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include "read_stage1_decoded.h"

void getMeanBiasDP(const radarLoc_record_struct * pRadarLocRecord,
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

        calculateMeanBiasDP(pRadarLocRecord->radarID, datetime ,
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
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/getMeanBiasDP.c,v $";
 static char rcs_id2[] = "$Id: getMeanBiasDP.c,v 1.1 2012/04/25 16:32:31 pst Exp $";}
/*  ===================================================  */

}
