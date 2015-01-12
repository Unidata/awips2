/*******************************************************************************
* FILENAME:            pair_gage_radar.c
*
* Purpose:
* This function is converted from FORTRAN code: grpairs.f.
* it performs pairing of positive gage and radar rainfall
* in a given geo grid.
*
* calling function: getMeanBias
* functions called: LatLongToHrap
*
* input variables
*
* pRadarLocRecord - lat/lon data from radarLoc table
*
* radarMiscBins - misc bin data
*
* radar - two-dimensional radar data
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
* pGageRadarPairTable - array of positive gage/radar pair data
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   03/04/2005   Guoxian Zhou      finish conversion to C Language 
*   9/15/2006    Paul Tilles       added print of gage id to gage/radar pair
*
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include "read_stage1_decoded.h"

void MPEFieldGen_pairGageRadar(const radarLoc_record_struct * pRadarLocRecord,
                short int radarMiscBins[][NUM_DPA_COLS] , 
                float radar [ ] [ NUM_DPA_COLS ] ,
                const geo_data_struct * pGeoData ,
                const gage_table_struct * pGageArray ,
                const mpe_params_struct * pMPEParams ,
                gage_radar_pair_table_struct * pGageRadarPairTable)
{

    double row, col ; 
    int hrap_x, hrap_y, tmp_hrap_x, tmp_hrap_y ;
    int local_hrap_x, local_hrap_y ;
    int pair_num ;
    int i ;
	
	const double min_gr_value_bias =
			(double)pMPEParams->ptrRWBiasStat->min_gr_value_bias ;

    /**      
     * get hrap coordinates of radar
     **/
    LatLongToHrapByReference ( pRadarLocRecord->latitude , 
                    pRadarLocRecord->longitude ,
                    & row , & col ) ;

    /**      
     * find lower left corner of 131x131 radar grid in
     * national hrap coordinates
     **/
    hrap_x = (int)(col - 65) ;
    hrap_y = (int)(row - 65) ;

    /**      
     * find lower left corner of 131x131 radar grid in
     * local coordinate which start at array
     * location (0,0) in lower left
     **/
    local_hrap_x = hrap_x - pGeoData->hrap_x ;
    local_hrap_y = hrap_y - pGeoData->hrap_y ;

    pair_num = 0 ;
    for(i = pGageArray->pseudoGageNum;
        i < pGageArray->totalGageNum;
        i++)
    {
        if(pGageArray->ptrGageRecords[i].gageValue 
            <= min_gr_value_bias)
            continue ;

        tmp_hrap_x = pGageArray->ptrGageRecords[i].hrap_x - local_hrap_x ;
        tmp_hrap_y = pGageArray->ptrGageRecords[i].hrap_y - local_hrap_y ;

        if(tmp_hrap_x < 0) continue ;

        if(tmp_hrap_x >= NUM_DPA_COLS)  continue ;

        if(tmp_hrap_y < 0)  continue ;

        if(tmp_hrap_y >= NUM_DPA_COLS)  continue ;

        if(radarMiscBins[tmp_hrap_y][tmp_hrap_x] == 0) continue ;

        if((double)radar[tmp_hrap_y][tmp_hrap_x] <=
            min_gr_value_bias) continue ;

        /**      
         * match collocated radar rainfall
         * exclude pseudo gages from consideration
         **/
        pGageRadarPairTable->ptrGageRadarPair[pair_num].hrap_x
            = pGageArray->ptrGageRecords[i].hrap_x ;
        pGageRadarPairTable->ptrGageRadarPair[pair_num].hrap_y
            = pGageArray->ptrGageRecords[i].hrap_y ;
        pGageRadarPairTable->ptrGageRadarPair[pair_num].gageValue
            = pGageArray->ptrGageRecords[i].gageValue ;
        strcpy(
        pGageRadarPairTable->ptrGageRadarPair[pair_num].lid,
        pGageArray->ptrGageRecords[i].gageID
              );
        pGageRadarPairTable->ptrGageRadarPair[pair_num].radarValue
            = (double)radar[tmp_hrap_y][tmp_hrap_x] ;

        pair_num ++ ;
    }

    pGageRadarPairTable->pairNum = pair_num ;

    /**      
     * print out the number of positive gage-radar pairs
     **/
    if(pair_num > 0)
    {
        sprintf ( message , "*** gage-radar pairs ***"
            "\n\tGageID\tHRAPx\tHRAPy\tgage value\tradar value");
        printMessage(message, logFile);

        for(i = 0 ; i < pair_num ; i++)
        {
            sprintf ( message , "\t%s\t%d\t%d\t%10.2f\t %10.2f", 
                pGageRadarPairTable->ptrGageRadarPair[i].lid, 
                pGageRadarPairTable->ptrGageRadarPair[i].hrap_x, 
                pGageRadarPairTable->ptrGageRadarPair[i].hrap_y, 
                pGageRadarPairTable->ptrGageRadarPair[i].gageValue, 
                pGageRadarPairTable->ptrGageRadarPair[i].radarValue ) ;
            printMessage(message, logFile);        
        }
    }
    else
    {
        sprintf ( message , "STATUS: no additional gage-radar pairs.");
        printMessage(message, logFile);        
    }
}
