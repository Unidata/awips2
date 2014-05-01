/***********************************************************************
* Filename: pair_gage_radar.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: March 2005
*
* Development Group: OHD
*
* Description:
* Contains routine for pairing positive gage and radar rainfall
* in a given geo grid.
* 
* Modules:
* pairGageRadar
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"
#include "read_stage1_decoded.h"

/***********************************************************************
* Module Name: pairGageRadar
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: March 2005
*
* Description:
* This function is converted from FORTRAN code: grpairs.f.
* it performs pairing of positive gage and radar rainfall
* in a given geo grid.
* 
* Calling Arguments:
* Name            Input/Output Type        Description
* pRadarLocRecord Input        radarLoc_record_struct
*                                          info from table radarloc
* radarMiscBins   Input        float **    two-dimensional misc bin data
* grid_rows       Input        const int   row value for current grid 
* grid_cols       Input        const int   column value for current grid 
* radar           Input        floar **    two-dimensional dsp radar data
* pGeoData        Input        geo_data_struct  geo data
* pGageArray      Input        gage_table_struct  info from gage table
* pMPEParams      Input        mpe_params_struct  static parameters
* pGageRadarPairTable
*                 Output       gage_radar_pair_table_struct *
*                                          positive gage/radar pair data
*   
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* LatLongToScaledHrap
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
* Date        Developer     Action
* March 2005  Guoxian Zhou  finish conversion to C Language
* 09/18/2006  Guoxian Zhou  Modified for dynamic hrap grid
*
***********************************************************************/

void pairGageRadar(const radarLoc_record_struct * pRadarLocRecord,
                   const int grid_rows,
                   const int grid_cols,
                   short ** radarMiscBins, 
                   float ** radar,
                   const geo_data_struct * pGeoData ,
                   const gage_table_struct * pGageArray ,
                   const empe_params_struct * pMPEParams ,
                   gage_radar_pair_table_struct * pGageRadarPairTable)
{

    double row, col ; 
    int hrap_x, hrap_y, tmp_hrap_x, tmp_hrap_y ;
    int local_hrap_x, local_hrap_y ;
    int pair_num ;
    int i ;
	
	const double min_gr_value_bias =
			(double)pMPEParams->ptrRWBiasStat->min_gr_value_bias ;

    /*      
     * get scaled hrap coordinates of radar
     */

    double factor = (double)grid_rows / (double)NUM_DPA_ROWS;

    LatLongToScaledHrap ( pRadarLocRecord->latitude , 
                    pRadarLocRecord->longitude ,
                    factor, &row, &col ) ;

    /*      
     * find lower left corner of grid_rows * grid_cols radar grid in
     * national hrap coordinates
     */

    hrap_x = (int)(col - grid_rows / 2) ;
    hrap_y = (int)(row - grid_cols / 2) ;

    /*      
     * find lower left corner of 131x131 radar grid in
     * local coordinate which start at array
     * location (0,0) in lower left
     */

    local_hrap_x = hrap_x - pGeoData->hrap_x ;
    local_hrap_y = hrap_y - pGeoData->hrap_y ;

    pair_num = 0 ;

    for(i = pGageArray->pseudoGageNum;
        i < pGageArray->totalGageNum;
        i++)
    {
        if(pGageArray->ptrGageRecords[i].gageValue 
            <= min_gr_value_bias)
        {
        	continue ;
        }

        tmp_hrap_x = pGageArray->ptrGageRecords[i].hrap_x - local_hrap_x ;
        tmp_hrap_y = pGageArray->ptrGageRecords[i].hrap_y - local_hrap_y ;

        if(tmp_hrap_x < 0)
        {
        	continue ;
        }

        if(tmp_hrap_x >= grid_rows)
        {
        	continue ;
        }

        if(tmp_hrap_y < 0)
        {
        	continue ;
        }

        if(tmp_hrap_y >= grid_cols)
        {
        	continue ;
        }

        if(radarMiscBins[tmp_hrap_y][tmp_hrap_x] == 0)
        {
        	continue ;
        }

        if((double)radar[tmp_hrap_y][tmp_hrap_x] <=
            min_gr_value_bias)
        {
        	continue ;
        }

        /*      
         * match collocated radar rainfall
         * exclude pseudo gages from consideration
         */

        pGageRadarPairTable->ptrGageRadarPair[pair_num].hrap_x
            = pGageArray->ptrGageRecords[i].hrap_x ;
        pGageRadarPairTable->ptrGageRadarPair[pair_num].hrap_y
            = pGageArray->ptrGageRecords[i].hrap_y ;
        pGageRadarPairTable->ptrGageRadarPair[pair_num].gageValue
            = pGageArray->ptrGageRecords[i].gageValue ;
        pGageRadarPairTable->ptrGageRadarPair[pair_num].radarValue
            = (double)radar[tmp_hrap_y][tmp_hrap_x] ;

        pair_num ++ ;
    }

    pGageRadarPairTable->pairNum = pair_num ;

    /*      
     * print out the number of positive gage-radar pairs
     */

    if(pair_num > 0)
    {
        sprintf ( message , "*** gage-radar pairs ***"
            "\n\tgage value\tradar value");
        hpe_fieldgen_printMessage( message);

        for(i = 0 ; i < pair_num ; i++)
        {
            sprintf ( message , "\t%10.2f\t %10.2f", 
                pGageRadarPairTable->ptrGageRadarPair[i].gageValue, 
                pGageRadarPairTable->ptrGageRadarPair[i].radarValue ) ;
            hpe_fieldgen_printMessage( message);        
        }
    }
    else
    {
        sprintf ( message , "STATUS: no additional gage-radar pairs.");
        hpe_fieldgen_printMessage( message);        
    }
}
