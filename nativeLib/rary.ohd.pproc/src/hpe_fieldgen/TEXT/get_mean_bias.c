/***********************************************************************
* Filename: get_mean_bias.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: March 2005
*
* Development Group: OHD
*
* Description:
* Contains routine for getting the mean field bias value for each radar.
* 
* Modules:
* getMeanBias
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: getMeanBias
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: March 2005
*
* Description:
* This function is converted from FORTRAN code: get_bias.f.
* it gets the mean field bias value for each radar.
* 
* Calling Arguments:
* Name            Input/Output Type        Description
* pRadarLocRecord Input        const radarLoc_record_struct *
*                                          info from table radarloc
* datetime        Input        const char* date and time of current run
* grid_rows       Input        const int   row value for current grid 
* grid_cols       Input        const int   column value for current grid 
* radar           Input        float **    two-dimensional dsp radar data
* radarMiscBins   Input        short **    two-dimensional misc bin data
* pGeoData        Input        const geo_data_struct *
*                                          geo data
* pGageArray      Input        const gage_table_struct *
*                                          info from gage table
* pMPEParams      Input        const mpe_params_struct *
*                                          static parameters
* dualpol_data_avail Input     int         0/1 if dual pol data avialable
* meanBias        Output       double*     mean bias data array
* memSpanBias     Output       double*     memory span bias data
* gageRadarPairNum Output      int *       gage/radar pair number
*  
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* pairGageRadar, calculateMeanBias
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
* 9/28/2006   Guoxian Zhou  Modified for dynamic grid for empe
* 07/2013     JingtaoD      modified for dual pol
***********************************************************************/

void getMeanBias(const radarLoc_record_struct * pRadarLocRecord,
                 const char * datetime ,
                 const int grid_rows,
                 const int grid_cols,
                 short ** radarMiscBins , 
                 float ** radar ,
                 const geo_data_struct   * pGeoData ,
                 const gage_table_struct * pGageArray ,
                 const empe_params_struct * pMPEParams ,
		         int   dualpol_data_avail ,
                 double * meanBias,
                 double * memSpanBias,
                 int *  gageRadarPairNum)
{
    gage_radar_pair_table_struct * pGageRadarPairTable = NULL ;

    *gageRadarPairNum = 0 ;
    *meanBias = 0 ;
	*memSpanBias = 0 ;

    /*      
     * allocate memory for gage radar pair struct data
     */

    const int pairSize 
        = pGageArray->totalGageNum - pGageArray->pseudoGageNum ; 

    pGageRadarPairTable
        = (gage_radar_pair_table_struct *) malloc(
          sizeof(gage_radar_pair_table_struct) ); 

    if(pGageRadarPairTable == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in get_mean_bias function."
            "\n\tProgram exit.");
        shutdown( message);
    }

    pGageRadarPairTable->ptrGageRadarPair = 
        (gage_radar_pair_struct *) malloc ( pairSize
         * sizeof(gage_radar_pair_struct)); 

    if(pGageRadarPairTable->ptrGageRadarPair == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in getMeanBias function."
            "\n\tProgram exit.");
        shutdown( message);
    }

    pairGageRadar(pRadarLocRecord, 
                  grid_rows, grid_cols,
                  radarMiscBins, radar,
                  pGeoData, pGageArray, pMPEParams,
                  pGageRadarPairTable) ;

    *gageRadarPairNum = pGageRadarPairTable->pairNum ;

    /*      
     * qc the gage/radar pairs and calculate mean field bias.
     */

    calculateMeanBias(pRadarLocRecord->radarID, datetime,
                pMPEParams, pGageArray, pGageRadarPairTable,
                dualpol_data_avail, meanBias, memSpanBias ) ;

    /*
     * release memory for gage radar pair struct data
     */

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
