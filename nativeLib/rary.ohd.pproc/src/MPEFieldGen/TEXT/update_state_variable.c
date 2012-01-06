/*******************************************************************************
* FILENAME:            update_state_variable.c
*
* Purpose:
* This function is converted from FORTRAN code: update_state_var.f.
* it updates state variables via exponential smoothing 
*
* calling function: calculateMeanBias
* functions called: none
*
* input variables
*
* pMPEParams - static parameters
*
* mem_span - array of memory spans
*
* lag - number of hours elapsed since the last bias update
*
* pGageRadarPairTable - array of positive gage/radar pair data
*
* sumGage	- old array of estimates of spatial average of
*             positive gage rainfall
*
* sumRadar	- old array of estimates of spatial average of
*             positive radar rainfall
*
* num_pairs	- old array of Fisher information content (i.e., the effective
*			  number of cumulative positive radar-gage pairs)
*
* output variables
*
* sumGage	- updated array of estimates of spatial average of positive gage rainfall
*
* sumRadar	- updated array of estimates of spatial average of positive radar rainfall
*
* num_pairs	- updated array of Fisher information content (i.e., the effective
*			  number of cumulative positive radar-gage pairs)
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*	10/22/1998   D-J Seo           first version
*   02/18/2005   Guoxian Zhou      finish conversion to C Language 
*
********************************************************************************
*/

#include "mpe_fieldgen.h"

void MPEFieldGen_updateStateVariable(const mpe_params_struct * pMPEParams ,
				const float mem_span[] ,
				const int lag ,
				const gage_radar_pair_table_struct * pGageRadarPairTable , 
				double sumGage[] ,
				double sumRadar[] ,
				double num_pairs[] )
{
	/**      
	 * compute sample estimates of spatial averages of
	 * positive gage and radar rainfall
	 **/
	const int npair = pGageRadarPairTable->pairNum ;
	double sumg, sumr ;
	int i;
	
	sumg = 0.0 ;
	sumr = 0.0 ;
	if (npair >= pMPEParams->ptrRWBiasStat->npair_svar_update)
	{
		for(i = 0; i < npair; i++)
		{
			sumg += pGageRadarPairTable->ptrGageRadarPair[i].gageValue ;
			sumr += pGageRadarPairTable->ptrGageRadarPair[i].radarValue ;
		}
	}
	
	for(i = 0; i < pMPEParams->ptrRWBiasStat->num_span; i++)
	{
		/**      
		 * Update the Fisher information content if there are enough pairs.
		 * If there are not enough pairs the state variables will not be updated
		 * and persistence will be assumed
		 **/
		num_pairs[i] = exp(-(1.0 / mem_span[i]) * lag) * num_pairs[i] + npair ;

		if (npair >= pMPEParams->ptrRWBiasStat->npair_svar_update)
		{
			/**      
			 * update the estimates of spatial averages of
			 * positive gage and radar rainfall
			 **/
			sumGage[i] = (1.0 - npair / num_pairs[i]) * sumGage[i]
						 + sumg / num_pairs[i] ;
			sumRadar[i] = (1.0 - npair / num_pairs[i]) * sumRadar[i]
						 + sumr / num_pairs[i] ;
		}
	}
}
