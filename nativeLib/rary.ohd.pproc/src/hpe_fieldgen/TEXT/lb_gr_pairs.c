/*******************************************************************************
* FILENAME:            lb_gr_pairs.c
*
* Purpose:
* This function is converted from FORTRAN code: lb_gr_pairs.f.
* it stores positive gage/radar pair values or positive
* gage/satellite pair values for local bias calculations
* make copies of gage values and gage location arrays
* for positive pairs for local bias calculations.
*
* calling function: runLMosaic
* functions called: none
*
* input variables
*
* gr_min_value - min gage/radar value
*
* mosaic - two-dimensional radar data
*
* gageSize - the number of gage records
*
* iug - the hrap x_direction value of gage records
*
* ivg - the hrap y_direction value of gage records
*
* zg - the gage value of gage records
*
*
* output variables
*
* pGageRadarPairTable - array of positive gage/radar pair data
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*
********************************************************************************
*/

#include "empe_fieldgen.h"

void lb_gr_pairs ( float gr_min_value , 
                   const int gageSize,
                   short * iug ,
                   short * ivg ,
                   float * zg ,
                   double ** mosaic ,
                   gage_radar_pair_table_struct * pGageRadarPairTable)
{
    int i ;
    int npair = 0 ;
    for(i = 0; i < gageSize; i ++)
    {
        /* Check if the gage value is greater than the minimum 
           allowable (the cut) value. */

    	if ( zg[i] > gr_min_value )
    	{

              /* Check if the radar value corresponding to this 
                 gage location is also greater than the minimum
                 value. */

              if ( mosaic[ivg[i]][iug[i]] > gr_min_value )
              {
            	  pGageRadarPairTable->ptrGageRadarPair[npair].hrap_x = iug[i];
            	  pGageRadarPairTable->ptrGageRadarPair[npair].hrap_y = ivg[i];
            	  pGageRadarPairTable->ptrGageRadarPair[npair].gageValue=zg[i];
            	  pGageRadarPairTable->ptrGageRadarPair[npair].radarValue
                                                	 = mosaic[ivg[i]][iug[i]];

                  npair ++ ;
              }
         }
    }

    pGageRadarPairTable->pairNum = npair ;
}
