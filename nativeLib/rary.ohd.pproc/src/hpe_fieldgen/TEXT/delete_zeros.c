/*******************************************************************************
* FILENAME:            delete_zeros.c
*
* Purpose:
* This function is converted from FORTRAN code: delete_zeros.f.
* it excludes zero-value gage reports where radar mosaic is positive.
*
* calling function: runGageonly, runLMosaic, runMMosaic, runLSatpre
* functions called: none
*
* input variables
*
* gageSize - the number of gage records
*
* iug - the hrap x_direction value of gage records
*
* ivg - the hrap y_direction value of gage records
*
* zg - the gage value of gage records
*
* mosaic - array of mosaic data
*
* output variables
*
* gageSize - the number of gage records
*
* iug - the hrap x_direction value of gage records
*
* ivg - the hrap y_direction value of gage records
*
* zg - the gage value of gage records
*
*   HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   May 2005     Guoxian Zhou      finish conversion to C Language 
*
********************************************************************************
*/

#include "empe_fieldgen.h"

void deleteZeros( int * gageSize,
                  short  * iug, short * ivg, float * zg,  
                  double ** mosaic)
{
    int i ;
    int count = 0 ;
    
    for(i = 0; i < *gageSize ; i ++ )
    {
        if((zg[i] >  0.0 ) || (mosaic[ivg[i]][iug[i]] <= 0.0))
        {
            iug[count] = iug[i] ;
            ivg[count] = ivg[i] ;
             zg[count] = zg[i] ;

            count ++ ;
        }
    }

    *gageSize = count ;

}
