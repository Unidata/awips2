
/**********************************************************************
 * buildLocalHrap ( )
 * 
 * This function creates local HRAP value based on
 * latitude/longitude. And check if the calculated HRAP value is 
 * within the geo boundary. 
 *  
 * MODULE #         DATE            PROGRAMMER          DESCRIPTION/REASON
 *  1               Feb 16 2006     Guoxian Zhou        First version
 * 
 *********************************************************************/

#include "dqc_preproc_setup.h"

int buildLocalHrap(const geo_data_struct * pGeoData,
                   double lat, double lon,
                   int * irow, int * icol)
{
    double row, col;

    /*
     * Retrieve the HRAP coordinates for this gage.
     */
    LatLongToHrapByReference ( lat , lon , &row , &col ) ;

    /*
     * Truncate to find integer HRAP coordinates which gage is in.
     * Translate the origin to the lowerleft corner of the MPE
     * estimation domain, that is, convert from the global to the 
     * local HRAP grid.
     */
    *irow = ( int )row ;
    *icol = ( int )col ;

    *irow -= pGeoData->hrap_y ;
    *icol -= pGeoData->hrap_x ;

    /* 
     * Test to determine whether or not this gage
     * is inside the office's MPE area. 
     */ 
    if (   ( *irow >= 0 )
        && ( *irow < pGeoData->num_rows )
        && ( *icol >= 0 )
        && ( *icol < pGeoData->num_cols ) )
        return 1;
    else
        return 0;

}

