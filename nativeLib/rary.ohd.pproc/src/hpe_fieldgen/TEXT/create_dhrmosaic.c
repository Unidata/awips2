/***********************************************************************
* Filename: create_dhrmosaic.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 08/16/2006
*
* Development Group: HSEB/OHD
*
* Description:
* create DHR mosaic product
* 
* Modules:
* createDHRMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: createDHRMosaic
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 08/16/2006
* 
* Description:
*   This function creates DHR mosaic product.
*
*   calling function: runDHRMosaic 
*   functions called: LatLongToScaledHrap
*
* Calling Arguments:
* Name         Input/Output Type          Description
*
* radar        Input        float **      two-dimensional radar data
* pGeoData     Input        geo_data_struct *
*                                         geo data
* index        Input        const int     radar index
* RadarBeamHeight Input     double **     two-dimensional radar beam height
* miscBinArray Input        short **      two-dimensional misc bin data
* pRadarLocRecord
*              Input        radarLoc_record_struct *
*                                         record from radarLoc table
* mosaic       Output       double **     two-dimensional mosaic data
* height       Output       double **     two-dimensional radar height data
* ID           Output       int **        two-dimensional radar index data
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* 
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer     Action
* 8/16/2006   Guoxian Zhou  Build operational version 
*
***********************************************************************/

void createDHRMosaic(const radarLoc_record_struct * pRadarLocRecord,
                     const int dhr_rows,
                     const int dhr_cols,
                     float ** radar,
                     short ** miscBinArray,
                     const geo_data_struct * pGeoData ,
                     const int index ,                    
                     double ** RadarBeamHeight,
                     double ** height,
                        int ** ID,
                     double ** mosaic)
{

    double row, col, pixelHeight = 0 ; 
    int    intRow, intCol ;
    int    i, j;

    /*      
     * locate hrap pixel that radar is in
     */

    double factor = (double)dhr_rows / (double)NUM_DPA_ROWS;

    LatLongToScaledHrap ( pRadarLocRecord->latitude , 
                          pRadarLocRecord->longitude ,
                          factor, &row , &col ) ;

    intCol = (int)col ;
    intRow = (int)row ;

    /*
     * calculate starting hrap coordinants
     */

    for(i = 0; i < dhr_rows; i ++)
    {
        for(j = 0; j < dhr_cols; j ++)
        {

            /*      
             * check radar pixel to make sure it exists, 
             * this check ensures that the height array
             * is dynamic and varies with radar availabilty.
             */

            if(radar[i][j] < 0.0)
            {
                continue ;
            }

            /*      
             * compute location of radar pixel on national hrap grid,
             * then check to make sure this point is in rfc area.
             */

            int tmpRow = intRow - dhr_rows / 2 + i ;
            int tmpCol = intCol - dhr_cols / 2 + j ;

            /*
             * Retrieve the pixel height.
             */

            pixelHeight = RadarBeamHeight [ i ][ j ]; 
            pixelHeight += pRadarLocRecord->elevation ;

            /*      
             * check to make sure pixel height
             * is less than that on mosaic.
             *
             * convert x,y coordinates to local
             * coordinates of DHRMosaic
             * and MHeight array
             */

            tmpCol -= pGeoData->hrap_x ;
            tmpRow -= pGeoData->hrap_y ;

            if ((tmpCol < 0) || (tmpRow < 0))
            {
                continue ;
            }

            if ((tmpCol >= pGeoData->num_cols) ||
                (tmpRow >= pGeoData->num_rows))
            {
                continue ;
            }

            if (pixelHeight < height[tmpRow][tmpCol])
            {

                /*      
                 * check to make sure pixel is not a miscBinArray
                 * additional check for fake miscBinArray data.
                 */

                if(miscBinArray[i][j] == 1)
                {
                     mosaic[tmpRow][tmpCol] = (double)radar[i][j] ;
                         ID[tmpRow][tmpCol] = index ;
                     height[tmpRow][tmpCol] = pixelHeight ;
                }
            }
        }
    }
}
