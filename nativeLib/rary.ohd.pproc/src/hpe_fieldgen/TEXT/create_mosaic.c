/***********************************************************************
* Filename: create_mosaic.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 09/12/2006
*
* Development Group: HSEB/OHD
*
* Description:
* This function is converted from FORTRAN code: mosaic.f.
* it creates mosaics of the raw radar data and radar height data.
* 
* Modules:
* createMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: createMosaic
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 09/12/2006
* 
* Description:
*   This function creates DSP mosaic product.
*
*
* Calling Arguments:
* Name         Input/Output Type          Description
*
* radar        Input        float **      two-dimensional radar data
* pGeoData     Input        geo_data_struct * 
* index        Input        const int     radar index
* RadarBeamHeight Input     double **     two-dimensional radar beam height
* miscBinArray Input        short **      two-dimensional misc bin data
* pRadarLocRecord
*              Input        radarLoc_record_struct*
*                                         record from radarLoc table
* RMosaic      Output       double **     two-dimensional mosaic data
* MHeight      Output       double **     two-dimensional radar height data
* radarID      Output       int    **     two-dimensional radar index data
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
* 9/12/2006   Guoxian Zhou  Build operational version 
*
***********************************************************************/

void createMosaic(const radarLoc_record_struct * pRadarLocRecord,
                  const int radar_rows,
                  const int radar_cols,
                  float ** radar ,
                  short ** miscBinArray,
                  const geo_data_struct * pGeoData ,
                  const int index ,                    
                  double ** RadarBeamHeight,
                  double ** RMosaic ,
                  double ** MHeight,
                  int    ** radarID,
                  double ** MaxMosaic,
                  double ** AvgMosaic,
                  int    ** AvgMosaicNumRadars,
                  int    *  blnMosaic)
{

    double    row, col, pixelHeight = 0 ; 

    int intRow, intCol ;
    int i, j ;

    /*
     * locate hrap pixel that radar is in
     */

    double factor = (double)radar_rows / (double)NUM_DPA_ROWS;

    LatLongToScaledHrap ( pRadarLocRecord->latitude , 
                    pRadarLocRecord->longitude ,
                    factor, &row , &col ) ;

    intCol = (int)col ;
    intRow = (int)row ;

    /*
     * calculate starting hrap coordinants
     */

    for(i = 0; i < radar_rows; i ++)
    {
        for(j = 0; j < radar_cols; j ++)
        {

            /*
             * check radar pixel to make sure it exists, 
             * this check insures that the height array
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

            int tmpRow = intRow - radar_rows / 2 + i ;
            int tmpCol = intCol - radar_cols / 2 + j ;

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
             * coordinates of RMosaic
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

            if ((blnMosaic[ermosaic] == 1) &&
                (pixelHeight < MHeight[tmpRow][tmpCol]))
            {
                /*      
                 * check to make sure pixel is not a radarMiscBins
                 * additional check for fake radarMiscBins data.
                 */

                if(miscBinArray[i][j] == 1)
                {
                    RMosaic[tmpRow][tmpCol] = (double)radar[i][j] ;
                    MHeight[tmpRow][tmpCol] = pixelHeight ;
                    radarID[tmpRow][tmpCol] = index ;
                }
            }

            // additional condition added by Ram for selective generation of
            // mosaics modification in the following line of code
            // Added by Ram for the average and max mosaic calculations
            
            // code added to calculate the average and max mosaics
            // the following code just adds up the values for the average
            // mosaic from each contributing radar. this is later on used
            // by the code in the run_rmosaic.c to calculate the real average
            // by taking into account how many radars actually contributed.
            // for the max mosaic, end of the radar loop would produce the
            // final result
            // -------------------------------

            if(miscBinArray[i][j] == 1)
            {
                if(blnMosaic[avgermosaic] == 1 && radar[i][j] >= 0.0)
                {
                    if(AvgMosaic[tmpRow][tmpCol] >= 0.0)
                    {
                        AvgMosaic[tmpRow][tmpCol] += (double)radar[i][j];
                    }
                    else
                    {
                        AvgMosaic[tmpRow][tmpCol] = (double)radar[i][j];
                    }
                    (AvgMosaicNumRadars[tmpRow][tmpCol])++;
                }

                if(blnMosaic[maxermosaic] == 1 &&
                   MaxMosaic[tmpRow][tmpCol] < (double)radar[i][j])
                {
                    MaxMosaic[tmpRow][tmpCol] = (double)radar[i][j];    
                }
            }
        }
    }
}
