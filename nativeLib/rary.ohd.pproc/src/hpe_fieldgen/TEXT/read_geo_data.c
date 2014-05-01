/*******************************************************************************
* FILENAME:            read_geo_data.c
*
* DESCRIPTION:         this function reads the geo data into geo_data_struct.
*                      calling subroutine: main_mpe_fieldgen
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         March 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   June 2005    Guoxian Zhou      Finish first version 
*  08/18/2006    Guoxian Zhou      Add hrap grid factor value 
*                                  for grid expansion  
*
********************************************************************************
*/

#include "HydroStatus.h"
#include "empe_fieldgen.h"
#include "rfcwide.h"

extern HydroStatus read_mpe_coordinate_file ( int * xor, int * yor, int * maxx,
                                       int * maxy ); 

void readGeoData(const int hrap_grid_factor,
                 geo_data_struct* pGeoData)
{
    int xor, yor, maxx, maxy;
    HydroStatus hydroStatus ;
    
    hydroStatus = read_mpe_coordinate_file(&xor, &yor, &maxx, &maxy);
    if(hydroStatus == HydroStatus_OK)
    {
        pGeoData->hrap_x   = xor  * hrap_grid_factor;
        pGeoData->hrap_y   = yor  * hrap_grid_factor;        	
        pGeoData->num_cols = maxx * hrap_grid_factor;
        pGeoData->num_rows = maxy * hrap_grid_factor;        	

        sprintf ( message , "\nSTATUS: loading geographic data...\n"
            "\thrap_x  hrap_y  num_cols  num_rows\n"
            "\t%6d  %6d  %8d  %8d\n", 
            pGeoData->hrap_x, pGeoData->hrap_y,
            pGeoData->num_cols, pGeoData->num_rows) ;
        hpe_fieldgen_printMessage( message );
    }
    else
    {
        sprintf ( message , "ERROR: loading geo data failure.") ;
        shutdown( message );
    }
}
