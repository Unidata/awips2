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
*
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include "rfcwide.h"
#include "HydroStatus.h"

extern HydroStatus read_mpe_coordinate_file ( int * xor, int * yor, int * maxx,
                                       int * maxy ); 

void MPEFieldGen_readGeoData(geo_data_struct* pGeoData)
{
    int xor, yor, maxx, maxy;
    HydroStatus hydroStatus ;
    
    hydroStatus = read_mpe_coordinate_file(&xor, &yor, &maxx, &maxy);
    if(hydroStatus == HydroStatus_OK)
    {
        pGeoData->hrap_x = xor;
        pGeoData->hrap_y = yor;
        pGeoData->num_cols = maxx;
        pGeoData->num_rows = maxy;
        sprintf ( message , "\nSTATUS: loading geographic data...\n"
            "\thrap_x  hrap_y  num_cols  num_rows\n"
            "\t%6d  %6d  %8d  %8d\n", 
            pGeoData->hrap_x, pGeoData->hrap_y,
            pGeoData->num_cols, pGeoData->num_rows) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf ( message , "ERROR: loading geo data failure.") ;
        shutDownMPE( message, logFile );
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
