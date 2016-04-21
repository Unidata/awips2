/***********************************************************************
* Filename: load_radar_Beam_Height.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: December 06, 2006
*
* Development Group: OHD/HSEB
*
* Description:
* Contains routine for reading radar beam height data.
* 
*
* Modules:
* loadRadarBeamHeight
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: loadRadarBeamHeight
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: December 06, 2006
*
* Description:
*   This subroutine reads the array containing the radar beam height values
*
* Calling Arguments:
* Name                  Input/Output Type          Description
* radar_bean_height     Output       doduble **    height array
* grid_rows             input        const int     grid row value
* grid_cols             input        const int     grid column value
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
* void
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
* 12/06/2006  Guoxian Zhou  First version
*
***********************************************************************/

void loadRadarBeamHeight(double ** radar_bean_height ,
                         const int grid_rows,
                         const int grid_cols)
{
    static const char * fname = "mpe_radar_beam_height";
    char beamheight_dir [PATH_LEN] = {'\0'};
    static char * empe_beamheight_dir_token = "rfcwide_beamheight_dir";
    char station_file_path [ PATH_LEN + FNAME_LEN ] = {'\0'};

    FILE * fp = NULL;
    int    i, j;
    int    length;
    int    status;

    double *  orig_beam = NULL;
    double ** source_beam = NULL;

    orig_beam = init1DDoubleArray(HEIGHT_DEFAULT, NUM_DPA_ELEMENTS);

    source_beam = init2DDoubleArray(HEIGHT_DEFAULT, NUM_DPA_ROWS,NUM_DPA_COLS);

    hpe_fieldgen_getAppsDefaults ( empe_beamheight_dir_token, beamheight_dir );
    length = strlen ( beamheight_dir ); 

    if ( length > 0 )
    {
       sprintf ( station_file_path, "%s/%s", beamheight_dir, fname );
    }
    else
    {
       sprintf ( message, "Token %s is no defined. Application will look "
                          "for the radar beam height file in the current "
                          "working directory.\n", 
                          empe_beamheight_dir_token );
       hpe_fieldgen_printMessage( message);
       strcpy ( station_file_path, fname ); 
    }

    /*
     * Write a log message indicating that the height file
     * is about to be read. 
     */

    sprintf ( message, "radar height file = %s", station_file_path );
    hpe_fieldgen_printMessage( message);

    /*
     * Attempt to open the height data file.
     */

    fp = fopen ( station_file_path, "r" );

    if ( fp == NULL )
    {
       sprintf ( message, "Could not open file %s. "
                          "Create radar height file and rerun "
                          "EMPE Fieldgen.\n\tProgram exit.", 
                          station_file_path );
       shutdown( message );
    }

    status = fread ( orig_beam, sizeof(double),
                     NUM_DPA_ELEMENTS, fp );

    fclose ( fp );
    fp = NULL ;

    if ( status != NUM_DPA_ELEMENTS )
    {
       sprintf ( message, "Error reading radar height file %s.\n"
                          "\tProgram exit.", station_file_path );
       shutdown( message );
    }

    /*      
     * Convert radar beam height array to current hrap grid
     */

    int index ;
    for(i = 0; i < NUM_DPA_ROWS; i++)
    {
        for(j = 0; j < NUM_DPA_COLS; j++)
        {
            index = i * NUM_DPA_COLS + j;
            source_beam[i][j] = orig_beam[index] ;
        }
    }

    convertDoubleArray(NUM_DPA_ROWS,
                       NUM_DPA_COLS,
                       source_beam,
                       HEIGHT_DEFAULT,
                       grid_rows,
                       grid_cols,
                       radar_bean_height );

    free1DDoubleArray( orig_beam );

    free2DDoubleArray( source_beam, NUM_DPA_ROWS );
}

