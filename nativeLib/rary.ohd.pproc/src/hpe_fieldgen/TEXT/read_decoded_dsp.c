/***********************************************************************
* Filename: read_decoded_dsp.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: August 15, 2006
*
* Development Group: OHD
*
* Description:
* Contains routine for reading decoded dsp radar data.
* 
*
* Modules:
* readDecodedDSP
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: readDecodedDSP
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: August 15, 2006
*
* Description:
*   This subroutine reads the array containing the decoded dsp radar values
*
* Calling Arguments:
* Name         Input/Output Type          Description
* filename     Input        const char *  DSP file name
* radar        Output       float **      DSP data array
* status       Output       int *         File read status
*
* Required
* None
*
* Required Files/Databases:
* None
*
* calling function: readDSPRadar
*
* Non System Routines Called:
* getAppsDefaults
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
* 8/15/2006   Guoxian Zhou  First version
*
***********************************************************************/

void readDecodedDSP(const char * filename,
                    float ** radar ,
                    int * status)
{
    int i;
    short head[6];
    
    static char dsp_grid_dir[PATH_LEN] = {'\0'} ;
    static char file_path[PATH_LEN] = {'\0'} ;

    int num_records = 0;

    FILE * fp = NULL;

    *status = 0 ;

    if(hpe_fieldgen_getAppsDefaults("dsp_grid_dir", dsp_grid_dir) == -1)
    {
        sprintf ( message , "ERROR: Invalid token value"
            " for token \"dsp_grid_dir\"."
            "\n\tProgram exit.") ;
        shutdown( message);
    }

    sprintf(file_path, "%s/%s", dsp_grid_dir, filename );

    if((fp = fopen(file_path, "rb")) == NULL)
    {
       sprintf ( message , "ERROR:in read_decoded_dsp, "
            "cannot open input file: %s"
            "\n\tProgram exit.", file_path) ;
       shutdown( message);
    }

    /*
     * read the head info
     * (start_date, start_time, opmode )
     * (end_date, end_time, opmode )
     */

    fread(head, sizeof(short), 6, fp);

    /*
     * read the radar data array
     */

    for(i = 0; i < NUM_DSP_ROWS; i++)
    {
        num_records = fread(radar[i], sizeof(float), NUM_DSP_COLS, fp);
    
        if( num_records != NUM_DSP_COLS )
        {
            sprintf ( message , " loss of data encountered -- "
                                " number of records expected = %d"
                                " number of records found = %d",
                                NUM_DSP_COLS, num_records) ;
            hpe_fieldgen_printMessage( message);
    
            *status = -1;
            return;
        }
    }

    fclose(fp);
    fp = NULL;
}

