/***********************************************************************
* Filename: read_decoded_dhr.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: August 12, 2006
*
* Development Group: OHD/HSEB
*
* Description:
* Contains routine for reading decoded dhr radar data.
* 
*
* Modules:
* readDecodedDHR
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: readDecodedDHR
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: August 12, 2006
*
* Description:
*   This subroutine reads the array containing the decoded dhr radar values
*
* Calling Arguments:
* Name         Input/Output Type          Description
* filename     Input        const char *  DHR file name
* radar        Output       float **      DHR data array
* status       Output       int *         File read status
*
* Required
* None
*
* Required Files/Databases:
* None
*
* calling function: readDHRData
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
* 8/12/2006   Guoxian Zhou  First version
*
***********************************************************************/

void readDecodedDHR(const char * filename,
                    float ** radar ,
                    int * status)
{
    int i;
    short head[3];

    static char dhr_grid_dir[PATH_LEN] = {'\0'} ;
    char file_path[PATH_LEN] = {'\0'} ;

    static int first = 0;

    int num_records = 0;

    FILE * fp = NULL;

    *status = 0 ;

    /*
     * load the token value only once
     */

    if( first == 0)
    {
	    if(hpe_fieldgen_getAppsDefaults("dhr_grid_dir", dhr_grid_dir) == -1)
	    {
	        sprintf ( message , "ERROR: Invalid token value"
	            " for token \"dhr_grid_dir\"."
	            "\n\tProgram exit.") ;
	        shutdown( message);
	    }
	    first = 1;
    }

    sprintf(file_path, "%s/%s", dhr_grid_dir, filename );

    if((fp = fopen(file_path, "rb")) == NULL)
    {
       sprintf ( message , "ERROR:in read_decode_dhr, "
            "cannot open input file: %s"
            "\n\tProgram exit.", file_path) ;
       shutdown( message);
    }

    /*
     * read the head info (end_date, end_time and opmode )
     */

    fread(head, sizeof(short), 3, fp);
    
    /*
     * read the radar data array
     */

    for(i = 0; i < NUM_DHR_ROWS; i++)
    {
        num_records = fread(radar[i], sizeof(float), NUM_DHR_COLS, fp);
    
        if( num_records != NUM_DHR_COLS )
        {
            sprintf ( message , " loss of data encountered -- "
                                " number of records expected = %d"
                                " number of records found = %d",
                                NUM_DHR_COLS, num_records) ;
	        hpe_fieldgen_printMessage( message);
    
            *status = -1;
            return;
        }
    }

    fclose(fp);
    fp = NULL;
}

