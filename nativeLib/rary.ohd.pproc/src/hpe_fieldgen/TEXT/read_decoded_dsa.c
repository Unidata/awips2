/***********************************************************************
* Filename: read_decoded_dsa.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: August 15, 2006
*
* Development Group: OHD
*
* Description:
* Contains routine for reading decoded dsa dual pol radar data. This is
* based on original readDecodedDSP function. When the DSA product is not 
* available, then tyr to use DSP product.
* Modules:
* readDecodedDSA
*
* 
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: readDecodedDSA
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: August 15, 2006
*
* Description:
*   This subroutine reads the array containing the decoded dsa radar values
*
* Calling Arguments:
* Name         Input/Output Type          Description
* filename     Input        const char *  DSA file name
* radar        Output       float **      DSA data array
* status       Output       int *         File read status
*
* Required
* None
*
* Required Files/Databases:
* None
*
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
* Modified b
* Feb. 2012   Jingtao Deng    - dual pol product DSA
***********************************************************************/

void readDecodedDSA(const char * filename,
                    float ** radar ,
                    int * status)
{
    int i;
    short head[6];
    
    static char dsa_grid_dir[PATH_LEN] = {'\0'} ;
    static char file_path[PATH_LEN] = {'\0'} ;

    int num_records = 0;

    FILE * fp = NULL;

    *status = 0 ;

    if(hpe_fieldgen_getAppsDefaults("dsa_grid_dir", dsa_grid_dir) == -1)
    {
        sprintf ( message , "ERROR: Invalid token value"
            " for token \"dsa_grid_dir\"."
            "\n\tProgram exit.") ;
        shutdown( message);
    }

    sprintf(file_path, "%s/%s", dsa_grid_dir, filename );

    if((fp = fopen(file_path, "rb")) == NULL)
    {
       sprintf ( message , "ERROR:in read_decoded_dsa, "
            "cannot open input file: %s"
            "\n\tProgram exit.", file_path) ;
     //  shutdown( message);
       printLogMessage(message) ;
       hpeDeleteLogFile();
       CloseDbms();
       exit(-1);
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

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc/src/hpe_fieldgen/RCS/read_decoded_dsa.c,v $";
 static char rcs_id2[] = "$Id: read_decoded_dsa.c,v 1.1 2012/09/12 18:04:44 deng Exp $";}
/*  ===================================================  */

}

