/***********************************************************************
* Filename: read_decoded_dpr.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: August 12, 2006
*
* Development Group: OHD/HSEB
*
* Description:
* Contains routine for reading decoded dpr radar data.This is
* based on original readDecodedDHR function. When the dual pol DPR
* product is not available, then tyr to use DHR product.
* 
*
* Modules:
* readDecodedDPR
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: readDecodedDPR
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
* filename     Input        const char *  DPR file name
* radar        Output       float **      DPR data array
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
* 05/2012     Jingtao Deng  For dual pol DPR product
***********************************************************************/

void readDecodedDPR(const char * filename,
                    float ** radar ,
                    int * status)
{
    int i, j;
    int head[6];

    static char dpr_grid_dir[PATH_LEN] = {'\0'} ;
    char file_path[PATH_LEN] = {'\0'} ;

    static int first = 0;

    int num_records = 0;

    FILE * fp = NULL;
    FILE * dBugFile = NULL;

    *status = 0 ;

    /*
     * load the token value only once
     */

    if( first == 0)
    {
	    if(hpe_fieldgen_getAppsDefaults("dpr_grid_dir", dpr_grid_dir) == -1)
	    {
	        sprintf ( message , "ERROR: Invalid token value"
	            " for token \"dpr_grid_dir\"."
	            "\n\tProgram exit.") ;
	        shutdown( message);
	    }
	    first = 1;
    }

    sprintf(file_path, "%s/%s", dpr_grid_dir, filename );

    if((fp = fopen(file_path, "rb")) == NULL)
    {
       sprintf ( message , "ERROR:in read_decoded_dpr, "
            "cannot open input file: %s"
            "\n\tProgram exit.", file_path) ;
       printLogMessage(message) ;
       hpeDeleteLogFile();
       CloseDbms();
       exit(-1);
    //   shutdown( message);

    }
    
    /* For debug purposes only */
 /*   if((dBugFile = fopen("DPR_HPEFieldgenInput", "w")) == (FILE *) NULL)
    {       
        printf ("ERROR: cannot open file 'DPR_HPEFieldgenInput'\n");
        return;
    }*/
     
    
    /*
     * read the head info (end_date, end_time and opmode )
     */

    fread(head, sizeof(int), 6, fp);
 
 /*   fprintf (dBugFile, "head1=%d, head2=%d, head3=%d, head4=%d, head5=%d, head6=%d",
             head[0], head[1], head[2], head[3], head[4], head[5]);
    fprintf(dBugFile, "\n");	 */    
    
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
        
/*    for(j = 0; j < NUM_DHR_ROWS; j++ )
    {
        for(i = 0; i< NUM_DHR_COLS; i++)
        {
	    fprintf(dBugFile, "%6.2f", radar[i][j]);	    
	}   
	fprintf(dBugFile, "\n");  
    }
    
    fclose(dBugFile);
    dBugFile = NULL; */
    
    fclose(fp);
    fp = NULL;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc/src/hpe_fieldgen/RCS/read_decoded_dpr.c,v $";
 static char rcs_id2[] = "$Id: read_decoded_dpr.c,v 1.1 2012/09/12 18:03:49 deng Exp $";}
/*  ===================================================  */

}

