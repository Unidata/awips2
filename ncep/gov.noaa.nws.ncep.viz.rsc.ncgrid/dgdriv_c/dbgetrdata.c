#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include "imgdef.h"

void db_getRData ( char *dbHost, char *fileName, float *rdata, int *nword,
                   int *iret )
/************************************************************************
 *                                                                      *
 * void db_getRData                                                	*
 *                                                                      *
 * This function fills the imgData array by reading image data from     * 
 * AWIPS II database given the location of the file on the server.	*
 *                                                                      *
 * void db_getRData ( char *dbHost, char *fileName, float *rdata, 	*
 *		      int *nword, int *iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *dbHost         char            Database host                   *
 *      *fileName       char            Name of the file on the server	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *rdata          float           Data				*
 *      *nword          int             size of rdata array             *
 *      *iret           int             return code:                    *
 *                                      -1: data acquisition error	* 
 *                                      -2: binary data src unsupported	*
 *                                       0: normal                      *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * m.gamazaychikov/CWS  01/10   Created                                 *
 ************************************************************************/
{
    int      chars_read = 0, ii, istmax, ier;
    int      fdatsize = 300000000;
    int      max_n;
    int      max_nlength = 20;
    char     fileAddress1[8]  = "http://";
    char     fileAddress2[25] = ":8080/uEngineProducts/";
    char     fileAddress[100];
    char     popncmd[1024];
    char     *fBuffer;
    char     **starr;
    FILE     *read_fp;
/*---------------------------------------------------------------------*/

    *iret = 0;
    sprintf(fileAddress,"%s", fileAddress1);
    strcat (fileAddress, dbHost);
    strcat (fileAddress, fileAddress2);
    strcat (fileAddress, fileName);
    sprintf (popncmd, "%s %s", "curl -s", fileAddress);

    if ( strcmp ( eSrc, "GRID")   == 0 ) {
       cst_numb (gDattim, &max_n, &ier);
      /*
       * Initialize the dataBuffer string, open the pipe, 
       * and read data from the pipe
       */
       G_MALLOC ( fBuffer, char, fdatsize+1, "db_getRData:dataBuffer" );
       memset (fBuffer, '\0', sizeof(fBuffer));
       read_fp = popen(popncmd, "r");
       if ( read_fp != NULL ) {
          chars_read = fread(fBuffer, sizeof(char), fdatsize, read_fp);
          while (chars_read > 0) {
             fBuffer[chars_read - 1] = '\0';
             chars_read = fread(fBuffer, sizeof(char), fdatsize, read_fp);
          }
          pclose(read_fp);
       }
       starr = (char **)malloc((size_t)max_n * sizeof(char *));
       for( ii=0; ii < max_n; ii++ )
          starr[ii] = (char *)malloc( max_nlength * sizeof(char));
       cst_clst (fBuffer, ';', " ", max_n, max_nlength, starr, &istmax, &ier);
       if ( istmax <= 1 ) {
          *iret = -2;
          return;
       }
      /*
       * Convert the data values:
       * 1. GRID  - no conversion
       */
        
       for ( ii = 0; ii < istmax; ii++ ) {
           cst_crnm(starr[ii], &rdata[ii], &ier);
       }
       *nword = istmax;
       for ( ii = 0; ii < max_n; ii++ ) free( starr[ii] );
       if( starr ) free( (char **)starr );
       G_FREE ( fBuffer, char );
    }
    else {
       *iret = -1;
       return;
    }
    return;
}
