
/*******************************************************************************
* FILENAME:            mpe_write_xmrg.c
*
* Purpose:
* This function is converted from FORTRAN code: write_xmrg.f
* it writes to an xmrg format file.
*
* input variables
*
* parameter list with const identifier
*
* output variables
*
* irc -- i/o operation status
*
*   HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
* May 26, 2006   Guoxian Zhou      Original Coding
*
********************************************************************************
*/

#include "mpe_write_xmrg.h"

void writeXmrg( const geo_data_struct * pGeoData ,
               const char * filename ,
               const char * user ,
               const time_t tRunTime ,
               const char * proc_flag ,
               const short * out_mosaic ,
               size_t record_length,
               long int * irc)
{
    FILE * fp = NULL;

    int    maxval = MAX_VALUE ;
    float    vernum = VERNUM ;
    static char    saveDatetime[ANSI_YEARSEC_TIME_LEN + 1];
    static char    validDatetime[ANSI_YEARSEC_TIME_LEN + 1] ;
    struct tm * pRunTime = NULL ;
    int    byteCount;

    *irc = 0 ;

    memset(saveDatetime, '\0', ANSI_YEARSEC_TIME_LEN + 1);
    memset(validDatetime, '\0', ANSI_YEARSEC_TIME_LEN + 1);

    pRunTime = gmtime(&tRunTime) ;
    strftime ( saveDatetime, ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:00:00", pRunTime ) ;
    strftime ( validDatetime, ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:00:00", pRunTime ) ;

    if((fp = fopen(filename, "wb")) == NULL)
	{
       *irc = -1;
       printf ("ERROR:in write_xmrg, "
        	"cannot open output file: %s"
        	"\n\tProgram exit", filename) ;
       exit(-1);
	}

    /* Write the xmrg header. */
    byteCount = 4 * sizeof(int) ;
    fwrite(&byteCount,    sizeof(int), 1, fp);
    fwrite(&pGeoData->hrap_x,    sizeof(int), 1, fp);
    fwrite(&pGeoData->hrap_y,    sizeof(int), 1, fp);
    fwrite(&pGeoData->num_cols,    sizeof(int), 1, fp);
    fwrite(&pGeoData->num_rows,    sizeof(int), 1, fp);
    fwrite(&byteCount,    sizeof(int), 1, fp);

    byteCount = 66 ;
    fwrite(&byteCount,    sizeof(int), 1, fp);
    fwrite(user, sizeof(char), 10, fp);
    fwrite(saveDatetime, sizeof(char), 
           ANSI_YEARSEC_TIME_LEN + 1, fp);
    fwrite(proc_flag, sizeof(char), 8, fp);
    fwrite(validDatetime, sizeof(char),
       ANSI_YEARSEC_TIME_LEN + 1, fp);
    fwrite(&maxval, sizeof(int) , 1, fp);
    fwrite(&vernum, sizeof(float), 1, fp);
    fwrite(&byteCount, sizeof(int), 1, fp);

    /* The writing of the record is complete. Write out the 
       body of the xmrg file. */
    fwrite ( out_mosaic, 1, 
             pGeoData->num_rows * record_length, fp );

    /* Close the file. */
    fclose ( fp );
    fp = NULL;
}
