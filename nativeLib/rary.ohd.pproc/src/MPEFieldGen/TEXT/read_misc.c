/*******************************************************************************
* FILENAME:            read_misc.c
*
* Purpose:
* This function is converted from FORTRAN code: rdmisc.f.
* it reads miscellaneous input data
*
* calling function: run_rmosaic
* functions called: Swap2Bytes_
*
* input variables
*
* radarID - radar id
* os -  operate system
*
* output variables
*
* radarMiscBins - two-dimensional array of misbin data
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2005   Guoxian Zhou      finish conversion to C Language
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"

void MPEFieldGen_readMisc(const radarLoc_table_struct * pRadarLocTable,
              const char * os,
              short int ** radarMiscBins )
{
    enum TestByteResult result = DontFlipBytes;
    FILE * fp = NULL;
    static char misbinDir[PATH_LEN] = {'\0'};
    static char filename[FNAME_LEN] = {'\0'};
    static int first = 1 ;
    int  i;
    /*int num_elements = NUM_DPA_ELEMENTS;*/
    const size_t num_elements = NUM_DPA_ELEMENTS;

    int num_dpa_bytes = NUM_DPA_ELEMENTS * 2;
    int status;
    int word_position = 1;

    /**
     * read radar missing bins array
     * if file not found, then use default array of all 1
     **/
    if ( first == 1 )
    {
        getAppsDefaults("rfcwide_misbin_dir", misbinDir) ;
        first = 0 ;
    }

    for ( i = 0; i < pRadarLocTable->radarNum; ++i )
    {
       sprintf(filename, "%s/misbin.%s", misbinDir,
               pRadarLocTable->ptrRadarLocRecords [ i ].radarID );

       /* Determine if the MISBIN file is Big Endian or Little Endian. */
       TestByteOrder_ ( filename, &num_dpa_bytes, & word_position, & result );

       if ( result == FlipTestFailed )
       {
          sprintf ( message , "STATUS: Could not determine byte ordering of\n"
                              "file %s -- default array used.",
                              filename);
          printMessage(message, logFile);
          memset ( radarMiscBins [ i ], 1,
                   NUM_DPA_ELEMENTS * sizeof ( short ) );
       }
       else
       {

          if((fp = fopen(filename, "rb")) != NULL)
          {
             /* Need to skip the first 4 bytes.  These are a relic
                of FORTRAN and contain the number of bytes of data
                in the file. */
             fseek ( fp, sizeof ( int ), SEEK_SET );

             status = fread ( radarMiscBins[i], sizeof(short int),
                              NUM_DPA_ELEMENTS, fp);

             if ( status != NUM_DPA_ELEMENTS )
             {
                sprintf ( message, "STATUS: Error reading misbin file %s.\n"
                                   "Default array used.", filename );
                printMessage ( message, logFile );
                memset ( radarMiscBins [ i ], 1,
                         NUM_DPA_ELEMENTS * sizeof ( short ) );
             }
             else
             {

                /* Don't need to read the last 4 bytes in the MISBIN file.
                   This contains the same value as the first 4 bytes in the
                   file. */
                fclose(fp);
                fp = NULL ;

                /**
                 * The misbin file is assumed Big Endian.
                 * The bytes must be swapped.
                 **/
                if ( result == FlipBytes )
                {
                   Swap2Bytes_ (radarMiscBins[i], &num_elements) ;
                }
             }
          }
          else
          {
             sprintf ( message , "STATUS: misbin data loading failure.\n"
                                 "File %s not found -- default array used.",
                                 filename);
             printMessage(message, logFile);

             memset ( radarMiscBins [ i ], 1,
                      NUM_DPA_ELEMENTS * sizeof ( short ) );
          }
       }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/read_misc.c,v $";
 static char rcs_id2[] = "$Id: read_misc.c,v 1.1 2007/10/15 12:19:12 dsa Exp $";}
/*  ===================================================  */

}
