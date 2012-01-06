/*******************************************************************************
* FILENAME:            write_array.c
*
* Purpose:
* This function is converted from FORTRAN code: writearray.f.
*
* calling function: 
* functions called: writeXmrg
*
* input variables
const *
* parameter list with const qualifiers
*
* output variables
*
* irc -- i/o operation status
*
*   HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2005   Guoxian Zhou      finish conversion to C Language 
*
********************************************************************************
*/

#include "empe_fieldgen.h"

static short * out_mosaic = NULL;

/*******************************************************************************
* FILENAME:            write_xmrg.c
*
* Purpose:
* This function is converted from FORTRAN code: write_xmrg.f
* it writes to an xmrg format file.
*
* calling function: writeArray
* functions called: none
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
*   March 2005   Guoxian Zhou      finish conversion to C Language 
*
********************************************************************************
*/

static void writeXmrg( const geo_data_struct * pGeoData ,
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
    float  vernum = VERNUM ;
    static char saveDatetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'};
    static char validDatetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    struct tm * pRunTime = NULL ;
    int    byteCount;

    *irc = 0 ;

    memset(saveDatetime, '\0', ANSI_YEARSEC_TIME_LEN + 1);
    memset(validDatetime, '\0', ANSI_YEARSEC_TIME_LEN + 1);

    pRunTime = gmtime(&tRunTime) ;
    strftime ( saveDatetime, ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:%M:00", pRunTime ) ;
    strftime ( validDatetime, ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:%M:00", pRunTime ) ;

    if((fp = fopen(filename, "wb")) == NULL)
    {
       *irc = -1;
       sprintf ( message , "ERROR:in write_xmrg, "
            "cannot open output file: %s"
            "\n\tProgram exit", filename) ;
       shutdown( message);
    }

    /* Write the xmrg header. */
    byteCount = 4 * sizeof(int) ;
    fwrite(&byteCount, sizeof(int), 1, fp);
    fwrite(&pGeoData->hrap_x, sizeof(int), 1, fp);
    fwrite(&pGeoData->hrap_y, sizeof(int), 1, fp);
    fwrite(&pGeoData->num_cols, sizeof(int), 1, fp);
    fwrite(&pGeoData->num_rows, sizeof(int), 1, fp);
    fwrite(&byteCount,    sizeof(int), 1, fp);

    byteCount = 66 ;
    fwrite(&byteCount, sizeof(int), 1, fp);
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

static size_t get_record_size ( num_cols )
{
   size_t record_length;
   record_length = ( num_cols * sizeof ( short ) ) + 2 * sizeof ( int ) ;
   return record_length;
}

void writeArrayConstructor ( const geo_data_struct * pGeoData )
{
   size_t record_length;

   /* Only do this if memory has not been dynamically allocated
      for the output mosaic. */
   if ( out_mosaic == NULL )
   {
      /* Compute the number of columns plus 8 extra bytes to contain 
         record length information. */
      record_length = get_record_size ( pGeoData->num_cols ); 
                     
      out_mosaic = ( short * ) malloc ( pGeoData->num_rows * 
                                        record_length );

      if ( out_mosaic == NULL )
      {
         sprintf ( message, "ERROR: memory allocation failure in write"
                            " array constructor function."
                            "\n\tProgram exit.");
         shutdown( message );
      }
   }
}

void writeArrayDestructor ( const geo_data_struct * pGeoData )
{

   if ( out_mosaic != NULL )
   {
      free ( out_mosaic );
      out_mosaic = NULL;
   }
}

void writeArray( const geo_data_struct * pGeoData ,
                 const char * filedir,
                 const char * filename,
                 const double factor ,
                 const int    replace_missing ,
                 const char * user ,
                 const time_t tRunTime ,
                 const char * proc_flag ,
                 double **    real_mosaic ,
                 long int *   irc )
{
    static char filePath[PATH_LEN] = {'\0'} ;
    int byte_count;
    register int i;
    register int j;
    short int * pRecord = NULL;
    register short int value;
    size_t record_length;
 
    sprintf(filePath, "%s/%s", filedir, filename );

    if ( out_mosaic == NULL )
    {
        /* Call the constructor. */
        writeArrayConstructor ( pGeoData );      
    }

    /* Determine the byte_count. */
    byte_count = pGeoData->num_cols * sizeof ( short );

    /* Determine the record length. */
    record_length = get_record_size ( pGeoData->num_cols );

    pRecord = out_mosaic ;

    /* Copy the array to the output mosaic. */ 
    for ( i = 0; i < pGeoData->num_rows; i++)
    {
        memcpy ( pRecord, & byte_count, sizeof ( int ) ); 
        pRecord += 2;
 
        for ( j = 0; j < pGeoData->num_cols; j++)
        {
            value = (short int) (real_mosaic[i][j] * factor + 0.5) ;

            if ( ( replace_missing == 1 ) && ( value < 0 ) )
            {
                * pRecord = 0;
            }
            else
            {
                * pRecord = value;
            }

            ++ pRecord;
        }

        memcpy ( pRecord, & byte_count, sizeof ( int ) ); 
        pRecord +=2;
    }

    /* Write the mosaic data in a file in xmrg format. */
    writeXmrg (pGeoData, filePath, user, tRunTime, proc_flag,
               out_mosaic, record_length, irc) ;

    return ;
}
