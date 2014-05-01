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
*   Sep   2007   Paul Tilles       add check for max value
*   Nov   2007   Bryon Lawrence    Changed variables used in max value check to ints.
*
********************************************************************************
*/

#include <limits.h>

#include "mpe_fieldgen.h"
#include "write_xmrg.h"

static short * out_mosaic = NULL;

static size_t get_record_size ( num_cols )
{
   size_t record_length;
   record_length = ( num_cols * sizeof ( short ) ) + 2 * sizeof ( int ) ;
   return record_length;
}

void MPEFieldGen_writeArrayConstructor ( const geo_data_struct * pGeoData )
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
         shutDownMPE ( message, logFile );
      }
   }
}

void MPEFieldGen_writeArrayDestructor ( const geo_data_struct * pGeoData )
{

   if ( out_mosaic != NULL )
   {
      free ( out_mosaic );
      out_mosaic = NULL;
   }
}

void MPEFieldGen_writeArray( const geo_data_struct * pGeoData ,
		 const char *	filedir,
                 const char *   filename,
		 const double	factor ,
	 	 const int	replace_missing ,
		 const char *	user ,
		 const time_t	tRunTime ,
		 const char *	proc_flag ,
		 double **	real_mosaic ,
		 long int *	irc )
{
        static char filePath[PATH_LEN] = {'\0'} ;
        int byte_count;
	register int i;
        register int j;
        short int * pRecord = NULL;
        register int value;
        size_t record_length;
 
        sprintf(filePath, "%s/%s", filedir, filename );

        if ( out_mosaic == NULL )
        {
           /* Call the constructor. */
          MPEFieldGen_writeArrayConstructor ( pGeoData );      
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
              value = (int) (real_mosaic[i][j] * factor + 0.5) ;

              if ( ( replace_missing == 1 ) && ( value < 0 ) )
              {
                 * pRecord = 0;
              }
              else if (value > SHRT_MAX)
              {
                 * pRecord = SHRT_MAX;
              }
              else
              {
                 * pRecord = (short)value;
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

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/write_array.c,v $";
 static char rcs_id2[] = "$Id: write_array.c,v 1.3 2007/11/01 17:28:22 lawrence Exp $";}
/*  ===================================================  */

}
