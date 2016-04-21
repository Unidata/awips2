/*******************************************************************************
* FILENAME:            readxmrg.c
*
* Purpose:
* This function is converted from FORTRAN code: readxmrg.f
* This function reads in prism data
*
* calling function: get_climate
* functions called: read_xmrg_file, Swap2Bytes_
*
* input variables
*
* output variables
*
* xmrg - the xmrg data array.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      convert to C Language 
*
*********************************************************************************/

#include "empe_fieldgen.h"
#include "read_xmrg.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"

extern void read_xmrg ( const int * mx , const int * my , const int * irow ,
                 const char * fname , const int * length , int * ifile ,
                 short * mosaic ) ;

void readxmrg(const char * os, const int rowSize, const int colSize, 
    const char * fname, const int lenf, const double factor,
    double ** xmrg , int * irc)
{
    int i, j ;
    short int * mosaic = NULL ;
    int num_bytes = 16;
    int num_elements ;
    int word_position = 1;
    enum TestByteResult result = DontFlipBytes; 

    mosaic = init1DShortArray(MOSAIC_DEFAULT, colSize);

    /*
     * note:  factor = 100  could be a problem for large
     *        precip  accumulations (> 12 in)
     *        because resulting value could be > 32000
     *        (max for I*2)
     * 
     */

    /* Test to determine if the bytes in this file need to be flipped. */
    TestByteOrder_ ( fname, & num_bytes, & word_position, & result );

    if ( result == FlipTestFailed )
    {
       sprintf ( message , "ERROR: Couldn't determine PRISM byte ordering. "
                 " array of 1.0 used.") ;
       hpe_fieldgen_printMessage(message);
       free1DShortArray(mosaic);
       return ;
    }

    for(i = 0; i < rowSize; i ++)
    {
        read_xmrg(&colSize, &rowSize, &i,
            fname, &lenf, irc, mosaic) ;

        /*
         * the file read is assumed Big Endian
         * if Linux, then swap bytes
         * 
         * misbin and prism files are delivered
         * to sites in Big Endian format
         * if running on Linux, bytes in misbin
         * and prism files are swapped
         */

        if( result == FlipBytes )
        {
            num_elements = colSize ;
            Swap2Bytes_(mosaic, &num_elements) ;
        }

        if(*irc != 0)
        {
            sprintf ( message , "ERROR: reading XMRG array failure. ") ;
            hpe_fieldgen_printMessage( message );

            free1DShortArray(mosaic);
            return ;
        }

        for(j = 0; j < colSize; j ++)
        {
            xmrg[i][j] = mosaic[j] * 1.0 / factor ;
        }
    }

    free1DShortArray(mosaic); 

}
