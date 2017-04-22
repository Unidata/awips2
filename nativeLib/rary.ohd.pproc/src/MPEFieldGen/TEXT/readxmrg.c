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

#include "mpe_fieldgen.h"
#include "read_xmrg.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"

extern void read_xmrg ( const int * mx , const int * my , const int * irow ,
                 const char * fname , const int * length , int * ifile ,
                 short * mosaic ) ;

void MPEFieldGen_readxmrg(const char * os, const int rowSize, const int colSize,
    const char * fname, const int lenf, const double factor,
    double ** xmrg , int * irc)
{
    int i, j ;
    short int * mosaic = NULL ;
    int num_bytes = 16;
    int num_elements ;
    int word_position = 1;
    enum TestByteResult result = DontFlipBytes;

    mosaic = (short *)malloc(colSize * sizeof(short));

    if(mosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in readxmrg function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    /**
    *  note:  mfactor = 100  could be a problem for large
    *         precip  accumulations (> 12 in)
    *         because resulting value could be > 32000
    *         (max for I*2)
    *
    **/

    /* Test to determine if the bytes in this file need to be flipped. */
    TestByteOrder_ ( fname, & num_bytes, & word_position, & result );

    if ( result == FlipTestFailed )
    {
       sprintf ( message , "ERROR: Couldn't determine byte ordering of %s. ", fname );
       printMessage( message, logFile );
       free(mosaic) ;
       mosaic = NULL ;
       return ;
    }


    for(i = 0; i < rowSize; i ++)
    {
        read_xmrg(&colSize, &rowSize, &i,
            fname, &lenf, irc, mosaic) ;

        if( result == FlipBytes )
        {
            num_elements = colSize ;

            Swap2Bytes_(mosaic, (size_t *)&num_elements) ;
        }

        if(*irc != 0)
        {
            sprintf ( message , "ERROR: reading file %s. ", fname );
            printMessage( message, logFile );
            free(mosaic) ;
    	    mosaic = NULL ;
            return ;
        }

        for(j = 0; j < colSize; j ++)
            xmrg[i][j] = mosaic[j] * 1.0 / factor ;
    }

    free(mosaic) ;
    mosaic = NULL ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
