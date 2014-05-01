/*******************************************************************************
* FILENAME:             Swap4Bytes.c
* CII NUMBER:
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*    MODULE 1:          Swap4Bytes_
* DESCRIPTION:          Reverses the ordering of the bytes in each 4-byte word
*                       of an integer array.
*
* ORIGINAL AUTHOR:      Bill Mattison / Bryon Lawrence
* CREATION DATE:        March 26, 2001 
* ORGANIZATION:         GSC / MDL
* MACHINE:              HP 9000 / xx
*
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        3/26/2001    Bryon Lawrence	   Original Coding
*
********************************************************************************
*/

#include <stddef.h>
#include "Swap4Bytes.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   Swap4Bytes
*
* PURPOSE:       This routine reverses the ordering of the bytes in each 4-byte
*                word of an integer array.  For example consider the following
*                4-byte word whose bytes contain the characters 'A', 'B', 
*                'C', and 'D':
*
*                  byte 1   byte 2   byte 3   byte 4 
*                  A        B        C        D
*
*                This routine will reverse the ordering of these bytes in this
*                4-byte word so that the contents of each byte of the word 
*                will appear as follows:
*
*                  byte 1   byte 2   byte 3   byte 4 
*                  D        C        B        A
*
*                The need for this routine arises from differences in
*                memory architecture across different computer platforms.
*                The two memory configurations that need to be accomodated
*                are the "Big Endian" and "Little Endian" architectures.
*
*                In the "Big Endian" architecture, the left-most byte in a
*                word is the most significant byte.  In the "Little Endian"
*                architecture, the right-most byte in a word is the most
*                significant byte.
*
*                As another example, consider a 4-byte integer which contains
*                the value 66.  On a "Big Endian" system, the binary pattern
*                in a word would appear as follows:
*
*                00000000 00000000 00000000 01000010
*
*                On a "Little Endian" system, the binary pattern would 
*                appear as follows:
*
*                01000010 00000000 00000000 00000000
*
*                This routine ensures that a GRIB2 message will be accurately
*                decoded regardless of the memory architecture of the computer
*                that it is being decoded on.
*
* ARGUMENTS:
*   TYPE           DATA TYPE   NAME         DESCRIPTION/UNITS
*   Input/Output   int *       p_data       A pointer to an array of integer
*                                           values. 
*   Input          size_t *    num_elements A pointer to a size_t value
*                                           containing the number of elements
*                                           in the p_data array.
*
* RETURNS:
*   None.
*
* APIs UTILIZED:
*   None.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME         DESCRIPTION
*   signed char *   p_d          Pointer to the current element in p_data being
*                                processed
*   signed char *   p_t          Pointer to the temporary variable containing
*                                the original value of the current element
*                                in p_data being processed.
*   int             temp         Contains the original value of the 
*                                current element in p_data being processed.
*   unsigned int    i            Loop indexing variable.
*   unsigned int    k            Loop Indexing variable.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   None.
*
* NOTES:
*   The "_" underscore at the end of this routine's name was intentional.
*   This has been done to make linking this routine in Fortran code compiled
*   with the pgf90 compiler on Linux less cumbersome.
*
********************************************************************************
*/

void Swap4Bytes_ ( void * p_data , const size_t * num_elements )
{

  signed char * p_d = NULL ;
  signed char * p_t = NULL ;
  int temp;
  unsigned int i;
  unsigned int k;

  p_d = ( signed char * ) p_data - 1; 

  for ( k = 0 ; k < *num_elements ; ++k )
  {
     temp = *( ( int * ) p_data + k );
     p_t = ( signed char * ) ( &temp ) + 4;

     for  ( i = 0 ; i < 4 ; ++i )
     {
       *(++p_d) = *(--p_t);
     }
  }

}
