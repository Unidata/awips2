/*******************************************************************************
* FILENAME:            TestXmrg.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          TestXmrgOS_
* DESCRIPTION:         This routine tests the Operating System upon which 
*                      a xmrg formatted file was created.
*   MODULE 2:          TestXmrgByteOrder_
* DESCRIPTION:         This routine determines whether or not bytes being
*                      read in from an Xmrg file need to be swapped to conform
*                      to the memory architecture that this routine is being
*                      run on.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       May 30, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE/OS:          HP-UX / Dell Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        5/30/2002    Bryon Lawrence    Created.
*          2        5/30/2002    Bryon Lawrence    Created.
*
********************************************************************************
*/
#include <stdio.h>

#include "GetOS.h"
#include "TestByteOrder.h"
#include "TestXmrg.h"

/*******************************************************************************
* MODULE NUMBER:       1
* MODULE NAME:         TestXmrgOS_
* PURPOSE:             This routine tests the Operating System upon which
*                      a xmrg-formatted file was created.  This is accomplished
*                      by simply looking at the first 2 bytes in the second
*                      record of the file.  If these bytes spell out "HP" or
*                      "hp", then the file was created on a HP workstation
*                      running Unix.  If these bytes spell out "LX" or "lx",
*                      then the file was created on a computer with a Linux
*                      operating system.  If these two bytes do not spell out
*                      a recognizeable string, then an error is returned. 
*
*                      This routine has been implemented in a fashion that
*                      allows it to be called from both C and Fortran.
*                      It has been assumed that the XMRG file has been created
*                      using Fortran I/O routines.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

const int NUM_BYTES_TO_SKIP = 28 ;
const int LENGTH_OF_IDENTIFIER = 2 ; 

void TestXmrgOS_ ( const char * filename , OperSys * oper )
{
   char garbage [ NUM_BYTES_TO_SKIP ] ;
   char identifier [ LENGTH_OF_IDENTIFIER ] ;
   FILE * pFile = NULL ;

   * oper = OS_ERROR ;

   if ( filename == NULL || * filename == '\0' || oper == NULL )
   {
      return ;
   }

   pFile = fopen ( filename , "r" ) ;

   if ( pFile == NULL )
   {
      return ;
   }

   fread ( garbage , NUM_BYTES_TO_SKIP , 1 , pFile ) ;

   if ( feof ( pFile ) )
   {
      fclose ( pFile ) ;
      return ;
   }

   fread ( identifier , LENGTH_OF_IDENTIFIER , 1 , pFile ) ;

   if ( feof ( pFile ) )
   {
      fclose ( pFile ) ;
      * oper = OS_ERROR ;
      pFile = NULL ;
      return ;
   }

   if ( * identifier == 'H' || * identifier == 'h' )
   {
      if ( * ( identifier + 1 ) == 'P' || * ( identifier + 1 ) == 'p' ) 
      {
         * oper = OS_UNIX ;
      }
   }
   else if ( * identifier == 'L' || * identifier == 'l' )
   {
      if ( * ( identifier + 1 ) == 'X' || * ( identifier + 1 ) == 'x' )
      {
         * oper = OS_LINUX ;
      }
   }

   fclose ( pFile ) ;
   pFile = NULL ;

}
/*******************************************************************************
* MODULE NUMBER: TestXmrgByteOrder_ 
* MODULE NAME:   2
* PURPOSE:       This routine .
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void TestXmrgByteOrder_ ( const char * filename , const int * XOR ,
                          enum TestByteResult * result )
{
   const int word_position = 2 ;
   OperSys file_oper ;
   OperSys oper ;
   * result = FlipTestFailed ;

   if ( filename == NULL || * filename == '\0' || result == NULL )
   {
      return ;
   }

   oper = GetOS ( ) ;

   if ( oper == OS_ERROR )
   {
      return ;
   }
   
   TestXmrgOS_ ( filename , & file_oper ) ;

   if ( file_oper == OS_ERROR )
   {
      /* As an alternative test, try comparing the known XOR coordinate
         with the one that can be found in the XMRG file.  This may be
         useful for XMRG files that predate the addition of the "HP" and
         "LX" tags in the XMRG files. */
      TestByteOrder_ ( filename , XOR , & word_position , result ) ;

   }
   else
   {

      if ( ( ( oper == OS_LINUX ) && ( file_oper == OS_UNIX ) ) ||
           ( ( oper == OS_UNIX ) && ( file_oper == OS_LINUX ) ) )
      {
         * result = FlipBytes ;
      }
      else
      {
         * result = DontFlipBytes ;
      }

   }

} 
