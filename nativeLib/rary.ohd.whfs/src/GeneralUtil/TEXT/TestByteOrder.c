/*******************************************************************************
* FILENAME:             TestByteOrder.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           TestByteOrder
* DESCRIPTION:          Given a known 4-byte value in a file and the position
*                       of the word it is located in relative to the beginning
*                       of the file, this routine tests the value to determine
*                       if the bytes in the file need to be "flipped" while
*                       being read in.
*
*                       Example:
*                          In a standard xmrg file the second 4 byte field in
*                          the first record file contains the value of "XOR",
*                          the HRAP x coordinate of the southwest corner of
*                          the HRAP grid.  MPE and Hmap_mpe initially retrieve
*                          this value from the the coord file for the site
*                          these applications are running for.  Knowing this
*                          value, whenever these programs read a xmrg file,
*                          they can compare it to the "XOR" value contained
*                          in field 2 of the first record.  If they are equal,
*                          then the byte ordering in the file (the Endianess)
*                          is compatible with the operating system the
*                          program is running on.  If the two values are not
*                          equal, then there two are possibilities:
*
*                              1) The byte ordering in the file is not
*                                 compatible with the memory architecture
*                                 of the operating system.  The bytes need
*                                 to be properly swapped while being read
*                                 in order to make the data understandable
*                                 to the program.  The bytes in the word
*                                 read in from the file are flipped.  If the
*                                 resulting value matches the XOR value
*                                 read from the coord.dat file, then this is
*                                 known to be the case. 
*
*                              2) If the value of the swapped bytes in the word
*                                 does not match the XOR value from the 
*                                 coord.dat file, then the file is 
*                                 incompatible with the site it is being read
*                                 at.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         May 24, 2002
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell Redhat Linux     
* MODIFICATION HISTORY:  
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        5/24/2002    Bryon Lawrence    Original Coding
********************************************************************************
*/
#include <string.h>
#include <stdio.h>

#include "Swap4Bytes.h"
#include "TestByteOrder.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    TestByteOrder
* PURPOSE:        Given a known 4-byte value in a file and the position
*                 of the word it is located in relative to the beginning
*                 of the file, this routine tests the value to determine
*                 if the bytes in the file need to be "flipped" while
*                 being read in.
*
*                 Example:
*                    In a standard xmrg file the second 4 byte field in
*                    the first record file contains the value of "XOR",
*                    the HRAP x coordinate of the southwest corner of
*                    the HRAP grid.  MPE and Hmap_mpe initially retrieve
*                    this value from the the coord file for the site
*                    these applications are running for.  Knowing this
*                    value, whenever these programs read a xmrg file,
*                    they can compare it to the "XOR" value contained
*                    in field 2 of the first record.  If they are equal,
*                    then the byte ordering in the file (the Endianess)
*                    is compatible with the operating system the
*                    program is running on.  If the two values are not
*                    equal, then there two are possibilities:
*
*                        1) The byte ordering in the file is not
*                           compatible with the memory architecture
*                           of the operating system.  The bytes need
*                           to be properly swapped while being read
*                           in order to make the data understandable
*                           to the program.  The bytes in the word
*                           read in from the file are flipped.  If the
*                           resulting value matches the XOR value
*                           read from the coord.dat file, then this is
*                           known to be the case. 
*
*                        2) If the value of the swapped bytes in the word
*                           does not match the XOR value from the 
*                           coord.dat file, then the file is 
*                           incompatible with the site it is being read
*                           at.
*
* ARGUMENTS:
*   TYPE   DATA TYPE             NAME          DESCRIPTION/UNITS
*   Input  char *                filename      The name of the file to retrieve
*                                              the test value from.
*   Input  void *                value         A pointer to the four byte 
*                                              value to be used in the
*                                              comparison with the four byte
*                                              value read in from the 
*                                              file.
*   Input  int *                 word_position The position of the word in the
*                                              file containing the value to
*                                              be tested.  Positions are
*                                              counted starting with "1".
*   Output enum TestByteResult * result        The result specifying the
*                                              action that must be taken
*                                              in order to process the data
*                                              in the file.
* RETURNS:
*   void
*
* APIs UTILIZED:
*   NAME             HEADER FILE      DESCRIPTION
*   _Swap4Bytes      Swap4Bytes.h     Swaps the ordering of 4 bytes in a word
*                                     from Big Endian to Little Endian or
*                                     vice versa.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME                DESCRIPTION
*   FILE *        pFile               The file pointer.
*   int           i                   Loop index.
*   int           value_original      The value passed into this routine
*                                     to be compared to the value read in
*                                     from the file.
*   int           value_from_file     The value read in from the file to
*                                     be compared to the original value.
*   size_t        num_elements        The number of 4 byte elements that
*                                     the "Swap4Bytes_" routine needs to
*                                     process.
*
* DATA FILES AND/OR DATABASE:
*   This routine relies on the existence and readability of the file 
*   passed in via the "filename" argument.
*
* ERROR HANDLING:
*   This routine returns a value of "FlipTestFailed" in the "result"
*   calling argument if one of the following occurs:
*
*      1) The filename parameter is NULL.
*      2) The filename parameter has a zero length.
*      3) The value parameter has a zero length. 
*      4) The file cannot be opened.
*      5) The specified value in the file does not match the user-supplied
*         value even after byte swapping.
********************************************************************************
*/
void TestByteOrder_ ( const char * filename , 
                      const void * value ,
                      const int * word_position ,
                      enum TestByteResult * result )
{
   FILE * pFile = NULL ;
   int i ;
   int value_original ;
   int value_from_file ;
   const size_t num_elements = 1 ;

   value_original = * ( int * ) value ;

   /* Test to make sure that a filename has been passed in, it has
      a non-zero length, the value is not NULL, and the word position
      is not NULL. */
   if ( ( filename == NULL ) || ( strlen ( filename ) == 0 ) ||
        ( value == NULL ) || ( word_position == NULL ) )
   {
      /*fprintf ( stderr , "\nIn routine 'TestByteOrder_':\n"
                         "A NULL parameter was passed into this\n"
                         "function.\n" ) ;*/
      * result = FlipTestFailed ;
   }
   else
   {
      /* Attempt to open the file. */
      pFile = fopen ( filename , "r" ) ;

      if ( pFile == NULL )
      {
         /*fprintf ( stderr , "\nIn routine 'TestByteOrder_':\n"
                            "Could not open file %s.\n" , filename ) ;*/
         * result = FlipTestFailed ;
      }
      else
      {
         fread ( & value_from_file , sizeof ( int ) , 1 , pFile ) ;

         for ( i = 1 ; ( i < * word_position ) && ! feof ( pFile )  ; ++ i )
         {
            fread ( & value_from_file , sizeof ( int ) , 1 , pFile ) ;
         }

         fclose ( pFile ) ;

         if ( i != * word_position )
         {
            /*fprintf ( stderr , "\nIn routine 'TestByteOrder_':\n"
                               "An error was encountered reading file %s.\n" ,
                               filename ) ;*/
            * result = FlipTestFailed ;
         }
         else
         {
            /*fprintf ( stderr , "In file %s:\n", filename );
            fprintf ( stderr , "Original value: %d\n" , value_original ) ;
            fprintf ( stderr , "Value from file: %d\n" , value_from_file ) ;*/

            if ( value_from_file == value_original )
            {
               * result = DontFlipBytes ;
            }
            else
            {
               Swap4Bytes_ ( & value_from_file , & num_elements ) ;
               /*fprintf ( stderr , "Value from file flipped: %d\n" , 
                         value_from_file ) ;*/


               if ( value_from_file == value_original )
               {
                  * result = FlipBytes ;
               }
               else
               {
                  * result = FlipTestFailed ;
               }
                 
            }
            
         }
          
      }

   }
      

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
