/*******************************************************************************
* FILENAME:            ReadAndSwap.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          ReadandSwap
* DESCRIPTION:         Performs a smart read of a file with Big Endian byte
*                      ordering.  If this routine is being run on a Linux
*                      operating system, then, depending on the data type
*                      being read in by the user, the bytes will be flipped
*                      to emulate Little Endian byte ordering.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 16, 2001
* ORGANIZATION:        HSEB / OHD
* MACHINE:             HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE            PROGRAMMER        DESCRIPTION/REASON
*          1        Nov. 16, 2001   Bryon Lawrence    Original Coding
********************************************************************************
*/
#include <stdio.h>

#include "GetOS.h"
#include "ReadAndSwap.h"
#include "Swap2Bytes.h"
#include "Swap4Bytes.h"

/*******************************************************************************
* MODULE NUMBER:       1
* MODULE NAME:         ReadAndSwap
* PURPOSE:             This routine is a wrapper around the "C" library "fread"
*                      function.  It adds the additional logic of swapping
*                      the bytes of the data read in from a file if the data
*                      needs to be forced to conform to the memory architecture
*                      of the operating system that this application is being
*                      run on.
*
*                      This routine assumes that all data files have been
*                      created on HP-UX and therefore have a "Big Endian"
*                      byte ordering.  If this routine is being run on a 
*                      Linux platform, then it will swap the ordering of 
*                      the bytes in the file as it reads data.  This will
*                      ensure that the user of this routine will be returned
*                      meaningful and useful data from the file being read 
*                      regardless of the operating system being utilized.
*
*                      This routine will be able to handle the following
*                      data types:
*                         char 
*                         short integer
*                         integer
*                         long integer ( assuming it is four bytes long)
*                         float
*
*                      This routine will recognize and correctly process the
*                      unsigned counterparts of the above data types as well.
*                      This routine cannot handle 8 byte doubles or data 
*                      storage types that occupy 3 or 5 bytes of memory.
*
* ARGUMENTS:
*   TYPE          DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input/output  void *      ptr         A pointer to an allocated area
*                                         of memory or an array.  This will
*                                         receive the data values read in
*                                         from the file, so it must be made 
*                                         large enough to receive the number
*                                         of elements specified by "nitems"
*                                         below.
*   Input         size_t     size         The size in bytes of each individual
*                                         data element to be read in.
*   Input         size_t     nitems       The number of data elements of 
*                                         size "size" to read into 
*                                         the memory pointed to by the 
*                                         pointer "ptr".
*   Input         FILE *     stream       A pointer to the file stream.
*                                         The user is responsible for 
*                                         opening this stream before
*                                         calling this routine and closing
*                                         this stream after calling this
*                                         routine.
*
* RETURNS:
*   DATA TYPE   NAME                      DESCRIPTION
*   OperSys     os                        An enumerated value as defined in
*                                         GetOS.h is stored into this 
*                                         variable.
*   size_t      num_elements              The actual number of data elements
*                                         read in from the file.
*
* APIs UTILIZED:
*   NAME             HEADER FILE          DESCRIPTION
*   GetOS            GetOS.h              This routine determines the type
*                                         of the operating system that this
*                                         application is being run on.
*   Swap2Bytes_      Swap2Bytes.h         This routine changes the ordering
*                                         of bytes from Big Endian to Little
*                                         Endian or vice versa, depending 
*                                         on the operating system it it
*                                         being run on.  This is a General 
*                                         Utility.  It "swaps" the bytes in
*                                         2 byte words.
*   Swap4Bytes_      Swap4Bytes.h         This routine changes the ordering
*                                         of bytes from Big Endian to Little
*                                         Endian or vice versa, depending 
*                                         on the operating system it it
*                                         being run on.  This is a General 
*                                         Utility.  It "swaps" the bytes in
*                                         4 byte words.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME            DESCRIPTION
*   size_t     num_elements    The actual number of data elements read in
*                              from the file.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                    DESCRIPTION
*     READ_AND_SWAP_BAD_VALUE      An incorrect value for "size" has 
*                                  been supplied by the user.      
*     READ_AND_SWAP_IO_ERROR       An error was encountered reading from
*                                  the file represented by "stream."
*     READ_AND_SWAP_OS_ERROR       An error was encountered retrieving the
*                                  the type of operating system. 
*
*   ( These manifest constants are defined in the ReadAndSwap.h file. )
*
********************************************************************************
*/

size_t ReadAndSwap ( void * ptr , size_t size , size_t nitems , FILE * stream )
{
   size_t num_elements ;
   OperSys os ;

   /* Determine if the size of the user's data is valid. */
   if ( ( size != ( size_t ) 1 ) &&
        ( size != ( size_t ) 2 ) &&
        ( size != ( size_t ) 4 ) )
   {
      return ( size_t )  READ_AND_SWAP_BAD_VALUE ;
   }

   /* Read in the file.  Check to determine if there are any errors 
      encountered. */
   num_elements = fread ( ptr , size , nitems , stream ) ;

   if ( num_elements < nitems ) 
   {
      return ( size_t )  READ_AND_SWAP_IO_ERROR ; 
   }

   /* Determine the operating system. */
   os = GetOS ( ) ;

   if ( os == OS_ERROR )
   {
      return ( size_t )  READ_AND_SWAP_OS_ERROR ;
   }

   if ( os == OS_LINUX )
   {
      /* This is the Linux operating system. */
      /* If the data element size is 2 or 4 bytes then, swap the bytes. */
      if ( size == 2 )
      {
         Swap2Bytes_ ( ( short * ) ptr , & num_elements ) ;
      }
      else if ( size == 4 )
      {
         Swap4Bytes_ ( ( int * ) ptr , & num_elements ) ;
       
      }
   } 

   return num_elements ;
}
