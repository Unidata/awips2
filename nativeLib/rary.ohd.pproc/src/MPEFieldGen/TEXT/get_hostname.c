/*******************************************************************************
* FILENAME:            get_hostname.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          get_hostname
* DESCRIPTION:         Retrieves the name of the host system mpe_fieldgen
*                      is running on.  This is a C wrapper around the
*                      gethostname ( ) system utility which retrieves
*                      the name of the system the program is running on.
*                      This routine is designed to be called from FORTRAN.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       March 10, 2005
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1       3/10/2005     Bryon Lawrence    Original Coding
********************************************************************************
*/
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    get_hostname
* PURPOSE:        Retrieves the name of the host system on which mpe_fieldgen
*                 is running.  This is a C wrapper around the gethostname ( )
*                 system call which retrieves the name of the system the
*                 program is running on.  This routine is designed to be
*                 called from FORTRAN.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   In/Out char *      hostname             The hostname is written to this
*                                           character array supplied by the
*                                           calling routine.  This is space
*                                           padded.  It is assumed that
*                                           a FORTRAN routine will be calling 
*                                           this routine.
*   In     int *       len                  The number of characters the
*                                           hostname array can contain.
*
* RETURNS:
*   None   
*
* APIs UTILIZED:
*   Only system routines are utilized. 
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME      DESCRIPTION
*   char       host      Array into which the system name is read.
*   int        status    Contains the return code of the call to 
*                        gethostname. 
*   size_t     name_len  Contains the length of the host name returned
*                        by the call to gethostname ( ).
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    The hostname string will be blank if the hostname could not be retrieved
*    or does not exist or if there was not enough space provided for the
*    hostname string.
********************************************************************************
*/
void MPEFieldGen_get_hostname ( char * hostname, const int * len )
{
   char host [ * len + 1 ];
   int status;
   size_t name_len;

   /* Initialize the supplied hostname array to contain spaces. */
   memset ( hostname, ' ', (size_t) * len);

   /* Initialize the host array to contain '\0'. */
   memset ( host, '\0', (size_t) * len + 1);

   /* Retrieve the name of the host mpe_fieldgen is running on. */
   status = gethostname ( host,  (size_t) * len);

   if ( status == 0 )
   {
      /* gethostname did not report an error. */
      name_len = strlen ( host ) ; 

      /* Copy but exclude the terminating '\0'. This in effect
         space pads the result of the call to gethostname which makes
         it FORTRAN friendly. */
      strncpy ( hostname, host, name_len );
   }

   return;
}
