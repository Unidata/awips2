/*******************************************************************************
* FILENAME:             GetSuffix.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*           MODULE 1:   GetSuffix
*        DESCRIPTION:   This routine retrieves the contents of the "OS_SUFFIX"
*                       environmental variable.  This suffix can be used to
*                       determine which operating system (Linux or HP-UX)
*                       the WHFS application is running on.  This suffix can
*                       also be appended to the end of the name of a program
*
* ORIGINAL AUTHOR:      Bryon Lawrence 
* CREATION DATE:        October 3, 2001
* ORGANIZATION:         OHD/HSEB
* MACHINES:             HP / Linux Workstations
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        10/3/01      Bryon Lawrence    Original Coding
*
********************************************************************************
*/
#include <string.h>
#include "GeneralUtil.h"
#include "GetSuffix.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   GetSuffix
* PURPOSE:       This routine retrieves the contents of the "OS_SUFFIX"
*                environmental variable.  The primary purpose of this
*                variable is to indicate the suffix that needs to be 
*                appended onto the name of a binary file.  The need for
*                this arises from the fact that in build 5.2.1, the HP-UX
*                and Linux generated binaries are comingled into a common
*                directory.  They need a suffix to indicate which
*                executable can be ran from which operating system.
*
*                For example, if OS_SUFFIX is set as ".HP" for the HP-UX
*                operating system and ".LX" for the Linux operating 
*                system, then the db_purge binary will be installed into
*                the standard binary repository as "db_purge.HP" when built
*                on a HP workstation and "db_purge.LX" when built on a Linux
*                workstation.
*
*                It is important to note that the suffix returned by this
*                routine will have a "." prepended to it.  The caller of this
*                routine need not worry about suppying the ".".  All the
*                caller has to do is concatenate the returned suffix to
*                the name of his executable.
*
*                If no value can be found for "OS_SUFFIX", then an empty 
*                string, that is "", is returned to the caller of this
*                routine.
*
*                This routine relies on the "get_apps_defaults" routine
*                to retrieve the value of "OS_SUFFIX".  This means that
*                it will search the environment for a definition of 
*                "OS_SUFFIX" before rumaging through the token files.
*                However, "OS_SUFFIX" is only meant to be defined in the
*                environment and should not be tokenized.  In short, if
*                you need "OS_SUFFIX", it is up to you to make sure that it
*                is properly defined in your environment depending on the
*                operating system you are working on.
*
*                IMPORTANT!!!  This routine returns a constant pointer to a 
*                static local variable.  The value of this variable will
*                change with subsequent calls to this routine.
*                
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   char *      os_suffix                   Contains the suffix to use for
*                                           the operating system this routine
*                                           is being run on, or it will
*                                           contain an empty string ("") if
*                                           no definition can be found.
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*   get_apps_defaults ( )                   GeneralUtil.h
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   char       suffix [ ]                   The suffix string read from the
*                                           "OS_SUFFIX" environmental variable 
*                                           is placed into this array and
*                                           returned to the user.  This
*                                           array is MAX_SUFFIX_SIZE 
*                                           characters big.
*   char *     suffix_var_name              Contains the name of the
*                                           environmental variable to
*                                           retrieve the value of.
*   int        gad_token_len                Contains the length of "OS_SUFFIX",
*                                           the name of the environmental 
*                                           variable containing the suffix
*                                           to place on exectuables.  In this
*                                           case, it will be "9".
*   int        gad_value_len                "Get_apps_defaults" returns
*                                           the length of the suffix in
*                                           this variable.
*   int        status                       Contains a value indicating 
*                                           whether or not "get_apps_defaults"
*                                           worked.  "0" it worked; "1" it
*                                           didn't work.
*                                           
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   If "OS_SUFFIX" is not defined in the environment, then this routine
*   will return an empty string ("").
*
********************************************************************************
*/

const char * GetSuffix ( )
{
   static const char * suffix_var_name = "OS_SUFFIX" ;
   static char suffix [ MAX_SIZE_OF_SUFFIX ] ;
   int gad_token_len = strlen ( suffix_var_name ) ;
   int gad_value_len ;
   int status ;

   /* Initialize the output character array with '\0' characters. */
   memset ( suffix , '\0' , MAX_SIZE_OF_SUFFIX ) ;

   /* Attempt to retrieve the value of the "OS_SUFFIX" environmental
      variable.  If it is not defined in the environment, then 
      get_apps_defaults will return a value of "1".  If the operation
      is successful, then it will return a value of "0". */  
   status = get_apps_defaults ( ( char * ) suffix_var_name ,
                                & gad_token_len ,
                                suffix ,
                                & gad_value_len ) ;

   /* Did the "get_apps_routine" successfully find a value for the
      "OS_SUFFIX" environmental variable?  If it didn't, then copy an
      empty string into the "suffix" array. */
   if ( status != 0 )
   {
      strcpy ( suffix , "" ) ;
   }

   return suffix ;
                                
}
