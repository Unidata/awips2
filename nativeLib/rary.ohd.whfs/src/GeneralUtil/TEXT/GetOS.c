/*******************************************************************************
* FILENAME:              GetOS.c
* NUMBER OF MODULES:     2
* GENERAL INFORMATION:
*   MODULE 1:            GetOS
* DESCRIPTION:           This routine determines the operating system that it
*                        is being executed on.  It returns this information
*                        to the user as an enumerated value.
*   Module 2:            get_oper_sys
* DESCRIPTION:           This routine determines the operating system that it
*                        is being executed on.  It is setup to be callable from
*                        Fortran.  It returns the operating system type
*                        as a function parameter as opposed to GetOS which
*                        returns the operating system value as a function
*                        value.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         October 24, 2001
* ORGANIZATION:          HSEB / WHFS
* MACHINE:               HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        10/24/01     Bryon Lawrence    Original Coding.
*          2        06/04/03     Bryon Lawrence    Original Coding.
*
********************************************************************************
*/

#include <string.h>
#include <sys/utsname.h>
#include "GetOS.h"
#include "create_fortran_link.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:  GetOS
* PURPOSE:      Since WHFS applications are sharing the same source code on
*               the HP-UX and Linux platforms, the need occasionally arises
*               for the software to have different functionality based on
*               which operating system it is being run on.  This utility
*               provides a runtime means of determining which operating
*               system a program is executing on.  It returns a value of
*               type OperSys which indicates what the operating system is
*               (see the return values below).
*
* ARGUMENTS:
*      None
*
* RETURNS:
*   DATA TYPE   NAME             DESCRIPTION
*   OperSys     os               This is an enumerated value that indicates
*                                which operating system the application
*                                is being executed on:
*                                OS_UNIX -  The UNIX Operating System.
*                                OS_LINUX - The Linux Operating System.
*                                OS_ERROR - An error was encountered.  The
*                                           caller of this routine can test
*                                           errno if more information is
*                                           desired.
*
*                                (The OperSys enumeration is defined in the
*                                 GetOS.h header file.)
*
* APIs UTILIZED:
*   NAME                HEADER FILE       DESCRIPTION
*   uname               sys/utsname.h     This is the "C" library routine
*                                         that determines which operating
*                                         system the application is running
*                                         on.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME           DESCRIPTION
*   const char *    os             Receives the name of the operating system
*                                  as determined by "uname".
*   int             status         Receives the status code indicating whether
*                                  or not "uname" successfully executed.
*   OperSys         oper           The enumerated value indicating the
*                                  operating system as returned to the
*                                  caller of this routine.
*   struct utsname  machine_info   The machine specific information returned
*                                  from the call to "uname".
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*    OS_ERROR                              An error was encountered by the
*                                          "uname" routine while attempting
*                                          to determine the operating system.
*                                          This value is defined in GetOS.h.
********************************************************************************
*/

OperSys GetOS ( )
{
   const char * os = NULL ;
   int status ;
   OperSys oper ;
   struct utsname machine_info ;

   /* Call the uname routine to determine the operating system
      that the application is running on. */
   status = uname ( & machine_info ) ;

   if ( status == 0 )
   {
      os = machine_info.sysname ;

      if ( strcmp ( os , "HP-UX" ) == 0 )
      {
         oper = OS_UNIX ;
      }
      else if ( strcmp ( os , "Linux" ) == 0 )
      {
         oper = OS_LINUX ;
      }
      else
      {
         oper = OS_ERROR ;
      }

   }
   else
   {
      oper = OS_ERROR ;
   }

   return oper ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:  get_oper_sys
* PURPOSE:      This version of GetOS is for use with Fortran.
*               Since WHFS applications are sharing the same source code on
*               the HP-UX and Linux platforms, the need occasionally arises
*               for the software to have different functionality based on
*               which operating system it is being run on.  This utility
*               provides a runtime means of determining which operating
*               system a program is executing on.  It returns a value of
*               type OperSys which indicates what the operating system is
*               (see the return values below).
*
*
* ARGUMENTS:
*     int * opersys              The type of operating system is returned
*                                in the memory pointed by this pointer.
*                                0 - The UNIX Operating System.
*                                1 - The Linux Operating System.
*                                2 - An error was encountered.  The
*                                    caller of this routine can test
*                                    errno if more information is
*                                    desired.
*
* RETURNS:
*    None
*
* APIs UTILIZED:
*   NAME                HEADER FILE       DESCRIPTION
*   uname               sys/utsname.h     This is the "C" library routine
*                                         that determines which operating
*                                         system the application is running
*                                         on.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME           DESCRIPTION
*   const char *    os             Receives the name of the operating system
*                                  as determined by "uname".
*   int             status         Receives the status code indicating whether
*                                  or not "uname" successfully executed.
*   OperSys         oper           The enumerated value indicating the
*                                  operating system as returned to the
*                                  caller of this routine.
*   struct utsname  machine_info   The machine specific information returned
*                                  from the call to "uname".
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                            DESCRIPTION
*    2                                     An error was encountered by the
*                                          "uname" routine while attempting
*                                          to determine the operating system.
********************************************************************************
*/

create_fortran_link( void, get_oper_sys, ( int * opersys ),( opersys ))
{
   const char * os = NULL ;
   int status ;
   OperSys oper ;
   struct utsname machine_info ;

   /* Call the uname routine to determine the operating system
      that the application is running on. */
   status = uname ( & machine_info ) ;

   if ( status == 0 )
   {
      os = machine_info.sysname ;

      if ( strcmp ( os , "HP-UX" ) == 0 )
      {
         oper = OS_UNIX ;
      }
      else if ( strcmp ( os , "Linux" ) == 0 )
      {
         oper = OS_LINUX ;
      }
      else
      {
         oper = OS_ERROR ;
      }

   }
   else
   {
      oper = OS_ERROR ;
   }

   switch ( oper )
   {

      case OS_UNIX :

         * opersys = 0 ;
         break ;

      case OS_LINUX :

         * opersys = 1 ;
         break ;

      case OS_ERROR :

         * opersys = 2 ;
         break ;

      default :

         * opersys = 2 ;
         break ;
   }

}
