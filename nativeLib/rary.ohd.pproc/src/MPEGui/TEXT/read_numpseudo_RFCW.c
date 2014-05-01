/*******************************************************************************
* FILENAME:            read_numpseudo_RFCW.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:  read_numpseudo_RFCW
* DESCRIPTION: This function checks for previously defined pseudo gages by
*              checking the PseudoGageVal table.  If no previously defined 
*              pseudo gages for the date/hour are found, then num = 0
*              else num = number of previously defined unique pseudo gages
*
* ORIGINAL AUTHOR:   Bryon Lawrence
* CREATION DATE:     November 3, 2004
* ORGANIZATION:      HSEB / OHD
* MACHINE:           Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/3/2004    Bryon Lawrence    Converted to Use Dbgen.
********************************************************************************
*/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "DbmsDefs.h"
#include "LoadUnique.h"
#include "mpe_log_utils.h"
#include "read_numpseudo_RFCW.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   read_numpseudo_RFCW
* PURPOSE:       This function checks for previously defined pseudo gages by
*                checking PseudoGageVal table. If no previously defined pseudoo
*                gages for the date/hour are found, then num = 0.
*                Otherwise num = number of previously defined unique pseudo
*                gages.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  char *      dt          The time to retrieve the pseudo gage data 
*                                  for.
*   Output int *       num         The number of pseudo gages retrieved.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                  HEADER FILE      DESCRIPTION
*   FreeUnique            LoadUnique.h     Frees a linked list of UniqueList
*                                          nodes.
*   LoadFirst             ListFirst.h      Loads the first node in a linked 
*                                          list.
*   LoadNext              ListFirst.h      Loads the next node in a linked
*                                          list.
*   LoadUnique            LoadUnique.h     Loads a list of unique fields
*                                          from a user-specified IHFS table.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME       DESCRIPTION
*   char *     ptchar     Points to the portion of the pseudo gage name
*                         which contains the gage number.
*   char       cnum [ ]   Contains the number portion of the pseudo gage name.
*   char       ggid [ ]   The gage id.
*   char       where [ ]  There where clause is built in here.
*   int        i          A loop index.
*   int        n          Contains the numeric of cnum.
*   int        nmax       Contains the maximum psuedo gage number found.
*   int        statuscnt  Contains return code or number of unique rows 
*                         found by LoadUnique.
*
* DATA FILES AND/OR DATABASE:
*
* Reads the PseudoGageVal table in the IHFS database.  The connection to the
* database must be established before calling this routine.
*
* ERROR HANDLING:
*   None.
*
* CALLING FUNCTION:
*    write_pseudo
*
********************************************************************************
*/
void read_numpseudo_RFCW ( const char *dt, int *num )
{
   char * ptchar = NULL;
   char cnum [ 3 ];
   char ggid [ LOC_ID_LEN + 1 ];
   char where [ 50 ] ;
   int i;
   int n;
   int nmax = 0;
   int statuscnt;

   UniqueList * pUniqueListHead = NULL ;
   UniqueList * pUniqueListNode = NULL ;

   cnum[2]='\0';
   ggid[8]='\0';

   /* Build the where clause. */
   sprintf ( where , "WHERE obstime = '%s'", dt ) ; 

   pUniqueListHead = LoadUnique ( "pseudo_gage_id", "PseudoGageVal", where,
                                  & statuscnt ) ;
                          
   if ( statuscnt < 0 )
   {
      flogMessage ( stderr , "In routine 'read_numpseudo_RFCW':\n"
                         "An error was encountered in LoadUnique\n"
                         "loading unique pseudo gage ids for time %s. "
                         "Error code %d.\n", dt, statuscnt ) ;
   }
   else if ( statuscnt > 0 ) 
   {
      pUniqueListNode = ( UniqueList * ) ListFirst ( &pUniqueListHead->list ) ;

      for ( i = 0; i < statuscnt; ++ i )
      {
         strncpy(ggid,pUniqueListNode->uchar,8);
         ptchar=&ggid[6];
         strcpy(cnum,ptchar);

         n=atoi(cnum);
         if(n > nmax) nmax=n;
         pUniqueListNode = ( UniqueList * ) ListNext ( &pUniqueListNode->node );
      }
   }

   *num=nmax;
   
   /* Free the linked list of unique pseudo gage ids. */
   FreeUnique ( pUniqueListHead ) ;
   pUniqueListHead = NULL ;
   flogMessage( stdout , "%d pseudo gages previously defined for %s\n",
            *num, dt );

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
