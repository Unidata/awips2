
/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
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


void write_file (char *prefix, int num, struct pcp *pcp)
{
   FILE *fp = NULL;
   char fname[GAGEQC_FILENAME_LEN];
   char message[GAGEQC_MESSAGE_LEN];
   const char * scratch_dir = NULL;
   extern struct hrap_grid *hrap_grid;
   int i;
   int tread;

   scratch_dir = get_mpe_scratch_dir ( );

   if ( scratch_dir != NULL )
   {
      sprintf (fname, "%s/%s.%d.%d", scratch_dir, prefix, num, getpid ());
   }
   else
   {
      /* Place the scratch files in the current working directory. */
      sprintf (fname, "%s.%d.%d", prefix, num, getpid ());
   }

   fp = fopen (fname, "w");

   if (fp == NULL)
   {
     logMessage ("could not open %s\n", fname);
      memset (message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf (message, "Could not open file: %s\n", fname);
      logMessage (message);
      return; 

   }
   else
   {
      memset (message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf (message, "Opened file: %s\n", fname);
      logMessage (message);
   }

   for (i = 0; i < hrap_grid->maxi; i++)
      tread =
	 fwrite (&pcp->value[i][0], sizeof (short int), hrap_grid->maxj, fp);

   fclose (fp);

   return;

}

const char * get_mpe_scratch_dir ( )
{
   static char * mpe_scratch_dir = "mpe_scratch_dir";
   static char scratch_dir[GAGEQC_FILENAME_LEN];
   static int first = 1;
   int reply_len;
   int string_len;

   /* Retrieve the path of the scratch file the first time this routine
      is called. */
   if ( first == 1 )
   {
      first = 0;
      memset ( scratch_dir, '\0', GAGEQC_FILENAME_LEN );
      string_len = strlen ( mpe_scratch_dir );
      get_apps_defaults ( mpe_scratch_dir, & string_len, scratch_dir, 
                          & reply_len );
   }

   string_len = strlen ( scratch_dir );

   if ( string_len > 0 )
   {
      return scratch_dir;
   }
   else
   {
      return NULL;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
