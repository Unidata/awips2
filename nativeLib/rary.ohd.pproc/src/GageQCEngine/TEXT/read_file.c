#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

void read_file (char *prefix, int num, struct pcp *pcp)
{

/*--------------------------------------*/
/*  generic read routine for reading gridded precip data  */
/*--------------------------------------*/
   const char * scratch_dir = NULL;
   FILE *fp = NULL;
   char fname[100];
   char message[GAGEQC_MESSAGE_LEN];
   extern struct hrap_grid *hrap_grid;
   int i, j;
   int string_len;
   int tread;

   scratch_dir = get_mpe_scratch_dir ( );

   if ( scratch_dir != NULL )
   {
      sprintf (fname, "%s/%s.%d.%d", scratch_dir, prefix, num, getpid ());
   }
   else
   {
      sprintf (fname, "%s.%d.%d", prefix, num, getpid ());
   }

   fp = fopen (fname, "r");

   if (fp == NULL)
   {

      for (i = 0; i < hrap_grid->maxi; i++)
      {

	 for (j = 0; j < hrap_grid->maxj; j++)
	 {

	    pcp->value[i][j] = 0;

	 }

      }

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
	 fread (&pcp->value[i][0], sizeof (short int), hrap_grid->maxj, fp);

   fclose (fp);

   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
