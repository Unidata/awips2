#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

int read_snow (char *fname,
               const struct station * station,
               int max_stations,
               int i)
{
   extern struct pdata pdata[10];
   FILE *fr = NULL;
   int j, k, ier, m, qual;
   char kbuf[100], *p, *q, buf[100], hb5[10];
   char message[GAGEQC_MESSAGE_LEN];
   char pc, datbuf[50], parmbuf[50];
   int maxk, startk;

   fr = fopen (fname, "r");

   if (fr == NULL)
   {
	 memset (message, '\0', GAGEQC_MESSAGE_LEN);
	 sprintf (message, "Could not open file: %s\n", fname);
	 logMessage (message);
   }
   else
   {
	 memset (message, '\0', GAGEQC_MESSAGE_LEN);
	 sprintf (message, "Opened file: %s\n", fname);
	 logMessage (message);

   }
   for (k = 0; k < max_stations; k++)
      {

	 for (m = 0; m < 5; m++)
	    {

	       pdata[i].stn[k].srain[m].data = -99;
	       pdata[i].stn[k].srain[m].qual = -99;
	       pdata[i].stn[k].sflag[m] = -1;

	    }

      }

   if (fr == NULL)
      {

	 return (-1);

      }

   for (;;)
      {

       bad:
	 p = fgets (kbuf, 100, fr);



	 if (p == NULL)
	    break;

	 if (kbuf[0] == ':')
	    continue;

	 ier = sscanf (&kbuf[2], "%s %s %s", hb5, datbuf, parmbuf);
	 if (ier == 0)
	    continue;

	 p = strchr (parmbuf, '/');
	 if (p == NULL)
	    continue;

	 pc = *(p + 5);

	 for (j = 0; j < max_stations; j++)
	    {

	       if (strcmp (hb5, station[j].hb5) == 0 &&
		   pc == station[j].parm[4])
		  {

		     break;

		  }

	    }

	 if (j == max_stations)
	    {

	       continue;

	    }

	 p = strchr (kbuf, '/');
	 if (p == NULL)
	    continue;

	 q = strchr (p, ' ');
	 if (p == NULL)
	    continue;

	 maxk = 5;
	 startk = 4;

	 for (k = startk; k < maxk; k++)
	    {

	       pdata[i].stn[j].srain[k].qual = 0;

	       if ((p = strchr (q, '/')) == NULL &&
		   (p = strchr (q, '\n')) == NULL)
		  goto bad;

	       *p = 0;

	       strcpy (buf, q);

	       if ((p = strchr (buf, '.')) == NULL)
		  {

		     if ((p = strchr (buf, 'm')) == NULL &&
			 (p = strchr (buf, 'M')) == NULL)
			{

			   pdata[i].stn[j].srain[k].data = -1;
			   pdata[i].stn[j].srain[k].qual = -1;

			}

		  }

	       else
		  {

		     pdata[i].stn[j].srain[k].data = atof (buf);

		     qual = 8;

		     break;

		  }

	    }


      }



   fclose (fr);

   return (1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
