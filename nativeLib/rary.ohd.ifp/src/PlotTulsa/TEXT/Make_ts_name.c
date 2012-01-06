/* File: Make_ts_name.c
 *
 * Makes the time series name.
 *
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void Make_ts_name(novrsn, num_ts, tsid, p_char, locp, tul_locp)
      int               novrsn;      /* Tul Plot version number (i.e. 1 or 2) */
      int               num_ts;      /* Number of time series used in the current
					forcast program. */
      int               *locp;       /* Time series parameter location pointer */
      int               tul_locp;    /* Location of the beginning of the Tulsa
					plot parameters. */
      char              **tsid;      /* address of the time series identifier pointer */
      char              p_char[][4]; /* Parameter character data */

	 {
	 int            i,k,j;       /* counters */
         int            increment;   /* increment of 12 or 18 positions depending upon version number */
 	 char           *loc_dot;    /* Location of the dot in the Tulsa Plot time series */

	    for(i = 0; i < num_ts; i++)
	       {
	       /*tsid[i] = (char*)malloc(14);*/
	       memset(tsid[i], '\0', 14);
	       strncpy(tsid[i], p_char[*locp-1 + tul_locp], 8);
	       loc_dot = strstr(tsid[i], " ");
	       if(loc_dot == 0)
		  loc_dot = strcat(tsid[i], ".");
	       else
		  loc_dot = strcpy(loc_dot, ".");
	       strncat(loc_dot + 1, p_char[*locp-1 + tul_locp + 2], 4);
/*	       tul_locp = tul_locp + 12; */
               increment = 12;
               if (novrsn == 2) 
                  {
                     increment = 18;
                  } 
               tul_locp = tul_locp + increment;
	       }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/Make_ts_name.c,v $";
 static char rcs_id2[] = "$Id: Make_ts_name.c,v 1.2 1996/12/10 20:10:37 dws Exp $";}
/*  ===================================================  */

	 }  /* end of Make_ts_name */
/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */

