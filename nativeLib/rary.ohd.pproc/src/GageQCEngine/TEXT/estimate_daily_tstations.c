#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

/* j is the day number. */

void estimate_daily_tstations (int j,
			       struct station *tstation, int max_tstations)
{

   extern struct tdata tdata[10];
   extern int isom;
   extern int maxmin_used;
   extern int mpe_dqc_max_temp_neighbors;
   extern int mpe_dqc_min_good_stations;

   int m, k, i, l, ii;
   double lat1, lon1, fdist, fdata, lat, lon;
   double distlon, dist, df;
   float conv = .0174;
   float temp_climo;
   float temp_climo1;
   int h;

   if (tdata[j].data_time == 0)
   {
      return;
   }

   for (m = 0; m < max_tstations; m++)
   {

      /* Don't estimate missing 24 hour tstations */
      /* Set the estimate factor which specifies where the estimate
         falls in relation to the max/min temperatures to missing. */
      for (h = 0; h < 4; h++)
      {
	 tdata[j].stn[m].tlevel2[h].a = -99;
      }

      /* Only compute estimate factors for cases where there are data for 
         all four six hour periods and the max/min. */
      for (h = 0; h < 6; h++)
	 if (tdata[j].stn[m].tlevel2[h].data == -99 ||
	     (tdata[j].stn[m].tlevel2[h].qual != 0 &&
	      tdata[j].stn[m].tlevel2[h].qual != 8 &&
	      tdata[j].stn[m].tlevel2[h].qual != 3 &&
	      tdata[j].stn[m].tlevel2[h].qual != 2))
	    break;

      if (h != 6)
	 continue;

      for (h = 0; h < 4; h++)
      {
         /* Added Nov. 28, 2007.  Check to make sure the 6 hour value falls within
            the daily max/min value.  If it doesn't then this station
            should not be used to estimate the temperature values of
            neighboring stations. */
         if ( ( tdata[j].stn[m].tlevel2[h].data >= tdata[j].stn[m].tlevel2[5].data ) &&
              ( tdata[j].stn[m].tlevel2[h].data <= tdata[j].stn[m].tlevel2[4].data ) )
         {
	    tdata[j].stn[m].tlevel2[h].a =
	       (float) (tdata[j].stn[m].tlevel2[h].data -
		        tdata[j].stn[m].tlevel2[5].data) /
	       (float) (tdata[j].stn[m].tlevel2[4].data -
		        tdata[j].stn[m].tlevel2[5].data);
         }

      }

   }

   for (m = 0; m < max_tstations; m++)
   {

      /* If there are any six hour periods which have non-missing,
         non-time-distributed data, then do not estimate for this station. */
      for (k = 0; k < 4; k++)
	 if (tdata[j].stn[m].tlevel2[k].data != -99 &&
	     tdata[j].stn[m].tlevel2[k].qual != 6)
	    break;

      if (k != 4)
	 continue;

      /* Make sure all four six hour periods is set to missing.  This
         erases previously time distributed data. */
      for (k = 0; k < 4; k++)
	 tdata[j].stn[m].tlevel2[k].data = -99;

      /* If either or both the max/min values are missing,
         then do not estimate for this station. */
      if (tdata[j].stn[m].tlevel2[4].data == -99 ||
	  tdata[j].stn[m].tlevel2[5].data == -99)
	 continue;

      /* If the max/min value is bad, then do not estimate for this station. */
      if (tdata[j].stn[m].tlevel2[4].qual == 1 ||
	  tdata[j].stn[m].tlevel2[4].qual == 5 ||
	  tdata[j].stn[m].tlevel2[5].qual == 1 ||
	  tdata[j].stn[m].tlevel2[5].qual == 5)
	 continue;

      lat1 = tstation[m].lat;
      lon1 = tstation[m].lon;

      /* Retrieve the climate information for this station. */
      if ( maxmin_used == 1 )
      {
         temp_climo1 = (tstation[m].max[isom] + tstation[m].min[isom] ) / 2;
      }

      /* For each six hour period ... */
      for (k = 0; k < 4; k++)
      {
	 fdist = 0.0;
	 fdata = 0.0;

	 l = 0;

         /* Look at the temperature values of the station's
            neighbors. */
	 for (ii = 0; ii < mpe_dqc_max_temp_neighbors; ii++)
	 {

	    i = tstation[m].index[ii];

	    /* dont estimate unless good or forced good */

	    if (tdata[j].stn[i].tlevel2[k].qual != 0 &&
		tdata[j].stn[i].tlevel2[k].qual != 8 &&
		tdata[j].stn[i].tlevel2[k].qual != 3 &&
		tdata[j].stn[i].tlevel2[k].qual != 2)
	       continue;

	    /*dont use missing tstations */

	    if (tdata[j].stn[i].tlevel2[k].data == -99 ||
		tdata[j].stn[i].tlevel2[k].a < -98)
	       continue;

            /* Retrieve the climate information for this station. */
            if ( maxmin_used == 1 )
            {
               temp_climo = (tstation[i].max[isom] + tstation[i].min[isom] ) / 2;
            }

            /* Compute distance between stations. */
	    lat = tstation[i].lat;
	    lon = tstation[i].lon;

	    distlon = (lon1 - lon) * cos ((lat1 + lat) / 2 * conv);

	    dist = pow ((double) (lat1 - lat), 2) +
	       pow ((double) (distlon), 2);

	    dist = pow (dist, .5) * 60;

	    df = 50 * abs (tstation[m].elev - tstation[i].elev) / 5280;

	    dist = dist + df;

	    if (dist == 0.0)
	       dist = .000001;
	    dist = 1 / dist;

            if ( ( maxmin_used == 1 ) && ( temp_climo1 > -99 ) && ( temp_climo > -99 ) )
            {
	       fdata = fdata + tdata[j].stn[i].tlevel2[k].a * dist * ( temp_climo1 / temp_climo );
            }
            else
            {
	       fdata = fdata + tdata[j].stn[i].tlevel2[k].a * dist;
            }

	    fdist = fdist + dist;

	    l++;

	    if (l == mpe_dqc_min_good_stations)
	       break;

	 }


	 if (l < mpe_dqc_min_good_stations)
	 {

	    fdist = 0.0;
	    fdata = 0.0;

	    l = 0;

	    for (i = 0; i < max_tstations; i++)
	    {

	       if (i == m)
		  continue;

	       if (tdata[j].stn[i].tlevel2[k].qual != 0 &&
		   tdata[j].stn[i].tlevel2[k].qual != 8 &&
		   tdata[j].stn[i].tlevel2[k].qual != 3 &&
		   tdata[j].stn[i].tlevel2[k].qual != 2)
		  continue;

	       if (tdata[j].stn[i].tlevel2[k].data == -99 ||
		   tdata[j].stn[i].tlevel2[k].a < -98)
		  continue;

               /* Retrieve the climate information for this station. */
               if ( maxmin_used == 1 )
               {
                  temp_climo = (tstation[i].max[isom] + tstation[i].min[isom] ) / 2;
               }

	       lat = tstation[i].lat;
	       lon = tstation[i].lon;

	       distlon = (lon1 - lon) * cos ((lat1 + lat) / 2 * conv);

	       dist = pow ((double) (lat1 - lat), 2) +
		  pow ((double) (distlon), 2);

	       dist = pow (dist, .5) * 60;

	       df = 50 * abs (tstation[m].elev - tstation[i].elev) / 5280;

	       dist = dist + df;

	       if (dist == 0.0)
		  dist = .000001;

	       dist = 1 / dist;

               if ( ( maxmin_used == 1 ) && ( temp_climo1 > -99 ) && ( temp_climo > -99 ) )
               {
	          fdata = fdata + tdata[j].stn[i].tlevel2[k].a * dist * ( temp_climo1 / temp_climo );
               }
               else
               {
	          fdata = fdata + tdata[j].stn[i].tlevel2[k].a * dist;
               }

	       fdist = fdist + dist;

	       l++;

	    }

	 }

	 if (l != 0)
	 {

	    tdata[j].stn[m].tlevel2[k].a = fdata / fdist;
	    tdata[j].stn[m].tlevel2[k].data = tdata[j].stn[m].tlevel2[k].a *
	       (tdata[j].stn[m].tlevel2[4].data -
		tdata[j].stn[m].tlevel2[5].data) +
	       tdata[j].stn[m].tlevel2[5].data;
	    tdata[j].stn[m].tlevel2[k].qual = 6;

	 }
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
