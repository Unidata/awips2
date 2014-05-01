#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"


/* j is the day number. */

void estimate_missing_stations ( int j,
                                 const struct station * station,
                                 int max_stations,
                                 const struct pdata * pdata
                                )
{

   extern int mpe_dqc_max_precip_neighbors;
   extern int mpe_dqc_min_good_stations;
   extern int isom;
   extern int isohyets_used;
   extern int method;
   int m, i, h, l, ii;
   double lat1, lon1, fdist, fdata, fvalue[4], lat, lon, testdist, isoh =
      0., isoh1 = 0., padj, distlon;
   float conv = .0174;
   double fvalue24 = 0., fvalue06, fmult, stotal;
   int num_missing;

   for (m = 0; m < max_stations; m++)
   {

      /* only estimate missing data */

      if (pdata[j].stn[m].frain[4].data >= 0 &&
	  pdata[j].stn[m].frain[4].qual != 5 &&
	  pdata[j].stn[m].frain[4].qual != 1)
	 continue;

      for (h = 4; h >= 0; h--)
      {

	 if (pdata[j].used[h] == 0)
	 {

	    fvalue24 = -1;
	    continue;

	 }


	 lat1 = station[m].lat;
	 lon1 = station[m].lon;

	 /*get isohyet */

	 if (isohyets_used != 0)
	    isoh1 = station[m].isoh[isom];

	 fdist = 0.0;
	 fdata = 0.0;
	 l = 0;

	 for (ii = 0; ii < mpe_dqc_max_precip_neighbors; ii++)
	 {

	    i = station[m].index[ii];

	    /* dont estimate unless good or forced good */

	    if (pdata[j].stn[i].frain[h].qual != 0 &&
		pdata[j].stn[i].frain[h].qual != 8 &&
		pdata[j].stn[i].frain[h].qual != 6 &&
		pdata[j].stn[i].frain[h].qual != 3 &&
		pdata[j].stn[i].frain[h].qual != 2)
	       continue;

	    /*dont use missing stations */

	    if (pdata[j].stn[i].frain[h].data < 0)
	       continue;

	    lat = station[i].lat;
	    lon = station[i].lon;

	    if (isohyets_used != 0)
	       isoh = station[i].isoh[isom];

	    distlon = (lon1 - lon) * cos ((lat1 + lat) / 2 * conv);

	    testdist = pow ((double) (lat1 - lat), 2) +
	       pow ((double) distlon, 2);

	    if (testdist == 0.0)
	       testdist = .0001;

	    if (method == 2 && isoh > 0 && isoh1 > 0)
	       padj = pdata[j].stn[i].frain[h].data * isoh1 / isoh;

	    else
	       padj = pdata[j].stn[i].frain[h].data;


	    fdist = 1 / testdist + fdist;
	    fdata = padj / testdist + fdata;

	    l++;

	 }


	 if (l < mpe_dqc_min_good_stations)
	 {

	    fdist = 0.0;
	    fdata = 0.0;
	    l = 0;

	    for (i = 0; i < max_stations; i++)
	    {

	       if (i == m)
		  continue;

	       /* dont estimate unless good or forced good */

	       if (pdata[j].stn[i].frain[h].qual != 0 &&
		   pdata[j].stn[i].frain[h].qual != 8 &&
		   pdata[j].stn[i].frain[h].qual != 6 &&
		   pdata[j].stn[i].frain[h].qual != 3 &&
		   pdata[j].stn[i].frain[h].qual != 2)
		  continue;

	       /*dont use missing stations */

	       if (pdata[j].stn[i].frain[h].data < 0)
		  continue;

	       lat = station[i].lat;
	       lon = station[i].lon;

	       if (isohyets_used != 0)
		  isoh = station[i].isoh[isom];

	       distlon = (lon1 - lon) * cos ((lat1 + lat) / 2 * conv);

	       testdist = pow ((double) (lat1 - lat), 2) +
		  pow ((double) distlon, 2);

	       if (testdist == 0.0)
		  testdist = .0001;

	       if (method == 2 && isoh > 0 && isoh1 > 0)
		  padj = pdata[j].stn[i].frain[h].data * isoh1 / isoh;

	       else
		  padj = pdata[j].stn[i].frain[h].data;


	       fdist = 1 / testdist + fdist;
	       fdata = padj / testdist + fdata;

	       l++;

	    }

	 }

	 /* 24 hourly has stations */

	 if (h == 4 && l > 0)
	    fvalue24 = fdata / fdist;

	 /* 24 hourly has no stations */

	 else if (h == 4)
	    fvalue24 = -1;

	 /* 6 hourly has stations */

	 else if (l > 0)
	    fvalue[h] = fdata / fdist;

	 /* 6 hourly no stations */

	 else
	    fvalue[h] = -1;

      }


/* have all 24 hour data */

/* 24 hourly data available */

      if (fvalue24 >= 0)
      {

	 fvalue06 = 0.0;
	 stotal = 0.0;
	 num_missing = 0;

	 for (h = 0; h < 4; h++)
	 {

	    /* use good, forced good and questionable data to estimate */

	    /* need to caculate partial total to ensure that 24 hourly
	       and 6 hourly data match */

	    if ((pdata[j].stn[m].frain[h].qual == 0 ||
		 pdata[j].stn[m].frain[h].qual == 8 ||
		 pdata[j].stn[m].frain[h].qual == 3 ||
		 pdata[j].stn[m].frain[h].qual == 2) &&
		pdata[j].stn[m].frain[h].data >= 0)
	       stotal = stotal + pdata[j].stn[m].frain[h].data;

	    else
	    {

	       num_missing++;
	       fvalue06 = fvalue06 + fvalue[h];

	    }


	 }

	 /* stotal will now be difference between 24 hour estimate and 6 hourly
	    partial total */

	 stotal = fvalue24 - stotal;

	 if (stotal <= 0)
	    stotal = 0;

	 if (fvalue06 == 0)
	    fmult = 0;

	 /* fmult is that ratio of actual summed (missing) 6 hourly rain to estimated
	    6 hourly rain */

	 else
	    fmult = stotal / fvalue06;

	 /* now rescale the estimates so they equal the 24 hour estimate */

	 for (h = 0; h < 4; h++)
	 {

	    if ((pdata[j].stn[m].frain[h].qual != 0 &&
		 pdata[j].stn[m].frain[h].qual != 8 &&
		 pdata[j].stn[m].frain[h].qual != 3 &&
		 pdata[j].stn[m].frain[h].qual != 2) ||
		pdata[j].stn[m].frain[h].data < 0)
	    {

	       if (fvalue06 != 0)
		  pdata[j].stn[m].frain[h].data = fvalue[h] * fmult;

	       else
		  pdata[j].stn[m].frain[h].data =
		     stotal / (float) num_missing;

	       pdata[j].stn[m].frain[h].qual = 5;

	    }


	 }

	 pdata[j].stn[m].frain[4].qual = 5;

	 pdata[j].stn[m].frain[4].data = fvalue24;


      }

/* no 24 hour data - estimate done on 6 hour period only*/

      else
      {

	 for (h = 0; h < 4; h++)
	 {

	    if (pdata[j].stn[m].frain[h].qual != 0 &&
		pdata[j].stn[m].frain[h].qual != 8 &&
		pdata[j].stn[m].frain[h].qual != 3 &&
		pdata[j].stn[m].frain[h].qual != 2)
	    {

	       pdata[j].stn[m].frain[h].data = fvalue[h];
	       pdata[j].stn[m].frain[h].qual = 5;


	    }

	 }

      }

   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
