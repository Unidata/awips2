/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
* DESCRIPTION:         Maps precipitation station values to an HRAP grid.
*                      Produces the DailyQC version of the GageOnly analysis.
*
* ORIGINAL AUTHOR:     Craig Peterson
* CREATION DATE:       
* ORGANIZATION:        CBRFC
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     3/2006       Bryon Lawrence    Ported into MPE Editor.
********************************************************************************
*/
#include "gageqc_types.h"
#include "gageqc_defs.h"

/*******************************************************************************
* MODULE NAME:  render_pcp
* PURPOSE:  Converts point precipitation data in struct pdata to hrap grid.  The
*           20 closest stations are precalculated for each grid point - this
*           speeds computations for data sets with many precipitation points.
*           If there are no good precipitation points for a grid point, then
*           a recalculation is made using all precipitation points. 1/R**2
*           interpolation is used.  If requested, the final interpolation
*           is scaled using seasonal isohyets. The grid is saved as a disk
*           file and used later for a raster or vector (HRAP) plot.
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

void
render_pcp (int pcpn_day, int pcpn_time, int pcpn_time_step,
	    int max_stations, const struct station *station,
	    const struct hrap_grid *hrap_grid,
	    const struct pdata *pdata, int *pcp_in_use)
{
   extern int isom;
   extern int method;
   extern int mpe_dqc_max_precip_neighbors;

   int i, j, h, hh, time_pos, htotal;
   double distance, dist1, dist2, dist, value;
   double temp;
   extern struct pcp *pcp;
   int totals[5];

   for (i = 0; i < 5; i++)
   {
      totals[i] = 0;
   }

   if (pcpn_time_step == 0)
   {
      time_pos = pcpn_time;
   }
   else
   {
      time_pos = 4;
   }

   for (i = 0; i < hrap_grid->maxi; i++)
   {
      for (j = 0; j < hrap_grid->maxj; j++)
      {
	 /* Check if the grid cell is covered by an HSA.  If not, then
	  * do not estimate precipitation for it. */
	 if (hrap_grid->owner[i][j] == -1)
	 {
	    pcp->value[i][j] = 0;
	    continue;
	 }

	 value = 0.0;
	 distance = 0.0;
	 htotal = 0;

	 /* For each of the closest stations. */
	 for (h = 0; h < mpe_dqc_max_precip_neighbors; h++)
	 {
	    hh = hrap_grid->gage[i][j].index[h];

	    if (pdata[pcpn_day].stn[hh].frain[time_pos].data < 0)
	    {
	       /* No precip data. */
	       continue;
	    };

	    if (pdata[pcpn_day].stn[hh].frain[time_pos].qual != 0 &&
		pdata[pcpn_day].stn[hh].frain[time_pos].qual != 8 &&
		pdata[pcpn_day].stn[hh].frain[time_pos].qual != 6 &&
		pdata[pcpn_day].stn[hh].frain[time_pos].qual != 3 &&
		pdata[pcpn_day].stn[hh].frain[time_pos].qual != 4 &&
		pdata[pcpn_day].stn[hh].frain[time_pos].qual != 2)
	    {
	       /* The station has a bad qc flag. Do not use it. */
	       continue;
	    }

	    /* Convert the coordinates of the grid and station in lat/lon
	     * into distance. */
	    dist1 = (double) (i + hrap_grid->hrap_minx - station[hh].hrap_x);
	    dist2 = (double) (j + hrap_grid->hrap_miny - station[hh].hrap_y);

	    dist = pow (dist1, 2) + pow (dist2, 2);

	    if (dist < .00001)
	       dist = .00001;

	    dist = 1 / dist;

	    temp = pdata[pcpn_day].stn[hh].frain[time_pos].data * dist;

	    if (method == 2 && station[hh].isoh[isom] > 0)
	    {
	       temp = temp * (float) hrap_grid->isoh[isom][i][j] /
		  (station[hh].isoh[isom] * 25.4);
	    }

	    if (method == 2 && station[hh].isoh[isom] <= 0)
	    {
	       continue;
	    }

	    value += temp;

	    distance += dist;

	    htotal++;

	    if (htotal == 10)
	       break;

	 }


	 if (htotal < 4)
	 {

	    value = 0.0;
	    distance = 0.0;
	    htotal = 0;

	    for (h = 0; h < max_stations; h++)
	    {
	       if (pdata[pcpn_day].stn[h].frain[time_pos].data < 0)
	       {
		  continue;
	       }

	       if (pdata[pcpn_day].stn[h].frain[time_pos].qual != 0 &&
		   pdata[pcpn_day].stn[h].frain[time_pos].qual != 8 &&
		   pdata[pcpn_day].stn[h].frain[time_pos].qual != 6 &&
		   pdata[pcpn_day].stn[h].frain[time_pos].qual != 3 &&
		   pdata[pcpn_day].stn[h].frain[time_pos].qual != 4 &&
		   pdata[pcpn_day].stn[h].frain[time_pos].qual != 2)
	       {
		  continue;
	       }

	       dist1 = (double) (i + hrap_grid->hrap_minx
				 - station[h].hrap_x);
	       dist2 = (double) (j + hrap_grid->hrap_miny
				 - station[h].hrap_y);

	       dist = pow (dist1, 2) + pow (dist2, 2);

	       if (dist < .00001)
		  dist = .00001;

	       dist = 1 / dist;

	       temp = pdata[pcpn_day].stn[h].frain[time_pos].data * dist;

	       if (method == 2 && station[h].isoh[isom] <= 0)
	       {
		  continue;
	       }

	       if (method == 2 && station[h].isoh[isom] > 0)
	       {
		  temp = temp * (float) hrap_grid->isoh[isom][i][j] /
		     (station[h].isoh[isom] * 25.4);
	       }

	       value = value + temp;

	       distance = distance + dist;

	       htotal++;

	    }

	 }

	 if (htotal == 0)
	    pcp->value[i][j] = 0;

	 else
	    pcp->value[i][j] = (int) (value / distance * 100);

	 if (pcp->value[i][j] < .01)
	    pcp->value[i][j] = 0;

      }

   }

   if (pcpn_time_step == 0)
   {
      time_pos = pcpn_day * 4 + 3 - pcpn_time;
   }
   else
   {
      time_pos = 40 + pcpn_day;
   }

   pcp_in_use[time_pos] = 1;

   write_file ("pcp", time_pos, pcp);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
