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
#include "gageqc_types.h"
#include "gageqc_defs.h"

/* Converts point precipitation data in struct pdata to hrap grid.  The
   20 closest stations are precalculated for each grid point - this
   speeds computations for data sets with many precipitation points.
   If there are no good precipitation points for a grid point, then
   a recalculation is made using all precipitation points. 1/R**2
   interpolation is used.  If requested, the final interpolation
   is scaled using seasonal isohyets. The grid is saved as a disk
   file and used later for a raster or vector (HRAP) plot. */

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

void render_z (int pcpn_day, int pcpn_time, int pcpn_time_step,
	       int max_zstations, const struct station *zstation,
	       const struct hrap_grid * hrap_grid, const struct zdata *zdata, 
               int *pcp_in_use)
{

   int i, j, h, hh, time_pos, htotal;
   double distance, dist1, dist2, dist, value;
   double temp;
   extern struct pcp *pcp;
   int totals[5];

   for (i = 0; i < 5; i++)
   {
      totals[i] = 0;
   }

   time_pos = pcpn_time;

   for (i = 0; i < hrap_grid->maxi; i++)
   {

      for (j = 0; j < hrap_grid->maxj; j++)
      {

	 if (hrap_grid->owner[i][j] == -1)
	 {

	    pcp->value[i][j] = -9999;
	    continue;

	 }

	 value = 0.0;
	 distance = 0.0;
	 htotal = 0;

	 for (h = 0; h < 5; h++)
	 {

	    hh = hrap_grid->gage[i][j].zindex[h];

	    if (zdata[pcpn_day].stn[hh].zlevel2[time_pos].data < 0)
	       continue;

	    if (zdata[pcpn_day].stn[hh].zlevel2[time_pos].qual != 5 &&
		zdata[pcpn_day].stn[hh].zlevel2[time_pos].qual != 8 &&
		zdata[pcpn_day].stn[hh].zlevel2[time_pos].qual != 2)
            {
	       continue;
            }

            dist1 = (double) (i + hrap_grid->hrap_minx - zstation[hh].hrap_x);
            dist2 = (double) (j + hrap_grid->hrap_miny - zstation[hh].hrap_y);

	    dist = pow (dist1, 2) + pow (dist2, 2);

	    if (dist < .00001)
	       dist = .00001;

	    dist = 1 / dist;

	    temp = zdata[pcpn_day].stn[hh].zlevel2[time_pos].data * dist;

	    value = value + temp;

	    distance = distance + dist;

	    htotal++;

	 }


	 if (htotal < 4)
	 {

	    value = 0.0;
	    distance = 0.0;
	    htotal = 0;

	    for (h = 0; h < max_zstations; h++)
	    {

	       if (zdata[pcpn_day].stn[h].zlevel2[time_pos].data < 0)
		  continue;

	       if (zdata[pcpn_day].stn[h].zlevel2[time_pos].qual != 5 &&
		   zdata[pcpn_day].stn[h].zlevel2[time_pos].qual != 8 &&
		   zdata[pcpn_day].stn[h].zlevel2[time_pos].qual != 2)
               {
		  continue;
               }

               dist1 = (double) (i + hrap_grid->hrap_minx
                                 - zstation[h].hrap_x);
               dist2 = (double) (j + hrap_grid->hrap_miny
                                 - zstation[h].hrap_y);

	       dist = pow (dist1, 2) + pow (dist2, 2);

	       if (dist < .00001)
		  dist = .00001;

	       dist = 1 / dist;

	       temp = zdata[pcpn_day].stn[h].zlevel2[time_pos].data * dist;

	       value = value + temp;

	       distance = distance + dist;

	       htotal++;

	    }

	 }

	 if (htotal == 0)
	    pcp->value[i][j] = -9999;

	 else
	    pcp->value[i][j] = (int) (value / distance * 100);


      }

   }

   time_pos = 100 + pcpn_day * 4 + 3 - pcpn_time;

   pcp_in_use[time_pos] = 1;

   write_file ("pcp", time_pos, pcp);

}
