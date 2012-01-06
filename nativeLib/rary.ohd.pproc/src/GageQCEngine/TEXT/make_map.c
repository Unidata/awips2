/*******************************************************************************
* FILENAME:            create_map.c
* DESCRIPTION:         Contains the routines which create MAPs from
*                      gridded precipitation data.
*
* ORIGINAL AUTHOR:     Craig Peterson
* CREATION DATE:       
* ORGANIZATION:        CBRFC
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     3/2006       Bryon Lawrence    Ported into MPE Editor to use Map Lib.
********************************************************************************
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

struct dval dval;

/*******************************************************************************
* MODULE NAME:   create_map
* PURPOSE:       Creates an MAP from a gridded precipitation field.
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
void create_map (int num)
{

   extern int render_all;
   extern int wfo_orig;
   extern int pcp_in_use[];
   extern struct map mean_areal_precip_global[];
   extern struct pcp *pcp;
   struct hrap_grid *hrap_grid = NULL;
   char fbuf[100];
   int x, y, ix, iy, l;
   int minx, miny, totx, toty, uzpts, lzpts, mzpts, gzpts;
   float lz, uz, mz, gz;
   int ib;

   hrap_grid = get_hrap_grid ( );

   if (pcp_in_use[num] == -1)
   {
      return;
   }

   strcpy (fbuf, "pcp");

   read_file (fbuf, num, pcp);

   minx = hrap_grid->hrap_minx;
   miny = hrap_grid->hrap_miny;
   totx = hrap_grid->maxi;
   toty = hrap_grid->maxj;

   for (ib = 0; mean_areal_precip_global[ib].hb5[0] != 0; ib++)
   {

      if (render_all == 0)
      {

	 /* skip if you are not the owner and there is no value */
	 /* should not affect daily_qc */
	 /* may affect verify */
	 /* should fix specify */
	 /* auto_specify ??? */



	    if (mean_areal_precip_global[ib].owner != wfo_orig &&
	        mean_areal_precip_global[ib].uz[num] < 0 &&
	        mean_areal_precip_global[ib].lz[num] < 0 &&
	        mean_areal_precip_global[ib].mz[num] < 0 &&
	        mean_areal_precip_global[ib].gz[num] < 0)
        {    
	       continue;
        }

      }

      mz = 0;
      uz = 0;
      lz = 0;
      gz = 0;
      uzpts = 0;
      lzpts = 0;
      mzpts = 0;
      gzpts = 0;


      for (l = 0; l < mean_areal_precip_global[ib].hrap_points; l++)
      {

	 x = mean_areal_precip_global[ib].hrap_data[l].x;
	 y = mean_areal_precip_global[ib].hrap_data[l].y;

	 ix = x - minx;
	 iy = y - miny;

	 if (ix < 0 || iy < 0 || ix >= totx || iy >= toty ||
	     pcp->value[ix][iy] == -9999)
	 {

	    continue;

	 }

	 if (mean_areal_precip_global[ib].hrap_data[l].zone[3] == 1)
	 {

	    gz = gz + (float) pcp->value[ix][iy] / 100;
	    gzpts++;

	 }

	 if (mean_areal_precip_global[ib].hrap_data[l].zone[2] == 1)
	 {

	    uz = uz + (float) pcp->value[ix][iy] / 100;
	    uzpts++;

	 }

	 if (mean_areal_precip_global[ib].hrap_data[l].zone[1] == 1)
	 {

	    mz = mz + (float) pcp->value[ix][iy] / 100;
	    mzpts++;

	 }

	 if (mean_areal_precip_global[ib].hrap_data[l].zone[0] == 1)
	 {

	    lz = lz + (float) pcp->value[ix][iy] / 100;
	    lzpts++;

	 }


      }

      if (gzpts == 0)
	 gz = -1;
      else
	 gz = gz / (float) gzpts;

      if (uzpts == 0)
	 uz = -1;
      else
	 uz = uz / (float) uzpts;

      if (mzpts == 0)
	 mz = -1;
      else
	 mz = mz / (float) mzpts;

      if (lzpts == 0)
	 lz = -1;

      else
	 lz = lz / (float) lzpts;

      mean_areal_precip_global[ib].uz[num] = uz;
      mean_areal_precip_global[ib].lz[num] = lz;
      mean_areal_precip_global[ib].mz[num] = mz;
      mean_areal_precip_global[ib].gz[num] = gz;

      mean_areal_precip_global[ib].maps_done[num] = 1;

   }

   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
