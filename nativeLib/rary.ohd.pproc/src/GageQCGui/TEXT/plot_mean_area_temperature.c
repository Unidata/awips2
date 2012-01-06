#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "gageqc_gui.h"
#include "map_library.h"
#include "map_defines.h"
#include "post_functions.h"

/****************************************************************
plot_mean_areal_temperature()

PURPOSE: plot image for callback function of "Points+MATs"/"MATs"
buttons.
*****************************************************************/
void plot_mean_area_temperature (int h, int display_flag, int num)
{

   extern struct map mean_areal_precip_global[];
   extern double ** dqc_temp_delim;
   extern int   dqc_temp_numcol;
   extern int tscale;
   extern int wfo_all;
   extern int wfo_in_use[20];
   extern int  pcp_in_use[500];
   
   char *color = NULL;
   char **temperature_colormap = NULL;
   int hh;
   struct hrap_grid *hrap_grid = NULL;
   int k;   
   int l;   
   int x, y, ip, ib;
   int xpos;
   int ypos;  
   XPoint point[10000];
   XPoint PolyPoints[5];
   double mapvalue;
   int minx, miny, ix, iy, maxx, maxy;
   float uz, mz, lz, gz;
   const HRAP **hrap_lat_lon = NULL;


   temperature_colormap = get_temperature_colors ();
   
   /* get latest color limits */
   
   set_dqc_colordelimit();
   
   /* check pcp_in_use flag before plot */
   
   if (pcp_in_use[num] == -1)
   {
      return;
   } 
   
   hrap_grid = get_hrap_grid ();
   hrap_lat_lon = getLatLonGrid ();

   minx = hrap_grid->hrap_minx;
   miny = hrap_grid->hrap_miny;
   maxx = hrap_grid->maxi;
   maxy = hrap_grid->maxj;

   if (hrap_lat_lon == NULL)
   {
      return;
   }


   for (ib = 0; mean_areal_precip_global[ib].hb5[0] != 0; ib++)
   {

      if (mean_areal_precip_global[ib].tmaps_done[num - 150] <= 0)
	 continue;

      if (wfo_all != 1)
      {

	 for (hh = 0; hh < 20; hh++)
	 {

	    if (wfo_in_use[hh] == -1)
	       break;

	    if (mean_areal_precip_global[ib].owner == wfo_in_use[hh])
	       break;

	 }

	 if (wfo_in_use[hh] == -1)
	    continue;

      }

      for (l = 0; l < mean_areal_precip_global[ib].basin_points; l++)
      {

	 mConvertLatLon2XY (mean_areal_precip_global[ib].basin[l].lat,
			    mean_areal_precip_global[ib].basin[l].lon, &xpos,
			    &ypos);
	 point[l].x = (short) xpos;
	 point[l].y = (short) ypos;

      }


      lz = mean_areal_precip_global[ib].tlz[num - 150];
      mz = mean_areal_precip_global[ib].tmz[num - 150];
      uz = mean_areal_precip_global[ib].tuz[num - 150];
      gz = mean_areal_precip_global[ib].tgz[num - 150];

      if (mean_areal_precip_global[ib].zones[1] != 1 &&
	  mean_areal_precip_global[ib].zones[2] != 1 &&
	  mean_areal_precip_global[ib].zones[3] != 1)
      {

	 mapvalue = lz;

	 /* If the value is < 0 do not draw this basin. */
	 if (mapvalue < 0)
	 {
	    continue;
	 }

	 for (k = 0; k < dqc_temp_numcol - 1; k++)
	 {

	    if (mapvalue >= dqc_temp_delim[tscale][k] &&
		mapvalue < dqc_temp_delim[tscale][k + 1])
	    {

	       color = temperature_colormap[k];
	       break;

	    }

	 }

	 if (mapvalue > .01 || mapvalue < -.01)
	 {


	    if (k == (dqc_temp_numcol - 1))
	       color = temperature_colormap[dqc_temp_numcol - 1];


	    mSetColor (color);

	    /* Draw a filled basin. */
	    mDrawFillPolygon (M_EXPOSE, 0, point,
			      mean_areal_precip_global[ib].basin_points,
			      Complex, CoordModeOrigin);


	 }

	 continue;

      }

      for (l = 0; l < mean_areal_precip_global[ib].hrap_points; l++)
      {

	 x = mean_areal_precip_global[ib].hrap_data[l].x;
	 y = mean_areal_precip_global[ib].hrap_data[l].y;

	 /* search for highest zone number is hrap block */

	 ip = 1;

	 if (mean_areal_precip_global[ib].hrap_data[l].zone[1] >= 0)
	    ip = 2;

	 if (mean_areal_precip_global[ib].hrap_data[l].zone[2] >= 0)
	    ip = 3;

	 if (mean_areal_precip_global[ib].hrap_data[l].zone[3] >= 0)
	    ip = 4;

	 if (ip == 4)
	    mapvalue = gz;

	 else if (ip == 3)
	    mapvalue = uz;

	 else if (ip == 2)
	    mapvalue = mz;

	 else
	    mapvalue = lz;


	 /* If the value is smaller than 0, do not draw this HRAP grid 
	    bin. */
	 if (mapvalue < 0)
	 {
	    continue;
	 }

	 ix = x - minx;
	 iy = y - miny;


	 if (ix < 0 || iy < 0 || ix >= maxx - 1 || iy >= maxy - 1)
	 {

	    continue;

	 }



	 for (k = 0; k < dqc_temp_numcol - 1; k++)
	 {

	    if (mapvalue >= dqc_temp_delim[tscale][k] &&
		mapvalue < dqc_temp_delim[tscale][k + 1])
	    {

	       color = temperature_colormap[k];
	       break;

	    }

	 }

	 if (mapvalue <= -999)
	    continue;

	 else if (k == (dqc_temp_numcol - 1))
	    color = temperature_colormap[dqc_temp_numcol - 1];


         mConvertLatLon2XY (hrap_lat_lon[ix][iy].y,
			    hrap_lat_lon[ix][iy].x, &xpos, &ypos);

         PolyPoints[0].x = (short) xpos;
         PolyPoints[0].y = (short) ypos;


         mConvertLatLon2XY (hrap_lat_lon[ix][iy + 1].y,
			    hrap_lat_lon[ix][iy + 1].x, &xpos, &ypos);


         PolyPoints[1].x = (short) xpos;
         PolyPoints[1].y = (short) ypos;

         mConvertLatLon2XY (hrap_lat_lon[ix + 1][iy + 1].y,
	   		    hrap_lat_lon[ix + 1][iy + 1].x, &xpos, &ypos);

         PolyPoints[2].x = (short) xpos;
         PolyPoints[2].y = (short) ypos;

         mConvertLatLon2XY (hrap_lat_lon[ix + 1][iy].y,
			    hrap_lat_lon[ix + 1][iy].x, &xpos, &ypos);

         PolyPoints[3].x = (short) xpos;
         PolyPoints[3].y = (short) ypos;

         PolyPoints[4].x = PolyPoints[0].x;
         PolyPoints[4].y = PolyPoints[0].y;

         mSetColor (color);
         mDrawFillPolygon (M_EXPOSE, 0, PolyPoints, 5, Complex, 
                           CoordModeOrigin);
      }


   }

   /* Toggle on the basin boundaries. */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
