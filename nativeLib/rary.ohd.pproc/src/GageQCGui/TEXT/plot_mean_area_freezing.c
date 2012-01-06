#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "gageqc_gui.h"
#include "map_library.h"
#include "map_defines.h"
#include "post_functions.h"

/****************************************************************
plot_mean_areal_freezing()

PURPOSE: plot image for callback function of "Points+MAZs"/"MAZs"
 buttons.
*****************************************************************/
void plot_mean_areal_freezing (int h, 
                              int display_flag, 
                              int num)
{
   
   extern struct map mean_areal_precip_global[];
   extern int wfo_all;
   extern int wfo_in_use[20];
   extern int zscale;
   extern struct hrap_grid *hrap_grid;   
   extern double ** dqc_freezing_delim;
   extern int   dqc_freezing_numcol;
   extern int  pcp_in_use[500];
   
   char *color = NULL;
   char ** freezing_colormap = NULL;
   const HRAP ** hrap_lat_lon = NULL;
   int hh;
   int k;
   int x, y, ip, ib;
   int xpos;
   int ypos;
   XPoint PolyPoints[5];
   XPoint point[10000];
   int l;
   double mapvalue;
   int minx, miny, ix, iy, maxx, maxy;
   float uz, mz, lz, gz;

   freezing_colormap = get_freezing_colors ( );
   
   /* get latest color limits */
   
   set_dqc_colordelimit();

   /* check pcp_in_use flag before plot */
   
   if (pcp_in_use[num] == -1)
   {
      return;
   } 

   /* Retrieve the HRAP grid. */
   
   hrap_grid = get_hrap_grid ( );

   if ( hrap_grid == NULL )
   {
      return;
   }

   minx = hrap_grid->hrap_minx;
   miny = hrap_grid->hrap_miny;
   maxx = hrap_grid->maxi;
   maxy = hrap_grid->maxj;

   /* Retrieve the MPE Lat/Lon grid grid. */
   hrap_lat_lon = getLatLonGrid ( );

   if ( hrap_lat_lon == NULL )
   {
     return;
   }

   for (ib = 0; mean_areal_precip_global[ib].hb5[0] != 0; ib++)
   {

      if (mean_areal_precip_global[ib].zmaps_done[num - 100] <= 0)
	 continue;

      if (wfo_all != 1)
      {

	 for (hh = 0; hh < 20; hh++)
	 {

	    if (wfo_in_use[hh] == -1)
            {
	       break;
            }

	    if (mean_areal_precip_global[ib].owner == wfo_in_use[hh])
            {
	       break;
            }

	 }

	 if (wfo_in_use[hh] == -1)
         {
	    continue;
         }

      }

      for (l = 0; l < mean_areal_precip_global[ib].basin_points; l++)
      {
         mConvertLatLon2XY ( mean_areal_precip_global[ib].basin[l].lat,
                             mean_areal_precip_global[ib].basin[l].lon,
                             & xpos,
                             & ypos );
         point [ l ].x = ( short )xpos;
         point [ l ].y = ( short )ypos;

      }


      lz = mean_areal_precip_global[ib].zlz[num - 100];
      mz = mean_areal_precip_global[ib].zmz[num - 100];
      uz = mean_areal_precip_global[ib].zuz[num - 100];
      gz = mean_areal_precip_global[ib].zgz[num - 100]; 

      /* Check if there are any subareas in this basin. */
      if (mean_areal_precip_global[ib].zones[1] != 1 &&
	  mean_areal_precip_global[ib].zones[2] != 1 &&
          mean_areal_precip_global[ib].zones[3] != 1)
      {

	 mapvalue = lz; 

         /* If the value is smaller than 0, do not draw this basin. */
         if ( mapvalue < 0 )
         {
            continue;
         }

	 for (k = 0; k < dqc_freezing_numcol - 1; k++)
	 {

	    if (mapvalue >= dqc_freezing_delim[zscale][k] &&
		mapvalue < dqc_freezing_delim[zscale][k + 1])
	    {

	       color = freezing_colormap[k];
	       break;

	    }

	 }

	 if (mapvalue > .01 || mapvalue < -.01)
	 {


	    if (k == (dqc_freezing_numcol - 1))
            {
	       color = freezing_colormap[dqc_freezing_numcol - 1];
            }


            mSetColor ( color );
            mDrawFillPolygon (M_EXPOSE, 0, point, 
                              mean_areal_precip_global[ib].basin_points,
                              Complex, CoordModeOrigin);

	 } 

         /* Turn on the basin boundaries. */
	
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

         /* If the value is smaller than 0, do not draw this HRAP bin. */
         if ( mapvalue < 0 )
         {
            continue;
         }


	 ix = x - minx;
	 iy = y - miny;
	
	 
	 if (ix < 0 || iy < 0 || ix >= maxx -1 || iy >= maxy -1)
	 { 

            /* The HRAP cell is out of range.  Do not draw it. */
	    continue;

	 }

	 for (k = 0; k < dqc_freezing_numcol - 1; k++)
	 {
	    if (mapvalue >= dqc_freezing_delim[zscale][k] &&
		mapvalue < dqc_freezing_delim[zscale][k + 1])
	    {

	       color = freezing_colormap[k];
	       break;

	    }

	 }

	 if (mapvalue <= 0.01 && mapvalue >= -0.01)
         {
	    continue;
         }
	 else if (k == (dqc_freezing_numcol - 1))
         {
	    color = freezing_colormap[dqc_freezing_numcol - 1];
         }

         /* Using the MPE Lat/Lon Grid, draw the point. */

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
         mDrawFillPolygon (M_EXPOSE, 0, PolyPoints, 5,
                           Complex, CoordModeOrigin);

      }

   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
