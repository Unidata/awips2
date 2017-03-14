#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "gageqc_gui.h"
#include "map_library.h"
#include "map_defines.h"
#include "post_functions.h"

/****************************************************************
plot_mean_areal_precip()

PURPOSE: plot image for callback function of "Points+MAPs" button
*****************************************************************/
void plot_mean_areal_precip (int h, int display_flag, int num)
{
   
   extern double ** dqc_precip_delim;
   extern int   dqc_precip_numcol;
   extern int dmvalue;  
   extern int kscale;
   extern struct map mean_areal_precip_global[];
   extern int pcp_in_use[500];
   extern int pcpn_time_step;
   extern int rsmode;     
   extern struct pcp *spf;
   extern struct pcp *pcp;
   extern int wfo_all;
   extern int wfo_in_use[20];

   XPoint point[10000];
   XPoint PolyPoints[5];
   char *color = NULL;
   char **precip_colormap = NULL;
   double mapvalue;
   int hh;
   struct hrap_grid *hrap_grid = NULL;
   int k;
   int l;
   int minx, miny, ix, iy, maxx, maxy;
   int x, y, ip, ib;
   int xpos;
   int ypos;
   float uz, mz, lz, gz;
   int i1 = 0;
   int i, j;
   char file[100];
   const HRAP **hrap_lat_lon = NULL;

   /* Retrieve the precipitation colormap. */
   
   precip_colormap = get_precip_colors ();
   
   /* get latest color limits */
   
   set_dqc_colordelimit();
    
   /* check pcp_in_use flag before plot */
   
   if (pcp_in_use[num] == -1)
   {
      return;
   }
    
   /* Retrieve the HRAP grid. */
   
   hrap_grid = get_hrap_grid ();

   /* Retrieve the MPE Lat/Lon grid. */
   
   hrap_lat_lon = getLatLonGrid ();

   if (hrap_lat_lon == NULL)
   {
      return;
   }

   strcpy (file, "pcp");

   if (pcpn_time_step == 0 && rsmode != 1)
   {
      i1 = 1;

      if (num == 0)
      {
	 i1 = 0;
      }

      if (pcp_in_use[num + 100] != -1 && pcp_in_use[num + 100 - i1] != -1)
      {
	 read_file (file, num + 100, spf);
	 read_file (file, num + 100 - i1, pcp);

	 for (i = 0; i < hrap_grid->maxi - 1; i++)
	 {
	    for (j = 0; j < hrap_grid->maxj - 1; j++)
	    {
	       spf->value[i][j] = (spf->value[i][j] + pcp->value[i][j]) / 2;
	    }
	 }
      }
      else if (pcp_in_use[num + 100] == 1)
      {
	 read_file (file, num + 100, spf);
      }
      else if (pcp_in_use[num + 100 - i1] == 1)
      {
	 read_file (file, num + 100 - i1, spf);
      }
   }

   minx = hrap_grid->hrap_minx;
   miny = hrap_grid->hrap_miny;
   maxx = hrap_grid->maxi;
   maxy = hrap_grid->maxj;

   for (ib = 0; mean_areal_precip_global[ib].hb5[0] != 0; ib++)
   {
      if (mean_areal_precip_global[ib].maps_done[num] <= 0)
      {
	 continue;
      }

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
	 mConvertLatLon2XY (mean_areal_precip_global[ib].basin[l].lat,
			    mean_areal_precip_global[ib].basin[l].lon,
			    &xpos, &ypos);
	 point[l].x = (short) xpos;
	 point[l].y = (short) ypos;
      }


      lz = mean_areal_precip_global[ib].lz[num];
      mz = mean_areal_precip_global[ib].mz[num];
      uz = mean_areal_precip_global[ib].uz[num];
      gz = mean_areal_precip_global[ib].gz[num];

      /* If there are no subareas and this is raster mode. */
      if ((mean_areal_precip_global[ib].zones[1] != 1) &&
	  (mean_areal_precip_global[ib].zones[2] != 1) &&
	  (mean_areal_precip_global[ib].zones[3] != 1) && (rsmode == 1))
      {
	 mapvalue = lz;

         /* If the value is smaller than 0, then do not draw this basin. */
         if ( mapvalue < 0 )
         {
            continue;
         }

	 for (k = 0; k < dqc_precip_numcol - 1; k++)
	 {
	    if (mapvalue >= dqc_precip_delim[kscale][k] &&
		mapvalue < dqc_precip_delim[kscale][k + 1])
	    {
	       color = precip_colormap[k];
	       break;
	    }

	 }

	 if (mapvalue > .01 || mapvalue < -.01)
	 {


	    if (k == (dqc_precip_numcol - 1))
	    {
	       color = precip_colormap[dqc_precip_numcol - 1];
	    }

	    mSetColor (color);

	    /* Draw a filled basin. */
	    mDrawFillPolygon (M_EXPOSE, 0, point,
			      mean_areal_precip_global[ib].basin_points,
			      Complex, CoordModeOrigin);
	 }


	 /* Turn on the basin boundaries. */
	 continue;

      }

      for (l = 0; l < mean_areal_precip_global[ib].hrap_points; l++)
      {

	 /* Retrieve the HRAP Coordinates of the HRAP Cell being processed. */
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

         /* If the value is smaller than 0, then do not draw this HRAP 
            grid bin. */
         if ( mapvalue < 0 )
         {
            continue;
         }


	 /* Not sure if Linesegs is relative or absolute. */
	 ix = x - minx;
	 iy = y - miny;

	 if (ix < 0 || iy < 0 || ix >= maxx - 1 || iy >= maxy - 1)
	 {
	    /* The HRAP grid cell is out of range. Don't draw it. */
	    continue;
	 }

	 if (rsmode == 1)
	 {
	    for (k = 0; k < dqc_precip_numcol - 1; k++)
	    {
	       if (mapvalue >= dqc_precip_delim[kscale][k] &&
		   mapvalue < dqc_precip_delim[kscale][k + 1])
	       {
		  color = precip_colormap[k];
		  break;
	       }
	    }

	    if (mapvalue <= 0.01 && mapvalue >= -0.01)
	    {
	       continue;
	    }
	    else if (k == (dqc_precip_numcol - 1))
	    {
	       color = precip_colormap[dqc_precip_numcol - 1];
	    }
	 }
	 else
	 {
	    for (k = 0; k < 4; k++)
	    {

	       if (mapvalue >= dqc_precip_delim[kscale][k] &&
		   mapvalue < dqc_precip_delim[kscale][k + 1])
	       {
		  color = precip_colormap[k];
		  break;
	       }
	    }

	    if (mapvalue <= 0.01 && mapvalue >= -0.01)
	       continue;

	    if (k == 4)
	       color = precip_colormap[4];

	    if (mapvalue < -99.98)
	       color = precip_colormap[7];

	    if (color == 0)
	       continue;
	 }

	 if (rsmode != 1 &&
	     (pcp_in_use[100 + num] == 1 || pcp_in_use[100 + num - i1] == 1))
	 {

	    if ((spf->value[ix][iy] * 10 - dmvalue <
		 hrap_grid->elev[ix][iy]) && spf->value[ix][iy] >= 0)
	    {
	       color = precip_colormap[k + 5];
	    }

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

      /* Toggle on the basin boundaries. */
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
