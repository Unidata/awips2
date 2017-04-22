#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "get_mpe_colors.h"
#include "get_colorvalues.h"
#include "rfcwide.h"

extern int contour_flag;
extern int plot_view;
extern int points_flag;
extern int qpf_on;
extern int flf_on;
extern int maxmin_on;
extern int isom;
extern int pcpn_time_step;
extern int map_flag;
extern int pcp_flag;
extern int pcpn_day;
extern int grids_flag;
extern int dflag[];
extern int pcpn_time;
extern struct pdata pdata[];
extern int old_isom;

extern struct tdata tdata[];
extern struct zdata zdata[];

/* Used for containing values which delimit levels in the legend. */
float delim[3][16] =
   { {0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
      1.1, 1.2, 1.3, 1.4, 1.5},
{0.0, 2.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0,
 10.0, 11.0, 12.0, 12.5, 13.0, 13.5, 14.0, 14.5},
{1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
 60, 65, 70, 75, 100}
};

float precip_delim[NUM_PRECIP_RANGES][NUM_LEGEND_COLORS] =
   { {0.01, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1., 1.1, 1.2, 1.3, 1.4, 1.5},
{0.01, .2, .4, .6, .8, 1., 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8, 3.0},
{0.01, .3, .6, .9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3.0, 3.3, 3.6, 3.9, 4.2,
 4.5},
{0.01, .3, .6, 1.2, 1.8, 2.4, 3.0, 3.6, 4.2, 4.8, 5.4, 6.0, 6.6, 7.2, 7.8,
 8.4},
{0.01, .3, .6, 1.2, 2.4, 3.6, 4.8, 6.0, 7.2, 8.4, 9.6, 10.8, 12.0, 13.2, 14.4,
 15.6}
};

float temp_delim[NUM_TEMP_RANGES][NUM_LEGEND_COLORS] =
   { {-15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60},
{-5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70},
{5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80}
};

float freezing_delim[NUM_FREEZING_RANGES][NUM_LEGEND_COLORS] =
   { {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15} };


/* global variables */

double **dqc_precip_delim;
double **dqc_temp_delim;
double **dqc_freezing_delim;


int kscale = 0;
int zscale = 0;
int tscale = 0;

char tbuf[100];

int dqc_preprocessor_basetime_set;

void _plotGageQCData (int map_index)
{
   int time_pos;
   int add_max_flag = -1;
   int add_min_flag = -1;
   int add_topo_flag = -1;
   int change_map_flag = 1;
   int add_isohyet_flag = -1;
   int display_flag = 0;
   int h;
   char mbuf[100];

   time_t ltime;
   struct tm *gm = NULL;

   /* get the token dqc_preprocessor_basetime, the default value is 12Z which
      means that the temperature/freezingL point level1 data are at 6~12Z, 12~18Z, 18~00Z 
      nd 00~06Z time periods.
      If the token is set as 18Z, then the temperature/freezingL point level1 data
      are at 12~18Z, 18~00Z, 00~06Z and 06~12Z.
      If the token is set as 00Z, then the temperature/freezingL point level1 data
      are at 18~00Z, 00~06Z, 06~12Z and 12~18Z.
      If the token is set as 06Z, then the temperature/freezingL point level1 data
      are at 00~06Z, 06~12Z, 12~18Z.
      The label is changed for temperature and freezing level based on the token value */

   dqc_preprocessor_basetime_set = getDqcBasetime ();
   logMessage ("Token dqc_preprocessor_basetime is set as %02dZ.\n",
	       dqc_preprocessor_basetime_set);

   if (add_isohyet_flag == 1 && isom != old_isom)
   {

      old_isom = isom;
      change_map_flag = 1;

   }

   if (change_map_flag == 1)
   {

//            clear_drawable(pixb);

      if (add_topo_flag == 1)
      {

//                 if(contour_topo_flag==-1)
//                 plot_topo(pixb,h,display_flag);

//                 else
//                 contour_topo(pixb,h,display_flag);

	 change_map_flag = 0;


      }

      if (add_isohyet_flag == 1)
      {

//                  if(contour_pcp_flag==-1)
//                     plot_isohyets(isom,pixb,h,display_flag);

//                  else
//                     contour_isohyets(isom,pixb,h,display_flag);

	 change_map_flag = 0;

      }

      if (add_max_flag == 1)
      {

//                  if(contour_maxmin_flag==-1)
	 //                  plot_maxmin(isom,h,display_flag,0);

//                  else
//                     contour_maxmin(isom,pixb,h,display_flag,0);

	 change_map_flag = 0;

      }

      if (add_min_flag == 1)
      {

	 //                if(contour_maxmin_flag==-1)
	 //           plot_maxmin(isom,h,display_flag,1);

//                  else
//                     contour_maxmin(isom,pixb,h,display_flag,1); 

	 change_map_flag = 0;

      }
   }

   for (h = 0; h < display_flag + 1; h++)
   {


      /* copy raster backgrounds */

//             if(add_topo_flag==1 || add_isohyet_flag==1 ||
//              add_max_flag==1 || add_min_flag==1) 

//                  XCopyArea(display,pixb,pixm,gc,0,0,xsize/10,ysize/10,0,0); 

/* maps background */

//             if(fgbgflag==-1)
//                  draw_map(pixm,h,display_flag);


      /* precipitation point, gridded, MAP, and contoured data. */
      if (qpf_on == 1)
      {

	 if (map_flag == 1)
	 {

	    if (pcpn_time_step == 0)
	       time_pos = pcp_flag;

	    else
	       time_pos = 40 + pcpn_day;

	    plot_mean_areal_precip (h, display_flag, time_pos);

	 }

	 /* gridded precipitation */

	 if (grids_flag == 1)
	 {

	    if (pcpn_time_step == 0)
	    {
	       time_pos = pcp_flag;
	    }
	    else
	    {
	       time_pos = 40 + pcpn_day;
	    }

	    plot_gridded_precip (h, display_flag, "pcp", time_pos, 100);

	 }

	 /* Contoured precipitation. */
	 if (contour_flag == 1)
	 {
	    if (pcpn_time_step == 0)
	    {
	       time_pos = pcp_flag;
	    }
	    else
	    {
	       time_pos = 40 + pcpn_day;
	    }

	    contour_precip (h, display_flag, "pcp", time_pos);
	 }

	 /* Point precipitation data. */
	 if (plot_view > 0 && points_flag == 1)
	 {
	    plot_precip_stations (plot_view, map_index);
	 }

      }
      else if (flf_on == 1)
      {
	 /* Plot freezing level data. */
	 if (map_flag == 1)
	 {
	    time_pos = 100 + pcp_flag;

	    plot_mean_areal_freezing (h, display_flag, time_pos);
	 }

	 if (grids_flag == 1)
	 {
	    time_pos = 100 + pcp_flag;
	    plot_gridded_freezing (h, display_flag, "pcp", time_pos);
	 }

	 if (contour_flag == 1)
	 {
	    time_pos = 100 + pcp_flag;
	    contour_freezing (h, display_flag, "pcp", time_pos);
	 }

	 if (plot_view > 0 && points_flag == 1)
	 {
	    plot_freezing_stations (plot_view, map_index);
	 }


      }

      else if (maxmin_on == 1)
      {

	 if (map_flag == 1 && pcpn_time_step == 0)
	 {

	    time_pos = 150 + pcp_flag;

	    plot_mean_area_temperature (h, display_flag, time_pos);

	 }

	 if (grids_flag == 1)
	 {

	    if (pcpn_time_step == 0)
	       time_pos = 150 + pcp_flag;

	    else if (pcpn_time_step == 1)
	       time_pos = 190 + pcpn_day;

	    else if (pcpn_time_step == 2)
	       time_pos = 200 + pcpn_day;

	    plot_gridded_temperature (h, display_flag, "pcp", time_pos);

	 }

	 if (contour_flag == 1)
	 {
	    if (pcpn_time_step == 0)
	       time_pos = 150 + pcp_flag;

	    else if (pcpn_time_step == 1)
	       time_pos = 190 + pcpn_day;

	    else if (pcpn_time_step == 2)
	       time_pos = 200 + pcpn_day;


	    contour_temperature (h, display_flag, "pcp", time_pos);
	 }

	 if (plot_view > 0 && points_flag == 1)
	    plot_temperature_stations (plot_view, map_index);


      }


      /* map backgrounds foreground */

//           if(fgbgflag==1)
//                      draw_map(pixm,h,display_flag);

   }

   if (qpf_on == 1)
   {

      if (pcpn_time_step == 0)
	 time_pos = pcpn_time;

      else
	 time_pos = 4;

      /* Precipitation period is always 12z-12z. */
      if (pcpn_time < 2 && pcpn_time_step == 0)
	    ltime = pdata[pcpn_day].data_time - 24L * 3600L;
      else
	    ltime = pdata[pcpn_day].data_time;

      gm = gmttime (&ltime);

      sprintf (tbuf, "Precipitation ");

      if (points_flag == 1 && grids_flag == -1 && map_flag == -1)
	 strcpy (mbuf, "Points ");

      else if (points_flag == -1 && grids_flag == 1 && map_flag == -1)
	 strcpy (mbuf, "Grids ");

      else if (points_flag == -1 && grids_flag == -1 && map_flag == 1)
	 strcpy (mbuf, "MAPs ");

      else if (points_flag == 1 && grids_flag == 1 && map_flag == -1)
	 strcpy (mbuf, "Points+Grids ");

      else if (points_flag == 1 && grids_flag == -1 && map_flag == 1)
	 strcpy (mbuf, "Points+MAPs ");

      else if (points_flag == -1 && grids_flag == -1 && map_flag == -1)
	 strcpy (mbuf, " ");

      strcat (tbuf, mbuf);

      sprintf (mbuf, "%02d-%02d-%04d", gm->tm_mon + 1, gm->tm_mday,
	       gm->tm_year + 1900);

      strcat (tbuf, mbuf);

      if (pcpn_time_step == 0)
      {
	 /* Precipitation is always 12z-12z. */ 
         if (pcpn_time == 0)
	 {
	    strcat (tbuf, " 12z-18z");
	 }
	 else if (pcpn_time == 1)
	 {
	    strcat (tbuf, " 18z-00z");
	 }
	 else if (pcpn_time == 2)
	 {
	    strcat (tbuf, " 00z-06z");
	 }
	 else if (pcpn_time == 3)
	 {
	    strcat (tbuf, " 06z-12z");
	 }
      }

      else
      {
         strcat (tbuf, " ending at 12z");
      }

      if (pdata[pcpn_day].level == 1)
	 strcat (tbuf, " - Level 1");

      else if (pdata[pcpn_day].level == 2)
	 strcat (tbuf, " - Level 2");


      if (pdata[pcpn_day].used[time_pos] == 4)
	 strcat (tbuf, " Saved");


      else if (pdata[pcpn_day].used[time_pos] == 3 ||
	       pdata[pcpn_day].used[time_pos] == 2)
	 strcat (tbuf, " Modified");

      else if (pdata[pcpn_day].used[time_pos] == 1)
	 strcat (tbuf, " Not Modified");

      else
	 strcat (tbuf, " - No Data");


   }

   else if (flf_on == 1)
   {

      time_pos = pcpn_time;

      if (dqc_preprocessor_basetime_set == 18)
      {
	 if (pcpn_time < 1)
	    ltime = zdata[pcpn_day].data_time - 24L * 3600L;

	 else
	    ltime = zdata[pcpn_day].data_time;

      }
      else if (dqc_preprocessor_basetime_set == 0)
      {
	 ltime = zdata[pcpn_day].data_time;

      }
      else if (dqc_preprocessor_basetime_set == 6)
      {
	 if (pcpn_time < 3)
	    ltime = zdata[pcpn_day].data_time - 24L * 3600L;

	 else
	    ltime = zdata[pcpn_day].data_time;

      }
      else
      {
	 if (pcpn_time < 2)
	    ltime = zdata[pcpn_day].data_time - 24L * 3600L;

	 else
	    ltime = zdata[pcpn_day].data_time;
      }

      gm = gmttime (&ltime);

      sprintf (tbuf, "Freezing Level ");

      if (points_flag == 1 && grids_flag == -1 && map_flag == -1)
	 strcpy (mbuf, "Points ");

      else if (points_flag == -1 && grids_flag == 1 && map_flag == -1)
	 strcpy (mbuf, "Grids ");

      else if (points_flag == -1 && grids_flag == -1 && map_flag == 1)
	 strcpy (mbuf, "MAZs ");

      else if (points_flag == 1 && grids_flag == 1 && map_flag == -1)
	 strcpy (mbuf, "Points+Grids ");

      else if (points_flag == 1 && grids_flag == -1 && map_flag == 1)
	 strcpy (mbuf, "Points+MAZs ");

      else if (points_flag == -1 && grids_flag == -1 && map_flag == -1)
	 strcpy (mbuf, " ");

      strcat (tbuf, mbuf);

      sprintf (mbuf, "%02d-%02d-%04d", gm->tm_mon + 1, gm->tm_mday,
	       gm->tm_year + 1900);

      strcat (tbuf, mbuf);


      if (dqc_preprocessor_basetime_set == 18)
      {
	 if (pcpn_time == 0)
	    strcat (tbuf, " 18z");
	 else if (pcpn_time == 1)
	    strcat (tbuf, " 00z");
	 else if (pcpn_time == 2)
	    strcat (tbuf, " 06z");
	 else if (pcpn_time == 3)
	    strcat (tbuf, " 12z");

      }
      else if (dqc_preprocessor_basetime_set == 00)
      {
	 if (pcpn_time == 0)
	    strcat (tbuf, " 00z");
	 else if (pcpn_time == 1)
	    strcat (tbuf, " 06z");
	 else if (pcpn_time == 2)
	    strcat (tbuf, " 12z");
	 else if (pcpn_time == 3)
	    strcat (tbuf, " 18z");
      }
      else if (dqc_preprocessor_basetime_set == 6)
      {
	 if (pcpn_time == 0)
	    strcat (tbuf, " 06z");
	 else if (pcpn_time == 1)
	    strcat (tbuf, " 12z");
	 else if (pcpn_time == 2)
	    strcat (tbuf, " 18z");
	 else if (pcpn_time == 3)
	    strcat (tbuf, " 00z");
      }
      else
      {
	 if (pcpn_time == 0)
	    strcat (tbuf, " 12z");
	 else if (pcpn_time == 1)
	    strcat (tbuf, " 18z");
	 else if (pcpn_time == 2)
	    strcat (tbuf, " 00z");
	 else if (pcpn_time == 3)
	    strcat (tbuf, " 06z");

      }

      if (zdata[pcpn_day].level[pcpn_time] == 1)
	 strcat (tbuf, " - Level 1");

      else if (zdata[pcpn_day].level[pcpn_time] == 2)
	 strcat (tbuf, " - Level 2");


      if (zdata[pcpn_day].used[time_pos] == 6)
	 strcat (tbuf, " Calculated");


      else if (zdata[pcpn_day].used[time_pos] == 4)
	 strcat (tbuf, " Saved");


      else if (zdata[pcpn_day].used[time_pos] == 3 ||
	       zdata[pcpn_day].used[time_pos] == 2)
	 strcat (tbuf, " Modified");

      else if (zdata[pcpn_day].used[time_pos] == 1)
	 strcat (tbuf, " Not Modified");

      else
	 strcat (tbuf, " - No Data");

   }

   else if (maxmin_on == 1)
   {

      if (pcpn_time_step == 0)
	 time_pos = pcpn_time;

      else if (pcpn_time_step == 1)
	 time_pos = 4;

      else if (pcpn_time_step == 2)
	 time_pos = 5;


      if (dqc_preprocessor_basetime_set == 18)
      {
	 if (pcpn_time < 1 && pcpn_time_step == 0)
	    ltime = tdata[pcpn_day].data_time - 24L * 3600L;

	 else
	    ltime = tdata[pcpn_day].data_time;


      }
      else if (dqc_preprocessor_basetime_set == 0)
      {
	 ltime = tdata[pcpn_day].data_time;
      }
      else if (dqc_preprocessor_basetime_set == 6)
      {
	 if (pcpn_time < 3 && pcpn_time_step == 0)
	    ltime = tdata[pcpn_day].data_time - 24L * 3600L;

	 else
	    ltime = tdata[pcpn_day].data_time;

      }
      else
      {
	 if (pcpn_time < 2 && pcpn_time_step == 0)
	    ltime = tdata[pcpn_day].data_time - 24L * 3600L;

	 else
	    ltime = tdata[pcpn_day].data_time;
      }

      gm = gmttime (&ltime);

      if (pcpn_time_step == 1)
	 sprintf (tbuf, "Maximum Temperature ");

      else if (pcpn_time_step == 2)
	 sprintf (tbuf, "Minimum Temperature ");

      else
	 sprintf (tbuf, "Temperature ");

      if (points_flag == 1 && grids_flag == -1 && map_flag == -1)
	 strcpy (mbuf, "Points ");

      else if (points_flag == -1 && grids_flag == 1 && map_flag == -1)
	 strcpy (mbuf, "Grids ");

      else if (points_flag == -1 && grids_flag == -1 && map_flag == 1)
	 strcpy (mbuf, "MATs ");

      else if (points_flag == 1 && grids_flag == 1 && map_flag == -1)
	 strcpy (mbuf, "Points+Grids ");

      else if (points_flag == 1 && grids_flag == -1 && map_flag == 1)
	 strcpy (mbuf, "Points+MATs ");

      else if (points_flag == -1 && grids_flag == -1 && map_flag == -1)
	 strcpy (mbuf, " ");

      strcat (tbuf, mbuf);

      sprintf (mbuf, "%02d-%02d-%04d", gm->tm_mon + 1, gm->tm_mday,
	       gm->tm_year + 1900);

      strcat (tbuf, mbuf);


      if (dqc_preprocessor_basetime_set == 18)
      {
	 if (pcpn_time_step == 0)
	 {

	    if (pcpn_time == 0)
	       strcat (tbuf, " 18z");
	    else if (pcpn_time == 1)
	       strcat (tbuf, " 00z");
	    else if (pcpn_time == 2)
	       strcat (tbuf, " 06z");
	    else if (pcpn_time == 3)
	       strcat (tbuf, " 12z");
	 }

	 else
	    strcat (tbuf, " ending at 18z");

      }
      else if (dqc_preprocessor_basetime_set == 0)
      {
	 if (pcpn_time_step == 0)
	 {
	    if (pcpn_time == 0)
	       strcat (tbuf, " 00z");
	    else if (pcpn_time == 1)
	       strcat (tbuf, " 06z");
	    else if (pcpn_time == 2)
	       strcat (tbuf, " 12z");
	    else if (pcpn_time == 3)
	       strcat (tbuf, " 18z");
	 }

	 else
	    strcat (tbuf, " ending at 00z");

      }
      else if (dqc_preprocessor_basetime_set == 6)
      {
	 if (pcpn_time_step == 0)
	 {
	    if (pcpn_time == 0)
	       strcat (tbuf, " 06z");
	    else if (pcpn_time == 1)
	       strcat (tbuf, " 12z");
	    else if (pcpn_time == 2)
	       strcat (tbuf, " 18z");
	    else if (pcpn_time == 3)
	       strcat (tbuf, " 00z");
	 }

	 else
	    strcat (tbuf, " ending at 06z");

      }

      else
      {
	 if (pcpn_time_step == 0)
	 {

	    if (pcpn_time == 0)
	       strcat (tbuf, " 12z");
	    else if (pcpn_time == 1)
	       strcat (tbuf, " 18z");
	    else if (pcpn_time == 2)
	       strcat (tbuf, " 00z");
	    else if (pcpn_time == 3)
	       strcat (tbuf, " 06z");
	 }

	 else
	    strcat (tbuf, " ending at 12z");

      }

      if (tdata[pcpn_day].level[pcpn_time] == 1)
	 strcat (tbuf, " - Level 1");

      else if (tdata[pcpn_day].level[pcpn_time] == 2)
	 strcat (tbuf, " - Level 2");

      if (tdata[pcpn_day].used[time_pos] == 4)
	 strcat (tbuf, " Saved");

      else if (tdata[pcpn_day].used[time_pos] == 3 ||
	       tdata[pcpn_day].used[time_pos] == 2)
	 strcat (tbuf, " Modified");

      else if (tdata[pcpn_day].used[time_pos] == 1)
	 strcat (tbuf, " Not Modified");

      else
	 strcat (tbuf, " - No Data");


   }

   /* Trigger the legend to update. */
   mUpdateLegend (map_index);


/*
if(change_topo_flag==1)
             redraw_topo_legend(pixm);

else if(change_isohyet_flag==1)
             redraw_isohyet_legend(pixm);

else if(change_pcpn_flag==1)
             redraw_pcpn_legend(pixm);

else if(change_diff_flag==1)
             redraw_diff_legend(pixm);

else if(change_frz_flag==1)
             redraw_frz_legend(pixm);

else if(change_rpcpn_flag==1)
             redraw_rpcpn_legend(pixm);
     
else if(change_maxmin_flag==1)
             redraw_maxmin_legend(pixm);
     
else if(first_through != 0)
             redraw_legend_display(pixm);

*/

}

/**************************************************************
set_dqc_colorvalues()
PURPOSE: retrieve color name and color value from ColorValue table
in case the user has modified the color information through
ColorThresholds window. If not found from ColorValue table, use
default sets from gridded_precip_colormap array defined in
colormaps.c
**************************************************************/
void
set_dqc_colorvalues (char *cv_use_name,
		     int cv_default_dur,
		     int *cv_numcol,
		     const NamedColorSetGroup * dqc_default_pColors)
{

   /* Global variables. */

   extern char LOGNAME[];

   extern char **dqc_colorlevels_name;
   extern double *dqc_colorlevels_value;

   /* the APPLICATION_NAME is defined as hmapmpe */

   const char *application_name = APPLICATION_NAME;

   int numcol, i;
   int numlev;

   ColorValue *cvHead = NULL;
   ColorValue *cvPtr = NULL;


   /*-----------------------------------------------*/
   /*  read levels and colors from ColorValue table */
   /*  or from hardcoded defaults.                  */
   /*-----------------------------------------------*/

   cvHead = get_colorvalues (LOGNAME, application_name,
			     cv_use_name, cv_default_dur, 'E',
			     &numcol, &numlev, dqc_default_pColors);


   /* malloc dqc_colorlevels_name and dqc_colorlevels_value */

   dqc_colorlevels_name = (char **) malloc (sizeof (char *) * numcol);

   for (i = 0; i < numcol; i++)
   {
      dqc_colorlevels_name[i] = (char *) malloc (COLOR_NAME_LEN + 1);
      if (dqc_colorlevels_name[i] == NULL)
	 logMessage
	    ("failed malloc for dqc_colorlevels_name in set_dqc_colorvalue.\n");
   }

   dqc_colorlevels_value = (double *) malloc (sizeof (double) * numcol);
   if (dqc_colorlevels_value == NULL)
      logMessage
	 ("failed malloc for dqc_colorlevels_value in set_dqc_colorvalue.\n");


   numcol = 0;
   numlev = 0;

   if (cvHead != NULL)
   {
      cvPtr = (ColorValue *) ListFirst (&cvHead->list);

      while (cvPtr != NULL)
      {

	 strcpy (dqc_colorlevels_name[numcol], cvPtr->color_name);
	 numcol++;

	 /*  if(cvPtr->threshold_value >= 0.0)
	    {
	    dqc_colorlevels_value[numlev] = cvPtr->threshold_value;
	    numlev++;
	    } */

	 dqc_colorlevels_value[numlev] = cvPtr->threshold_value;
	 numlev++;

	 cvPtr = (ColorValue *) ListNext (&cvPtr->node);
      }

      FreeColorValue (cvHead);

      *cv_numcol = numcol;
   }

   return;

}

/********************************************************************************************
set_dqc_colordelimit()

Retrieve color thresholds from colorvalue table for DQC precipitation, temperature and freezing level.
The retrieval value is used as initial range, there are 5 (NUM_PRECIP_RANGES) for precipitation
fields, 3 [NUM_TEMP_RANGES) for temperatue and 1 (NUM_FREEZING_RANGES) for freezing level.
*********************************************************************************************/
void set_dqc_colordelimit ()
{

   extern double *dqc_colorlevels_value;
   extern int dqc_precip_numcol;
   extern int dqc_temp_numcol;
   extern int dqc_freezing_numcol;
   int i, j;

   /* malloc space for dqc_precip_delim, dqc_temp_delim, dqc_freezing_delim */

   dqc_precip_delim =
      (double **) malloc (sizeof (double *) * NUM_PRECIP_RANGES);
   if (dqc_precip_delim == NULL)
      logMessage
	 ("failed malloc for dqc_precip_delim in set_dqc_colordelimit.\n");

   dqc_temp_delim = (double **) malloc (sizeof (double *) * NUM_TEMP_RANGES);
   if (dqc_temp_delim == NULL)
      logMessage
	 ("failed malloc for dqc_temp_delim in set_dqc_colordelimit.\n");

   dqc_freezing_delim =
      (double **) malloc (sizeof (double *) * NUM_FREEZING_RANGES);
   if (dqc_freezing_delim == NULL)
      logMessage
	 ("failed malloc for dqc_freezing_delim in set_dqc_colordelimit.\n");

   for (i = 0; i < NUM_PRECIP_RANGES; i++)
   {
      dqc_precip_delim[i] =
	 (double *) malloc (sizeof (double) * dqc_precip_numcol);
      if (dqc_precip_delim[i] == NULL)
	 logMessage
	    ("failed malloc for dqc_precip_delim[] in set_dqc_colordelimit.\n");
   }

   for (i = 0; i < NUM_TEMP_RANGES; i++)
   {
      dqc_temp_delim[i] =
	 (double *) malloc (sizeof (double) * dqc_temp_numcol);
      if (dqc_temp_delim[i] == NULL)
	 logMessage
	    ("failed malloc for dqc_temp_delim[] in set_dqc_colordelimit.\n");
   }

   for (i = 0; i < NUM_FREEZING_RANGES; i++)
   {
      dqc_freezing_delim[i] =
	 (double *) malloc (sizeof (double) * dqc_freezing_numcol);
      if (dqc_freezing_delim[i] == NULL)
	 logMessage
	    ("failed malloc for dqc_freezing_delim[] in set_dqc_colordelimit.\n");
   }

   /* set color limit for precip */

   for (i = 0; i < NUM_PRECIP_RANGES; i++)
   {
      for (j = 0; j < dqc_precip_numcol; j++)
      {
	 if (i == 0)
	    dqc_precip_delim[0][j] = dqc_colorlevels_value[j];
	 else if (i == 1)
	 {
	    if (j == 0)
	       dqc_precip_delim[1][0] = dqc_colorlevels_value[0];
	    else
	       dqc_precip_delim[1][j] =
		  dqc_colorlevels_value[dqc_precip_numcol - 1] * 2.0 -
		  (0.2 * (dqc_precip_numcol - 1 - j));
	 }
	 else if (i == 2)
	 {
	    if (j == 0)
	       dqc_precip_delim[2][0] = dqc_colorlevels_value[0];
	    else
	       dqc_precip_delim[2][j] =
		  dqc_colorlevels_value[dqc_precip_numcol - 1] * 3.0 -
		  (0.3 * (dqc_precip_numcol - 1 - j));

	 }
	 else if (i == 3)
	 {
	    if (j == 0)
	       dqc_precip_delim[3][0] = dqc_colorlevels_value[0];
	    else if (j == 1)
	       dqc_precip_delim[3][1] = 30.0 * dqc_colorlevels_value[0];
	    else
	       dqc_precip_delim[3][j] =
		  dqc_colorlevels_value[dqc_precip_numcol - 2] * 6.0 -
		  (0.6 * (dqc_precip_numcol - 1 - j));

	 }

	 else if (i == 4)
	 {
	    if (j == 0)
	       dqc_precip_delim[4][0] = dqc_colorlevels_value[0];
	    else if (j == 1)
	       dqc_precip_delim[4][1] = 30.0 * dqc_colorlevels_value[0];
	    else if (j == 2)
	       dqc_precip_delim[4][2] = 60.0 * dqc_colorlevels_value[0];
	    else
	       dqc_precip_delim[4][j] =
		  (dqc_colorlevels_value[dqc_precip_numcol - 1] * 10.0 +
		   0.6) - (1.2 * (dqc_precip_numcol - 1 - j));

	 }

	 else			/* default to initial */
	    dqc_precip_delim[i][j] = dqc_colorlevels_value[j];
      }

   }

   /* set color limit for temp */

   for (i = 0; i < NUM_TEMP_RANGES; i++)
   {
      for (j = 0; j < dqc_temp_numcol; j++)
      {
	 if (i == 0)
	    dqc_temp_delim[0][j] = dqc_colorlevels_value[j];
	 else if (i == 1)
	    dqc_temp_delim[1][j] =
	       (dqc_colorlevels_value[dqc_temp_numcol - 1] + 10.0) -
	       (5.0 * (dqc_temp_numcol - 1 - j));
	 else if (i == 2)
	    dqc_temp_delim[2][j] =
	       (dqc_colorlevels_value[dqc_temp_numcol - 1] + 20.0) -
	       (5.0 * (dqc_temp_numcol - 1 - j));
	 else			/* default to initial */
	    dqc_temp_delim[0][j] = dqc_colorlevels_value[j];
      }
   }

   /* set color limit for freezing level */

   for (i = 0; i < NUM_FREEZING_RANGES; i++)
   {
      for (j = 0; j < dqc_freezing_numcol; j++)
      {
	 dqc_freezing_delim[i][j] = dqc_colorlevels_value[j];
      }
   }

   return;


/*  ==============  Statements containing RCS keywords:  */
   {
      static char rcs_id1[] =
	 "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/draw_dqc_stations.c,v $";
      static char rcs_id2[] =
	 "$Id: draw_dqc_stations.c,v 1.13 2007/10/18 15:46:22 lawrence Exp $";
   }
/*  ===================================================  */

}
