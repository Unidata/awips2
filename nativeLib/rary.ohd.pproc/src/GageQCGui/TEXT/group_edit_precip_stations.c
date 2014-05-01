#include "gageqc_gui.h"
#include "map_library.h"

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
extern int max_stations;
extern float filter_value;
extern struct station *station;
extern int gage_char[];
extern int tsmax;
extern struct ts ts[];
extern int qflag[];
extern struct dval dval;
extern int group_qual;

void group_edit_precip_stations (int map_num, int x, int y)
{
   int time_pos;
   int i, m, k;
   float lat, lon;
   int x1;
   int y1;
   double testdist, maxdist;
   int isave;
   extern int pcp_in_use[];

extern Widget rpbutton;

   if (pcpn_time_step == 0)
      time_pos = pcpn_time;

   else
      time_pos = 4;

   isave = -1;
   maxdist = 9999;

   /* Loop over all stations.  Determine which station is the closest
      to the mouse click point. */
   for (i = 0; i < max_stations; i++)
   {
      if (pdata[pcpn_day].stn[i].frain[time_pos].data < 0)
      {
	 continue;
      }

      if (pdata[pcpn_day].stn[i].frain[time_pos].data < filter_value)
      {
	 continue;
      }

      /* Retrieve the latitude and longitude of this station. */
      lat = station[i].lat;
      lon = station[i].lon;

      if (station[i].tip == 0 && gage_char[0] == -1)
      {
	 continue;
      }

      if (station[i].tip == 1 && gage_char[1] == -1)
      {
	 continue;
      }

      for (m = 0; m < tsmax; m++)
      {

	 if (strncmp (&station[i].parm[3], ts[m].abr, 2) == 0
	     && dflag[m + 1] == 1)
	    break;

      }


      if (m == tsmax)
      {
	 continue;
      }

      for (m = 0; m < 9; m++)
      {

	 if (m == pdata[pcpn_day].stn[i].frain[time_pos].qual &&
	     qflag[m] == 1)
	    break;

      }

      if (m == 9)
      {
	 continue;
      }

      mConvertLatLon2XY (lat, -1 * lon, &x1, &y1);

      testdist = pow ((double) (x - (float)x1), 2) + 
                 pow ((double) (y - (float)y1), 2);
      testdist = pow (testdist, .5);

      if (testdist < maxdist)
      {
	 isave = i;
	 maxdist = testdist;
      }

   }

   if (isave == -1)
      return;

   if(pdata[pcpn_day].stn[isave].frain[time_pos].qual != group_qual)
   {
      XtSetSensitive(rpbutton,True);
   }

   pdata[pcpn_day].stn[isave].frain[time_pos].qual = group_qual;

   if (group_qual == 1 &&
       time_pos == 4 && pdata[pcpn_day].stn[isave].sflag[time_pos] == 1)
   {

      pdata[pcpn_day].stn[isave].frain[time_pos].data =
	 pdata[pcpn_day].stn[isave].rain[time_pos].data;

      pdata[pcpn_day].stn[isave].sflag[time_pos] = -1;

   }

   if (time_pos == 4
       && (group_qual == 1 || group_qual == 0 || group_qual == 8))
   {

      for (k = 0; k < 4; k++)
	 pdata[pcpn_day].stn[isave].frain[k].qual = group_qual;

   }

   /* 6 hour data set bad set 24 hour bad too */

   if (time_pos != 4 && group_qual == 1 &&
       pdata[pcpn_day].stn[isave].frain[4].qual != 5 &&
       pdata[pcpn_day].stn[isave].frain[4].qual != 4)
      pdata[pcpn_day].stn[isave].frain[4].qual = group_qual;


   for (k = 0; k < 5; k++)
   {

      if (k < 4)
	 time_pos = pcpn_day * 4 + k;

      else
	 time_pos = 40 + pcpn_day;

      pcp_in_use[time_pos] = -1;


      if (pdata[pcpn_day].used[k] != 0)
	 pdata[pcpn_day].used[k] = 2;
   }
/*    
XtSetSensitive(diswidget[1],False);
XtSetSensitive(diswidget[2],False);
XtSetArg(args[0],XmNset,False);        
XtSetValues(diswidget[1],args,1);
XtSetArg(args[0],XmNset,False);        
XtSetValues(diswidget[2],args,1);
*/
//write_screen();

   send_expose ( );
   return;

}
