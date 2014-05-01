#include <stdio.h>
#include <time.h>
#include <Xm/Xm.h>

#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "gageqc_defs.h"
#include "map_resource.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "Xtools.h"
#include "drawa.h"

extern int pcpn_day;
extern int pcpn_time;
extern int pcpn_time_step;
extern char *ttimefile[][6];
extern char *ztimefile[][4];

void other_pcpn_options (Widget w, XtPointer clientdata, XtPointer call_data)
{
   Arg args[10];
   char dbuf[200];
   extern char grid_file[];
   extern char tgrid_file[];
   extern char zgrid_file[];
   int dqcBasetime;
   int dqcBaseIndex;
   int k;
   int m;
   int num;
   int num_stations;
   int num_tstations;
   int num_zstations;
   extern int grids_flag;
   extern int map_flag;
   extern int pcp_flag;
   extern int pcpn_day;
   extern int points_flag;
   extern int pcp_in_use[];
   struct hrap_grid *hrap_grid = NULL;
   int time_pos;
   extern struct pdata pdata[];
   extern struct tdata tdata[];
   extern struct zdata zdata[];
   extern const char *timefile[][5];
   struct station *precip_stations = NULL;
   struct station *temperature_stations = NULL;
   struct station *freezing_stations = NULL;
   struct tm *gm = NULL;
   time_t old_time;
   extern Widget diswidget[];
   extern Widget rowcol1;
   extern Widget rpbutton;

  /* Set cursor to Watch when button "Render Grids+MAZs"/MAPs/MATs" is selected */
   
   mSetCursor ( M_WATCH ) ;
   XFlush(XtDisplay(rad_data[0].w));

   dqcBasetime = getDqcBasetime ( );
   dqcBaseIndex = dqcBasetime % 6;
   
   precip_stations = get_precip_station_list (&num_stations);
   temperature_stations = get_temperature_station_list (&num_tstations);
   freezing_stations = get_freezing_station_list ( &num_zstations);

   hrap_grid = get_hrap_grid ();

   /* Stubbed out for your protection. */
   if ((int) clientdata == 1)
   {

      /* Find a gage point. */
      /*            t=XmStringCreateLocalized("Enter handbook 5 I.D.");

         argcount=0;

         XtSetArg(args[argcount],XmNselectionLabelString,t);argcount++;

         XtSetArg(args[argcount],XmNautoUnmanage,False);argcount++;
         find_widget=XmCreatePromptDialog(drawing_area,"find station",
         args,argcount); 

         XtUnmanageChild(XmSelectionBoxGetChild(find_widget,
         XmDIALOG_HELP_BUTTON));

         XtAddCallback(find_widget,XmNokCallback,find_name,(XtPointer)0);

         XtAddCallback(find_widget,XmNcancelCallback,find_name,(XtPointer)1);

         XtManageChild(find_widget);

         XmStringFree(t);

         return; */

   }

/* Rendering the grids and MAPs. */
   else if ((int) clientdata == 0)
   {
     logMessage ("Gridding precipitation and building MAPs");

      estimate_daily_stations (pcpn_day, precip_stations, num_stations);

      estimate_partial_stations (pcpn_day, precip_stations, num_stations);

      quality_control_stations (pcpn_day, precip_stations, num_stations);

      check_consistency (pcpn_day, precip_stations, num_stations);

      if (pdata[pcpn_day].used[4] != 0)
      {

	 render_pcp (pcpn_day, pcpn_time, 1, num_stations, precip_stations,
		     hrap_grid, pdata, pcp_in_use);

	 old_time = pdata[pcpn_day].data_time;

	 gm = gmttime (&old_time);

	 sprintf (dbuf, "%s%s_%04d%02d%02d", grid_file, timefile[2][4],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);
	 create_map (40 + pcpn_day);
      }


      for (m = 0; m < 4; m++)
      {

	 if (pdata[pcpn_day].used[m] == 0)
	    continue;

	 render_pcp (pcpn_day, m, 0, num_stations, precip_stations,
		     hrap_grid, pdata, pcp_in_use);

	 if (m < 2)
	    old_time = pdata[pcpn_day].data_time - 86400;

	 else
	    old_time = pdata[pcpn_day].data_time;

	 gm = gmttime (&old_time);

	 sprintf (dbuf, "%s%s_%04d%02d%02d", grid_file, timefile[2][m],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);

	 num = 4 * pcpn_day + 3 - m;

	 create_map (num);

	 pdata[pcpn_day].used[m] = 3;

      }


      estimate_missing_stations (pcpn_day,
				 precip_stations, num_stations, pdata);

      restore_bad_values (pcpn_day, precip_stations, num_stations);

      pdata[pcpn_day].used[4] = 3;

      for (k = 1; k < 7; k++)
      {
	 XtSetSensitive (diswidget[k], True);
      }
      if (pcpn_time_step == 0)
      {
	 time_pos = pcp_flag;
      }
      else
      {
	 time_pos = 40 + pcpn_day;
      }

      if (points_flag == 1 && pcp_in_use[time_pos] == -1)
      {
	 k = 0;
      }
      else if (points_flag == 1 && grids_flag == -1 && map_flag == -1)
      {
	 k = 0;
      }
      else if (points_flag == -1 && grids_flag == 1 && map_flag == -1)
      {
	 k = 1;
      }
      else if (points_flag == -1 && grids_flag == -1 && map_flag == 1)
      {
	 k = 2;
      }
      else if (points_flag == 1 && grids_flag == 1 && map_flag == -1)
      {
	 k = 3;
      }
      else if (points_flag == 1 && grids_flag == -1 && map_flag == 1)
      {
	 k = 4;
      }
      else if (points_flag == -1 && grids_flag == -1 && map_flag == -1)
      {
	 k = 5;
      }

      XtSetArg (args[0], XmNmenuHistory, diswidget[k]);
      XtSetValues (rowcol1, args, 1);

   }
   else if ((int) clientdata == 2)
   {
      flogMessage ( stdout, "Gridding freezing level and building MAZs" );

      for (m = 0; m < 4; m++)
      {

	 if (zdata[pcpn_day].used[m] == 0 ||
	     zdata[pcpn_day].used[m] == 3 ||
	     (zdata[pcpn_day].used[m] == 1 && zdata[pcpn_day].level[m] == 2)
	     || zdata[pcpn_day].used[m] == 4)
         {
	    continue;
         }

	 render_z ( pcpn_day, m, 0, num_zstations, freezing_stations,
                    hrap_grid, zdata, pcp_in_use);

	 if (m < 2)
         {
	    old_time = zdata[pcpn_day].data_time - 86400;
         }
	 else
         {
	    old_time = zdata[pcpn_day].data_time;
         }

	 gm = gmttime (&old_time);

	 sprintf (dbuf, "%s%s_%04d%02d%02d", zgrid_file, ztimefile[dqcBaseIndex][m],
		  gm->tm_year+1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);

	 num = 100 + 4 * pcpn_day + 3 - m;

	 zdata[pcpn_day].used[m] = 3;

	 make_rsel (num, num - 100);

      }

      for (k = 1; k < 7; k++)
      {
	 XtSetSensitive (diswidget[k], True);
      }

      time_pos = 100 + pcp_flag;

      if (points_flag == 1 && pcp_in_use[time_pos] == -1)
      {
	 k = 0;
      }
      else if (points_flag == 1 && grids_flag == -1 && map_flag == -1)
      {
	 k = 0;
      }
      else if (points_flag == -1 && grids_flag == 1 && map_flag == -1)
      {
	 k = 1;
      }
      else if (points_flag == -1 && grids_flag == -1 && map_flag == 1)
      {
	 k = 2;
      }
      else if (points_flag == 1 && grids_flag == 1 && map_flag == -1)
      {
	 k = 3;
      }
      else if (points_flag == 1 && grids_flag == -1 && map_flag == 1)
      {
	 k = 4;
      }
      else if (points_flag == -1 && grids_flag == -1 && map_flag == -1)
      {
	 k = 5;
      }

      XtSetArg (args[0], XmNmenuHistory, diswidget[k]);
      XtSetValues (rowcol1, args, 1);
   }
   else if ((int) clientdata == 3)
   {

     logMessage ("Gridding temperature and building MATs");

      estimate_daily_tstations (pcpn_day, temperature_stations,
				num_tstations);

      quality_control_tstations (pcpn_day, temperature_stations,
				 num_tstations);

      if (tdata[pcpn_day].used[4] != 0)
      {

	 render_t (pcpn_day, pcpn_time, 1, num_tstations,
		   temperature_stations, hrap_grid, tdata, pcp_in_use);

	 old_time = tdata[pcpn_day].data_time;

	 gm = gmttime (&old_time);

	 sprintf (dbuf, "%s%s_%04d%02d%02d", tgrid_file, ttimefile[dqcBaseIndex][4],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);

	 tdata[pcpn_day].used[4] = 3;

      }

      if (tdata[pcpn_day].used[5] != 0)
      {

	 render_t (pcpn_day, pcpn_time, 2, num_tstations,
		   temperature_stations, hrap_grid, tdata, pcp_in_use);

	 old_time = tdata[pcpn_day].data_time;

	 gm = gmttime (&old_time);
	 sprintf (dbuf, "%s%s_%04d%02d%02d", tgrid_file, ttimefile[dqcBaseIndex][5],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);

	 tdata[pcpn_day].used[5] = 3;

      }

      for (m = 0; m < 4; m++)
      {

	 if (tdata[pcpn_day].used[m] == 0 ||
	     tdata[pcpn_day].used[m] == 3 ||
	     (tdata[pcpn_day].used[m] == 1 && tdata[pcpn_day].level[m] == 2)
	     || tdata[pcpn_day].used[m] == 4)
	    continue;

	 render_t6 (pcpn_day, m, 0, num_tstations, temperature_stations,
		    hrap_grid, tdata, pcp_in_use);

	 if (m < 2)
	    old_time = tdata[pcpn_day].data_time - 86400;

	 else
	    old_time = tdata[pcpn_day].data_time;

	 gm = gmttime (&old_time);
	 sprintf (dbuf, "%s%s_%04d%02d%02d", tgrid_file, ttimefile[dqcBaseIndex][m],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);

	 num = 150 + 4 * pcpn_day + 3 - m;

	 tdata[pcpn_day].used[m] = 3;

	 make_mat (num, num - 150);

	 tdata[pcpn_day].used[m] = 3;

      }

      estimate_missing_tstations (pcpn_day, temperature_stations,
				  num_tstations, tdata);

      restore_bad_tvalues (pcpn_day, temperature_stations, num_tstations);

      for (k = 1; k < 7; k++)
      {
	 XtSetSensitive (diswidget[k], True);
      }

      time_pos = 150 + pcp_flag;

      if (points_flag == 1 && pcp_in_use[time_pos] == -1)
	 k = 0;

      else if (points_flag == 1 && grids_flag == -1 && map_flag == -1)
	 k = 0;

      else if (points_flag == -1 && grids_flag == 1 && map_flag == -1)
	 k = 1;

      else if (points_flag == -1 && grids_flag == -1 && map_flag == 1)
	 k = 2;

      else if (points_flag == 1 && grids_flag == 1 && map_flag == -1)
	 k = 3;

      else if (points_flag == 1 && grids_flag == -1 && map_flag == 1)
	 k = 4;

      else if (points_flag == -1 && grids_flag == -1 && map_flag == -1)
	 k = 5;

      XtSetArg (args[0], XmNmenuHistory, diswidget[k]);
      XtSetValues (rowcol1, args, 1);

   }

  
   send_expose ();
   
   XtSetSensitive(rpbutton,False);
   
   /* set cursor from watch to normal */
   
   mSetCursor ( M_NORMAL ) ;

}

void change_pcpn_time (Widget w, XtPointer data, XtPointer call_data)
{
   extern int change_pcpn_flag;
   extern int change_rpcpn_flag;
   extern int grids_flag;
   extern int isom;
   extern int map_flag;
   extern int pcpn_day;
   extern int pcp_flag;
   extern int pcp_in_use[];
   extern int pcpn_time;
   extern int pcpn_time_step;
   extern int points_flag;
   extern int rsmode;


   extern struct pdata pdata[];
   extern time_t btim;
   extern Widget diswidget[];
   extern Widget rowcol1;
   extern Widget rowcol10;
   extern Widget rowcol15;
   extern Widget rpbutton;
   extern Widget rswidget[];
   extern Widget scwidget[];

   int time_pos;
   int i;
   Arg args[10];
   time_t tget;
   struct tm *gm;

   /* 24 hour or 6 hour time step */

   if (pcpn_time_step == 0)
      time_pos = pcpn_time;

   else
      time_pos = 4;

   if ((int) data == 2 && pcpn_time_step == 0)
   {
      return;
   }
   else if ((int) data == 2 && pcpn_time_step == 1)
   {
      pcpn_time_step = 0;
      pcp_flag = 3 + pcpn_day * 4;
   }
   else if ((int) data == 3 && pcpn_time_step == 1)
   {
      return;
   }
   else if ((int) data == 3 && pcpn_time_step == 0)
   {
      pcpn_time_step = 1;

      if (rsmode == 0)
      {

	 change_rpcpn_flag = -1;
	 change_pcpn_flag = 1;
	 rsmode = 1;
	 get_legend ();

      }


   }

   /* backward or forward */

   if ((int) data == 0)
   {
      if (pcpn_time_step == 0)
	 pcp_flag--;

      else
	 pcp_flag = pcp_flag - 4;

   }
   else if ((int) data == 1)
   {
      if (pcpn_time_step == 0)
	 pcp_flag++;

      else
	 pcp_flag = pcp_flag + 4;
   }

   if (pcp_flag < 0)
   {
      pcp_flag = 0;
   }

   if (pcp_flag >= MAX_GAGEQC_DAYS * 4)
   {
      pcp_flag = MAX_GAGEQC_DAYS * 4 - 1;
   }

   pcpn_day = pcp_flag / 4;

   pcpn_time = 3 - (pcp_flag - pcpn_day * 4);

   if (pcpn_time_step == 0)
      time_pos = pcp_flag;

   else
      time_pos = 40 + pcpn_day;

   for (i = 1; i < 7; i++)
      XtSetSensitive (diswidget[i], True);

   if (pcp_in_use[time_pos] == -1)
   {
      for (i = 1; i < 7; i++)
	 XtSetSensitive (diswidget[i], False);

   }

   if (points_flag == 1 && pcp_in_use[time_pos] == -1)
      i = 0;

   else if (points_flag == 1 && grids_flag == -1 && map_flag == -1)
      i = 0;

   else if (points_flag == -1 && grids_flag == 1 && map_flag == -1)
      i = 1;

   else if (points_flag == -1 && grids_flag == -1 && map_flag == 1)
      i = 2;

   else if (points_flag == 1 && grids_flag == 1 && map_flag == -1)
      i = 3;

   else if (points_flag == 1 && grids_flag == -1 && map_flag == 1)
      i = 4;

   else if (points_flag == -1 && grids_flag == -1 && map_flag == -1)
      i = 5;

   XtSetArg (args[0], XmNmenuHistory, diswidget[i]);
   XtSetValues (rowcol1, args, 1);

   if (pdata[pcpn_day].stddev == 5.0)
      i = 0;

   else if (pdata[pcpn_day].stddev == 3.0)
      i = 1;

   else
      i = 2;

   XtSetArg (args[0], XmNmenuHistory, scwidget[i]);
   XtSetValues (rowcol10, args, 1);

   if ((pcp_in_use[time_pos] == -1) &&
	(((pcpn_time_step == 1) && (pdata[pcpn_day].used[4] != 0)) ||
          ((pcpn_time_step == 0) && (pdata[pcpn_day].used[pcpn_time] != 0))))
      XtSetSensitive (rpbutton, True);

   else
      XtSetSensitive (rpbutton, False);

   time_pos = 100 + pcp_flag;

   if (pcpn_time_step == 1)
   {

      for (i = 0; i < 1; i++)
	 XtSetSensitive (rswidget[i], False);

      XtSetArg (args[0], XmNmenuHistory, rswidget[1]);
      XtSetValues (rowcol15, args, 1);

   }
   else
   {


      if ((pcp_flag != 0 && (pcp_in_use[time_pos] == 1 ||
			     pcp_in_use[time_pos - 1] == 1))
	  || (pcp_flag == 0 && pcp_in_use[time_pos] != -1))
      {


	 for (i = 0; i < 1; i++)
	    XtSetSensitive (rswidget[i], True);

	 XtSetArg (args[0], XmNmenuHistory, rswidget[rsmode]);
	 XtSetValues (rowcol15, args, 1);

      }

      else
      {

	 for (i = 0; i < 1; i++)
	    XtSetSensitive (rswidget[i], False);

	 XtSetArg (args[0], XmNmenuHistory, rswidget[1]);
	 XtSetValues (rowcol15, args, 1);
      }

   }

   tget = btim - pcpn_day * 86400;

   gm = gmtime (&tget);

   isom = gm->tm_mon;

   /* Set the sensitivity of the precipitation time step arrows
      based on the current selected time. */
   set_precip_arrow_sensitivity ( );

   send_expose ();

}

void set_precip_arrow_sensitivity ( )
{
   int num_qc_days;
   extern int pcp_flag;
   extern int pcpn_time_step;
   Widget up_arrow;
   Widget down_arrow;

   /* Get the number of days being QC'd. */
   num_qc_days = get_num_days_to_qc ( );

   /* Retrieve the widgets corresponding to the up and down 
      time step arrows on the edit precip gages gui. */
   get_precip_time_step_arrows ( & up_arrow, & down_arrow );

   /* 6 or 24 hour mode? */
   if ( pcpn_time_step == 0 )
   {
      if (pcp_flag + 1 >= num_qc_days * 4 )
      {
         /* Grey out the down arrow. */
         XtSetSensitive ( down_arrow, False );
      }
      else
      {
         /* Make sure that the down arrow is available. */
        XtSetSensitive ( down_arrow, True );
      }

      if (pcp_flag - 1  < 0)
      {
         /* Grey out the up arrow. */
         XtSetSensitive ( up_arrow, False );
      }
      else
      {
        /* Make sure the up arrow is available. */
        XtSetSensitive ( up_arrow, True );
      }
   }
   else
   {
      if (pcp_flag + 4 >= num_qc_days * 4)
      {
         /* Grey out the down arrow. */
         XtSetSensitive ( down_arrow, False );
      }
      else
      {
         /* Make sure that the down arrow is available. */
        XtSetSensitive ( down_arrow, True );
      }

      if ( pcp_flag - 4 < 0 )
      {
          /* Grey out the up arrow. */
          XtSetSensitive ( up_arrow, False );
      }
      else
      {
          XtSetSensitive ( up_arrow, True );
      }
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/other_pcpn_options.c,v $";
 static char rcs_id2[] = "$Id: other_pcpn_options.c,v 1.7 2007/10/18 16:09:04 lawrence Exp $";}
/*  ===================================================  */

}
