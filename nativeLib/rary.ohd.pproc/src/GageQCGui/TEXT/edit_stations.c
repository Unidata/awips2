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

#include <stdlib.h>
#include <Xm/Label.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Xm.h>

#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "Xtools.h"
#include "display_pdc_tsl_jni.h"

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

Widget textz, snotelwidget;
Widget textk[5];
Widget cons_dialog;
static Widget edit_dialog = NULL;
static int isave;
static Position xposition = 100;
static Position yposition = 100;
static Position xoffset;
static Position yoffset;
int reset_value;
int new_qual;
int initial_qual, initial_pos;
extern int func[];

extern struct station *station;
extern int max_stations;

void time_series ();

extern char message[150];

static void read_text ()
{

   extern Widget rowcol1;
   extern int map_flag;
   extern int grids_flag;
   extern int points_flag;
   extern int pcp_flag;
   int k;
   char *cstr = NULL;
   Arg args[10];
   extern struct pdata pdata[10];
   extern int pcp_flag;
   extern int pcpn_day;
   extern int pcpn_time_step;
   extern int pcp_in_use[500];
   extern Widget diswidget[6];
   float val;
   char *p;
   int time_pos;
   float fdif;
   extern Widget rpbutton;

   for (k = 0; k < 5; k++)
   {

      cstr = XmTextGetString ( textk[k] );
      val = atof (cstr);
      p = strchr (cstr, 'M');

      fdif = fabs (val - pdata[pcpn_day].stn[isave].frain[k].data);

      if (p != NULL)
      {
	 pdata[pcpn_day].stn[isave].frain[k].data = -1;
         p = NULL;
      }
      else if (fdif > .005 && p == NULL)
      {

	 pdata[pcpn_day].stn[isave].frain[k].data = val;
	 pdata[pcpn_day].stn[isave].frain[k].qual = 2;
	 pdata[pcpn_day].stn[isave].sflag[k] = -1;


      }

      XtFree (cstr);
      cstr = NULL;

   }

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


   for (k = 1; k < 7; k++)
      XtSetSensitive (diswidget[k], False);

   if (pcpn_time_step == 0)
      time_pos = pcp_flag;

   else
      time_pos = 40 + pcpn_day;

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

   XtSetSensitive (rpbutton, True);

   update_bad_values (pcpn_day);

   estimate_daily_stations (pcpn_day, station, max_stations);

   estimate_partial_stations (pcpn_day, station, max_stations);

   quality_control_stations (pcpn_day, station, max_stations);

   check_consistency (pcpn_day, station, max_stations);

   restore_bad_values (pcpn_day, station, max_stations);

   send_expose ( );
   return;

}

static void textFieldListner ( Widget w,
                               XtPointer clientData,
                               XtPointer callData )
{
   char * textString = NULL;

   int timePeriod = (int) clientData;

   textString = XmTextGetString ( w ); 

   if ( textString != NULL )
   {
      XmTextSetString ( textk[timePeriod], textString );
   }

   XtFree ( textString );
   textString = NULL;
}

void edit_stations (int win_x, int win_y, unsigned int map_num)
{
   Atom atom;
   XEvent event;
   extern char *timefile[][5];
   const char * pClimateSource = NULL;
   extern float reverse_filter_value;
   extern int elevation_filter_value;
   extern int dcmode;
   extern int tcmode;
   extern float filter_value;
   extern struct ts ts[20];
   extern int tsmax;
   extern int isom;
   extern int gage_char[2];
   extern int method;
   extern int qflag[10];
   extern struct pdata pdata[10];
   extern unsigned long cmap[16];

   static int first = 1;

   extern void kill_widget ();

   extern int dflag[10];
   void change_station_location ();
   void change_custom_file ();
   void change_station_quality ();
   void reset_station_quality ();
   void cancel_edit (Widget w, XtPointer data, XtPointer call_data);
   void set_snotel ();
   extern int pcpn_day, pcpn_time;
   extern int pcpn_time_step;
   static Widget rowcol;
   Widget graph_button;
   Widget rowcol3, pbutton, rowcol1, sep, sep2, rowcol4, rowcol5;


   Arg args[20];
   Cardinal argcount;
   int i;
   double testdist, maxdist;
   int x, y;
   Position xpos;
   Position ypos;
   int x1;
   int y1;
   XmString t;
   int display_flag, h;
   float lat, lon;
   char *st[10], buf[100], muf[10];
   int time_pos, naflag, m;
   Widget parent_widget;
   Widget applyButton;
   Widget closeButton;
   Widget graphButton;

   event.xbutton.type = ButtonRelease;
   event.xbutton.x_root = 0;
   event.xbutton.y_root = 0;

   
   if ( edit_dialog != NULL )
   {
      XtVaGetValues ( edit_dialog, XmNx, & xposition, NULL );
      XtVaGetValues ( edit_dialog, XmNy, & yposition, NULL ); 
      XtPopdown ( edit_dialog );
      XtDestroyWidget (edit_dialog);
      edit_dialog = NULL;
   }

   xposition -= xoffset;
   yposition -= yoffset;

   if (pcpn_time_step == 0)
   {
      time_pos = pcpn_time;
   }
   else
   {
      time_pos = 4;
   }

   /* need to set flag to plot */

   /* need to set resource in widget also */
   display_flag = 0;
   h = 0;
   x = win_x;
   y = win_y;

   isave = -1;
   maxdist = 9999;

   for (i = 0; i < max_stations; i++)
   {

      if ((pdata[pcpn_day].stn[i].frain[time_pos].data > reverse_filter_value)
	  && (pdata[pcpn_day].stn[i].frain[time_pos].data < 20.00))
      {
	 continue;
      }

      if (station[i].elev > 0 && station[i].elev < elevation_filter_value)
      {
	 continue;
      }

      if (pdata[pcpn_day].stn[i].frain[time_pos].data < 0)
      {
	 continue;
      }

      if (pdata[pcpn_day].stn[i].frain[time_pos].data < filter_value)
      {
	 continue;
      }

      lat = station[i].lat;
      lon = station[i].lon;

      if (tcmode == 0 && pdata[pcpn_day].stn[i].tcons == -1)
      {
	 continue;
      }

      if (tcmode == 1 && pdata[pcpn_day].stn[i].tcons == 1)
      {
	 continue;
      }

      if (dcmode == 0 && pdata[pcpn_day].stn[i].scons[time_pos] == -1)
      {
	 continue;
      }

      if (dcmode == 1 && pdata[pcpn_day].stn[i].scons[time_pos] == 1)
      {
	 continue;
      }

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
         {
	    break;
         }
      }

      if (m == tsmax)
      {
	 continue;
      }

      for (m = 0; m < 9; m++)
      {
	 if (m == pdata[pcpn_day].stn[i].frain[time_pos].qual &&
	     qflag[m] == 1)
         {
	    break;
         }
      }

      if (m == 9)
      {
	 continue;
      }

      mConvertLatLon2XY ( lat, -1 * lon, &x1, &y1 );
      testdist = pow ( (double) (win_x - (float)x1), 2) + 
                 pow ( (double) (win_y - (float)y1), 2);
      testdist = pow (testdist, .5);

      if (testdist < maxdist)
      {
	 isave = i;
	 maxdist = testdist;
      }
   }

   if (isave == -1)
   {
      return;
   }

   reset_value = 0;
   initial_qual = pdata[pcpn_day].stn[isave].frain[time_pos].qual;
   new_qual = initial_qual;

   parent_widget = _get_map_shell();

   if (initial_qual == 6)
   {

      ErrorDialog ( parent_widget,
	            "You cannot quality control a time distributed station");
      return;

   }

   argcount = 0;
   XtSetArg (args[argcount], XmNdeleteResponse, XmDO_NOTHING);
   argcount++;
   XtSetArg (args[argcount], XmNallowShellResize, True);
   argcount++;
   XtSetArg (args[argcount], XmNx, xposition);
   argcount++;
   XtSetArg (args[argcount], XmNy, yposition);
   argcount++;

   edit_dialog = XtCreatePopupShell ( "Edit Precipitation Stations", 
                                       xmDialogShellWidgetClass,
                                       parent_widget, args, argcount );

   argcount = 0;
   XtSetArg (args[argcount], XmNnumColumns, 1);
   argcount++;
   rowcol = XmCreateRowColumn (edit_dialog, "Edit Stations", args, argcount);

   strcpy (buf, station[isave].hb5);
   strcat (buf, "  ");
   strcat (buf, station[isave].parm);
   t = XmStringCreateLocalized (buf);
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   strcpy (buf, station[isave].name);
   t = XmStringCreateLocalized (buf);
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   sprintf (buf, "%d", station[isave].elev);
   strcat (buf, " ft    ");
   if (station[isave].tip == 0)
      strcat (buf, "tipping");

   else
      strcat (buf, "weighing");

   t = XmStringCreateLocalized (buf);
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   if (method == 2 && station[isave].isoh[isom] > 0)
   {
      pClimateSource = getClimateSource ( station[isave].cparm );
      sprintf (buf, "monthly normal %5.2f in. source: %s", 
                    station[isave].isoh[isom],
                    pClimateSource);
      t = XmStringCreateLocalized (buf);
      argcount = 0;
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

   }

   if (pdata[pcpn_day].stn[isave].frain[time_pos].data >= 0)
   {

      sprintf (buf, "estimate %5.2f in. dev %5.2f",
	       pdata[pcpn_day].stn[isave].frain[time_pos].estimate,
	       pdata[pcpn_day].stn[isave].frain[time_pos].stddev);

      t = XmStringCreateLocalized (buf);
      argcount = 0;
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

   }

   if (station[isave].tip == 0 && time_pos != 4 &&
       pdata[pcpn_day].stn[isave].frzlvl[time_pos] > -99)
   {

      sprintf (buf, "Freezing level %dft",
	       pdata[pcpn_day].stn[isave].frzlvl[time_pos]);

      t = XmStringCreateLocalized (buf);
      argcount = 0;
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

   }

   if (pdata[pcpn_day].stn[isave].snoflag[time_pos] > 0)
   {

      sprintf (buf, "SNOTEL error is ");

      if (pdata[pcpn_day].stn[isave].snoflag[time_pos] == 1)
	 strcat (buf, " SNOTEL >> PCPN");

      if (pdata[pcpn_day].stn[isave].snoflag[time_pos] == 2)
	 strcat (buf, " CONTINUATION");

      if (pdata[pcpn_day].stn[isave].snoflag[time_pos] == 3)
	 strcat (buf, " PCPN RESET");

      if (pdata[pcpn_day].stn[isave].snoflag[time_pos] == 4)
	 strcat (buf, " PCPN >> SNOTEL");

      t = XmStringCreateLocalized (buf);
      argcount = 0;
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

   }

   snotelwidget = NULL;

   if (pdata[pcpn_day].stn[isave].srain[time_pos].data > -98)
   {

      sprintf (buf, "Snow water change is %5.2f in.",
	       pdata[pcpn_day].stn[isave].srain[time_pos].data);

      t = XmStringCreateLocalized (buf);
      argcount = 0;
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

      if (time_pos == 4
	  && pdata[pcpn_day].stn[isave].srain[time_pos].data >= 0)
      {

	 argcount = 0;
	 XtSetArg (args[argcount], XmNselectColor, cmap[4]);
	 argcount++;

	 if (pdata[pcpn_day].stn[isave].sflag[time_pos] == 1)
	    XtSetArg (args[argcount], XmNset, True);

	 else
	    XtSetArg (args[argcount], XmNset, False);

	 argcount++;

	 snotelwidget =
	    XmCreateToggleButton (rowcol, "Use SWD for PPD", args, argcount);

	 /*XtAddCallback(pbutton,XmNvalueChangedCallback,set_snotel,
	    (XtPointer)isave); */

	 XtManageChild (snotelwidget);

      }

   }

   if (pdata[pcpn_day].stn[isave].frain[time_pos].data < 0)
      strcpy (muf, "M");
   else
      sprintf (muf, "%5.2f", pdata[pcpn_day].stn[isave].frain[time_pos].data);

   argcount = 0;
   XtSetArg (args[argcount], XmNvalue, muf);
   argcount++;
   textz = XmCreateTextField (rowcol, "Point QPF", args, argcount);
   XtAddCallback ( textz, XmNvalueChangedCallback, textFieldListner,
                   (XtPointer) time_pos);
   XtManageChild (textz);

   if (initial_qual != 5 && initial_qual != 4)
   {

      t = XmStringCreateLocalized ("Station quality");

      argcount = 0;
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

   }

   if (initial_qual < 0
       || pdata[pcpn_day].stn[isave].frain[time_pos].data < 0)
      naflag = 1;

   else
      naflag = 0;


   argcount = 0;
   XtSetArg (args[argcount], XmNpacking, XmPACK_COLUMN);
   argcount++;
   XtSetArg (args[argcount], XmNnumColumns, 2);
   argcount++;
   XtSetArg (args[argcount], XmNorientation, XmVERTICAL);
   argcount++;
   rowcol1 = XmCreateRadioBox (rowcol, "Edit Stations", args, argcount);

   if (initial_qual == 2)
   {

      st[0] = "Manual";
      st[1] = "Reset to Original";

      for (i = 0; i < 2; i++)
      {

	 argcount = 0;
	 XtSetArg (args[argcount], XmNselectColor, cmap[4]);
	 argcount++;

	 if (i == 0)
	    XtSetArg (args[argcount], XmNset, True);

	 else
	    XtSetArg (args[argcount], XmNset, False);

	 argcount++;

	 pbutton = XmCreateToggleButton (rowcol1, st[i], args, argcount);
	 XtAddCallback (pbutton, XmNvalueChangedCallback,
			reset_station_quality, (XtPointer) i);

	 XtManageChild (pbutton);


      }


   }


   else if (initial_qual != 5 && initial_qual != 4)
   {

      st[0] = "Verified";
      st[1] = "Screened (Force)";
      st[2] = "Questionable";
      st[3] = "Bad";

      for (i = 0; i < 4; i++)
      {

	 argcount = 0;

	 if (func[i] == initial_qual && naflag != 1)
	    XtSetArg (args[argcount], XmNset, True);

	 else
	    XtSetArg (args[argcount], XmNset, False);

	 argcount++;

	 pbutton = XmCreateToggleButton (rowcol1, st[i], args, argcount);

	 if (func[i] == initial_qual && naflag != 1)
	 {

	    XtCallActionProc (pbutton, "Arm", &event, NULL, 0);
	    XtCallActionProc (pbutton, "Select", &event, NULL, 0);
	    XtCallActionProc (pbutton, "Disarm", &event, NULL, 0);


	 }

	 XtAddCallback (pbutton, XmNvalueChangedCallback,
			change_station_quality, (XtPointer) (i));

	 XtManageChild (pbutton);

	 if (naflag == 1)
	    XtSetSensitive (pbutton, False);

      }

   }

   if (station[isave].xadd == -1 && station[isave].yadd == -1)
      initial_pos = 0;

   else if (station[isave].xadd == 0 && station[isave].yadd == -1)
      initial_pos = 2;

   else if (station[isave].xadd == -1 && station[isave].yadd == 0)
      initial_pos = 1;

   else if (station[isave].xadd == 0 && station[isave].yadd == 0)
      initial_pos = 3;
   if (initial_qual != 5 && initial_qual != 4)
   {

      sep = XmCreateSeparator (rowcol, "swp", NULL, 0);

      XtManageChild (sep);

   }

   t = XmStringCreateLocalized ("Station Location");
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   argcount = 0;
   XtSetArg (args[argcount], XmNpacking, XmPACK_COLUMN);
   argcount++;
   XtSetArg (args[argcount], XmNnumColumns, 2);
   argcount++;
   XtSetArg (args[argcount], XmNorientation, XmVERTICAL);
   argcount++;
   rowcol3 = XmCreateRadioBox (rowcol, "Edit Stations", args, argcount);

   st[0] = "upper left";
   st[1] = "lower left";
   st[2] = "upper right";
   st[3] = "lower right";

   for (i = 0; i < 4; i++)
   {

      argcount = 0;
      XtSetArg (args[argcount], XmNselectColor, cmap[4]);
      argcount++;

      if (i == initial_pos)
	 XtSetArg (args[argcount], XmNset, True);

      else
	 XtSetArg (args[argcount], XmNset, False);


      argcount++;

      pbutton = XmCreateToggleButton (rowcol3, st[i], args, argcount);
      XtAddCallback (pbutton, XmNvalueChangedCallback,
		     change_station_location, (XtPointer) (i));

      if (i == initial_pos)
      {
	 XtCallActionProc (pbutton, "Arm", &event, NULL, 0);
	 XtCallActionProc (pbutton, "Select", &event, NULL, 0);
	 XtCallActionProc (pbutton, "Disarm", &event, NULL, 0);
      }

      XtManageChild (pbutton);

   }

   /* Create the station consistency check portion of the Edit
      Precipitation Station window.  This is new
      for ob83. */
   sep2 = XmCreateSeparator (rowcol, "swp2", NULL, 0);
   XtManageChild (sep2);

   t = XmStringCreateLocalized ("Station Consistency");
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);


   argcount = 0;
   XtSetArg (args[argcount], XmNpacking, XmPACK_COLUMN);
   argcount++;
   XtSetArg (args[argcount], XmNorientation, XmHORIZONTAL);
   argcount++;
   XtSetArg (args[argcount], XmNisAligned, True);
   argcount++;
   XtSetArg (args[argcount], XmNnumColumns, 5);
   argcount++;
   XtSetArg (args[argcount], XmNtopAttachment, XmATTACH_WIDGET);
   argcount++;
   XtSetArg (args[argcount], XmNtopWidget, pbutton);
   argcount++;
   rowcol4 =
      XmCreateRowColumn (rowcol, "Edit Stations", args, argcount);

   for (m = 0; m < 5; m++)
   {

      argcount = 0;

      t = XmStringCreateLocalized (timefile[2][m]);
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol4, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

      if (pdata[pcpn_day].stn[isave].frain[m].data < 0)
              strcpy (muf, "M");
      else
	  sprintf (muf, "%5.2f",
                    pdata[pcpn_day].stn[isave].frain[m].data);

      XtSetArg (args[argcount], XmNvalue, muf);
      argcount++;

      textk[m] =
	  XmCreateTextField (rowcol4, "Point QPF", args, argcount);
      XtManageChild (textk[m]);

   }

   /* Create the Apply, Close, and Graph buttons. */
   argcount = 0;
   XtSetArg (args[argcount], XmNpacking, XmPACK_COLUMN);
   argcount++;
   XtSetArg (args[argcount], XmNorientation, XmVERTICAL);
   argcount++;
   XtSetArg (args[argcount], XmNnumColumns, 3);
   argcount++;
   XtSetArg (args[argcount], XmNspacing, 50);
   argcount++;
   XtSetArg (args[argcount], XmNadjustLast, False );
   argcount++;
   XtSetArg (args[argcount], XmNmarginWidth, 30 );
   argcount++;
   XtSetArg (args[argcount], XmNmarginHeight, 30 );
   argcount++;
   rowcol5 = XmCreateRowColumn (rowcol, "Buttons", args, argcount);
   
   argcount = 0;
   t = XmStringCreateLocalized ("Apply");
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   XtSetArg (args[argcount], XmNwidth, 45);
   argcount++;
   XtSetArg (args[argcount], XmNrecomputeSize, False);
   argcount++;
   XtSetArg (args[argcount], XmNalignment, XmALIGNMENT_CENTER);
   argcount++;
   
   applyButton = XmCreatePushButton (rowcol5, "Apply Button", args, argcount);  
   XmStringFree ( t );
   XtManageChild ( applyButton );

   argcount = 0;
   t = XmStringCreateLocalized ("Close");
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   XtSetArg (args[argcount], XmNwidth, 45);
   argcount++;
   XtSetArg (args[argcount], XmNrecomputeSize, False);
   argcount++;
   XtSetArg (args[argcount], XmNalignment, XmALIGNMENT_CENTER);
   argcount++;
   
   closeButton = XmCreatePushButton (rowcol5, "Close Button", args, argcount);  
   XmStringFree ( t );
   XtManageChild ( closeButton );

   argcount = 0;
   t = XmStringCreateLocalized ("Graph");
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   XtSetArg (args[argcount], XmNwidth, 45);
   argcount++;
   XtSetArg (args[argcount], XmNrecomputeSize, False);
   argcount++;
   XtSetArg (args[argcount], XmNalignment, XmALIGNMENT_CENTER);
   argcount++;
   
   graphButton = XmCreatePushButton (rowcol5, "Graph Button", args, argcount);  
   XmStringFree ( t );
   XtManageChild ( graphButton );

   XtAddCallback (applyButton, XmNactivateCallback, change_custom_file,
		  (XtPointer) isave);

   XtAddCallback (closeButton, XmNactivateCallback, cancel_edit,
		  (XtPointer) NULL);

   XtAddCallback (graphButton, XmNactivateCallback, graph_file, 
                  (XtPointer) isave );



   atom = XmInternAtom(XtDisplay(edit_dialog), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(edit_dialog, atom, cancel_edit, NULL);

   XtManageChild (rowcol);
   XtManageChild (rowcol1);
   XtManageChild (rowcol3);
   XtManageChild (rowcol4);
   XtManageChild (rowcol5);

   if ( first == 1 )
   {
      XtVaGetValues ( edit_dialog, XmNx, & xpos, NULL );
      XtVaGetValues ( edit_dialog, XmNy, & ypos, NULL ); 

      yoffset = ypos - yposition;
      xoffset = xpos - xposition;
      first = 0;
   }
}

void
change_station_location (Widget w, XtPointer data, XtPointer call_data)
{


   if ((int) data == 0)
   {

      station[isave].xadd = -1;
      station[isave].yadd = -1;

   }

   else if ((int) data == 2)
   {

      station[isave].xadd = 0;
      station[isave].yadd = -1;

   }

   else if ((int) data == 1)
   {

      station[isave].xadd = -1;
      station[isave].yadd = 0;

   }

   else if ((int) data == 3)
   {

      station[isave].xadd = 0;
      station[isave].yadd = 0;

   }

   return;

}


void change_custom_file (Widget w, XtPointer data, XtPointer call_data)
{

   extern Widget rowcol1;
   extern int map_flag;
   extern int grids_flag;
   extern int points_flag;
   extern int pcp_flag;
    Widget drawing_area;
   extern Widget diswidget[6];
   extern struct pdata pdata[10];
   extern int pcpn_time;
   extern int pcpn_time_step;
   extern int pcpn_day;



   extern char station_list_custom_file[100];
   int i;
   FILE *fp;
   Cardinal argcount;
   Arg args[10];
   int time_pos;
   char *p;
   float val, fdif;
   char *cstr;
   int k;
   extern int pcp_in_use[500];
   Boolean bval;
   float rtotal;
   int m;
   extern Widget rpbutton;
   extern int tcmode;

   mSetCursor (M_WATCH);

   drawing_area = _get_map_shell ( );

   if (pcpn_time_step == 0)
      time_pos = pcpn_time;

   else
      time_pos = 4;


   fp = fopen (station_list_custom_file, "w");

   if (fp != NULL)
   {
      bzero (message, 150);
      sprintf (message, "Opened file: %s\n", station_list_custom_file);
      logMessage (message);

      for (i = 0; i < max_stations; i++)
	 fprintf(fp, "%s %s %d %d\n", station[i].hb5, station[i].parm,
		                      station[i].xadd, station[i].yadd);

      fclose (fp);
   }
   else
   {
      bzero (message, 150);
      sprintf (message, "Could not open file: %s\n",
	       station_list_custom_file);
      logMessage (message);
   }
   if (snotelwidget != NULL)
   {

      XtSetArg (args[0], XmNset, &bval);
      XtGetValues (snotelwidget, args, 1);

   }

/*snotel path */

   if (snotelwidget != NULL &&
       ((bval == True && pdata[pcpn_day].stn[isave].sflag[4] == -1) ||
	(bval == False && pdata[pcpn_day].stn[isave].sflag[4] == 1)))
   {


      pdata[pcpn_day].stn[isave].sflag[4] =
	 -pdata[pcpn_day].stn[isave].sflag[4];

      if (pdata[pcpn_day].stn[isave].sflag[4] == 1)
      {

	 pdata[pcpn_day].stn[isave].frain[4].data =
	    pdata[pcpn_day].stn[isave].srain[4].data;

	 pdata[pcpn_day].stn[isave].frain[4].qual = 8;

      }

      else
      {

	 pdata[pcpn_day].stn[isave].frain[4].data =
	    pdata[pcpn_day].stn[isave].rain[4].data;

	 pdata[pcpn_day].stn[isave].frain[4].qual = 8;

      }

   }


   else
   {


      /* other path */

      argcount = 0;
      XtSetArg (args[argcount], XmNvalue, &cstr);
      argcount++;
      XtGetValues (textz, args, argcount);

      val = atof (cstr);
      p = strchr (cstr, 'M');
      XtFree (cstr);

      /* use manually entered data */

      /* need to ensure consistency in 6 and 24 hour data??? */

      fdif = fabs (val - pdata[pcpn_day].stn[isave].frain[time_pos].data);

      if (fdif > .005 && p == NULL && reset_value == 0)
      {

	 pdata[pcpn_day].stn[isave].frain[time_pos].data = val;
	 pdata[pcpn_day].stn[isave].frain[time_pos].qual = 2;
	 pdata[pcpn_day].stn[isave].sflag[time_pos] = -1;

	/* for (m = 0; m < 5; m++)
	 {

	    if (pdata[pcpn_day].stn[isave].frain[m].data >= 0
		&& m != time_pos)
	       pdata[pcpn_day].stn[isave].frain[m].qual = 2;


	 } */

      }

      if (pdata[pcpn_day].stn[isave].frain[time_pos].qual == 2 || tcmode == 1)
      {

	 rtotal = 0;

	 for (m = 0; m < 4; m++)
	    if (pdata[pcpn_day].stn[isave].frain[m].data >= 0)
	       rtotal = rtotal + pdata[pcpn_day].stn[isave].frain[m].data;
      
         /* If setting a 24 hour value to 0, set all corresponding 6
            hour values to zero as well. */
         if ( ( pdata[pcpn_day].stn[isave].frain[4].data == 0 ) &&
              ( time_pos == 4 ) )
	 {
	    for (m = 0; m < 4; m++)
	    {
	       pdata[pcpn_day].stn[isave].frain[m].data = 0;
	    }

            rtotal = 0;
	 }

	 if (fabs (rtotal - pdata[pcpn_day].stn[isave].frain[4].data) > .005 )
	 {
            read_text ( );
            mSetCursor(M_NORMAL);
            return;

	 }


      }

      else
      {


	 pdata[pcpn_day].stn[isave].frain[time_pos].qual = new_qual;

	 /* 24 hour data set bad/good then 6 hourly bad/good also */

	 if (new_qual == 1 &&
	     time_pos == 4 && pdata[pcpn_day].stn[isave].sflag[time_pos] == 1)
	 {

	    pdata[pcpn_day].stn[isave].frain[time_pos].data =
	       pdata[pcpn_day].stn[isave].rain[time_pos].data;

	    pdata[pcpn_day].stn[isave].sflag[time_pos] = -1;

	 }

	 if (time_pos == 4
	     && (new_qual == 1 || new_qual == 0 || new_qual == 8))
	 {

	    for (k = 0; k < 4; k++)
	    {

	       /*  if(pdata[pcpn_day].stn[isave].frain[k].qual!=1) */
	       pdata[pcpn_day].stn[isave].frain[k].qual = new_qual;

	    }


	 }

	 /* 6 hour data set bad set 24 hour bad too */

	 if (time_pos != 4 && new_qual == 1 &&
	     pdata[pcpn_day].stn[isave].frain[4].qual != 5 &&
	     pdata[pcpn_day].stn[isave].frain[4].qual != 4 &&
	     pdata[pcpn_day].stn[isave].frain[4].data >= 0)
	 {

	   logMessage ("conditions met\n");

	    pdata[pcpn_day].stn[isave].frain[4].qual = new_qual;
	 }

      }

   }

  logMessage ("new_qual %d station qual %d\n", new_qual,
	   pdata[pcpn_day].stn[isave].frain[4].qual);

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

   for (k = 1; k < 7; k++)
      XtSetSensitive (diswidget[k], False);

   if (pcpn_time_step == 0)
      time_pos = pcp_flag;

   else
      time_pos = 40 + pcpn_day;

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

   XtSetSensitive (rpbutton, True);

   update_bad_values (pcpn_day);

   estimate_daily_stations (pcpn_day, station, max_stations);

   estimate_partial_stations (pcpn_day, station, max_stations);

   quality_control_stations (pcpn_day, station, max_stations);

   check_consistency (pcpn_day, station, max_stations);

   restore_bad_values (pcpn_day, station, max_stations);

  logMessage ("new_qual again %d station qual %d\n", new_qual,
	   pdata[pcpn_day].stn[isave].frain[4].qual);


   send_expose ( );
   mSetCursor (M_NORMAL);
   return;

}

//static int hmflag=0;

void
change_station_quality (Widget w, XtPointer data, XtPointer call_data)
{


   extern int pcpn_time;
   extern int pcpn_time_step;
   int time_pos;
/*
hmflag++;
if(hmflag==1)
         return;

hmflag=0;


*/

  logMessage ("thru station_quality %d\n", (int) data);
   if (pcpn_time_step == 0)
      time_pos = pcpn_time;

   else
      time_pos = 4;


   new_qual = func[(int) data];



}

void graph_file (Widget w, XtPointer data, XtPointer call_data)
{     
   int isave;
   
/*   char  stationId[10]="";   
   int  time_last;
   time_t ctime1, ctime2;
   struct tm *gm;
  */ 
   
   /*extern struct pdata pdata[10];
   isave = (int) data;
   strcpy (pc, station[isave].parm);

   pc[3] = 'R';
   if (pc[4] != 'Z')
   {

      pc[1] = 'C';
      pc[2] = 'I';

   }

   else
      pc[2] = 'D';

   ctime = pdata[0].data_time;

   gm = gmtime (&ctime);
   */
   
   
   /* Call Java class TimeSeriesLite from the "graph" button */
   
   /* get the current system GMT time */
   
   /*time(&ctime1);   */
   
   isave = (int) data;
/*   strcpy(stationId, station[isave].hb5);*/
   
   char header[] = "graph_file() to launch timeserieslite for precipitation";
   
  logMessage("Before startJavaVM() in %s\n", header);
   startjavavm();
  logMessage("After startJavaVM() in %s\n", header);
   
   displayPrecipTimeSeriesLite(isave);
       
   /*time(&ctime2);
   
   time_last = (int)(ctime2-ctime1);
  logMessage("The time periods in seconds to display timeserieslite gui on station %s: %d seconds\n",
          stationId, time_last);
   */	  
}

/***************************************************
  displayPrecipTimeSeriesLite()
  PURPOSE call Java class TimeSeriesLite to display
  timeseries of precipitation for the specified location and PE etc.
  Use Java Native Interface (JNI) to handle the function
  calling between two languages.
****************************************************/  
 void displayPrecipTimeSeriesLite(int isave)
{
   extern int pcpn_time_step;
   const char * jdbcUrlString = getenv("JDBCURL");
   char  stationId[10]="";
   char  stationParm[10]="";
     
   if (station[isave].hb5 != NULL)
      strcpy(stationId, station[isave].hb5);
   else
   {
     logMessage("No location has been selected. \n"
             "TimeSeriesLite cannot be launched. \n");
      return;
   }
      	     
   /* For precipitation, dislay the shef parm code what ever it is
      24hr or 6hr precipitation. Use the shef duration code 'Q'
      if 6hr mode is selected */
            
   if (station[isave].parm != NULL)
      strcpy(stationParm, station[isave].parm);
   else
   {
     logMessage("This location's (lid: %s) paramCode, %s, is incorrect. \n"
             "TimeSeriesLite cannot be launched. \n", station[isave].hb5,
	                                              station[isave].parm);
      return;
   }
      						         
   /* 6hr mode is selected */
   
   if (pcpn_time_step == 0)
      stationParm[2]='Q';
      
   callPDCTimeSeriesLiteThroughJNI(jdbcUrlString,
                                   stationId,
				   stationParm);

   return;

}

void cancel_edit (Widget w, XtPointer data, XtPointer call_data)
{


   XtVaGetValues ( edit_dialog, XmNx, & xposition, NULL );
   XtVaGetValues ( edit_dialog, XmNy, & yposition, NULL ); 
   XtPopdown (edit_dialog);
   XtDestroyWidget (edit_dialog);
   edit_dialog = NULL;
}

void reset_station_quality (Widget w, XtPointer data, XtPointer call_data)
{

   extern int pcpn_day;
   extern struct pdata pdata[10];
   extern int pcpn_time;
   extern int pcpn_time_step;
   int time_pos;
   int k;

   if (pcpn_time_step == 0)
      time_pos = pcpn_time;

   else
      time_pos = 4;

   if ((int) data == 1)
   {


      for (k = 0; k < 5; k++)
      {

	 pdata[pcpn_day].stn[isave].frain[k].qual =
	    pdata[pcpn_day].stn[isave].rain[k].qual;

	 pdata[pcpn_day].stn[isave].frain[k].data =
	    pdata[pcpn_day].stn[isave].rain[k].data;

      }

      reset_value = 1;
      new_qual = pdata[pcpn_day].stn[isave].rain[time_pos].qual;

   }

   else
      reset_value = 0;



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/edit_stations.c,v $";
 static char rcs_id2[] = "$Id: edit_stations.c,v 1.9 2007/10/25 14:39:29 lawrence Exp $";}
/*  ===================================================  */

}
