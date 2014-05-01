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
#include <Xm/TextF.h>
#include <Xm/Separator.h>

#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "Xtools.h"

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

int isave;
int reset_value;
int new_qual;
int initial_qual, initial_pos;
extern int func[];
Widget textz;
static Widget edit_dialog = NULL;
static Position xposition = 100;
static Position yposition = 100;
static Position xoffset;
static Position yoffset;

extern struct station *zstation;
extern int max_zstations;

extern char message[150];

void
edit_zstations (int win_x, int win_y, unsigned int map_num)
{
   Atom atom;
   extern struct zdata zdata[10];
   extern void kill_widget ();
   void change_zstation_location ();
   void change_zcustom_file ();
   void change_zstation_quality ();
   void reset_zstation_quality ();
   void cancel_zedit ();
   void set_snotel ();
   extern int pcpn_day, pcpn_time;
   Widget rowcol, rowcol3, pbutton, rowcol1, sep;
   Widget parent_widget;
   Widget graph_button;
   Arg args[20];
   Cardinal argcount;
   int i;
   double testdist, maxdist;
   static int first = 1;
   int x, y;
   int x1;
   int y1;
   XmString t;
   int display_flag, h;
   float lat, lon;
   char *st[10], buf[100], muf[10];
   int time_pos, naflag;
   Position xpos;
   Position ypos;
   XmString help_string;
   XmString apply_string;
   XmString close_string;
   time_pos = pcpn_time;
   Widget rowcol5;
   Widget applyButton;
   Widget closeButton;
   Widget graphButton;

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

   parent_widget = _get_map_shell ();

/* Need to set flag to plot. */
   display_flag = 0;
   h = 0;

   x = win_x;
   y = win_y;

   isave = -1;
   maxdist = 9999;

   for (i = 0; i < max_zstations; i++)
     {

	if (zdata[pcpn_day].stn[i].zlevel2[time_pos].data < 0)
	   continue;

	lat = zstation[i].lat;
	lon = zstation[i].lon;

	mConvertLatLon2XY (lat, -1 * lon, &x1, &y1);
	testdist = pow ((double) (win_x - (float) x1), 2) +
	   pow ((double) (win_y - (float) y1), 2);
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

   initial_qual = zdata[pcpn_day].stn[isave].zlevel2[time_pos].qual;

   new_qual = initial_qual;

   argcount = 0;
   XtSetArg (args[argcount], XmNdeleteResponse, XmDO_NOTHING);
   argcount++;
   XtSetArg (args[argcount], XmNallowShellResize, True);
   argcount++;
   XtSetArg (args[argcount], XmNx, xposition);
   argcount++;
   XtSetArg (args[argcount], XmNy, yposition);
   argcount++;

   edit_dialog = XtCreatePopupShell ( "Edit Freezing Level  Stations",
                                       xmDialogShellWidgetClass,
                                       parent_widget, args, argcount );

   argcount = 0;
   XtSetArg (args[argcount], XmNorientation, XmVERTICAL);
   argcount++;
   rowcol = XmCreateRowColumn (edit_dialog, "Edit Stations", args, argcount);

   strcpy (buf, zstation[isave].hb5);
   strcat (buf, "  ");
   strcat (buf, zstation[isave].parm);
   t = XmStringCreateLocalized (buf);
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   strcpy (buf, zstation[isave].name);
   t = XmStringCreateLocalized (buf);
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   if (zdata[pcpn_day].stn[isave].zlevel2[time_pos].data < 0)
      strcpy (muf, "M");
   else
     {
logMessage ("pcpn_day: %d, isave: %d, time_pos: %d", pcpn_day, isave,
		time_pos);
logMessage ("zdata: %f",
		zdata[pcpn_day].stn[isave].zlevel2[time_pos].data);
	sprintf (muf, "%5.2f",
		 zdata[pcpn_day].stn[isave].zlevel2[time_pos].data);
     }

   argcount = 0;
   XtSetArg (args[argcount], XmNvalue, muf);
   argcount++;
   textz = XmCreateTextField (rowcol, "Point QPF", args, argcount);
   XtManageChild (textz);

   t = XmStringCreateLocalized ("Station quality");

   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   if (initial_qual < 0
       || zdata[pcpn_day].stn[isave].zlevel2[time_pos].data < 0)
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

	     if (i == 0)
		XtSetArg (args[argcount], XmNset, True);

	     else
		XtSetArg (args[argcount], XmNset, False);

	     argcount++;

	     pbutton = XmCreateToggleButton (rowcol1, st[i], args, argcount);
	     XtAddCallback (pbutton, XmNvalueChangedCallback,
			    reset_zstation_quality, (XtPointer) i);

	     XtManageChild (pbutton);


	  }


     }

   st[0] = "Verified";
   st[1] = "Calculated";
   st[2] = "Bad";

   for (i = 0; i < 3; i++)
     {

	argcount = 0;

	/* If the station value is calculated, then only show the
	   calculated toggle button. */
	if (initial_qual == 5 && i != 1)
	   continue;

	/* If the station is not estimated do not show the estimated
	   toggle button. */
	if (initial_qual != 5 && i == 1)
	   continue;

	/* Set the verified button. */
	if (i == 0 && initial_qual == 8 && naflag != 1)
	   XtSetArg (args[argcount], XmNset, True);

	else if (i == 1 && initial_qual == 5 && naflag != 1)
	   XtSetArg (args[argcount], XmNset, True);

	/* Set the Bad toggle button?. */
	else if (i == 2 && initial_qual == 1 && naflag != 1)
	   XtSetArg (args[argcount], XmNset, True);

	else
	   XtSetArg (args[argcount], XmNset, False);

	argcount++;

	pbutton = XmCreateToggleButton (rowcol1, st[i], args, argcount);
	XtAddCallback (pbutton, XmNvalueChangedCallback,
		       change_zstation_quality, (XtPointer) (i));
	XtManageChild (pbutton);

	if (naflag == 1)
	   XtSetSensitive (pbutton, False);

     }

   if (zstation[isave].xadd == -1 && zstation[isave].yadd == -1)
      initial_pos = 0;

   else if (zstation[isave].xadd == 0 && zstation[isave].yadd == -1)
      initial_pos = 2;

   else if (zstation[isave].xadd == -1 && zstation[isave].yadd == 0)
      initial_pos = 1;

   else if (zstation[isave].xadd == 0 && zstation[isave].yadd == 0)
      initial_pos = 3;

   if (initial_qual != 5)
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

	if (i == initial_pos)
	   XtSetArg (args[argcount], XmNset, True);

	else
	   XtSetArg (args[argcount], XmNset, False);

	argcount++;

	pbutton = XmCreateToggleButton (rowcol3, st[i], args, argcount);
	XtAddCallback (pbutton, XmNvalueChangedCallback,
		       change_zstation_location, (XtPointer) (i));
	XtManageChild (pbutton);

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

   XtAddCallback (applyButton, XmNactivateCallback, change_zcustom_file,
                  (XtPointer) isave);

   XtAddCallback (closeButton, XmNactivateCallback, cancel_zedit,
                  (XtPointer) isave);

   XtSetSensitive ( graphButton, False );

   atom = XmInternAtom(XtDisplay(edit_dialog), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(edit_dialog, atom, cancel_zedit, NULL);

   XtManageChild (rowcol);
   XtManageChild (rowcol1);
   XtManageChild (rowcol3);
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
change_zstation_location (Widget w, XtPointer data, XtPointer call_data)
{


   if ((int) data == 0)
     {

	zstation[isave].xadd = -1;
	zstation[isave].yadd = -1;

     }

   else if ((int) data == 2)
     {

	zstation[isave].xadd = 0;
	zstation[isave].yadd = -1;

     }

   else if ((int) data == 1)
     {

	zstation[isave].xadd = -1;
	zstation[isave].yadd = 0;

     }

   else if ((int) data == 3)
     {

	zstation[isave].xadd = 0;
	zstation[isave].yadd = 0;

     }

   return;

}


void
change_zcustom_file (Widget w, XtPointer data, XtPointer call_data)
{

   extern Widget rowcol1;
   extern int map_flag;
   extern int grids_flag;
   extern int points_flag;
   extern int pcp_flag;
   extern Widget diswidget[6];
   extern struct zdata zdata[10];
   extern int pcpn_time;
   extern int pcpn_day;
   extern char zstation_list_custom_file[100];
   int i;
   int m;
   FILE *fp;
   Cardinal argcount;
   Arg args[10];
   int time_pos;
   char *p;
   float val, fdif;
   char *cstr;
   int k;
   extern int pcp_in_use[500];
   extern Widget rpbutton;
   
   mSetCursor ( M_WATCH );

   time_pos = pcpn_time;

   send_expose ();

   fp = fopen (zstation_list_custom_file, "w");
   if (fp != NULL)
     {
	bzero (message, 150);
	sprintf (message, "Opened file: %s\n", zstation_list_custom_file);
	logMessage (message);

	for (i = 0; i < max_zstations; i++)
	   fprintf(fp, "%s %s %d %d\n", zstation[i].hb5, zstation[i].parm,
		                        zstation[i].xadd, zstation[i].yadd);

	fclose (fp);
     }
   else
     {
	bzero (message, 150);
	sprintf (message, "Could not open file: %s\n",
		 zstation_list_custom_file);
	logMessage (message);
     }
   argcount = 0;
   XtSetArg (args[argcount], XmNvalue, &cstr);
   argcount++;
   XtGetValues (textz, args, argcount);

   val = atof (cstr);
   p = strchr (cstr, 'M');
   XtFree (cstr);

/* use manually entered data */

   fdif = fabs (val - zdata[pcpn_day].stn[isave].zlevel2[time_pos].data);

   if (fdif > .01 && p == NULL && reset_value == 0)
     {

	zdata[pcpn_day].stn[isave].zlevel2[time_pos].data = val;
	zdata[pcpn_day].stn[isave].zlevel2[time_pos].qual = 2;
     }

   else
     {

	zdata[pcpn_day].stn[isave].zlevel2[time_pos].qual = new_qual;

	/* 24 hour data set bad/good then 6 hourly bad/good also */

     }

  /* time_pos = 100 + pcpn_day * 4 + pcpn_time;*/
 
   
   /* If there is edit for freezing level, force to go back to Point and
      Render Grids+MAPS for all 4 periods in full day */
  
   for (k = 0; k < 4; k++)
   {    
      time_pos = 100 + pcpn_day * 4 + k;
      pcp_in_use[time_pos] = -1;     
         
   } 

   for (k = 0; k < 5; k++)
   {
      if (zdata[pcpn_day].used[k] != 0)     
	 zdata[pcpn_day].used[k] = 2;  
   
   }
   
   for (k = 1; k < 7; k++)
   {
      XtSetSensitive (diswidget[k], False);
   }

   time_pos = 100 + pcp_flag;

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

   k = 0; 

   XtSetArg (args[0], XmNmenuHistory, diswidget[k]);
   XtSetValues (rowcol1, args, 1);

   XtSetSensitive (rpbutton, True);

   send_expose ();
   mSetCursor ( M_NORMAL );
   return;

}

static int hmflag = 0;

void
change_zstation_quality (Widget w, XtPointer data, XtPointer call_data)
{



   hmflag++;
   if (hmflag == 1)
      return;

   hmflag = 0;

   if ((int) data == 0)
      new_qual = 8;

   else if ((int) data == 1)
      new_qual = 5;

   else if ((int) data == 2)
      new_qual = 1;

}


void cancel_zedit ()
{
   XtVaGetValues ( edit_dialog, XmNx, & xposition, NULL );
   XtVaGetValues ( edit_dialog, XmNy, & yposition, NULL );
   XtPopdown (edit_dialog);
   XtDestroyWidget (edit_dialog);
   edit_dialog = NULL;
}

void
reset_zstation_quality (Widget w, XtPointer data, XtPointer call_data)
{

   extern int pcpn_day;
   extern struct zdata zdata[10];
   extern int pcpn_time;
   int time_pos;
   int k;

   time_pos = pcpn_time;

   if ((int) data == 1)
     {


	for (k = 0; k < 5; k++)
	  {

	     zdata[pcpn_day].stn[isave].zlevel2[k].qual =
		zdata[pcpn_day].stn[isave].zlevel1[k].qual;

	     zdata[pcpn_day].stn[isave].zlevel2[k].data =
		zdata[pcpn_day].stn[isave].zlevel1[k].data;

	  }

	reset_value = 1;
	new_qual = zdata[pcpn_day].stn[isave].zlevel1[time_pos].qual;

     }

   else
      reset_value = 0;



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/edit_zstations.c,v $";
 static char rcs_id2[] = "$Id: edit_zstations.c,v 1.5 2007/10/18 16:08:56 lawrence Exp $";}
/*  ===================================================  */

}
