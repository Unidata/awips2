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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <Xm/Text.h>
#include <Xm/Xm.h>

#include "gageqc_defs.h"
#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_library.h"
#include "map_menubar_cb.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "Xtools.h"
#include "disagg.h"

extern char * area_val_local;
extern char grid_file[];
extern char map_file[];
extern char mat_file[];
extern char mpe_rfc_name_tok_val[];
extern char pcpn_bad_file[];
extern char pcpn_dev_file[];
extern char proc_pcpn_file[];
extern char rsel_file[];
extern char temp_bad_file[];
extern char temp_dev_file[];
extern char tgrid_file[];
extern char *timefile[][5];
extern char tpoint2_file[];
extern char *ttimefile[][6];
extern char type[];
extern char zgrid_file[];
extern char zpoint2_file[];
extern char *ztimefile[][4];
extern struct _dqc_run_date dqc_run_date;
extern struct station *station;
extern struct station *tstation;
extern struct station *zstation;
extern int flf_on;
extern int grids_flag;
extern int isom;
extern int map_flag;
extern int maxmin_on;
extern int max_stations;
extern int max_tstations;
extern int max_zstations;
extern int pcp_flag;
extern int pcp_in_use[];
extern int pcpn_day;
extern int pcpn_time_step;
extern int points_flag;
extern int qpf_on;
extern struct pdata pdata[];
extern struct tdata tdata[];
extern struct zdata zdata[];
extern struct map mean_areal_precip_global[];
extern Widget diswidget[];
extern Widget rowcol1;

int quit_flag;
static int new_area_flag = 0;
Widget dbase_dialog;

static Widget precipitation_button = 0;
static Widget temperature_button = 0;
static Widget freezing_level_button = 0;

void set_button_handles ( Widget precipitation_push_button,
                          Widget temperature_push_button,
                          Widget freezing_level_push_button )
{
   precipitation_button = precipitation_push_button;
   temperature_button = temperature_push_button;
   freezing_level_button = freezing_level_push_button;
}


static void quit_callback (Widget w, XtPointer client_data, XtPointer call_data)
{
   /* int ier; */
   char ibuf[GAGEQC_FILENAME_LEN];
   const char * scratch_dir = NULL;

   scratch_dir = get_mpe_scratch_dir ( );

   /* Check if the "pcp" files are being placed in a scratch directory
      or in the current working directory. */
   if ( scratch_dir != NULL )
   {
      sprintf (ibuf, "rm -f %s/pcp.*.%d", scratch_dir, getpid ());
   }
   else
   {
      sprintf (ibuf, "rm -f pcp.*.%d", getpid ());
   }

   system (ibuf);
  
   /* Free the dqc memory at this point only when exiting. */
   free_dqc_data ( );

   exit (1);

}

static void quit ()
{

   Cardinal argcount;
   Arg args[10];
   XmString msg;
   Widget err;
   Widget parent_widget;

   parent_widget = _get_map_shell ();

   quit_flag = 0;

   msg = XmStringCreateLocalized ("Quit daily_qc?");

   argcount = 0;
   XtSetArg (args[argcount], XmNmessageString, msg);
   argcount++;
   XtSetArg (args[argcount], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);
   argcount++;
   err = XmCreateErrorDialog (parent_widget, "info", args, argcount);
   XtUnmanageChild (XmMessageBoxGetChild (err, XmDIALOG_HELP_BUTTON));

   XtAddCallback (err, XmNokCallback, quit_callback, NULL);
   XtManageChild (err);
   XmStringFree (msg);

}


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

void send_dbase (Widget w, XtPointer client_data, XtPointer call_data)
{

   Cardinal argcount;
   Arg args[20];
   char text[100000];
   char tbuf[1000];
   int j, m;
   XmString btext;
   time_t ltime;
   Widget parent_widget;
   Widget rowcol, ztext;
   struct tm *gm;
   XmString cancel;
   XmString t;

   text[0] = 0;
   cancel = XmStringCreateLocalized ("No");

   parent_widget = _get_map_shell ();

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {
      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 5; m++)
      {

	 if (pdata[j].used[m] != 3 && pdata[j].used[m] != 2)
	 {

	    if (zdata[j].used[m] != 3 && zdata[j].used[m] != 2)
	    {

	       if (tdata[j].used[m] != 3 && tdata[j].used[m] != 2)
		  continue;

	    }

	 }

	 ltime = pdata[j].data_time;
	 gm = gmttime (&ltime);

	 sprintf (tbuf, "%02d-%02d-%04d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);

	 strcat (text, tbuf);

	 break;

      }

   }

   btext = XmStringCreateLocalized ("Save the following Precipitation "
				    "and MAP datasets?");
   argcount = 0;
   XtSetArg (args[argcount], XmNmessageString, btext);
   argcount++;
   XtSetArg (args[argcount], XmNcancelLabelString, cancel);
   argcount++;
   XtSetArg (args[argcount], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);
   argcount++;

   /* Set the title! */
   t = XmStringCreateLocalized ( "Send to Database" );
   XtSetArg ( args[argcount], XmNdialogTitle, t);
   argcount++;

   dbase_dialog =
      XmCreateMessageDialog (parent_widget, "Send database", args, argcount);

   /* Free the String. */
   XmStringFree ( t );

   XtAddCallback (dbase_dialog, XmNokCallback, save_dbase, NULL);
   XtAddCallback (dbase_dialog, XmNcancelCallback, cancel_dbase, NULL);

   XtUnmanageChild (XmMessageBoxGetChild
		    (dbase_dialog, XmDIALOG_HELP_BUTTON));

   argcount = 0;
   rowcol =
      XtCreateWidget ("rowcolo", xmRowColumnWidgetClass, dbase_dialog, args,
		      argcount);

   argcount = 0;
   XtSetArg (args[argcount], XmNeditMode, XmMULTI_LINE_EDIT);
   argcount++;
   XtSetArg (args[argcount], XmNscrollBarDisplayPolicy, XmSTATIC);
   argcount++;
   XtSetArg (args[argcount], XmNeditable, False);
   argcount++;
   XtSetArg (args[argcount], XmNcolumns, 40);
   argcount++;
   XtSetArg (args[argcount], XmNrows, 10);
   argcount++;
   XtSetArg (args[argcount], XmNscrollVertical, True);
   argcount++;
   XtSetArg (args[argcount], XmNscrollHorizontal, False);
   argcount++;
   XtSetArg (args[argcount], XmNcursorPositionVisible, False);
   argcount++;
   XtSetArg (args[argcount], XmNscrollingPolicy, XmAPPLICATION_DEFINED);
   argcount++;
   XtSetArg (args[argcount], XmNvalue, &text);
   argcount++;
   ztext = XmCreateScrolledText (rowcol, "text", args, argcount);

   XtManageChild (ztext);
   XtManageChild (rowcol);
   XtManageChild (dbase_dialog);

}

void send_dbase_new_area ( Widget w, 
                           XtPointer client_data, 
                           XtPointer call_data)
{

   Cardinal argcount;
   Arg args[20];
   char text[100000];
   char tbuf[1000];
   int j, m;
   XmString btext;
   time_t ltime;
   Widget parent_widget;
   Widget rowcol, ztext;
   struct tm *gm;
   XmString cancel;
   XmString t;

   text[0] = 0;
   cancel = XmStringCreateLocalized ("No");

   parent_widget = _get_map_shell ();

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {
      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 5; m++)
      {

	 if (pdata[j].used[m] != 3 && pdata[j].used[m] != 2)
	 {

	    if (zdata[j].used[m] != 3 && zdata[j].used[m] != 2)
	    {

	       if (tdata[j].used[m] != 3 && tdata[j].used[m] != 2)
		  continue;

	    }

	 }

	 ltime = pdata[j].data_time;
	 gm = gmttime (&ltime);

	 sprintf (tbuf, "%02d-%02d-%04d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);

	 strcat (text, tbuf);

	 break;

      }

   }

   btext = XmStringCreateLocalized ("Save the following Precipitation "
				    "and MAP datasets?");
   argcount = 0;
   XtSetArg (args[argcount], XmNmessageString, btext);
   argcount++;
   XtSetArg (args[argcount], XmNcancelLabelString, cancel);
   argcount++;
   XtSetArg (args[argcount], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);
   argcount++;

   /* Set the title! */
   t = XmStringCreateLocalized ( "Send to Database" );
   XtSetArg ( args[argcount], XmNdialogTitle, t);
   argcount++;

   dbase_dialog =
      XmCreateMessageDialog (parent_widget, "Send database", args, argcount);

   XmStringFree ( t );

   XtAddCallback (dbase_dialog, XmNokCallback, save_dbase, client_data);
   XtAddCallback (dbase_dialog, XmNcancelCallback, cancel_dbase, client_data);

   XtUnmanageChild (XmMessageBoxGetChild
		    (dbase_dialog, XmDIALOG_HELP_BUTTON));

   argcount = 0;
   rowcol =
      XtCreateWidget ("rowcolo", xmRowColumnWidgetClass, dbase_dialog, args,
		      argcount);

   argcount = 0;
   XtSetArg (args[argcount], XmNeditMode, XmMULTI_LINE_EDIT);
   argcount++;
   XtSetArg (args[argcount], XmNscrollBarDisplayPolicy, XmSTATIC);
   argcount++;
   XtSetArg (args[argcount], XmNeditable, False);
   argcount++;
   XtSetArg (args[argcount], XmNcolumns, 40);
   argcount++;
   XtSetArg (args[argcount], XmNrows, 10);
   argcount++;
   XtSetArg (args[argcount], XmNscrollVertical, True);
   argcount++;
   XtSetArg (args[argcount], XmNscrollHorizontal, False);
   argcount++;
   XtSetArg (args[argcount], XmNcursorPositionVisible, False);
   argcount++;
   XtSetArg (args[argcount], XmNscrollingPolicy, XmAPPLICATION_DEFINED);
   argcount++;
   XtSetArg (args[argcount], XmNvalue, &text);
   argcount++;
   ztext = XmCreateScrolledText (rowcol, "text", args, argcount);

   XtManageChild (ztext);
   XtManageChild (rowcol);
   XtManageChild (dbase_dialog);

}

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

static void quit_all2 ()
{

   int j, m;

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      for (m = 0; m < 5; m++)
      {

	 /* not databased */

	 if (pdata[j].used[m] != 3 && pdata[j].used[m] != 2)
         {
	    if (zdata[j].used[m] != 3 && zdata[j].used[m] != 2)
	    {

	       if (tdata[j].used[m] != 3 && tdata[j].used[m] != 2)
               {
		  continue;
               }
	    }
         }

	 send_dbase (NULL, NULL, NULL);

	 quit_flag = 1;

	 return;

      }

   }

   quit ( );
}

static void check_saved_datasets (Widget w,
                                  XtPointer client_data,
                                  XtPointer call_data )
{
   Display * pMapDisplay = _get_map_display ( );

   int j, m;
   XtCallbackProc qc_gui_function = (XtCallbackProc) client_data;

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      for (m = 0; m < 5; m++)
      {

	 /* not databased */
	 if (pdata[j].used[m] != 3 && pdata[j].used[m] != 2)
         {
	    continue;
         }

	 send_dbase_new_area (w, client_data, NULL);

	 return;

      }

   }

   /* The datasets have all been QC'd and saved to the database. */
   /* Go ahead and load the new DailyQC dataset. */
   if ( qpf_on == 1 )
   {
      change_pcpn_edit_mode ( w, NULL, NULL );
   }

   set_dqc_area_and_date ( );

   if ( qpf_on == 1 )
   {
      change_pcpn_edit_mode ( w, NULL, NULL );
   }

   if ( maxmin_on == 1 )
   {
      change_maxmin_edit_mode ( w, NULL, NULL );
   }

   if ( flf_on == 1 )
   {
      change_z_edit_mode ( w, NULL, NULL );
   }

   /* Free the data from the previous run. */
   free_dqc_data ( );
   mSetCursor ( M_WATCH );
   XFlush ( pMapDisplay );

   load_gage_data ( area_val_local,
                    is_area_master ( ),
                    dqc_run_date.dqc_data_year,
                    dqc_run_date.dqc_data_month,
                    dqc_run_date.dqc_data_day,
                    dqc_run_date.dqc_num_days);
   
   /* Launch the qc gui function. */
   qc_gui_function ( w, NULL, NULL );
   mSetCursor ( M_NORMAL );
}


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
static void send_dbase2 ()
{

   Cardinal argcount;
   Arg args[20];
   char text[10000];
   char tbuf[1000];
   int j, m;
   XmString btext;
   time_t ltime;
   Widget parent_widget;
   Widget rowcol, ztext;
   struct tm *gm = NULL;
   XmString cancel;
   XmString t;

   parent_widget = _get_map_shell ();

   text[0] = 0;
   cancel = XmStringCreateLocalized ("quit anyway");

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 5; m++)
      {

	 if (pdata[j].used[m] != 1 || pdata[j].level != 1)
	 {
	    continue;
	 }

	 ltime = pdata[j].data_time;

	 gm = gmttime (&ltime);

	 sprintf (tbuf, "Precipitation %02d-%02d-%02d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);

	 strcat (text, tbuf);

	 break;

      }

   }

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 5; m++)
      {

	 if (zdata[j].used[m] != 1 || zdata[j].level[m] != 1)
	 {
	    continue;
	 }

	 ltime = pdata[j].data_time;
	 gm = gmttime (&ltime);
	 sprintf (tbuf, "Freezing level %02d-%02d-%02d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);
	 strcat (text, tbuf);
	 break;

      }

   }

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 6; m++)
      {

	 if (tdata[j].used[m] != 1 || tdata[j].level[m] != 1)
	    continue;

	 ltime = pdata[j].data_time;

	 gm = gmttime (&ltime);

	 sprintf (tbuf, "Temperature %02d-%02d-%02d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);

	 strcat (text, tbuf);

	 break;

      }

   }

   btext =
      XmStringCreateLocalized ("The following datasets need to be QC'd!");

   argcount = 0;
   XtSetArg (args[argcount], XmNmessageString, btext);
   argcount++;
   XtSetArg (args[argcount], XmNcancelLabelString, cancel);
   argcount++;
   XtSetArg (args[argcount], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);
   argcount++;

   /* Set the title! */
   t = XmStringCreateLocalized ( "Send to Database" );
   XtSetArg ( args[argcount], XmNdialogTitle, t);
   argcount++;

   dbase_dialog =
      XmCreateMessageDialog (parent_widget, "Send database", args, argcount);

   XmStringFree ( t );

   XtAddCallback (dbase_dialog, XmNokCallback, ok_dbase, NULL);
   XtAddCallback (dbase_dialog, XmNcancelCallback, quit_all2, NULL);

   XtUnmanageChild (XmMessageBoxGetChild
		    (dbase_dialog, XmDIALOG_HELP_BUTTON));

   argcount = 0;
   rowcol =
      XtCreateWidget ("rowcolo", xmRowColumnWidgetClass, dbase_dialog, args,
		      argcount);

   argcount = 0;
   XtSetArg (args[argcount], XmNeditMode, XmMULTI_LINE_EDIT);
   argcount++;
   XtSetArg (args[argcount], XmNscrollBarDisplayPolicy, XmSTATIC);
   argcount++;
   XtSetArg (args[argcount], XmNeditable, False);
   argcount++;
   XtSetArg (args[argcount], XmNcolumns, 40);
   argcount++;
   XtSetArg (args[argcount], XmNrows, 10);
   argcount++;
   XtSetArg (args[argcount], XmNscrollVertical, True);
   argcount++;
   XtSetArg (args[argcount], XmNscrollHorizontal, False);
   argcount++;
   XtSetArg (args[argcount], XmNcursorPositionVisible, False);
   argcount++;
   XtSetArg (args[argcount], XmNscrollingPolicy, XmAPPLICATION_DEFINED);
   argcount++;
   XtSetArg (args[argcount], XmNvalue, &text);
   argcount++;
   ztext = XmCreateScrolledText (rowcol, "text", args, argcount);

   XtManageChild (ztext);
   XtManageChild (rowcol);
   XtManageChild (dbase_dialog);

}

/* When a different subarea or gageqc period is chosen, then
   check to make sure that the former gageqc data have been 
   quality controlled. */
static void check_gageqc_status ( XtCallbackProc qc_gui_function )
{

   Cardinal argcount;
   Arg args[20];
   char text[10000];
   char tbuf[1000];
   int j, m;
   XmString btext;
   time_t ltime;
   Widget parent_widget;
   Widget rowcol, ztext;
   struct tm *gm = NULL;
   XmString cancel;
   XmString t;

   parent_widget = _get_map_shell ();

   text[0] = 0;
   cancel = XmStringCreateLocalized ("Do not qc");

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 5; m++)
      {

	 if (pdata[j].used[m] != 1 || pdata[j].level != 1)
	 {
	    continue;
	 }

	 ltime = pdata[j].data_time;

	 gm = gmttime (&ltime);

	 sprintf (tbuf, "Precipitation %02d-%02d-%02d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);

	 strcat (text, tbuf);

	 break;

      }

   }

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 5; m++)
      {

	 if (zdata[j].used[m] != 1 || zdata[j].level[m] != 1)
	 {
	    continue;
	 }

	 ltime = pdata[j].data_time;
	 gm = gmttime (&ltime);
	 sprintf (tbuf, "Freezing level %02d-%02d-%02d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);
	 strcat (text, tbuf);
	 break;

      }

   }

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 6; m++)
      {

	 if (tdata[j].used[m] != 1 || tdata[j].level[m] != 1)
	    continue;

	 ltime = pdata[j].data_time;

	 gm = gmttime (&ltime);

	 sprintf (tbuf, "Temperature %02d-%02d-%02d\n",
		  gm->tm_mon + 1, gm->tm_mday, gm->tm_year + 1900);

	 strcat (text, tbuf);

	 break;

      }

   }

   btext =
      XmStringCreateLocalized ("The following datasets need to be QC'd!");

   argcount = 0;
   XtSetArg (args[argcount], XmNmessageString, btext);
   argcount++;
   XtSetArg (args[argcount], XmNcancelLabelString, cancel);
   argcount++;
   XtSetArg (args[argcount], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);
   argcount++;

   /* Set the title! */
   t = XmStringCreateLocalized ( "Send to Database" );
   XtSetArg ( args[argcount], XmNdialogTitle, t);

   argcount++;

   dbase_dialog =
      XmCreateMessageDialog (parent_widget, "Send database", args, argcount);

   XmStringFree ( t );

   XtAddCallback (dbase_dialog, XmNokCallback, ok_dbase, NULL);
   XtAddCallback (dbase_dialog, XmNcancelCallback, check_saved_datasets, 
                  (XtPointer)qc_gui_function);

   XtUnmanageChild (XmMessageBoxGetChild
		    (dbase_dialog, XmDIALOG_HELP_BUTTON));

   argcount = 0;
   rowcol =
      XtCreateWidget ("rowcolo", xmRowColumnWidgetClass, dbase_dialog, args,
		      argcount);

   argcount = 0;
   XtSetArg (args[argcount], XmNeditMode, XmMULTI_LINE_EDIT);
   argcount++;
   XtSetArg (args[argcount], XmNscrollBarDisplayPolicy, XmSTATIC);
   argcount++;
   XtSetArg (args[argcount], XmNeditable, False);
   argcount++;
   XtSetArg (args[argcount], XmNcolumns, 40);
   argcount++;
   XtSetArg (args[argcount], XmNrows, 10);
   argcount++;
   XtSetArg (args[argcount], XmNscrollVertical, True);
   argcount++;
   XtSetArg (args[argcount], XmNscrollHorizontal, False);
   argcount++;
   XtSetArg (args[argcount], XmNcursorPositionVisible, False);
   argcount++;
   XtSetArg (args[argcount], XmNscrollingPolicy, XmAPPLICATION_DEFINED);
   argcount++;
   XtSetArg (args[argcount], XmNvalue, &text);
   argcount++;
   ztext = XmCreateScrolledText (rowcol, "text", args, argcount);

   XtManageChild (ztext);
   XtManageChild (rowcol);
   XtManageChild (dbase_dialog);

}

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
int quit_all( )

{
   int j,m;

   for(j=0;j<MAX_GAGEQC_DAYS;j++) 
   {
      for(m=0;m<5;m++)
      {
         /* not databased */
         if(pdata[j].used[m] !=1 || pdata[j].level !=1)
         {
            continue;
         }

         send_dbase2();
         quit_flag=1;
         return 0;

      }
   }

   for(j=0;j<MAX_GAGEQC_DAYS;j++)
   {
      for(m=0;m<5;m++)
      {
         /* not databased */
         if(zdata[j].used[m] !=1 || zdata[j].level[m] !=1)
         {
            continue;
         }

         send_dbase2();
         quit_flag=1;
         return 0;

       }
   }

   for(j=0;j<MAX_GAGEQC_DAYS;j++)
   {
      for(m=0;m<5;m++)
      {
         /* not databased */
         if(tdata[j].used[m] !=1 || tdata[j].level[m] !=1)
         {
            continue;
         }

         send_dbase2();
         quit_flag=1;
         return 0;

      }
   }

   quit_all2();
   return 0;
}

int load_new_data_set ( Widget w,
                        const char * area_id,
                        int master_file_flag,
                        int begin_year,
                        int begin_month,
                        int begin_day,
                        XtCallbackProc qc_gui_function )
{
   Display * pMapDisplay = _get_map_display ( );

   static int first = 1;
   int j,m;
   int return_value;

   if ( first == 1 )
   {
      first = 0;

      /* This is the first time that this routine has been called.
         There is no previous dataset which may still need to be 
         QC'd or saved. */
      set_dqc_area_and_date ( );

      if ( qpf_on == 1 )
      {
         change_pcpn_edit_mode ( w, NULL, NULL );
      }
    
      if ( maxmin_on == 1 )
      {
         change_maxmin_edit_mode ( w, NULL, NULL );
      }

      if ( flf_on == 1 )
      {
         change_z_edit_mode ( w, NULL, NULL );
      }

      mSetCursor ( M_WATCH );
      XFlush ( pMapDisplay );

      return_value = load_gage_data ( area_val_local,
                                      master_file_flag,
                                      dqc_run_date.dqc_data_year,
                                      dqc_run_date.dqc_data_month,
                                      dqc_run_date.dqc_data_day,
                                      dqc_run_date.dqc_num_days);

      if ( return_value == DAILYQC_FAILED )
      {
         /* An error occurred reading/processing the DailyQC datasets.
            Desensitize the precipitation, temperature and freezing
            buttons on the Choose Hour Window. */

         if ( precipitation_button != 0 )
         {
            XtSetSensitive ( precipitation_button, False );
         }

         if ( temperature_button != 0 )
         {
            XtSetSensitive ( temperature_button, False );
         }

         if ( freezing_level_button != 0 )
         {
            XtSetSensitive ( freezing_level_button, False );
         }

         return DAILYQC_FAILED;
      }

      qc_gui_function ( w, NULL, NULL );
      mSetCursor ( M_NORMAL );
   }
   else
   {
      new_area_flag = 1;
      /* Check if any of the GageQC datasets have not been QC'd. */
      /* Check the precipitation datasets. */
      for(j=0;j<MAX_GAGEQC_DAYS;j++) 
      {
         for(m=0;m<5;m++)
         {
            /* not databased */
            if(pdata[j].used[m] !=1 || pdata[j].level !=1)
            {
               continue;
            }

            /* Found a dataset which has not been QC'd.  List all of the
               datasets that have not been QC'd and ask user if he wants
               to continue. */
            check_gageqc_status ( qc_gui_function );
            return DAILYQC_OK;

         }
      }

      /* Check the freezing level datasets. */
      for(j=0;j<MAX_GAGEQC_DAYS;j++)
      {
         for(m=0;m<5;m++)
         {
            /* not databased */
            if(zdata[j].used[m] !=1 || zdata[j].level[m] !=1)
            {
               continue;
            }

            check_gageqc_status ( qc_gui_function );
            return DAILYQC_OK;

          }
      }

      /* Check the temperature datasets. */
      for(j=0;j<MAX_GAGEQC_DAYS;j++)
      {
         for(m=0;m<5;m++)
         {
            /* not databased */
            if(tdata[j].used[m] !=1 || tdata[j].level[m] !=1)
            {
               continue;
            }

            check_gageqc_status ( qc_gui_function );
            return DAILYQC_OK ;

         }
      }

      /* Each of the GageQC datasets has been QC'd. */
      /* Check if any of the datasets need to be saved to the database. */
      check_saved_datasets( w, (XtPointer) qc_gui_function, NULL);
   }


   return DAILYQC_OK;
}
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
void cancel_dbase2 (Widget w, XtPointer client_data, XtPointer call_data)
{
   Display * pMapDisplay = _get_map_display ( );

   XtCallbackProc qc_gui_function = (XtCallbackProc)client_data;
   XtDestroyWidget (dbase_dialog);

   if ( qc_gui_function != NULL )
   {
      new_area_flag = 0;
      set_dqc_area_and_date ( );

      if ( qpf_on == 1 )
      {
         change_pcpn_edit_mode ( w, NULL, NULL );
      }

      if ( maxmin_on == 1 )
      {
         change_maxmin_edit_mode ( w, NULL, NULL );
      }

      if ( flf_on == 1 )
      {
         change_z_edit_mode ( w, NULL, NULL );
      }

      free_dqc_data ( );
      mSetCursor ( M_WATCH );
      XFlush ( pMapDisplay );
      load_gage_data ( area_val_local,
                       is_area_master(),
                       dqc_run_date.dqc_data_year,
                       dqc_run_date.dqc_data_month,
                       dqc_run_date.dqc_data_day,
                       dqc_run_date.dqc_num_days);
      qc_gui_function ( w, NULL, NULL );
      mSetCursor ( M_NORMAL );
   }
   else
   {
      quit_all2 ();
   }
}

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

void ok_dbase (Widget w, XtPointer client_data, XtPointer call_data)
{
   XtDestroyWidget (dbase_dialog);
   
   /* Reset the date fields used by the Choose Data Period window. */
   reset_dqc_area_and_date ( );
   
}

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

void cancel_dbase (Widget w, XtPointer client_data, XtPointer call_data)
{
   Display * pMapDisplay = _get_map_display ( );

   XtCallbackProc qc_gui_function = (XtCallbackProc)client_data;
   XtDestroyWidget (dbase_dialog);

   if ( qc_gui_function != NULL )
   {
      new_area_flag = 0;
      set_dqc_area_and_date ( );

      if ( qpf_on == 1 )
      {
         change_pcpn_edit_mode ( w, NULL, NULL );
      }

      if ( maxmin_on == 1 )
      {
         change_maxmin_edit_mode ( w, NULL, NULL );
      }

      if ( flf_on == 1 )
      {
         change_z_edit_mode ( w, NULL, NULL );
      }

      free_dqc_data ( );
      mSetCursor ( M_WATCH );
      XFlush ( pMapDisplay );

      load_gage_data ( area_val_local,
                       is_area_master(),
                       dqc_run_date.dqc_data_year,
                       dqc_run_date.dqc_data_month,
                       dqc_run_date.dqc_data_day,
                       dqc_run_date.dqc_num_days);
      qc_gui_function ( w, NULL, NULL );
      mSetCursor ( M_NORMAL );
   }
   else
   {
      if (quit_flag == 1)
      {
         quit ();
      }
   }
}

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

void save_dbase (Widget w, XtPointer client_data, XtPointer call_data)
{
   Cardinal argcount;
   Arg args[10];

   XtCallbackProc qc_gui_function = (XtCallbackProc)client_data;

   char buf[2000], ebuf[2000], fbuf[2000], mbuf[2000], xbuf[2000],
      dbuf[2000], pbuf[2000];
   int dqcBasetime;
   int dqcBaseIndex;
   int k, j, m, l, ll, num, i, h, numzones;
   int jj;
   struct tm *gm = NULL;
   FILE *fp = NULL;
   char *spaces = NULL;
   Widget parent_widget;
   Widget dialog, rowcol, s_text;
   int posit;
   XmString text;
   float temp = 0.0;
   time_t old_time;
   char pchar[2];
   int save_isom;
   int time_pos = 0;
   int archive_flag[10];
   struct hrap_grid *hrap_grid = NULL;
   Display * pMapDisplay = _get_map_display ( );

   dqcBasetime = getDqcBasetime ( );
   dqcBaseIndex = dqcBasetime % 6;

   /* Retrieve the HRAP grid. */
   hrap_grid = get_hrap_grid ();

   parent_widget = _get_map_shell ();

   for (k = 0; k < 10; k++)
   {
      archive_flag[k] = -1;
   }

   save_isom = isom;
   pchar[1] = 0;

   posit = 0;
   spaces = "                      ";

   text =
      XmStringCreateLocalized
      ("Gridding precipitation,building MAPs and Saving to database");

   argcount = 0;
   XtSetArg (args[argcount], XmNmessageString, text);
   argcount++;
   XtSetArg (args[argcount], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);
   argcount++;
   dialog =
      XmCreateMessageDialog (parent_widget, "Save database", args, argcount);
   XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
   XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_OK_BUTTON));
   XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
   XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_DEFAULT_BUTTON));
   XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_SEPARATOR));

   argcount = 0;
   rowcol = XtCreateWidget ("rowcolo", xmRowColumnWidgetClass, dialog, args,
			    argcount);

   argcount = 0;
   XtSetArg (args[argcount], XmNeditMode, XmMULTI_LINE_EDIT);
   argcount++;
   XtSetArg (args[argcount], XmNscrollBarDisplayPolicy, XmSTATIC);
   argcount++;
   XtSetArg (args[argcount], XmNeditable, False);
   argcount++;
   XtSetArg (args[argcount], XmNcolumns, 40);
   argcount++;
   XtSetArg (args[argcount], XmNrows, 10);
   argcount++;
   XtSetArg (args[argcount], XmNscrollVertical, True);
   argcount++;
   XtSetArg (args[argcount], XmNscrollHorizontal, False);
   argcount++;
   XtSetArg (args[argcount], XmNcursorPositionVisible, False);
   argcount++;
   XtSetArg (args[argcount], XmNscrollingPolicy, XmAPPLICATION_DEFINED);
   argcount++;
   s_text = XmCreateScrolledText (rowcol, "text", args, argcount);

   XtManageChild (s_text);

   XtManageChild (rowcol);
   XtManageChild (dialog);
   XmStringFree (text);

   XmtWaitUntilMapped (dialog);

   send_expose ( );

   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 5; m++)
      {
	 if (pdata[j].used[m] == 3 || pdata[j].used[m] == 2)
	 {
	    break;
	 }
      }

      if (m == 5)
      {
	 continue;
      }


      /* re-create all pcpn and maps if necessary */
      /* next three lines are new */
      estimate_daily_stations (j, station, max_stations);
      estimate_partial_stations (j, station, max_stations);
      quality_control_stations (j, station, max_stations);
      check_consistency (j, station, max_stations);

      gm = gmttime (&pdata[j].data_time);

      sprintf (fbuf, "%s%04d%02d%02d", pcpn_bad_file, gm->tm_year + 1900,
	       gm->tm_mon + 1, gm->tm_mday);

      isom = gm->tm_mon;

      write_bad_values (fbuf, j);

      for (l = 0; l < 5; l++)
      {

	 if (pdata[j].used[l] == 0 || pdata[j].used[l] == 3)
	    continue;

         /* this needs to be fixed to ensure that the 
            gridded temperature files are
            named correctly. */
         if (l < 2)
	    old_time = pdata[j].data_time - 86400;
	 else
	    old_time = pdata[j].data_time;

	 gm = gmttime (&old_time);

	 if (l < 4)
	 {
	    ll = 0;
	 }
	 else
	 {
	    ll = 1;
	 }


	 render_pcp (j, l, ll, max_stations, station, hrap_grid, pdata,
		     pcp_in_use);

	 sprintf (dbuf, "%s%s_%04d%02d%02d", grid_file, timefile[2][l],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);

	 if (l < 4)
	 {
	    num = j * 4 + 3 - l;
	 }
	 else
	 {
	    num = 40 + j;
	 }

	 /* Create the MAP. */
	 create_map (num);

      }

      estimate_missing_stations (j, station, max_stations, pdata);

      old_time = pdata[j].data_time;


      gm = gmttime (&old_time);

      sprintf (fbuf, "%s%04d%02d%02d",
	       proc_pcpn_file, gm->tm_year + 1900, gm->tm_mon + 1,
	       gm->tm_mday);

      fp = fopen (fbuf, "w");

      if (fp == NULL)
      {
	 return;
      }

      for (m = 0; m < max_stations; m++)
      {

	 gm = gmtime (&pdata[j].data_time);
	 sprintf (pbuf, "PPD%s%s", type, &station[m].parm[4]);
	 sprintf (buf, ".AR %s %02d%02d%02d DH12/%s ",
		  station[m].hb5, 1900 + gm->tm_year, gm->tm_mon + 1,
		  gm->tm_mday, pbuf);


	 if (pdata[j].used[4] != 0)
	 {

	    if (pdata[j].stn[m].frain[4].data < 0)

	       strcpy (mbuf, "   M  ");

	    else
	    {

	       sprintf (mbuf, "%5.2f", pdata[j].stn[m].frain[4].data);

	       if (pdata[j].stn[m].sflag[4] != 1)
	       {

		  if (pdata[j].stn[m].frain[4].qual == 0)
		     strcat (mbuf, "S");

		  if (pdata[j].stn[m].frain[4].qual == 1)
		     strcat (mbuf, "F");

		  if (pdata[j].stn[m].frain[4].qual == 2)
		     strcat (mbuf, "W");

		  if (pdata[j].stn[m].frain[4].qual == 3)
		     strcat (mbuf, "Q");

		  if (pdata[j].stn[m].frain[4].qual == 4)
		     strcat (mbuf, "D");

		  if (pdata[j].stn[m].frain[4].qual == 8)
		     strcat (mbuf, "V");

		  if (pdata[j].stn[m].frain[4].qual == 5)
		     strcat (mbuf, "E");

		  if (pdata[j].stn[m].frain[4].qual == 6)
		     strcat (mbuf, "L");

	       }

	       else if (pdata[j].stn[m].sflag[4] == 1)
	       {

		  if (pdata[j].stn[m].frain[k].qual == 8)
		     strcat (mbuf, "A");

		  if (pdata[j].stn[m].frain[k].qual == 0)
		     strcat (mbuf, "B");

		  if (pdata[j].stn[m].frain[k].qual == 3)
		     strcat (mbuf, "C");

	       }

	    }

	    strcat (buf, mbuf);
	    strcat (buf, "\n");

	    fputs (buf, fp);

	 }

	 old_time = pdata[j].data_time - 86400;
	 gm = gmtime (&old_time);
	 sprintf (pbuf, "PPQ%s%s", type, &station[m].parm[4]);
	 sprintf (buf, ".ER %s %02d%02d%02d DH18/%s/DIH+6/",
		  station[m].hb5, 1900 + gm->tm_year, gm->tm_mon + 1,
		  gm->tm_mday, pbuf);

	 for (k = 0; k < 4; k++)
	 {

	    if (pdata[j].used[k] == 0)
	    {

	       strcpy (mbuf, "      ");

	       if (k != 3)
		  strcat (mbuf, "/");


	    }


	    else if (pdata[j].stn[m].frain[k].data < 0)
	    {

	       strcpy (mbuf, "   M  ");
	       if (k != 3)
		  strcat (mbuf, "/");

	    }

	    else
	    {

	       sprintf (mbuf, "%5.2f", pdata[j].stn[m].frain[k].data);

	       if (pdata[j].stn[m].frain[k].qual == 8)
		  strcat (mbuf, "V");

	       if (pdata[j].stn[m].frain[k].qual == 1)
		  strcat (mbuf, "F");

	       if (pdata[j].stn[m].frain[k].qual == 2)
		  strcat (mbuf, "W");

	       if (pdata[j].stn[m].frain[k].qual == 3)
		  strcat (mbuf, "Q");

	       if (pdata[j].stn[m].frain[k].qual == 4)
		  strcat (mbuf, "D");

	       if (pdata[j].stn[m].frain[k].qual == 0)
		  strcat (mbuf, "S");

	       if (pdata[j].stn[m].frain[k].qual == 5)
		  strcat (mbuf, "E");

	       if (pdata[j].stn[m].frain[k].qual == 6)
		  strcat (mbuf, "L");

	       if (k != 3)
		  strcat (mbuf, "/");

	    }

	    strcat (buf, mbuf);

	 }

	 strcat (buf, "\n");

	 fputs (buf, fp);
      }

      fclose (fp);
      fp = NULL;

      /*strcpy (cmd, "rcp ");
         strcat (cmd, fbuf);
         strcat (cmd, " ");
         strcat (cmd, dbase_out);

        logMessage ("%s\n", cmd);
         fflush (stdout);
         ier = system (cmd);

         if (ier != 0)
         {

         strcpy (ebuf, "could not execute ");
         strcat (ebuf, cmd);
         berror (drawing_area, ebuf);

         } */

      for (l = 0; l < 4; l++)
      {

	 jj = j * 4 + 3 - l;

	 if (pcp_in_use[jj] != 1)
	    continue;

	 if (l < 2)
	    old_time = pdata[j].data_time - 86400;

	 else
	    old_time = pdata[j].data_time;

	 gm = gmttime (&old_time);

	 /*sprintf (dbuf, "%s%02d-%02d-%02d.%s", grid_file,
	    gm->tm_mon + 1, gm->tm_mday, gm->tm_year+1900, timefile[2][l]);

	    strcpy (cmd, "rcp ");
	    strcat (cmd, dbuf);
	    strcat (cmd, " ");
	    strcat (cmd, grid_out);

	   logMessage ("%s\n", cmd);
	    fflush (stdout);
	    ier = system (cmd);


	   logMessage ("ier is %d %d\n", l, ier);
	   logMessage ("cmd is %s\n", cmd);

	    if (ier != 0)
	    {

	    strcpy (ebuf, "could not execute ");
	    strcat (ebuf, cmd);
	    ErrorDialog ( parent_widget, ebuf);

	    } */

      }

      /* build map file */

      for (m = 0; mean_areal_precip_global[m].hb5[0] != 0; m++)
      {

	 for (k = 0; k < 4; k++)
	 {

	    num = j * 4 + 3 - k;

	    if (mean_areal_precip_global[m].maps_done[num] == 1)
	       goto found;

	 }

      }

    found:

      if (mean_areal_precip_global[m].hb5[0] == 0)
	 continue;

      gm = gmttime (&pdata[j].data_time);

      sprintf (fbuf, "%s%04d%02d%02d", map_file, gm->tm_year + 1900,
	       gm->tm_mon + 1, gm->tm_mday);

      fp = fopen (fbuf, "w");

      if (fp == NULL)
      {
	 return;
      }

      /* at least processing level a data is available but
         no guarantee that MAPs have been rendered */
      old_time = pdata[j].data_time - 86400;
      gm = gmtime (&old_time);

      /* loop through and write maps to file */
      for (m = 0; mean_areal_precip_global[m].hb5[0] != 0; m++)
      {

	 numzones = 0;

	 for (l = 0; l < 4; l++)
	 {

	    if (mean_areal_precip_global[m].zones[l] < 0)
	       continue;

	    numzones++;

	 }

	 for (l = 0; l < 4; l++)
	 {

	    if (mean_areal_precip_global[m].zones[l] < 0)
	       continue;

	    strcpy (xbuf, ".ER ");
	    strcat (xbuf, mean_areal_precip_global[m].hb5);

	    i = 0;

	    while (xbuf[i] != 0)
	    {
	       xbuf[i] = toupper (xbuf[i]);
	       ++i;
	    }

	    /* fix for polygon basins */
	    if (strcmp (mpe_rfc_name_tok_val, "cbrfc") == 0 &&
		strlen (mean_areal_precip_global[m].hb5) == 7)
	    {

	      logMessage ("polygon write\n");

	       pchar[0] = mean_areal_precip_global[m].bchar[0];
	       strcat (xbuf, pchar);

	    }

	    else
	    {

	       if (l == 0 && numzones != 1)
	       {

		  strcat (xbuf, "L");
		  pchar[0] = mean_areal_precip_global[m].bchar[0];
		  strcat (xbuf, pchar);

	       }


	       else if (l == 0 && numzones == 1)
	       {

		  strcat (xbuf, "O");
		  pchar[0] = mean_areal_precip_global[m].bchar[0];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 1)
	       {

		  strcat (xbuf, "M");
		  pchar[0] = mean_areal_precip_global[m].bchar[1];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 2)
	       {

		  strcat (xbuf, "U");
		  pchar[0] = mean_areal_precip_global[m].bchar[2];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 3)
	       {

		  strcat (xbuf, "G");
		  pchar[0] = mean_areal_precip_global[m].bchar[3];
		  strcat (xbuf, pchar);

	       }

	    }

	    sprintf (mbuf, " %02d%02d%02d", 1900 + gm->tm_year,
		     gm->tm_mon + 1, gm->tm_mday);
	    strcat (xbuf, mbuf);

	    strcat (xbuf, " DH18/PPQPBZZ/DIH+6");

	    for (k = 0; k < 4; k++)
	    {

	       h = j * 4 + (3 - k);

	       /* fix for polygon basins */

	       if (strcmp (mpe_rfc_name_tok_val, "cbrfc") == 0 &&
		   strlen (mean_areal_precip_global[m].hb5) == 7)
		  temp = mean_areal_precip_global[m].lz[h];

	       else if (l == 0)
		  temp = mean_areal_precip_global[m].lz[h];

	       else if (l == 1)
		  temp = mean_areal_precip_global[m].mz[h];

	       else if (l == 2)
		  temp = mean_areal_precip_global[m].uz[h];

	       else if (l == 3)
		  temp = mean_areal_precip_global[m].gz[h];

	       if (mean_areal_precip_global[m].maps_done[h] != 1)
		  sprintf (mbuf, "      ");

	       else
		  sprintf (mbuf, "%5.2f ", temp);

	       if (k < 4)
		  strcat (xbuf, "/");

	       strcat (xbuf, mbuf);

	    }

	    strcat (xbuf, "\n");
	    fputs (xbuf, fp);

	 }

      }

      fclose (fp);

      fp = NULL;

      /* Copy the map file to directory called map_out. */
      /* strcpy (cmd, "rcp ");
         strcat (cmd, fbuf);
         strcat (cmd, " ");
         strcat (cmd, map_out);

        logMessage ("%s\n", cmd);
         fflush (stdout);
         ier = system (cmd);

         if (ier != 0)
         {

         strcpy (ebuf, "could not execute ");
         strcat (ebuf, cmd);
         berror (drawing_area, ebuf);


         } */

      for (m = 0; m < 5; m++)
      {

	 if (pdata[j].used[m] != 0)
	 {
	    pdata[j].used[m] = 4;
	 }

      }

      restore_bad_values (j, station, max_stations);

      /* write stddev file */
      gm = gmttime (&pdata[j].data_time);

      sprintf (ebuf, "%s%04d%02d%02d",
	       pcpn_dev_file,
	       gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

      fp = fopen (ebuf, "w");

      if (fp != NULL)
      {
	 sprintf (mbuf, "%f\n", pdata[j].stddev);
	 fputs (mbuf, fp);
	 fclose (fp);
	 fp = NULL;
      }

      sprintf (ebuf, "%s%04d%02d%02d",
	       temp_dev_file, gm->tm_year + 1900, gm->tm_mon + 1,
	       gm->tm_mday);

      fp = fopen (ebuf, "w");

      if (fp != NULL)
      {
	 sprintf (mbuf, "%f\n", tdata[j].stddev);
	 fputs (mbuf, fp);
	 fclose (fp);
	 fp = NULL;
      }

      archive_flag[j] = 1;

   }


   /* Process the freezing level data if they exist. */
   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */
      for (m = 0; m < 4; m++)
      {

	 if (zdata[j].used[m] == 3 || zdata[j].used[m] == 2)
	    break;

      }

      if (m == 4)
	 continue;

     logMessage ("create maz\n");

      /* re-create all pcpn and maps if necessary */

      /* next three lines are new */

      gm = gmttime (&zdata[j].data_time);

      for (l = 0; l < 4; l++)
      {

	 if (zdata[j].used[l] == 0 || zdata[j].used[l] == 3)
	    continue;

         if ( dqcBasetime == 12 )
         {
	    if (l < 2)
	       old_time = zdata[j].data_time - 86400;
	    else
	       old_time = zdata[j].data_time;
         }
         else if ( dqcBasetime == 18 )
         {
	    if (l < 3)
	       old_time = zdata[j].data_time - 86400;
	    else
	       old_time = zdata[j].data_time;
         }
         else if ( dqcBasetime == 00 )
         {
	    old_time = zdata[j].data_time;
         }
         else
         {
	    if (l < 1)
	       old_time = zdata[j].data_time - 86400;
	    else
	       old_time = zdata[j].data_time;
         }

	 gm = gmttime (&old_time);

	logMessage ("Freezing level %02d-%02d-%02d", gm->tm_mon + 1,
		 gm->tm_mday, gm->tm_year + 1900);

	 render_z ( j, l, 0, max_zstations, zstation, hrap_grid, zdata, 
                    pcp_in_use);

	 sprintf (dbuf, "%s%s_%04d%02d%02d", zgrid_file, ztimefile[dqcBaseIndex][l],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday );

	 write_qpf_grids (dbuf);

	 num = 100 + j * 4 + 3 - l;

	 make_rsel (num, num - 100);

      }

      old_time = zdata[j].data_time;
      gm = gmttime (&old_time);

      sprintf (fbuf, "%s%04d%02d%02d",
	       zpoint2_file, gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

      fp = fopen (fbuf, "w");

      if (fp == NULL)
      {
	 return;
      }

      for (m = 0; m < max_zstations; m++)
      {
         if ( dqcBasetime != 0 )
         {
	    old_time = zdata[j].data_time - 86400;
         }
         else
         {
	    old_time = zdata[j].data_time;
         }

	 gm = gmtime (&old_time);
	 sprintf (pbuf, "HZI%s%s", type, &zstation[m].parm[4]);
	 sprintf (buf, ".ER %s %02d%02d%02d DH%02d/%s/DIH+6/",
		  zstation[m].hb5, 1900 + gm->tm_year, gm->tm_mon + 1,
		  gm->tm_mday, dqcBasetime, pbuf);

	 for (k = 0; k < 4; k++)
	 {

	    if (zdata[j].used[k] == 0)
	    {

	       strcpy (mbuf, "     ");
	       if (k != 3)
		  strcat (mbuf, "/");


	    }


	    else if (zdata[j].stn[m].zlevel2[k].data < 0)
	    {

	       strcpy (mbuf, "   M ");
	       if (k != 3)
		  strcat (mbuf, "/");

	    }

	    else
	    {

	       sprintf (mbuf, "%4.1f", zdata[j].stn[m].zlevel2[k].data);

	       if (zdata[j].stn[m].zlevel2[k].qual == 8)
		  strcat (mbuf, "S");

	       if (zdata[j].stn[m].zlevel2[k].qual == 1)
		  strcat (mbuf, "F");

	       if (zdata[j].stn[m].zlevel2[k].qual == 2)
		  strcat (mbuf, "W");

	       if (zdata[j].stn[m].zlevel2[k].qual == 5)
		  strcat (mbuf, "E");

	       if (k != 3)
		  strcat (mbuf, "/");

	    }

	    strcat (buf, mbuf);

	 }

	 strcat (buf, "\n");

	 fputs (buf, fp);
      }

      fclose (fp);

      fp = NULL;

      /*strcpy (cmd, "rcp ");
         strcat (cmd, fbuf);
         strcat (cmd, " ");
         strcat (cmd, zdbase_out);

        logMessage ("%s\n", cmd);
         fflush (stdout);
         ier = system (cmd);

         if (ier != 0)
         {

         strcpy (ebuf, "could not execute ");
         strcat (ebuf, cmd);
         berror (drawing_area, ebuf);

         } */

      /* build map file */
      for (m = 0; mean_areal_precip_global[m].hb5[0] != 0; m++)
      {

	 for (k = 0; k < 4; k++)
	 {

	    num = j * 4 + 3 - k;

	    if (mean_areal_precip_global[m].zmaps_done[num] == 1)
	       goto foundz;

	 }

      }

    foundz:

      if (mean_areal_precip_global[m].hb5[0] == 0)
	 continue;

      gm = gmttime (&zdata[j].data_time);

      sprintf (fbuf, "%s%04d%02d%02d", rsel_file, gm->tm_year + 1900,
	       gm->tm_mon + 1, gm->tm_mday);

      fp = fopen (fbuf, "w");

      if (fp == NULL)
      {
	 return;
      }

      /* At least processing level a data is available but
         no guarantee that MAPs have been rendered */

      if ( dqcBasetime != 0 )
      {
         old_time = zdata[j].data_time - 86400;
      }
      else
      {
         old_time = zdata[j].data_time;
      }

      gm = gmtime (&old_time);

      /* loop through and write maps to file */
      for (m = 0; mean_areal_precip_global[m].hb5[0] != 0; m++)
      {
	 numzones = 0;

	 for (l = 0; l < 4; l++)
	 {

	    if (mean_areal_precip_global[m].zones[l] < 0)
	    {
	       continue;
	    }

	    numzones++;

	 }

	 for (l = 0; l < 4; l++)
	 {

	    if (mean_areal_precip_global[m].zones[l] < 0)
	       continue;

	    strcpy (xbuf, ".ER ");
	    strcat (xbuf, mean_areal_precip_global[m].hb5);

	    i = 0;

	    while (xbuf[i] != 0)
	    {
	       xbuf[i] = toupper (xbuf[i]);
	       ++i;
	    }


	    /* fix for polygon basins */
	    if (strcmp (mpe_rfc_name_tok_val, "cbrfc") == 0 &&
		strlen (mean_areal_precip_global[m].hb5) == 7)
	    {

	      logMessage ("polygon write\n");

	       pchar[0] = mean_areal_precip_global[m].bchar[0];
	       strcat (xbuf, pchar);

	    }

	    else
	    {


	       if (l == 0 && numzones != 1)
	       {

		  strcat (xbuf, "L");
		  pchar[0] = mean_areal_precip_global[m].bchar[0];
		  strcat (xbuf, pchar);

	       }


	       else if (l == 0 && numzones == 1)
	       {

		  strcat (xbuf, "O");
		  pchar[0] = mean_areal_precip_global[m].bchar[0];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 1)
	       {

		  strcat (xbuf, "M");
		  pchar[0] = mean_areal_precip_global[m].bchar[1];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 2)
	       {

		  strcat (xbuf, "U");
		  pchar[0] = mean_areal_precip_global[m].bchar[2];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 3)
	       {

		  strcat (xbuf, "G");
		  pchar[0] = mean_areal_precip_global[m].bchar[3];
		  strcat (xbuf, pchar);

	       }

	    }

	    sprintf (mbuf, " %02d%02d%02d", 1900 + gm->tm_year,
		     gm->tm_mon + 1, gm->tm_mday);
	    strcat (xbuf, mbuf);

	    sprintf (mbuf, " DH%02d/HZIPBZZ/DIH+6", dqcBasetime);
	    strcat (xbuf, mbuf ); 

	    for (k = 0; k < 4; k++)
	    {

	       h = j * 4 + (3 - k);


	       if (strcmp (mpe_rfc_name_tok_val, "cbrfc") == 0 &&
		   strlen (mean_areal_precip_global[m].hb5) == 7)
		  temp = mean_areal_precip_global[m].zlz[h];


	       else if (l == 0)
		  temp = mean_areal_precip_global[m].zlz[h];

	       else if (l == 1)
		  temp = mean_areal_precip_global[m].zmz[h];

	       else if (l == 2)
		  temp = mean_areal_precip_global[m].zuz[h];

	       else if (l == 3)
		  temp = mean_areal_precip_global[m].zgz[h];

	       if (mean_areal_precip_global[m].zmaps_done[h] != 1)
		  sprintf (mbuf, "     ");

	       else
		  sprintf (mbuf, "%4.1f ", temp);

	       if (k < 4)
		  strcat (xbuf, "/");

	       strcat (xbuf, mbuf);

	    }

	    strcat (xbuf, "\n");
	    fputs (xbuf, fp);

	 }

      }

      fclose (fp);
      fp = NULL;

      /* Copy the MAPZ to the zmap_out directory. */
      /*strcpy (cmd, "rcp ");
         strcat (cmd, fbuf);
         strcat (cmd, " ");
         strcat (cmd, zmap_out);

        logMessage ("%s\n", cmd);
         fflush (stdout);
         ier = system (cmd);

         if (ier != 0)
         {

         strcpy (ebuf, "could not execute ");
         strcat (ebuf, cmd);
         berror (drawing_area, ebuf);


         }
       */
      for (m = 0; m < 4; m++)
      {

	 if (zdata[j].used[m] != 0)
	    zdata[j].used[m] = 4;

      }

      archive_flag[j] = 1;

   }

   /* Process the temperature data. */
   for (j = 0; j < MAX_GAGEQC_DAYS; j++)
   {

      /* if processing level a data is not available do not write out
         processing level b data.  In addition, do not write MAPs
         if no processing level a data */

      for (m = 0; m < 4; m++)
      {

	 if (tdata[j].used[m] == 3 || tdata[j].used[m] == 2)
	    break;

      }

      if (m == 4)
	 continue;

      /* re-create all pcpn and maps if necessary */

      /* next three lines are new */

      estimate_daily_tstations (j, tstation, max_tstations);
      quality_control_tstations (j, tstation, max_tstations);

      gm = gmttime (&tdata[j].data_time);

      sprintf (fbuf, "%s%04d%02d%02d",
	       temp_bad_file, gm->tm_year + 1900, gm->tm_mon + 1,
	       gm->tm_mday);

      isom = gm->tm_mon;

      write_bad_tvalues (fbuf, j);

      for (l = 5; l >= 0; l--)
      {

	 if (tdata[j].used[l] == 0 || tdata[j].used[l] == 3)
	    continue;

         if ( dqcBasetime == 12 )
         {
	    if (l < 2)
	       old_time = tdata[j].data_time - 86400;
	    else
	       old_time = tdata[j].data_time;
         }
         else if ( dqcBasetime == 18 )
         {
	    if (l < 3)
	       old_time = tdata[j].data_time - 86400;
	    else
	       old_time = tdata[j].data_time;
         }
         else if ( dqcBasetime == 00 )
         {
	    old_time = tdata[j].data_time;
         }
         else
         {
	    if (l < 1)
	       old_time = tdata[j].data_time - 86400;
	    else
	       old_time = tdata[j].data_time;
         }

	 gm = gmttime (&old_time);

	logMessage ("Temperature %02d-%02d-%02d", gm->tm_mon + 1,
		 gm->tm_mday, gm->tm_year + 1900);


	 if (l == 5)
         {
	    render_t (j, l, 2, max_tstations, tstation, hrap_grid, tdata,
                      pcp_in_use);
         }
	 else if (l == 4)
         {
	    render_t (j, l, 1, max_tstations, tstation, hrap_grid, tdata,
                      pcp_in_use);
         }
	 else
         {
	    render_t6 (j, l, 0, max_tstations, tstation, hrap_grid, tdata,
                       pcp_in_use);
         }

	 sprintf (dbuf, "%s%s_%04d%02d%02d", tgrid_file,ttimefile[dqcBaseIndex][l],
		  gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

	 write_qpf_grids (dbuf);

	 num = 150 + j * 4 + 3 - l;

	 if (l < 4)
         {
	    make_mat (num, num - 150);
         }
      }


      estimate_missing_tstations (j, tstation, max_tstations, tdata);

      old_time = tdata[j].data_time;
      gm = gmttime (&old_time);

      sprintf (fbuf, "%s%04d%02d%02d",
	       tpoint2_file, gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday);

      fp = fopen (fbuf, "w");

      if (fp == NULL)
      {
	 return;
      }

      for (m = 0; m < max_tstations; m++)
      {

         if ( dqcBasetime != 0 )
         { 
	    old_time = tdata[j].data_time - 86400;
         }
         else
         {
	    old_time = tdata[j].data_time;
         }
	 gm = gmtime (&old_time);

	 sprintf (pbuf, "TAI%s%cZZ", type, tstation[m].parm[4]);
	 sprintf (buf, ".ER %s %02d%02d%02d DH%02d/%s/DIH+6/",
		  tstation[m].hb5, 1900 + gm->tm_year, gm->tm_mon + 1,
		  gm->tm_mday, dqcBasetime, pbuf);

	 for (k = 0; k < 4; k++)
	 {

	    if (tdata[j].used[k] == 0)
	    {

	       strcpy (mbuf, "     ");
	       if (k != 3)
		  strcat (mbuf, "/");


	    }
	    else if (tdata[j].stn[m].tlevel2[k].data == -99)
	    {

	       strcpy (mbuf, "   M ");
	       if (k != 3)
		  strcat (mbuf, "/");

	    }

	    else
	    {

	       sprintf (mbuf, "%3d", tdata[j].stn[m].tlevel2[k].data);

	       if (tdata[j].stn[m].tlevel2[k].qual == 8)
		  strcat (mbuf, "V");

	       if (tdata[j].stn[m].tlevel2[k].qual == 1)
		  strcat (mbuf, "F");

	       if (tdata[j].stn[m].tlevel2[k].qual == 3)
		  strcat (mbuf, "Q");

	       if (tdata[j].stn[m].tlevel2[k].qual == 0)
		  strcat (mbuf, "S");

	       if (tdata[j].stn[m].tlevel2[k].qual == 5)
		  strcat (mbuf, "E");

	       if (tdata[j].stn[m].tlevel2[k].qual == 6)
		  strcat (mbuf, "L");

	       if (k != 3)
		  strcat (mbuf, "/");

	    }

	    strcat (buf, mbuf);

	 }

	 strcat (buf, "\n");
	 fputs (buf, fp);

	 old_time = tdata[j].data_time;
	 gm = gmtime (&old_time);

	 sprintf (pbuf, "TAI%s%cXZ", type, tstation[m].parm[4]);
	 sprintf (buf, ".AR %s %02d%02d%02d DH%02d/%s ",
		  tstation[m].hb5, 1900 + gm->tm_year, gm->tm_mon + 1,
		  gm->tm_mday, dqcBasetime, pbuf);

	 if (tdata[j].used[4] == 0)
	    strcpy (mbuf, "     ");

	 else if (tdata[j].stn[m].tlevel2[4].data == -99)
	    strcpy (mbuf, "   M ");

	 else
	 {

	    sprintf (mbuf, "%3d", tdata[j].stn[m].tlevel2[4].data);

	    if (tdata[j].stn[m].tlevel2[4].qual == 8)
	       strcat (mbuf, "V");

	    if (tdata[j].stn[m].tlevel2[4].qual == 1)
	       strcat (mbuf, "F");

	    if (tdata[j].stn[m].tlevel2[4].qual == 3)
	       strcat (mbuf, "Q");

	    if (tdata[j].stn[m].tlevel2[4].qual == 0)
	       strcat (mbuf, "S");

	    if (tdata[j].stn[m].tlevel2[4].qual == 5)
	       strcat (mbuf, "E");

	    if (tdata[j].stn[m].tlevel2[4].qual == 6)
	       strcat (mbuf, "L");

	 }

	 strcat (buf, mbuf);
	 strcat (buf, "\n");
	 fputs (buf, fp);

	 old_time = tdata[j].data_time;
	 gm = gmtime (&old_time);

	 sprintf (pbuf, "TAI%s%cNZ", type, tstation[m].parm[4]);
	 sprintf (buf, ".AR %s %02d%02d%02d DH%02d/%s ",
		  tstation[m].hb5, 1900 + gm->tm_year, gm->tm_mon + 1,
		  gm->tm_mday, dqcBasetime, pbuf);

	 if (tdata[j].used[5] == 0)
	    strcpy (mbuf, "     ");

	 else if (tdata[j].stn[m].tlevel2[5].data == -99)
	    strcpy (mbuf, "   M ");

	 else
	 {

	    sprintf (mbuf, "%3d", tdata[j].stn[m].tlevel2[5].data);

	    if (tdata[j].stn[m].tlevel2[5].qual == 8)
	       strcat (mbuf, "V");

	    if (tdata[j].stn[m].tlevel2[5].qual == 1)
	       strcat (mbuf, "F");

	    if (tdata[j].stn[m].tlevel2[5].qual == 3)
	       strcat (mbuf, "Q");

	    if (tdata[j].stn[m].tlevel2[5].qual == 0)
	       strcat (mbuf, "S");

	    if (tdata[j].stn[m].tlevel2[5].qual == 5)
	       strcat (mbuf, "E");

	    if (tdata[j].stn[m].tlevel2[5].qual == 6)
	       strcat (mbuf, "L");

	 }

	 strcat (buf, mbuf);
	 strcat (buf, "\n");
	 fputs (buf, fp);



      }

      fclose (fp);
      fp = NULL;

      /*strcpy (cmd, "rcp ");
         strcat (cmd, fbuf);
         strcat (cmd, " ");
         strcat (cmd, tdbase_out);

        logMessage ("%s\n", cmd);
         fflush (stdout);
         ier = system (cmd);

         if (ier != 0)
         {

         strcpy (ebuf, "could not execute ");
         strcat (ebuf, cmd);
         berror (drawing_area, ebuf);

         } */

      /* build map file */
      for (m = 0; mean_areal_precip_global[m].hb5[0] != 0; m++)
      {

	 for (k = 0; k < 4; k++)
	 {

	    num = j * 4 + 3 - k;

	    if (mean_areal_precip_global[m].tmaps_done[num] == 1)
	       goto foundt;

	 }

      }

    foundt:

      if (mean_areal_precip_global[m].hb5[0] == 0)
	 continue;

      gm = gmttime (&tdata[j].data_time);

      sprintf (fbuf, "%s%04d%02d%02d", mat_file, gm->tm_year + 1900,
	       gm->tm_mon + 1, gm->tm_mday);

      fp = fopen (fbuf, "w");

      if (fp == NULL)
      {
	 return;
      }

      /* at least processing level a data is available but
         no guarantee that MAPs have been rendered */

      if ( dqcBasetime != 0 )
      {
         old_time = tdata[j].data_time - 86400;
      }
      else
      {
         old_time = tdata[j].data_time;
      }

      gm = gmtime (&old_time);

      /* loop through and write maps to file */

      for (m = 0; mean_areal_precip_global[m].hb5[0] != 0; m++)
      {

	 numzones = 0;

	 for (l = 0; l < 4; l++)
	 {

	    if (mean_areal_precip_global[m].zones[l] < 0)
	       continue;

	    numzones++;

	 }

	 for (l = 0; l < 4; l++)
	 {

	    if (mean_areal_precip_global[m].zones[l] < 0)
	       continue;

	    strcpy (xbuf, ".ER ");
	    strcat (xbuf, mean_areal_precip_global[m].hb5);

	    i = 0;

	    while (xbuf[i] != 0)
	    {
	       xbuf[i] = toupper (xbuf[i]);
	       ++i;
	    }


	    /* fix for polygon basins */

	    if (strcmp (mpe_rfc_name_tok_val, "cbrfc") == 0
		&& strlen (mean_areal_precip_global[m].hb5) == 7)
	    {

	      logMessage ("polygon write\n");

	       pchar[0] = mean_areal_precip_global[m].bchar[0];
	       strcat (xbuf, pchar);

	    }

	    else
	    {


	       if (l == 0 && numzones != 1)
	       {

		  strcat (xbuf, "L");
		  pchar[0] = mean_areal_precip_global[m].bchar[0];
		  strcat (xbuf, pchar);

	       }


	       else if (l == 0 && numzones == 1)
	       {

		  strcat (xbuf, "O");
		  pchar[0] = mean_areal_precip_global[m].bchar[0];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 1)
	       {

		  strcat (xbuf, "M");
		  pchar[0] = mean_areal_precip_global[m].bchar[1];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 2)
	       {

		  strcat (xbuf, "U");
		  pchar[0] = mean_areal_precip_global[m].bchar[2];
		  strcat (xbuf, pchar);

	       }

	       else if (l == 3)
	       {

		  strcat (xbuf, "G");
		  pchar[0] = mean_areal_precip_global[m].bchar[3];
		  strcat (xbuf, pchar);

	       }

	    }

	    sprintf (mbuf, " %02d%02d%02d", 1900 + gm->tm_year,
		     gm->tm_mon + 1, gm->tm_mday);
	    strcat (xbuf, mbuf);

            sprintf ( mbuf, " DH%02d/TAIPBZZ/DIH+6", dqcBasetime);
	    strcat (xbuf, mbuf);

	    for (k = 0; k < 4; k++)
	    {

	       h = j * 4 + (3 - k);


	       if (strcmp (mpe_rfc_name_tok_val, "cbrfc") == 0
		   && strlen (mean_areal_precip_global[m].hb5) == 7)
		  temp = mean_areal_precip_global[m].tlz[h];


	       else if (l == 0)
		  temp = mean_areal_precip_global[m].tlz[h];

	       else if (l == 1)
		  temp = mean_areal_precip_global[m].tmz[h];

	       else if (l == 2)
		  temp = mean_areal_precip_global[m].tuz[h];

	       else if (l == 3)
		  temp = mean_areal_precip_global[m].tgz[h];

	       if (mean_areal_precip_global[m].tmaps_done[h] != 1)
		  sprintf (mbuf, "     ");

	       else
		  sprintf (mbuf, "%4.1f ", temp);

	       if (k < 4)
		  strcat (xbuf, "/");

	       strcat (xbuf, mbuf);

	    }

	    strcat (xbuf, "\n");
	    fputs (xbuf, fp);

	 }

      }

      fclose (fp);
      fp = NULL;

      /*strcpy (cmd, "rcp ");
         strcat (cmd, fbuf);
         strcat (cmd, " ");
         strcat (cmd, tmap_out);

        logMessage ("%s\n", cmd);
         fflush (stdout);
         ier = system (cmd);

         if (ier != 0)
         {

         strcpy (ebuf, "could not execute ");
         strcat (ebuf, cmd);
         berror (drawing_area, ebuf);


         } */

      for (m = 0; m < 6; m++)
      {

	 if (tdata[j].used[m] != 0)
	    tdata[j].used[m] = 4;

      }
      restore_bad_tvalues (j, tstation, max_stations);

      archive_flag[j] = 1;

   }


   /*for (k = 0; k < MAX_GAGEQC_DAYS; k++)
      {

      if (archive_flag[k] == 1)
      write_archived_obs_data (scratch_file, "obs", rfc,
      pdata[k].data_time, obs_archive_file);

      } */

   if (qpf_on == 1 || flf_on == 1 || maxmin_on == 1)
   {

      for (k = 1; k < 7; k++)
      {
	 XtSetSensitive (diswidget[k], True);
      }

      if (qpf_on == 1)
      {

	 if (pcpn_time_step == 0)
	    time_pos = pcp_flag;

	 else

	    time_pos = 40 + pcpn_day;

      }

      else if (flf_on == 1)
	 time_pos = 100 + pcp_flag;

      else if (maxmin_on == 1)
      {

	 if (pcpn_time_step == 1)
	    time_pos = 190 + pcpn_day;

	 else if (pcpn_time_step == 2)
	    time_pos = 200 + pcpn_day;

	 else
	    time_pos = 150 + pcp_flag;

      }


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

   Disagg6hr();
   XtDestroyWidget (dialog);

   if (quit_flag == 1)
   {
      /* Free DailyQC resources.  */
      quit ();
   }
   else if ( qc_gui_function != NULL )
   {
      new_area_flag = 0;
      set_dqc_area_and_date ( );

      if ( qpf_on == 1 )
      {
         change_pcpn_edit_mode ( w, NULL, NULL );
      }

      if ( maxmin_on == 1 )
      {
         change_maxmin_edit_mode ( w, NULL, NULL );
      }

      if ( flf_on == 1 )
      {
         change_z_edit_mode ( w, NULL, NULL );
      }

      free_dqc_data ( ); 

      mSetCursor ( M_WATCH );
      XFlush ( pMapDisplay );
      load_gage_data ( area_val_local,
                       is_area_master(),
                       dqc_run_date.dqc_data_year,
                       dqc_run_date.dqc_data_month,
                       dqc_run_date.dqc_data_day,
                       dqc_run_date.dqc_num_days);
      qc_gui_function ( w, NULL, NULL );
      mSetCursor ( M_NORMAL );
   }
   else
   {
      isom = save_isom;
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/save_level2_data.c,v $";
 static char rcs_id2[] = "$Id: save_level2_data.c,v 1.3 2007/10/18 16:09:08 lawrence Exp $";}
/*  ===================================================  */

}
