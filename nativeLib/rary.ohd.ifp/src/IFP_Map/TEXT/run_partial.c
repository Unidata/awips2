#include <string.h>
#include <memory.h>
#include <sys/shm.h>
#include <sys/ipc.h>
#include <sys/types.h>
/*                   date: 09/03/02  A.V fixed bug r20-34               */
/*                                    Mouse cursor changed to an X      */
/*                                    it is difficult to make a query.  */
/*                                    Cursor stays with an X even after */
/*                                    quitting IFP.                     */

#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"            /* Added 10/02/92       */
#include "struct_defs.h"        /* Added 10/02/92       */
#include "run_partial.h"
#include "ifp_help.h"
#include "help.h"
#include "c_call_f/upinio.h"
#include "c_call_f/upchkd_wrapper.h"
#include "c_call_f/retrieve_hcl_techs_args.h"
#define   FGROUP   1


/* AV end linux port 7/16/01 */
static void set_time_zone_code_atom();
extern void post_nts_mods_available_atom(Widget, int);
extern int get_nts_mods_available_atom(Widget);
extern int handle_command_line_args(int, char **, the_widget_struct *);
int	fgroup_created_once = 0;
int	first_time = 1;
int     ssUpstreamFlag;

typedef struct
	{
	int     time_change;
	int     time;
	char    time_zone[5];
	}       date_data;


int run_partial_main(int argc, const char ** argv)
{
	XEvent          event;
	Display         *display;
	Window          root;



	int             type, format, nitems, left;
	int             i;

	int             IFP_is_already_running;

	int             mods_saved_or_deleted=0;
	int             numModsToDeleteFromFile;
	int             inSegmentAfterContinue;
	Arg             wargs[8];
	XmString        xmstr_showMods;


	XSetWindowAttributes    attrs;

	int             x, y;
	unsigned int    width, height;
	unsigned int    border_width, depth;
	Status          status;
	Window          root_window;
	Cursor          arrow_cursor;
        Cursor          watch_cursor;
	long            offset = 0;

	Position        tree_shell_x, tree_shell_y, working_x, working_y;
	Dimension       tree_shell_width, tree_shell_height;
	Dimension       working_width, working_height;
	int             n;

	int             number_of_Segments;
	XmString        *xmstring_Segments;

	char            current_segment_no_blanks[9];

	int             segment_callbacks_added = FALSE;
	char            system_command[80];
        int             len, len2;  /* used get_apps_defaults */
        int             nts_mods_available;
        int             activate_rerun;
        int             modsViewer;
/*=============================================================================================*/




	int             range_updated = 0;
	int             chkstream = 0;
        int             fgroup_flg = 0;
        int             range_upstream = 0;

        char		tmp_curr_seg[9];

        the_widget_struct       *some_widgets;
	date                    *newDate;
	node                    *the_node;
	seg_status              *flow_status;

        int             *doneMakingMods;
        int             *number_of_TulPlots;
	int             *rating_curve_is_available;

        int             *FGmods_save;

        int             *rangemods;
        int             *numberOfModsToWrite;
	int             *tschngMod;
	int             *mod_files_updated;
	int             *fgmod_files_updated;
	int             *modsHaveBeenDeleted;
        char            *last_segment_name;
        char            *segment_name;
	char            *first_segment_name;
	char            *previousSegment;
	char            *first_blank;
	char            *string;
	char            *TimeZoneCode;
        char            *first_range_mods_str;
        char            *currentSegment;
	char            *goto_downstream_segment;
	char            *goto_upstream_segment;



/*=============================================================================================*/
/* Check to see if program called correctly
 * if argc = 1 (program name) - OK
 * if argc = 6 then there are enough command line arguments - OK
 * dp 2 May 1997
 */
if (argc == 1 || argc == 6)
;
else
{
   printf("ERROR:  Wrong number of command line arguments.  You have entered:\n");
   for (i=1; i<argc; i++)
      printf("argv[%d] = %s\n", i, argv[i]);
   printf("You need to enter 5 arguments: STARTRUN (mmddyyyyhhtz), LSTCMPDY, ENDRUN, ");
   printf("FGROUP, and files to use (COPY or PREV)\n");
   exit(1);
}

 leave_NWSRFS = FALSE;
 inLastSegment = FALSE;
 selectedSegment_is_upstream = FALSE;
 sub_group_num = -1;

 previousSegment = (char *) malloc(9);
 string = (char *) malloc(9);
 first_segment_name = (char *) malloc(9);
 first_range_mods_str = (char *) malloc(9);
 memset(first_range_mods_str,'\0',9);
 memset(first_segment_name,'\0',9);


 /* ------------------------------------------------ */
 /* ------------------------------------------------ */

 global_toplevel = XtInitialize(argv[0], "IFP_map", NULL, 0, &argc, argv);

 display = XtDisplay(global_toplevel);
 root = DefaultRootWindow(display);
 /* set cursor to Arrow - bug fixed*/
 arrow_cursor = XCreateFontCursor(display, XC_top_left_arrow);
 XDefineCursor(display, root, arrow_cursor);
/*----------------------------------------------------------------------*/
/* Assign directory pathnames for NWSRFS OFS file assignments           */
/*----------------------------------------------------------------------*/
 {
  char            global_dirs[] = "syst oper";
  int             lenstr = 9;
  int             dir_chk_status;



  (void) UPINIO();
  (void) UPCHKD_WRAPPER(global_dirs,&lenstr,&dir_chk_status);

  if(dir_chk_status != 0) exit(0);
  }

 intern_the_atoms(global_toplevel);

 IFPA_status_change = XInternAtom(display, "Segment status", False);
 IFPA_status_change_type = XInternAtom(display, "Segment status type", False);


/*      Check to see whether or not NWSRFS is already running; if it is, do not allow another copy --   */
/*              startup a message dialog application that explains this to the user and exit from       */
/*              this process... otherwise, set a window property for the root window announcing that    */
/*              a copy of run_NWSRFS is running on this display...                                      */

 if(XGetWindowProperty
	(
	display,
	(Window) root,
	(Atom) IFPA_IFP_NWSRFS_is_running,
	(long) offset,
	(long) sizeof(int),
	(Bool) FALSE,
	(Atom) IFPA_IFP_NWSRFS_is_running_type,
	(Atom *) &type,
	(int *)&format,
	(unsigned long*) &nitems,
	(unsigned long*)&left,
	(unsigned char**)&IFP_is_already_running
	) == Success && type == IFPA_IFP_NWSRFS_is_running_type)
	{
	  /* call routine to get the path name for bin files */
	  memset(system_command, '\0', 80);
          len = strlen("ifp_bin_dir");
	  get_apps_defaults("ifp_bin_dir", &len, system_command, &len2);

	  strcat(system_command, "/NWSRFS_no_startup");

	  system(system_command);
	  exit(0);
	}

 IFP_is_already_running = TRUE;

 XChangeProperty
	 (
	 (Display *)display,
	 (Window) root,
	 (Atom) IFPA_IFP_NWSRFS_is_running,
	 (Atom) IFPA_IFP_NWSRFS_is_running_type,
	 8,
	 (int)PropModeReplace,
	 (unsigned char *)&IFP_is_already_running,
	 sizeof(int)
	 );


 if(XGetWindowProperty
	 (
	 (Display *)display,
	 (Window) root,
	 (Atom) IFPA_time_zone_code,
	 (long) offset,
	 (long) 9,
	 (Bool) FALSE,
	 (Atom) IFPA_time_zone_code_type,
	 (Atom *) &type,
	 (int *)&format,
	 (unsigned long *)&nitems,
	 (unsigned long *) &left,
	 (unsigned char **) &TimeZoneCode
	 ) == Success && type == IFPA_time_zone_code_type)
		/* printf("IFPA_time_zone_code atom found...\n") */ ;
 else    {/* The Time Zone Code Atom was not found, so set it from a file...             */

	 printf("The IFPA_time_zone_code atom was not found...\n");
	 set_time_zone_code_atom(display);
	 }

/* Post the nts_mods_available_atom - initially set to FALSE
 * dp - 23 Feb. 1996
 */
  nts_mods_available = FALSE;
  post_nts_mods_available_atom(global_toplevel, nts_mods_available);

/* Get the dimensions of the physical screen that holds the Root Window of the display server...        */
/*      then, set these dimensions as global constants.                                                 */

 status = XGetGeometry(display, root, &root_window, &x, &y, &width, &height, &border_width, &depth);

 Screen_Width  = width;
 Screen_Height = height;
 Screen_Depth  = depth;
 Screen_Resolution = Screen_Width/SCREEN_WIDTH_CM;

 delete_string = (char *) malloc(801);
 selected_string = (char *) malloc(801);
 memset(delete_string, '\0', 801);
 memset(selected_string, '\0', 801);


/*--------------------------------------------------------------*/
/*      Create a watch cursor to show that we're busy doing     */
/*      things...                                               */
/*--------------------------------------------------------------*/

 watch_cursor = XCreateFontCursor(display, XC_watch);
 attrs.cursor = watch_cursor;
 XChangeWindowAttributes(display, root, CWCursor, &attrs);


 some_widgets = (the_widget_struct *) malloc(sizeof(the_widget_struct));

 some_widgets->toplevel = global_toplevel;
 some_widgets->sw_for_delete_list = NULL;
 some_widgets->delete_list = NULL;
 some_widgets->delete_shell = NULL;
 some_widgets->tree_shell = NULL;
 some_widgets->sw_for_tree_widgets = NULL;

 for(i = 0; i < MAX_NUM_SUBGROUPS; i++) some_widgets->head[i] = NULL;

 some_widgets->viewDates = NULL;
 some_widgets->run_info_shell = NULL;
 some_widgets->keepSubset_widget = NULL;
 some_widgets->continue_widget = NULL;
 some_widgets->run_info_bb = NULL;
 some_widgets->previous_segment = NULL;
 some_widgets->currentSegment_widget = NULL;
 some_widgets->GoToNextForecastGroup = FALSE;

 memset(some_widgets->current_segment, '\0', 9);

 if(check_for_dates() != YES)
	{
	/* call routine to get the path name for script files */
	memset(system_command, '\0', 80);
        len = strlen("ifp_bin_dir");
	get_apps_defaults("ifp_bin_dir", &len, system_command, &len2);

	strcat(system_command, "/post_default_run_dates");

	system(system_command);
	}

/* -------------------------------------------------------------------- */
/*      Make the Interface...                                           */
/* -------------------------------------------------------------------- */
 create_run_interface(some_widgets);
 make_ForecastGroup_selectionBox(global_toplevel, some_widgets);

 XtRealizeWidget(global_toplevel);

 if(argc != 6)  /* normal run - no command line arguments */
 {
    XtPopup(some_widgets->FcstGroup_selectionBoxShell, XtGrabNone);

   /* -------------------------------------------------------------------- */
   /*      We draw to the DrawingArea widget & accompanying Pixmap at      */
   /*      this point to show it when we first startup; if the Pixmap      */
   /*      does not exist, it's created; in any case the DrawingArea       */
   /*      widget must be realized before we can draw to it !!!            */
   /*                                                                      */
   /*      'rad_data' is global...                                         */
   /*                                                                      */
   /* -------------------------------------------------------------------- */

    revert_to_default_view(some_widgets->main_canvas, some_widgets, NULL);
 }
 else /* (argc == 6) started with command line arguments */
    /* call to post dates and forecast group, do all things that would
     * have been done by thru the ForecastGroup_selectionBox
     */
    if(handle_command_line_args(argc, argv, some_widgets) != 0)
    {
       XDeleteProperty(display, root, IFPA_IFP_NWSRFS_is_running);
       XSync(display, 0);
       exit(1);
    }

/*--------------------------------------------------------------*/
/*      Change the watch back to the default cursor to show     */
/*      that we've finished doing things...                     */
/*--------------------------------------------------------------*/

 attrs.cursor = arrow_cursor;
 XChangeWindowAttributes(display, root, CWCursor, &attrs);
 XFreeCursor(display, arrow_cursor);

/*
  XGrabButton(display, AnyButton, AnyModifier,
	      XtWindow(some_widgets->main_canvas), TRUE,
	      ButtonPressMask | ButtonMotionMask |
	      ButtonReleaseMask,
	      GrabModeAsync, GrabModeAsync,
	      XtWindow(some_widgets->main_canvas),
	      None);
*/

 XtAddCallback(some_widgets->Scale_TextField, XmNmodifyVerifyCallback, check_scale_value, some_widgets);

 XSelectInput(XtDisplay(global_toplevel), root, PropertyChangeMask);


/***************************    MAIN EVENT LOOP   **************************/
 while(TRUE)            /*      We're checking for PropertyNotify events...             */
	{
	XtNextEvent(&event);

	/* ---------------------------------------------------------------------------- */
	/* 'IsContextHelpWindowDown' is a global flag for Context Sensitive Help:       */
	/*      it's used to test if we've passed through 'destroy_help_message'        */
	/* for the destruction of the help_shell... this kludge is needed to avoid an   */
	/* XProtocol error for a bad windowID when a ButtonRelease event is spawned by  */
	/* releasing the mouse button outside of the widget we just got help on;        */
	/* it's a nasty thing that happens when you want context-sensitive help on      */
	/* Pulldown & Option menus...                                                   */
	/* ---------------------------------------------------------------------------- */

/*      Check for property change events on the ROOT window before dispatching
		the event through the Intrinsics                                */

	switch  (event.type)
	     {

		case PropertyNotify:

/* ------------------------------------------------------------------------------------------------------       */

/* <<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< begin NWSRFS / continue >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>> */


	   if(event.xproperty.atom == IFPA_begin_NWSRFS ||
	      event.xproperty.atom == IFPA_continue_to_next_operation)
		  {

		  /* Get position and size of tree_shell widget for use in positioning the working popup.       */
		  n = 0;
		  XtSetArg(wargs[n], XmNx, &tree_shell_x); n++;
		  XtSetArg(wargs[n], XmNy, &tree_shell_y); n++;
		  XtSetArg(wargs[n], XmNwidth, &tree_shell_width); n++;
		  XtSetArg(wargs[n], XmNheight, &tree_shell_height); n++;
		  XtGetValues(some_widgets->tree_shell, wargs, n);

		  /* Get width of working popup to try to center the popup on the tree_shell horizontally.      */
		  n = 0;
		  XtSetArg(wargs[n], XmNwidth, &working_width); n++;
		  XtSetArg(wargs[n], XmNheight, &working_height); n++;
		  XtGetValues(some_widgets->NWSRFS_working_shell, wargs, n);

		  working_x = tree_shell_x + 0.5 * tree_shell_width - 0.5 * working_width;

		  if(working_x < 0) working_x = 0;
		  if(working_x + working_width > Screen_Width) working_x = Screen_Width - working_width;

		  working_y = tree_shell_y + tree_shell_height;
		  if(working_y < 0) working_y = 0;
		  if(working_y + working_height > Screen_Height) working_y = Screen_Height - working_height;

		  /* Set location for the popup shell.                                  */
		  n = 0;
		  XtSetArg(wargs[n], XmNx, working_x); n++;
		  XtSetArg(wargs[n], XmNy, working_y); n++;
		  XtSetValues(some_widgets->NWSRFS_working_shell, wargs, n);

		  /* Remove window properties related to saving/deleting mods.          */
		  XDeleteProperty(display, root, IFPA_number_of_mods_to_write);
		  XDeleteProperty(display, root, IFPA_current_mod_saved);
		  XDeleteProperty(display, root, IFPA_mods_have_been_deleted);
		  XDeleteProperty(display, root, IFPA_tschng_mod);
		  XDeleteProperty(display, root, IFPA_FGmods_save);
		  XDeleteProperty(display, root, IFPA_fgmod_files_updated);
		  XDeleteProperty(display, root, IFPA_mod_files_updated);
 		  XDeleteProperty(display, root, IFPA_rangemods_files_updated);

		  numModsToDeleteFromFile = 0;

		  XChangeProperty
			  (
			  display,
			  root,
			  IFPA_num_mods_to_delete_fromFile,
			  IFPA_num_mods_to_delete_fromFile_type,
			  8,
			  PropModeReplace,
			  (unsigned char *)&numModsToDeleteFromFile,
			  sizeof(int)
			  );

		  /* Set part of main menubar insensitive until begin NWSRFS event_loop...                      */
		  XtSetSensitive(run_cascade[0], FALSE);
		  XtSetSensitive(run_cascade[1], FALSE);
		  XtSetSensitive(run_cascade[3], FALSE);

		  for(i = 0; i <= sub_group_num; i++)
			if(some_widgets->head[i] != NULL)
				XtSetSensitive(some_widgets->head[i]->parent_widget, FALSE);

		  if(event.xproperty.atom == IFPA_begin_NWSRFS)
			{
			inSegmentAfterContinue = FALSE;
			XtSetSensitive(some_widgets->rerun_widget, FALSE);
			}

		  if(event.xproperty.atom == IFPA_continue_to_next_operation)
			{
                        inSegmentAfterContinue = TRUE;
			XtSetSensitive(some_widgets->rerun_widget, TRUE);
			}

		  if(segment_callbacks_added)
		    {
		     segment_callbacks_added = FALSE;
		     for(i = 0; i <= sub_group_num; i++)
			   if(some_widgets->head[i] != NULL)
			     {     /* remove all callbacks, then add popup info back */
			      remove_segment_callbacks(some_widgets->head[i]);
			      add_segment_callbacks(some_widgets->head[i],
						    popup_segment_info,
						    some_widgets->head[i]->popup_shell);
			     }
		    }

		  XSync(display, 0);

		  if(!segment_callbacks_added)
			{
			segment_callbacks_added = TRUE;
			for(i = 0; i <= sub_group_num; i++)
				if(some_widgets->head[i] != NULL)
					add_segment_callbacks(some_widgets->head[i], handle_segment_selected, some_widgets);
			}

		  XmToggleButtonSetState(some_widgets->showOtherMods_widget, FALSE, FALSE);
		  XmToggleButtonSetState(some_widgets->showModsViewer_widget, FALSE, FALSE);

		  /* don't manage the NWSRFS_working_shell if saving gif files - dp - 07/25/95 */
		  if(!save_gif || (event.xproperty.atom == IFPA_begin_NWSRFS))
		     XtManageChild(some_widgets->NWSRFS_working_shell);
		  some_widgets->stopped_timeout_id = XtAddTimeOut(NWSRFS_TIMEOUT_TIME,
								  popup_NWSRFS_stopped_working, some_widgets);

		  XtSetSensitive(some_widgets->showOperationsTable_widget, FALSE);
		  XmToggleButtonSetState(some_widgets->showOperationsTable_widget, FALSE, FALSE);
		  XtSetSensitive(some_widgets->showRatingCurve_widget, FALSE);
		  XmToggleButtonSetState(some_widgets->showRatingCurve_widget, FALSE, FALSE);
		  XtSetSensitive(some_widgets->showTulsaPlot_widget, FALSE);
		  XmToggleButtonSetState(some_widgets->showTulsaPlot_widget, FALSE, FALSE);
		  XtSetSensitive(some_widgets->showTimeSeriesTable_widget,FALSE);
	          XmToggleButtonSetState(some_widgets->showTimeSeriesTable_widget, FALSE, FALSE);

                  XtSetSensitive(some_widgets->showPlotTS_widget, FALSE);
	          XmToggleButtonSetState(some_widgets->showPlotTS_widget, FALSE, FALSE);
		  }





/*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
/*  <<<<<<<<<      rerun_segment      >>>>>>>>>> */
/*  <<<<<<<<<       next_segment      >>>>>>>>>> */
/*  <<<<<<<<<  goto_upstream_segment  >>>>>>>>>> */
/*  <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
	  else if(event.xproperty.window == root &&
		    (event.xproperty.atom == IFPA_rerun_segment ||
		     event.xproperty.atom == IFPA_next_segment  ||
		     event.xproperty.atom == IFPA_goto_upstream_segment))
			{

			XtSetSensitive(some_widgets->showOperationsTable_widget, FALSE);
			XmToggleButtonSetState(some_widgets->showOperationsTable_widget, FALSE, FALSE);
			XtSetSensitive(some_widgets->showRatingCurve_widget, FALSE);
			XmToggleButtonSetState(some_widgets->showRatingCurve_widget, FALSE, FALSE);
			XtSetSensitive(some_widgets->showTulsaPlot_widget, FALSE);
			XmToggleButtonSetState(some_widgets->showTulsaPlot_widget, FALSE, FALSE);
			XtSetSensitive(some_widgets->showTimeSeriesTable_widget,FALSE);
		        XmToggleButtonSetState(some_widgets->showTimeSeriesTable_widget, FALSE, FALSE);
			inSegmentAfterContinue = FALSE;

			XtSetSensitive(some_widgets->rerun_widget, FALSE);

                        XtSetSensitive(some_widgets->showPlotTS_widget, FALSE);
	                XmToggleButtonSetState(some_widgets->showPlotTS_widget, FALSE, FALSE);

			}




/* <<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< current segment >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
       else if (event.xproperty.window == root &&
		event.xproperty.atom == IFPA_current_segment)
       {

       if(XGetWindowProperty
	 (
	 display,
	 root,
	 IFPA_current_segment,
	 offset,
	 (long) 9,
	 FALSE,
	 IFPA_current_segment_type,
	 (Atom *)&type,
	 (int *)&format,
	 (unsigned long *)&nitems,
	 (unsigned long *)&left,
	 (unsigned char **)&segment_name
	 ) == Success && type == IFPA_current_segment_type)
	     {
	     memset(string, '\0', 9);
	     if((first_blank = strstr(segment_name, " ")) != NULL)
		     strncpy(string, segment_name, first_blank - segment_name);
	     else    strncpy(string, segment_name, 8);
	     strcpy(some_widgets->current_segment, string);

	     if (first_time) /*store first segment in fgroup */
	     {
	      memset(first_segment_name, '\0', 9);
	      first_time = 0;
	      strncpy(first_segment_name,segment_name,8);
	      memset(some_widgets->segment_selected, '\0', 9);
	      strncpy(some_widgets->segment_selected,segment_name,8);
             }

	     /* Get the PushButton widget for the current segment &  pass it as the          */
	     /*      previous widget in 'some_widgets'...                                    */

	     the_node = find_it(some_widgets->head[whichTree_index], string);
	     if(the_node != NULL)

		   some_widgets->currentSegment_widget = the_node->segment_widget;

	     else  some_widgets->currentSegment_widget = NULL;

	     highlight_current_segment(some_widgets, string, previousSegment);
	     strcpy(previousSegment, string);
	     }

       else XtDispatchEvent(&event);
       }



/* ------------------------------------------------------------------------------------------------------       */

/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< entering NWSRFS event loop >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>> */
	 else if(event.xproperty.atom == IFPA_entering_NWSRFS_eventLoop)
		 {

		 XtRemoveTimeOut(some_widgets->stopped_timeout_id);
		 XtUnmanageChild(some_widgets->NWSRFS_working_shell);
/*
 * See if apps_default set for number of columns - if it is, set
 * num_table_cols to that number (if that many are available)
 * if not, set num_table_cols to 1 and assume TulsaTable is displayed.
 * Code added by George Smith - HRL - 941120
 */
  {
   char   a_num_cols[3];  /* to hold ascii number value from get_apps_defaults */
   int    num_table_cols;  /* number of data columns visible */

   len = strlen("ifp_num_columns");
   get_apps_defaults("ifp_num_columns", &len, a_num_cols, &len2);

   if(strlen(a_num_cols) == 0)
     {
      printf("\nTulsaTable NOTICE\n\n");
      printf("A value for the number of columns initially displayed in the TulsaTable\n");
      printf("has not been entered in the .Apps_defaults file.\n");
      printf("If you want to set this value make an entry in your .Apps_defaults\n");
      printf("with the following format:\n");
      printf("  ifp_num_columns   :  N\n");
      printf("   - where N is the number of columns you want.\n");
      printf("IFP_Map will continue processing assuming that a TulsaTable is displayed.\n");
      printf("If you do not want to see a TulsaTable you must set ifp_num_columns to 0.\n\n");

      num_table_cols = 1;
     }
   else
     {
      num_table_cols = atoi(a_num_cols);
     }
    		  if(num_table_cols > 0)
    		     XtSetSensitive(some_widgets->showTimeSeriesTable_widget, TRUE);
    		  else
    		     XtSetSensitive(some_widgets->showTimeSeriesTable_widget, FALSE);
  } /* close code block where a_num_cols and num_table_cols are defined */

	XtSetSensitive(some_widgets->showTulsaPlot_widget, TRUE);

		  if(XtIsSensitive(some_widgets->showTimeSeriesTable_widget))
		     XmToggleButtonSetState(some_widgets->showTimeSeriesTable_widget, TRUE, FALSE);
		  else
		     XmToggleButtonSetState(some_widgets->showTimeSeriesTable_widget, FALSE, FALSE);

	XmToggleButtonSetState(some_widgets->showTulsaPlot_widget, TRUE, FALSE);


		 XtSetSensitive(run_cascade[0], TRUE);
		 XtSetSensitive(run_cascade[1], TRUE);
		 XtSetSensitive(run_cascade[3], TRUE);

		 for(i = 0; i <= sub_group_num; i++)
			if(some_widgets->head[i] != NULL)
				XtSetSensitive(some_widgets->head[i]->parent_widget, TRUE);

		 if(segment_callbacks_added)
		   {
		    segment_callbacks_added = FALSE;
		    for(i = 0; i <= sub_group_num; i++)
			   if(some_widgets->head[i] != NULL)
			     {     /* remove all callbacks, then add popup info back */
			      remove_segment_callbacks(some_widgets->head[i]);
			      add_segment_callbacks(some_widgets->head[i],
						    popup_segment_info,
						    some_widgets->head[i]->popup_shell);
			     }
		   }

		 XSync(display, 0);

		 if(!segment_callbacks_added)
		   {
		    segment_callbacks_added = TRUE;
		    for(i = 0; i <= sub_group_num; i++)
			   if(some_widgets->head[i] != NULL)
			     {
			      add_segment_callbacks(some_widgets->head[i],
						    handle_segment_selected,
						    some_widgets);
			     }
		   }


		 XtSetSensitive(some_widgets->showOperationsTable_widget, TRUE);
		 XmToggleButtonSetState(some_widgets->showOperationsTable_widget, FALSE, FALSE);

		 if(XGetWindowProperty
		    (
		    display,
		    root,
		    IFPA_rating_curve_available,
		    offset,
		    (long) sizeof(int),
		    FALSE,
		    IFPA_rating_curve_available_type,
		    (Atom *)&type,
		    (int *)&format,
		    (unsigned long *)&nitems,
		    (unsigned long *)&left,
		    (unsigned char **)&rating_curve_is_available
		    ) == Success && type == IFPA_rating_curve_available_type)
		     {
                     if(*rating_curve_is_available)
			XtSetSensitive(some_widgets->showRatingCurve_widget, TRUE);
		     else
			XtSetSensitive(some_widgets->showRatingCurve_widget, FALSE);
		     }
		 else XtSetSensitive(some_widgets->showRatingCurve_widget, FALSE);

		 /* Check if non time series mods are available in this segment
		  * and set the showOtherMods_widget sensitivity accordingly.
		  * dp - 23 Feb. 1996
		  */
		 nts_mods_available = get_nts_mods_available_atom(some_widgets->showOtherMods_widget);
		 if(nts_mods_available)
		    XtSetSensitive(some_widgets->showOtherMods_widget, TRUE);
		 else
		    XtSetSensitive(some_widgets->showOtherMods_widget, FALSE);

		/*
		 * See if we are in the last segment.
		 */
		 XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
		 XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
		 XtGetValues(some_widgets->listWidget_for_forecastSegments, wargs, 2);

		 XmStringGetLtoR(xmstring_Segments[number_of_Segments - 1],
				 XmSTRING_DEFAULT_CHARSET, &last_segment_name);

		 memset(current_segment_no_blanks, '\0', 9);
		 strcpy(current_segment_no_blanks, some_widgets->current_segment);

		 for(i = 0; i < 8; i++)     /* Remove trailing blanks */
		     if(current_segment_no_blanks[i] == ' ')
		       {
			current_segment_no_blanks[i] = '\0';
			break;
		       }

		 for(i = 0; i < 8; i++)     /* Remove trailing blanks */
		     if(last_segment_name[i] == ' ')
		       {
			last_segment_name[i] = '\0';
			break;
		       }
		 if(strcmp(current_segment_no_blanks, last_segment_name) == 0)
		    inLastSegment = TRUE;
		 else
		    inLastSegment = FALSE;

		}

/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< no Tulsa plot >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>> */
/*
 *  added by gfs, 8/19/91
 */
	 else if (event.xproperty.window == root &&
		  event.xproperty.atom == IFPA_no_Tulsa_plot)
		 {

		 XtRemoveTimeOut(some_widgets->stopped_timeout_id);
		 XtUnmanageChild(some_widgets->NWSRFS_working_shell);
/* ----------- changes down to ending comment added by gfs - hrl - 5 Oct 1994 - */
/*   These changes to properly control IFP_Map and ifp_nwsrfs program flow when
 *    the last segment in a forecast group does not have a Tulsa Plot operation.

		/*
		 * See if we are in the last segment.
		 */
		 XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
		 XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
		 XtGetValues(some_widgets->listWidget_for_forecastSegments, wargs, 2);

		 XmStringGetLtoR(xmstring_Segments[number_of_Segments - 1],
				 XmSTRING_DEFAULT_CHARSET, &last_segment_name);

		 memset(current_segment_no_blanks, '\0', 9);
		 strcpy(current_segment_no_blanks, some_widgets->current_segment);

		 for(i = 0; i < 8; i++)     /* Remove trailing blanks */
		     if(current_segment_no_blanks[i] == ' ')
		       {
			current_segment_no_blanks[i] = '\0';
			break;
		       }

		 for(i = 0; i < 8; i++)     /* Remove trailing blanks */
		     if(last_segment_name[i] == ' ')
		       {
			last_segment_name[i] = '\0';
			break;
		       }
		 if(strcmp(current_segment_no_blanks, last_segment_name) == 0)
		    inLastSegment = TRUE;
		 else
		    inLastSegment = FALSE;

                 if(inLastSegment == TRUE)
                   {
		    XtSetSensitive(run_cascade[0], TRUE);
            	    XtSetSensitive(some_widgets->continue_widget, FALSE);
            	    XtSetSensitive(some_widgets->next_widget, FALSE);

		    XtSetSensitive(run_cascade[1], TRUE);

		    XtSetSensitive(run_cascade[2], TRUE);
                    XmToggleButtonSetState(some_widgets->showOperationsTable_widget, FALSE, FALSE);
                    XmToggleButtonSetState(some_widgets->showRatingCurve_widget, FALSE, FALSE);

		    XtSetSensitive(run_cascade[3], FALSE);
                    /*
                     * XmToggleButtonSetState(some_widgets->showTulsaPlot_widget, FALSE, FALSE);
                     * XmToggleButtonSetState(some_widgets->showTimeSeriesTable_widget, FALSE, FALSE);
                     * XmToggleButtonSetState(some_widgets->showOtherMods_widget, FALSE, FALSE);
                     */
		    for(i = 0; i <= sub_group_num; i++)
			if(some_widgets->head[i] != NULL)
				XtSetSensitive(some_widgets->head[i]->parent_widget, TRUE);

                    printf("The last segment in the forecast group has no Tulsa plot.\n");
                    printf("Go to another segment, select another forecast group, or Quit.\n");
                   }
                 else
/* --------- end of code added by gfs - hrl - 5 Oct 1994 --------- */
                   {
		    /* reset upstream flag.  Completed goto selected segment */
                    ssUpstreamFlag = 0;
		    run_next_segment(global_toplevel, some_widgets, NULL);

		    if(currentSegment_is_the_gotoSegment(some_widgets))
			 {
			 restore_default_colors(some_widgets->currentSegment_widget);
			 invert_widget(some_widgets->currentSegment_widget);
			 }
		    else restore_default_colors(some_widgets->previous_segment);
                   }
		 break;
		 }
/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< has plot TS>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>> */
	 else if (event.xproperty.window == root &&
		  event.xproperty.atom == IFPA_has_plot_TS)
              {
            	 XtSetSensitive(some_widgets->showPlotTS_widget, TRUE);
                 XmToggleButtonSetState(some_widgets->showPlotTS_widget, TRUE, FALSE);

		 XtRemoveTimeOut(some_widgets->stopped_timeout_id);
		 XtUnmanageChild(some_widgets->NWSRFS_working_shell);

		 XtSetSensitive(run_cascade[0], TRUE);
		 XtSetSensitive(run_cascade[1], TRUE);
		 XtSetSensitive(run_cascade[3], TRUE);

		 for(i = 0; i <= sub_group_num; i++)
			if(some_widgets->head[i] != NULL)
				XtSetSensitive(some_widgets->head[i]->parent_widget, TRUE);

		 if(segment_callbacks_added)
		   {
		    segment_callbacks_added = FALSE;
		    for(i = 0; i <= sub_group_num; i++)
			   if(some_widgets->head[i] != NULL)
			     {     /* remove all callbacks, then add popup info back */
			      remove_segment_callbacks(some_widgets->head[i]);
			      add_segment_callbacks(some_widgets->head[i],
						    popup_segment_info,
						    some_widgets->head[i]->popup_shell);
			     }
		   }

		 XSync(display, 0);

		 if(!segment_callbacks_added)
		   {
		    segment_callbacks_added = TRUE;
		    for(i = 0; i <= sub_group_num; i++)
			   if(some_widgets->head[i] != NULL)
			     {
			      add_segment_callbacks(some_widgets->head[i],
						    handle_segment_selected,
						    some_widgets);
			     }
		   }


		 XtSetSensitive(some_widgets->showOperationsTable_widget, TRUE);
		 XmToggleButtonSetState(some_widgets->showOperationsTable_widget, FALSE, FALSE);

		 XtSetSensitive(some_widgets->showTimeSeriesTable_widget, FALSE);

		 /* Check if non time series mods are available in this segment
		  * and set the showOtherMods_widget sensitivity accordingly.
		  * dp - 23 Feb. 1996
		  */
		 nts_mods_available = get_nts_mods_available_atom(some_widgets->showOtherMods_widget);
		 if(nts_mods_available)
		    XtSetSensitive(some_widgets->showOtherMods_widget, TRUE);
		 else
		    XtSetSensitive(some_widgets->showOtherMods_widget, FALSE);

		/*
		 * See if we are in the last segment.
		 */
		 XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
		 XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
		 XtGetValues(some_widgets->listWidget_for_forecastSegments, wargs, 2);

		 XmStringGetLtoR(xmstring_Segments[number_of_Segments - 1],
				 XmSTRING_DEFAULT_CHARSET, &last_segment_name);

		 memset(current_segment_no_blanks, '\0', 9);
		 strcpy(current_segment_no_blanks, some_widgets->current_segment);

		 for(i = 0; i < 8; i++)     /* Remove trailing blanks */
		     if(current_segment_no_blanks[i] == ' ')
		       {
			current_segment_no_blanks[i] = '\0';
			break;
		       }

		 for(i = 0; i < 8; i++)     /* Remove trailing blanks */
		     if(last_segment_name[i] == ' ')
		       {
			last_segment_name[i] = '\0';
			break;
		       }
		 if(strcmp(current_segment_no_blanks, last_segment_name) == 0)
		    inLastSegment = TRUE;
		 else
		    inLastSegment = FALSE;
	      }
/* <<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< number of Tulsa plots >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>> */
      else if (event.xproperty.window == root &&
		event.xproperty.atom == IFPA_number_of_TulPlots)
       {

       if(XGetWindowProperty
	       (
	       display,
	       root,
	       IFPA_number_of_TulPlots,
	       offset,
	       (long) sizeof(int),
	       FALSE,
	       IFPA_number_of_TulPlots_type,
	       (Atom *)&type,
	       (int *)&format,
	       (unsigned long *)&nitems,
	       (unsigned long *)&left,
	       (unsigned char **)&number_of_TulPlots
	       ) == Success && type == IFPA_number_of_TulPlots_type)
		       {
		       if(*number_of_TulPlots > 0)
			       {

			       XtSetSensitive(some_widgets->continue_widget, TRUE);
			       XtSetSensitive(some_widgets->next_widget, FALSE);
			       }
		       else    {
			       XtSetSensitive(some_widgets->continue_widget, FALSE);
			      /*
			       * See if we are in the last segment.
			       */

			       XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
			       XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
			       XtGetValues(some_widgets->listWidget_for_forecastSegments, wargs, 2);

			       XmStringGetLtoR(xmstring_Segments[number_of_Segments - 1],
					       XmSTRING_DEFAULT_CHARSET, &last_segment_name);

			       memset(current_segment_no_blanks, '\0', 9);
			       strcpy(current_segment_no_blanks, some_widgets->current_segment);

			       for(i = 0; i < 8; i++)     /* Remove trailing blanks */
				   if(current_segment_no_blanks[i] == ' ')
				     {
				      current_segment_no_blanks[i] = '\0';
				      break;
				     }

			       for(i = 0; i < 8; i++)     /* Remove trailing blanks */
				   if(last_segment_name[i] == ' ')
				     {
				      last_segment_name[i] = '\0';
				      break;
				     }
			       if(strcmp(current_segment_no_blanks, last_segment_name) == 0)
				 {
				  inLastSegment = TRUE;
				  XtSetSensitive(some_widgets->next_widget, FALSE);
				 }
			       else
				 {
				  inLastSegment = FALSE;
				  XtSetSensitive(some_widgets->next_widget, TRUE);
				 }
			       }
		       }

       else XtDispatchEvent(&event);
       break;
       }
/* <<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<   number of mods to write    >>>>>>>>>>> */
/* <<<<<<<<<<<<    mods_have_been_deleted    >>>>>>>>>>> */
/* <<<<<<<<<<<<          tschng mod          >>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>> */
	    else if(event.xproperty.atom == IFPA_number_of_mods_to_write ||
		    event.xproperty.atom == IFPA_mods_have_been_deleted  ||
		    event.xproperty.atom == IFPA_tschng_mod ||
		    event.xproperty.atom == IFPA_mod_files_updated ||
		    event.xproperty.atom == IFPA_fgmod_files_updated)
		    {

		    if(XGetWindowProperty
		    (
		      display,
		      root,
		      IFPA_number_of_mods_to_write,
		      offset,
		      (long) sizeof(int),
		      FALSE,
		      IFPA_number_of_mods_to_write_type,
		      (Atom *)&type,
		      (int *)&format,
		      (unsigned long *)&nitems,
		      (unsigned long *)&left,
		      (unsigned char **)&numberOfModsToWrite
		      ) == Success && type == IFPA_number_of_mods_to_write_type)
                      {

			  if(XGetWindowProperty
			  (
				display,
				root,
				IFPA_FGmods_save,
				offset,
				(long) sizeof(int),
				FALSE,
				IFPA_FGmods_save_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&FGmods_save
				)== Success && type == IFPA_FGmods_save_type)
				{
				  if ((*FGmods_save == FGROUP) && (*numberOfModsToWrite > 0))
				  {

					fgroup_created_once = 1;
					range_upstream = 1;
					strcpy(some_widgets->segment_selected, first_segment_name);
		      			XtSetSensitive(some_widgets->rerun_widget, FALSE);
		        		XtSetSensitive(some_widgets->go_to_widget, TRUE);
                                        mods_saved_or_deleted = 1;
                                  }
                                }
				else /* FGmods_save is not set */
				{
                                         if(FGmods_save != NULL)free(FGmods_save);
 					 FGmods_save = (int *)malloc(sizeof(int));
					 *FGmods_save = FALSE;
                                         mods_saved_or_deleted = 0;
				}

			  if(XGetWindowProperty
			  (
				display,
				root,
				IFPA_rangemods_files_updated,
				offset,
				(long) 9,
				FALSE,
				IFPA_rangemods_files_updated_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&first_range_mods_str
				)== Success && type == IFPA_rangemods_files_updated_type)
				{
					range_updated = 1;

					if(!fgroup_created_once)
					{
						strcpy(some_widgets->segment_selected,first_range_mods_str);
						chkstream=is_computationally_before(some_widgets->current_segment, some_widgets);
						range_upstream = 0;
                                                if (chkstream) /* UPSTREAM*/
						    range_upstream = 1;
					}
                                }
				else
				{
					range_updated = 0;
					range_upstream = 0;
                                        chkstream = 0;
 					first_range_mods_str = (char *) malloc(9);
 					memset(first_range_mods_str,'\0',9);
				}
	               }	/*end else */
                       else
                       {
                           /* No number_of_mods_to_write property, set to 0...               */
 			   numberOfModsToWrite = (int *)malloc(sizeof(int));
		           *numberOfModsToWrite = 0;
		           fgroup_created_once = 0;
                       }

		    if(XGetWindowProperty
		       (
		       display,
		       root,
		       IFPA_mods_have_been_deleted,
		       offset,
		       (long) sizeof(int),
		       FALSE,
		       IFPA_mods_have_been_deleted_type,
		       (Atom *)&type,
		       (int *)&format,
		       (unsigned long *)&nitems,
		       (unsigned long *)&left,
		       (unsigned char **)&modsHaveBeenDeleted
		       ) == Success && type == IFPA_mods_have_been_deleted_type){
		       ;
		       }
                       else
                       {
                          /* No mods_have_been_deleted property, set to FALSE...           */
                           modsHaveBeenDeleted = (int *)malloc(sizeof(int));
		          *modsHaveBeenDeleted = 0;

                       }

		    if(XGetWindowProperty
		       (
		       display,
                       root,
		       IFPA_tschng_mod,
		       offset,
		       (long) sizeof(int),
		       FALSE,
		       IFPA_tschng_mod_type,
		       (Atom *)&type,
		       (int *)&format,
		       (unsigned long *)&nitems,
		       (unsigned long *)&left,
		       (unsigned char **)&tschngMod
		       ) == Success && type == IFPA_tschng_mod_type)
                     {

		          XtSetSensitive(some_widgets->rerun_widget, TRUE);/* XGetWindowProperty does not properly work with !clause on lx AV */
		     }

                   else
                     {
                        /* No tschng_mod property, set to FALSE...               */
                        tschngMod = (int *)malloc(sizeof(int));
                        *tschngMod = 0;
                     }


                    /*  This property was added for the new mods interface
                        so that after mods files were edited and written
                        out, the Rerun button would become sensitive.
                        dp - 4 Oct. 1995
                    */
		    if(XGetWindowProperty
		       (
		       display,
		       root,
		       IFPA_mod_files_updated,
		       offset,
		       (long) sizeof(int),
		       FALSE,
		       IFPA_mod_files_updated_type,
		       (Atom *)&type,
		       (int *)&format,
		       (unsigned long *)&nitems,
		       (unsigned long *)&left,
		       (unsigned char **)&mod_files_updated
		       ) == Success && type == IFPA_mod_files_updated_type)
                    {

		     ;


	            }
                    else
                    {
                    /* No IFPA_mod_files_updated property, set to FALSE...               */
                            mod_files_updated = (int *)malloc(sizeof(int));
			    *mod_files_updated = 0;

                    }



                   /* printf("*mod_files_updated =%d\n", *mod_files_updated);*/
		    if(XGetWindowProperty
		       (
		       display,
		       root,
		       IFPA_fgmod_files_updated,
		       offset,
		       (long) sizeof(int),
		       FALSE,
		       IFPA_fgmod_files_updated_type,
		       (Atom *)&type,
		       (int *)&format,
		       (unsigned long *)&nitems,
		       (unsigned long *)&left,
		       (unsigned char **)&fgmod_files_updated
		       ) == Success && type == IFPA_fgmod_files_updated_type){

                        ;

	        	}
                     else
                     {
                         /* No IFPA_mod_files_updated property, set to FALSE...               */
 			 fgmod_files_updated = (int *)malloc(sizeof(int));
			 *fgmod_files_updated = 0;


                     }


		/*
		 * Now have values for *numberOfModsToWrite, *modsHaveBeenDeleted, and
		 *  *tschngMod.  Use these to decide if mods have been made or deleted,
		 *  and consequently if Rerun, or Next, Cjontinue, or GoToSegment should
		 *  be sensitive and if the segment buttons should have callbacks.
		 * If any mods are saved or deleted Rerun is sensitive and the,
		 *  segment buttons should not have callbacks,
		 *  if no mod creation or deletion activity has been made Next, Continue,
		 *  or GoToSegment is sensitive and the buttons have callbacks.
		 */


                   if(*numberOfModsToWrite > 0 || *modsHaveBeenDeleted > 0 || *tschngMod > 0
		      || *mod_files_updated > 0|| *fgmod_files_updated > 0 || range_updated > 0)
		   {
		      mods_saved_or_deleted = 1;
		   }
		   else
		      mods_saved_or_deleted = 0;

		   if(mods_saved_or_deleted == 1)
		   {
                       if (*mod_files_updated > 0)
                       {

		       		XtSetSensitive(some_widgets->rerun_widget, TRUE);
		      		XtSetSensitive(some_widgets->go_to_widget, FALSE);
                                XtSetSensitive(some_widgets->next_widget, FALSE);
		   	        XtSetSensitive(some_widgets->continue_widget, FALSE);

		       }

                       if(fgroup_created_once == 1 || *fgmod_files_updated == 1
                                                         || range_upstream == 1)
		       {
		      	   XtSetSensitive(some_widgets->rerun_widget, FALSE);
		           XtSetSensitive(some_widgets->go_to_widget, TRUE);

                           if(fgroup_created_once || *fgmod_files_updated > 0  )
			   {
				strcpy(some_widgets->segment_selected,first_segment_name);
			   }

       		       }
		       else{

		       	   XtSetSensitive(some_widgets->rerun_widget, TRUE);
		           XtSetSensitive(some_widgets->go_to_widget, FALSE);
		       }
                       /* AV 08/11/03 bug r23-37 When making tschngMods make sure to set RERUN on from File Menu*/
                       if (*tschngMod){
                          XtSetSensitive(some_widgets->rerun_widget, TRUE);
		          XtSetSensitive(some_widgets->go_to_widget, FALSE);
                          range_upstream = 0;
                       }

		   	XtSetSensitive(some_widgets->next_widget, FALSE);
		   	XtSetSensitive(some_widgets->continue_widget, FALSE);
		   	restore_default_colors(some_widgets->previous_segment);
		   	some_widgets->previous_segment = NULL;
		   	XDeleteProperty(display, root, IFPA_goto_downstream_segment);
		        if(segment_callbacks_added)
			{
			 segment_callbacks_added = FALSE;
			 for(i = 0; i <= sub_group_num; i++)
			   if(some_widgets->head[i] != NULL)
			     {     /* remove all callbacks, then add popup info back */
			      remove_segment_callbacks(some_widgets->head[i]);
			      add_segment_callbacks(some_widgets->head[i],
						    popup_segment_info,
						    some_widgets->head[i]->popup_shell);
			     }
			}
		   }
		   else
		   {

		      if(inSegmentAfterContinue)
			 XtSetSensitive(some_widgets->rerun_widget, TRUE);
		      else
			 XtSetSensitive(some_widgets->rerun_widget, FALSE);

		      if(XGetWindowProperty
			(
			display,
			root,
			IFPA_number_of_TulPlots,
			offset,
			(long) sizeof(int),
			FALSE,
			IFPA_number_of_TulPlots_type,
			(Atom *)&type,
			(int *)&format,
			(unsigned long *)&nitems,
			(unsigned long *)&left,
			(unsigned char **)&number_of_TulPlots
			) == Success && type == IFPA_number_of_TulPlots_type)
			 {
			 if(*number_of_TulPlots > 0)
			    {
			    XtSetSensitive(some_widgets->continue_widget, TRUE);
			    XtSetSensitive(some_widgets->next_widget, FALSE);
			    }
			 else
			    {
			    XtSetSensitive(some_widgets->continue_widget, FALSE);
			   /*
			    * See if we are in the last segment.
			    */
			    XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
			    XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
			    XtGetValues(some_widgets->listWidget_for_forecastSegments, wargs, 2);

			    XmStringGetLtoR(xmstring_Segments[number_of_Segments - 1],
					    XmSTRING_DEFAULT_CHARSET, &last_segment_name);

			    memset(current_segment_no_blanks, '\0', 9);
			    strcpy(current_segment_no_blanks, some_widgets->current_segment);

			    for(i = 0; i < 8; i++)     /* Remove trailing blanks */
				if(current_segment_no_blanks[i] == ' ')
				  {
				   current_segment_no_blanks[i] = '\0';
				   break;
				  }

			    for(i = 0; i < 8; i++)     /* Remove trailing blanks */
				if(last_segment_name[i] == ' ')
				  {
				   last_segment_name[i] = '\0';
				   break;
				  }
			    if(strcmp(current_segment_no_blanks, last_segment_name) == 0)
			      {
			       inLastSegment = TRUE;
			       XtSetSensitive(some_widgets->next_widget, FALSE);
			      }
			    else
			      {
			       inLastSegment = FALSE;
			       XtSetSensitive(some_widgets->next_widget, TRUE);
			      }
			    }
			 }
		      if(!segment_callbacks_added)
			{
			 segment_callbacks_added = TRUE;
			 for(i = 0; i <= sub_group_num; i++)
			   if(some_widgets->head[i] != NULL)
			     {
			      add_segment_callbacks(some_widgets->head[i],
						    handle_segment_selected,
						    some_widgets);
			     }
			}
		     }
		    }

/* <<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< done making mods >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
      else if( event.xproperty.atom == IFPA_done_making_mods)
	      {
	      if(XGetWindowProperty
		      (
		      display,
		      root,
		      IFPA_done_making_mods,
		      offset,
		      (long) sizeof(int),
		      FALSE,
		      IFPA_done_making_mods_type,
		      (Atom *)&type,
		      (int *)&format,
		      (unsigned long *)&nitems,
		      (unsigned long *)&left,
		      (unsigned char **)&doneMakingMods
		      ) == Success && type == IFPA_done_making_mods_type)
		      {
		      if(*doneMakingMods)
			      {
			      XmToggleButtonSetState(some_widgets->showOtherMods_widget, FALSE, FALSE);
			      }
		      }
	      }


/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< segment status >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
	else if (event.xproperty.window == root &&
		 event.xproperty.atom == IFPA_segment_status)
	{
	if(XGetWindowProperty
		(
		display,
		root,
		IFPA_segment_status,
		offset,
		(long) sizeof(int),
		FALSE,
		IFPA_segment_status_type,
		(Atom *) &type,
		(int *)&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&flow_status
		) == Success && type == IFPA_segment_status_type)
			{

			change_current_segment_status(some_widgets, flow_status);
			}

	else XtDispatchEvent(&event);
	break;
	}


/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< run end date / run end obs date>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */
	 else if(event.xproperty.window == root && (event.xproperty.atom == IFPA_run_end_date ||
						    event.xproperty.atom == IFPA_run_end_obs_date))
		 {
		 if(some_widgets->run_info_bb != NULL && XtIsRealized(some_widgets->run_info_bb))
			 {
			 if(event.xproperty.atom == IFPA_run_end_date)
				 {
				 if(XGetWindowProperty
					 (
					 display,
					 root,
					 IFPA_run_end_date,
					 offset,
					 (long) sizeof(date),
					 FALSE,
					 IFPA_run_end_date_type,
					 (Atom *)&type,
					 (int *)&format,
					 (unsigned long *)&nitems,
					 (unsigned long *)&left,
					 (unsigned char **)&newDate
					 ) == Success && type == IFPA_run_end_date_type)
				 update_date_display(newDate, *(some_widgets->viewDates->end));
				 }

			 else if( event.xproperty.atom == IFPA_run_end_obs_date)
				 {
				 if(XGetWindowProperty
					 (
					 display,
					 root,
					 IFPA_run_end_obs_date,
					 offset,
					 (long) sizeof(date),
					 FALSE,
					 IFPA_run_end_obs_date_type,
					 (Atom *)&type,
					 (int *)&format,
					 (unsigned long *)&nitems,
					 (unsigned long *)&left,
					 (unsigned char **)&newDate
					 ) == Success && type == IFPA_run_end_obs_date_type)
				 update_date_display(newDate, *(some_widgets->viewDates->end_obs));
				 }
			 }
		 else    {
			 XtDispatchEvent(&event);
			 break;
			 }

		 }

/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<< activate rerun >>>>>>>>>>>>>> */
/* <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>> */
	else if (event.xproperty.window == root &&
		 event.xproperty.atom == IFPA_activate_rerun)
	{

	if(XGetWindowProperty
		(
		display,
		root,
		IFPA_activate_rerun,
		offset,
		(long) sizeof(int),
		FALSE,
		IFPA_activate_rerun_type,
		(Atom *)&type,
		(int *)&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&activate_rerun
		) == Success && type == IFPA_activate_rerun_type)
		   {
			XtSetSensitive(some_widgets->rerun_widget, TRUE);
		   }

	else XtDispatchEvent(&event);
	break;
	}


/*  <<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
/*  <<<<<<  showModsViewer  >>>>>>> */
/*  <<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> */
	else if(event.xproperty.window == root &&
		event.xproperty.atom == IFPA_show_mods_viewer)
        {
            modsViewer = get_show_mods_viewer_atom(global_toplevel);
            if(modsViewer == TRUE)
               XmToggleButtonSetState(some_widgets->showModsViewer_widget, TRUE, FALSE);
            else
               XmToggleButtonSetState(some_widgets->showModsViewer_widget, FALSE, FALSE);
        }
	 else    {
		 XtDispatchEvent(&event);
		 break;
		 }

/*  <<<<<<<<<<<<<<>>>>>>>>>>>>>>>> */
/*  <<<<<<<<<  default  >>>>>>>>>> */
/*  <<<<<<<<<<<<<<>>>>>>>>>>>>>>>> */

		default:
			XtDispatchEvent(&event);
	     }

	}       /*      End of 'while()'...                                     */

/* **********************        END MAIN EVENT LOOP     ********************** */
  return;
}       /*      end of main()           */






/* ****************************************************************************

	 amend_seg_list()
		       adds or removes segments from the list of segments to run
			depending on whether it's in the delete string

   **************************************************************************** */

void amend_seg_list (Widget w, tree_data_struct *tree_data, XmAnyCallbackStruct *call_data)
{
	Display         *display;
	int             i, length;
	int             number_Trees_remaining;
	int             fullTreeSelected = FALSE;

	char            seg_selected[9];
	char            seg_str[9];
	char            *blank = " ";


	node            *the_node;
	node            *head_node;


 if(call_data != NULL)
	{
        /*AV changed Control Mask to ShiftMask when port to Linux */
	if((call_data->event->xbutton.state & ShiftMask) == ShiftMask)
		{
		/* printf("CONTROL is pressed...\n");    */
		return;
		}
	}

 display = XtDisplay(global_toplevel);

 number_Trees_remaining = 0;
 if(tree_data->branch->parent == NULL)
	{       /* The parent node of the current node is NULL; this implies that we're trying          */
		/*      to select the entire tree. Since this selection leads to subsequent actions     */
		/*      that are either superfluous or disallowed, don't allow the selection...         */

	for(i = 0; i <= sub_group_num; i++)
		if(tree_data->dataStruct->head[i] != NULL) number_Trees_remaining++;

	if(number_Trees_remaining <= 1)
		{
		XBell(display, 100);
		XFlush(display);
		return;
		}

	fullTreeSelected = TRUE;

	}

 /* Identify which tree we're in; 'head_node' is the head of the tree...                */
 the_node = tree_data->branch;
 head_node = tree_data->dataStruct->head[the_node->which_tree];
 tree_data->dataStruct->currentSegment_widget = the_node->segment_widget;
 whichTree_index = the_node->which_tree;


 memset(seg_selected, '\0', 9);
 if(the_node->e19.name != NULL && strlen(the_node->e19.name) != 0)
     strcpy(seg_selected, the_node->e19.name);

 length = strlen(seg_selected);

 for(i = 1; i <= 8 - length; i++) {
    strcat(seg_selected, blank);
 }

/*--------------------------------------------------------------------------------------*/
/*      Test to see if segment_selected is in the selected_string;                      */
/*              if(TRUE):       remove the segment and all upstream segments            */
/*                              rom the delete list and invert its widget               */
/*                              and the widgets corresponding to the segments           */
/*                              upstream in the tree;                                   */
/*                                                                                      */
/*              if(FALSE):      add the segment and all upstream segments               */
/*                              to the delete list and invert its widget                */
/*                              and the widgets corresponding to the segments           */
/*                              upstream in the tree;                                   */
/*--------------------------------------------------------------------------------------*/


if(strstr(selected_string, seg_selected))
	{       /* 'seg_selected' is in the list, 'selected_string', of segments to delete...           */


        if(the_node->parent != NULL) {
	   strcpy(seg_str, the_node->parent->e19.name);

        }else strcpy(seg_str, the_node->e19.name);
	length = strlen(seg_str);
	for(i = 0; i < 8 - length; i++) strcat(seg_str, blank);

	if(the_node->parent != NULL && strstr(selected_string, seg_str) != NULL)

		{       /* We're trying to unselect segments that have downstream segments selected...  */
			/*      this is not allowed, since it would leave topologically unconnected     */
			/*      branches in the tree widget...                                          */
		XBell(display, 100);
		XFlush(display);
		return;
		}
	else    {       /*   Remove the segment(s) from the selected list       */
		delete_from_selected(the_node, head_node);
		fill_pixmap(tree_data->dataStruct->main_canvas, tree_data->dataStruct, NULL);

		/*      Set the tree widgets sensitive for the trees that had been set to insensitive...        */
		if(strlen(selected_string) == 0)
			{
			for(i = 0; i <= sub_group_num; i++)
				{
				if(tree_data->dataStruct->head[i] != NULL)
					{
					if(tree_data->dataStruct->head[i] != head_node)
					      XtSetSensitive(tree_data->dataStruct->head[i]->parent_widget, TRUE);
				      }
			      }
		      }

		if(strlen(selected_string) !=  0)
			{
			for(i = 0; i <= sub_group_num; i++)
				highlight_MAPBasins(tree_data->dataStruct->head[i], tree_data->dataStruct);
			}
		}
	}
else    {               /*      Add the segment(s) to the selected list         */
	add_to_selected(head_node, the_node);
	highlight_MAPBasins(the_node, tree_data->dataStruct);
	order_selected_string(head_node);                       /* gfs 6/23/91  */

	/*      Set the tree widgets insensitive for all other trees, except for the one corresponding to       */
	/*              'head_node'...                                                                          */
	for(i = 0; i <= sub_group_num; i++)
		{
		if(tree_data->dataStruct->head[i] != head_node)
			{
			if(tree_data->dataStruct->head[i] != NULL)
				XtSetSensitive(tree_data->dataStruct->head[i]->parent_widget, FALSE);
			}
		}
	}

/* Changed to search whole tree to get number of subgroups selected - gfs 6/23/91       */

 numberOfSubgroupsSelected = 0;
 check_multiple_selected(tree_data->dataStruct, head_node);

 if(numberOfSubgroupsSelected > 0)
	{               /* At least one subgroup has been selected...   */

	XtSetSensitive(tree_data->dataStruct->begin_widget, FALSE); /* added by gfs 9/2/93 */
	XtSetSensitive(tree_data->dataStruct->reset_widget, TRUE);

	if(numberOfSubgroupsSelected == 1)
		{       /* Exactly one subgroup is selected...          */
		XtSetSensitive(tree_data->dataStruct->deleteSegments_widget, TRUE);

		if(fullTreeSelected) XtSetSensitive(tree_data->dataStruct->keepSubset_widget, FALSE);
		else                 XtSetSensitive(tree_data->dataStruct->keepSubset_widget, TRUE);
		}
	else    {       /* More than one subgroup was selected...       */
		XtSetSensitive(tree_data->dataStruct->deleteSegments_widget, TRUE);
		XtSetSensitive(tree_data->dataStruct->keepSubset_widget, FALSE);
		}
	}
 else   {       /* No subgroups are selected (we just un-selected a subgroup)...        */
	XtSetSensitive(tree_data->dataStruct->begin_widget, TRUE);
	XtSetSensitive(tree_data->dataStruct->reset_widget, FALSE);
	XtSetSensitive(tree_data->dataStruct->deleteSegments_widget, FALSE);
	XtSetSensitive(tree_data->dataStruct->keepSubset_widget, FALSE);
	}


}




/* ****************************************************************************

	 add_to_the_list()
		       removes segments from the list of segments to run

   **************************************************************************** */

void add_to_the_list (some_widgets)
	the_widget_struct       *some_widgets;
{
	char            segmentName[9];
	char            *temp_delete_string;
	Arg             wargs[1];
	int             numItemsVisible;


 if(*delete_string == '\0') return;

 temp_delete_string = delete_string;
 memset(segmentName, '\0', 9);
 strncpy(segmentName, temp_delete_string, 8);
 strcpy(temp_delete_string, temp_delete_string + 8);


 XtSetArg(wargs[0], XmNvisibleItemCount, &numItemsVisible);
 XtGetValues(some_widgets->delete_list, wargs, 1);

 numItemsVisible++;
 XtSetArg(wargs[0], XmNvisibleItemCount, numItemsVisible);
 XtSetValues(some_widgets->delete_list, wargs, 1);
 XmListAddItem(some_widgets->delete_list, XmStringCreate(segmentName, XmSTRING_DEFAULT_CHARSET), 0);

 add_to_the_list(some_widgets);


}



/* ****************************************************************************

	 add_to_selected()

   **************************************************************************** */

void add_to_selected (head_node, current)
	node            *head_node;
	node            *current;
{
	int             i, length;
	char            seg_selected[9];
	char            *blank = " ";
	char            string[9];



 invert_segment(current->e19.name, head_node);

 strcpy(seg_selected, current->e19.name);
 length = strlen(seg_selected);
 for(i = 1; i <= 8 - length; i++) strcat(seg_selected, blank);

 strcat(selected_string, seg_selected);

 if(current->left != NULL)
	{
	strcpy(string, current->left->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) == NULL)
		add_to_selected(head_node, current->left);
	}
 if(current->mid_left != NULL)
	{
	strcpy(string, current->mid_left->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) == NULL)
		add_to_selected(head_node, current->mid_left);
	}
 if(current->center != NULL)
	{
	strcpy(string, current->center->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) == NULL)
		add_to_selected(head_node, current->center);
	}
 if(current->mid_right != NULL)
	{
	strcpy(string, current->mid_right->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) == NULL)
		add_to_selected(head_node, current->mid_right);
	}
 if(current->right != NULL)
	{
	strcpy(string, current->right->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) == NULL)
		add_to_selected(head_node, current->right);
	}
}




/* ****************************************************************************

	 delete_from_selected()
		       removes segments from the list of segments to run

   **************************************************************************** */

void delete_from_selected(current, head_node)
	node    *current;
	node    *head_node;
{

	int             i;
	int             length;
	char            string[9];
	char            *string_ptr;



invert_segment(current->e19.name, head_node);


 strcpy(string, current->e19.name);
 length = strlen(string);
 for(i = 0; i < 8 - length; i++) strcat(string, " ");

string_ptr = strstr(selected_string, string);
strcpy(string_ptr, string_ptr + 8);


if(current->left != NULL)       delete_from_selected(current->left, head_node);
if(current->mid_left != NULL)   delete_from_selected(current->mid_left, head_node);
if(current->center != NULL)     delete_from_selected(current->center, head_node);
if(current->mid_right != NULL)  delete_from_selected(current->mid_right, head_node);
if(current->right != NULL)      delete_from_selected(current->right, head_node);

}


/* ****************************************************************************

	 show_the_list()
		shows the list of segments to remove from the list
		of segments to run

   **************************************************************************** */

void show_the_list (some_widgets)
	the_widget_struct       *some_widgets;
{
	Widget          delete_shell, bb, sw_for_delete_list, delete_list;
	Arg             wargs[3];

delete_shell = XtCreatePopupShell
			(
			"delete_shell",
			transientShellWidgetClass,
			some_widgets->tree_shell,
			NULL,
			0
			);
some_widgets->delete_shell = delete_shell;


bb = XtCreateManagedWidget
			(
			"bb_for_delete_list",
			xmScrolledWindowWidgetClass,
			delete_shell,
			NULL,
			0
			);

XtSetArg(wargs[0], XtNwidth, 215);
XtSetArg(wargs[1], XtNheight, 300);
sw_for_delete_list = XtCreateManagedWidget
			(
			"sw_for_delete_list",
			xmScrolledWindowWidgetClass,
			bb,
			wargs,
			2
			);
some_widgets->delete_list = sw_for_delete_list;

delete_list = XtCreateManagedWidget
			(
			"delete_list",
			xmListWidgetClass,
			sw_for_delete_list,
			NULL,
			0
			);
some_widgets->delete_list = delete_list;

}

/* ****************************************************************************

	 set_the_segments()
		copies the string contained by 'delete_string' to
		the Atom IFPA_run_segments

   **************************************************************************** */

void set_the_segments (delete_ok, some_widgets, call_data)
	Widget                  delete_ok;
	the_widget_struct       *some_widgets;
	caddr_t                 call_data;
{
	Widget          cant_delete_shell;
	Widget          current_TreeWidget;

	char            all_segments[801], *string, *seg_name;
	char            *run_segments;
	char            *temp_delete_string;

	char            pad_string[9];
	char            messageString[500];

	Display         *display;
	Window          root;
	Window          treeWindow;
	Arg             wargs[8];

	char            *string_ptr;
	int             i;
        char            *location;
	int             type, format, nitems, left;
	int             j, num_list;
	int             length;
	long            offset = 0;

	XmString        xmMessageString;

	help_struct     *help_data;




 memset(messageString, '\0', 500);

 display = XtDisplay(delete_ok);
 root = DefaultRootWindow(display);

 current_TreeWidget = some_widgets->head[whichTree_index]->parent_widget;

 strcpy(delete_string, selected_string);

 temp_delete_string = (char *) malloc(strlen(delete_string) + 1);
 memset(messageString, '\0', strlen(delete_string) + 1);
 strcpy(temp_delete_string, selected_string);


 strcpy(pad_string, some_widgets->head[whichTree_index]->e19.name);
 length = strlen(pad_string);
 for(i = 0; i < 8 - length; i++) strcat(pad_string, " ");

 if(strstr(delete_string, pad_string) != NULL)
	{               /*      ie, we're trying to delete all the segments,    */
			/*      something that does'nt make sense - so, we      */
			/*      won't let the user do it.                       */

	XFlush(display);

	strcpy(messageString, "All the segments have been selected to delete\n");
	strcat(messageString, "from the run list - NWSRFS can't run without\n");
	strcat(messageString, "at least one segment in the run list.");
	xmMessageString = XmStringCreateLtoR(messageString, XmSTRING_DEFAULT_CHARSET);

	XtSetArg(wargs[0], XmNmessageString, xmMessageString);
	cant_delete_shell = XmCreateErrorDialog
			(
			global_toplevel,
			"cant_delete_shell",
			wargs,
			1
			);

	help_data = (help_struct *) malloc(sizeof(help_struct));
	help_data->parent = global_toplevel;
	help_data->message_widget_name = "cant_delete_dialog";

	XtAddCallback(cant_delete_shell, XmNhelpCallback, popup_help_window, "CANT_DELETE_ALL_SEGMENTS");

	}       /*      End of 'if', no 'else' to go to; return to calling function     */


string = (char *) malloc(9);
seg_name = (char *) malloc(9);

if(XGetWindowProperty
		(
		display,
		root,
		IFPA_run_segments,
		offset,
		(long) 801,
		FALSE,
		IFPA_run_segments_type,
		(Atom *)&type,
		(int *)&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&run_segments
		) == Success && type == IFPA_run_segments_type)
	{
	strcpy(all_segments, run_segments);

	if(some_widgets->head[whichTree_index] != NULL)
		{
		treeWindow = XtWindow(current_TreeWidget);
		XUnmapWindow(display, treeWindow);
		XUnmapSubwindows(display, treeWindow);
		destroy_tree_children(some_widgets->head[whichTree_index]);
		XtDestroyWidget(current_TreeWidget);
		}

	some_widgets->head[whichTree_index] = null_it(some_widgets->head[whichTree_index]);


	while(*temp_delete_string != '\0')
		{
		memset(string, '\0', 9);
		memset(seg_name, '\0', 9);
		strncpy(string, temp_delete_string, 8);
		temp_delete_string += 8;
		strcpy(seg_name, string);
		if((location = strchr(seg_name, ' ')) != NULL) strcpy(location, "");
		string_ptr = strstr(all_segments, string);
		strcpy(string_ptr, string_ptr + 8);
		}

	XChangeProperty
		(
		display,
		root,
		IFPA_run_segments,
		IFPA_run_segments_type,
		8,
		PropModeReplace,
		all_segments,
		strlen(all_segments)
		);


	add_to_the_list(some_widgets);
	create_partial_seg_tree(some_widgets->head[whichTree_index], some_widgets);

	/* Delete all of the items in Non-universal List widget...                      */
	XtSetArg(wargs[0], XmNitemCount, &num_list);
	XtGetValues(some_widgets->non_univ_list, wargs, 1);
	for(j = 0; j < num_list; j++) XmListDeletePos(some_widgets->non_univ_list, 1);


	/* Re-fill the list of items in Non-universal List widget...                    */
	fill_list(&num_list);
	XtSetArg(wargs[0], XmNitems, xmstr_Run_list);
	XtSetArg(wargs[1], XmNitemCount, num_list);
	XtSetArg(wargs[2], XmNvisibleItemCount, num_list);
	XtSetValues(some_widgets->non_univ_list, wargs, 3);

	}


 for(i = 0; i <= sub_group_num; i++)
	if(some_widgets->head[i] != NULL)
		{
		XtSetSensitive(some_widgets->head[i]->parent_widget, TRUE);
		}

 add_overlays(some_widgets->main_canvas, some_widgets, NULL);

 XtSetSensitive(some_widgets->begin_widget, TRUE);
 XtSetSensitive(some_widgets->revert_widget, TRUE);
 XtSetSensitive(some_widgets->reset_widget, FALSE);


 numberOfSubgroupsSelected = 0;
 XtSetSensitive(some_widgets->deleteSegments_widget, FALSE);
 XtSetSensitive(some_widgets->keepSubset_widget, FALSE);

 memset(selected_string, '\0', 801);

}






/* ****************************************************************************

		 reset_the_segments()
				resets the segments so that none are highlighted
				and 'delete_string' == '\0 ...'.

   **************************************************************************** */

void reset_the_segments (delete_reset, some_widgets, call_data)
	Widget                  delete_reset;
	the_widget_struct       *some_widgets;
	caddr_t                 call_data;
{
	node            *the_node;
	char            segment_name[9];
	char            *del_string;
	char            *str_pointer;
	int             i;



del_string = (char *) malloc(801);
memset(del_string, '\0', 801);
strcpy(del_string, selected_string);

while(*del_string != '\0')
	{
	strncpy(segment_name, del_string, 8);
	del_string += 8;

	if((str_pointer = strchr(segment_name, ' ')) != NULL) *str_pointer = '\0';

	the_node = find_it(some_widgets->head[whichTree_index], segment_name);

	invert_segment(the_node->e19.name, some_widgets->head[whichTree_index]);
	}

 memset(delete_string, '\0', 801);
 memset(selected_string, '\0', 801);

 numberOfSubgroupsSelected = 0;

 for(i = 0; i <= sub_group_num; i++)
	if(some_widgets->head[i] != NULL) XtSetSensitive(some_widgets->head[i]->parent_widget, TRUE);
 XtSetSensitive(some_widgets->begin_widget, TRUE);
 XtSetSensitive(some_widgets->reset_widget, FALSE);
 XtSetSensitive(some_widgets->deleteSegments_widget, FALSE);
 XtSetSensitive(some_widgets->keepSubset_widget, FALSE);

 add_overlays(some_widgets->main_canvas, some_widgets, NULL);

 /* fill_pixmap(some_widgets->main_canvas, some_widgets, NULL); */

}


/* *****************************************************************************

	null_it()
		using the current node structure, the pointer to the node
		corresponding to 'id' is set to NULL; returns NULL.

   ***************************************************************************** */

node *null_it(current)
	node    *current;
{

	char    string[9];
	int     i;
	int     length;



 memset(string, '\0', 9);
 strcpy(string, current->e19.name);

 length = strlen(string);
 for(i = 0; i < 8 - length; i++) strcat(string, " ");

 if(strstr(delete_string, string) != NULL)
	{
	free(current);
	return (NULL);
	}

 if(current->left != NULL)      current->left =      null_it(current->left);
 if(current->mid_left != NULL)  current->mid_left =  null_it(current->mid_left);
 if(current->center != NULL)    current->center =    null_it(current->center);
 if(current->mid_right != NULL) current->mid_right = null_it(current->mid_right);
 if(current->right != NULL)     current->right =     null_it(current->right);

 return (current);

}




/* *****************************************************************************

	pop_down_shell()
		pops down the popup_shell.

   ***************************************************************************** */

void pop_down_shell(w, the_shell, call_data)
	Widget          w;
	Widget          the_shell;
	caddr_t         call_data;
{

XtPopdown(the_shell);

}




/* *****************************************************************************

	create_runInfo_popup()

   ***************************************************************************** */

void create_runInfo_popup(w, some_widgets, call_data)
	Widget                  w;
	the_widget_struct       *some_widgets;
	caddr_t                 *call_data;
{

	Arg             wargs[3];
	Widget          info_bb;
	int             n;



info_bb = XtCreateManagedWidget
		(
		"info_board",
		xmBulletinBoardWidgetClass,
		some_widgets->run_info_shell,
		NULL,
		0
		);
some_widgets->run_info_bb = info_bb;

some_widgets->viewDates = make_display_widgets(some_widgets);

}



/* ****************************************************************************

	 make_forecast_group_subset()

   **************************************************************************** */

void make_forecast_group_subset (w, some_widgets, call_data)
	Widget                  w;
	the_widget_struct       *some_widgets;
	caddr_t                 call_data;
{
	Widget          cant_delete_shell, bb;

	char            *all_segments, *string, *segmentName;
	char            *temp_delete_string;
	char            *tempString;
	char            *delete_item;
	char            *run_segments;
	char            *first_blank;

	Display         *display;
	Window          root;
	Window          treeWindow;
	Arg             wargs[5];

	int             delete_str_length;
	int             i;
        char            *location, *string_ptr;
	int             type, format, nitems, left;
	int             j, num_list = 0;
	int             num_segments;
	long            offset = 0;

	XmString        xmMessageString;

	help_struct     *help_data;
	node            *the_node;


 display = XtDisplay(w);
 root = DefaultRootWindow(display);

 all_segments = (char *) malloc(801);
 tempString = (char *) malloc(801);
 string = (char *) malloc(9);
 segmentName = (char *) malloc(9);
 memset(segmentName, '\0', 9);


 /*     Get the string containing the current list of 'run segments'...                         */
 if(XGetWindowProperty
		(
		display,
		root,
		IFPA_run_segments,
		offset,
		(long) 801,
		FALSE,
		IFPA_run_segments_type,
		(Atom *)&type,
		(int *)&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&run_segments
		) == Success && type == IFPA_run_segments_type) strcpy(tempString, run_segments);



 /* Get the name of the first segment in the 'selected_string' list...                                  */
 strncpy(string, selected_string, 8);
 if((first_blank = strstr(string, " "))  != NULL)
	strncpy(segmentName, string, first_blank - string);
 else   strcpy(segmentName, string);


 if(some_widgets->head[whichTree_index] != NULL)
	 {
	 treeWindow = XtWindow(some_widgets->head[whichTree_index]->parent_widget);
	 XUnmapWindow(display, treeWindow);
	 XUnmapSubwindows(display, treeWindow);
	 destroy_tree_children(some_widgets->head[whichTree_index]);
	 XtDestroyWidget(some_widgets->head[whichTree_index]->parent_widget);
	 }

 /* Find the node corresponding to the first segment found in the 'selected_string' list...             */

 the_node = find_it(some_widgets->head[whichTree_index], segmentName);
 if(the_node == NULL) return;

 the_node->parent = NULL;
 some_widgets->head[whichTree_index] = the_node;

 XDeleteProperty(display, root, IFPA_run_segments);     /* Delete the IFPA_run_segments property...     */

 /* Create a new list of run segments & set the IFPA_run_segments property...                           */
 for(i = 0; i <= sub_group_num; i++)
	{
	if(some_widgets->head[i] != NULL) concat_the_ids(some_widgets->head[i]);
	}


 if(XGetWindowProperty
		(
		display,
		root,
		IFPA_run_segments,
		offset,
		(long) 801,
		FALSE,
		IFPA_run_segments_type,
		(Atom *)&type,
		(int *)&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&run_segments
		) == Success && type == IFPA_run_segments_type)
	{
	strcpy(all_segments, run_segments);

	/* 'selected_string' is the list of segments we're keeping...                                           */

	temp_delete_string = delete_string;

	strcpy(delete_string, selected_string);         /* 'delete_string' & 'selected_string' are the same...  */

	while(*delete_string != '\0')
		{
		memset(string, '\0', 9);
		memset(segmentName, '\0', 9);

		strncpy(string, delete_string, 8);      /* Get the 1st item in 'delete_string' & advance one    */
		delete_string += 8;                     /*      item past the beginning of the string...        */

		/* 'tempString' is a list containing the original list of run segments...                       */

		delete_item = strstr(tempString, string);       /* Find 'string' in 'tempString' and remove     */
		strcpy(delete_item, delete_item + 8);           /*      it from 'tempString'...                 */

		strcpy(segmentName, string);
		if((location = strchr(segmentName, ' ')) != NULL) strcpy(location, "");
		string_ptr = strstr(all_segments, string);

		if(string_ptr != NULL)                          /* <-- This 1 line added by gfs 6/23/91 - temp. */
			strcpy(string_ptr, string_ptr + 8);
		}


	delete_string = temp_delete_string;

	strcpy(delete_string, all_segments);

	create_partial_seg_tree(the_node, some_widgets);

	/* Delete all of the items in Non-universal List widget...                      */
	XtSetArg(wargs[0], XmNitemCount, &num_list);
	XtGetValues(some_widgets->non_univ_list, wargs, 1);
	for(j = 0; j < num_list; j++) XmListDeletePos(some_widgets->non_univ_list, 1);


	/* Re-fill the list of items in Non-universal List widget...                    */
	fill_list(&num_list);
	XtSetArg(wargs[0], XmNitems, xmstr_Run_list);
	XtSetArg(wargs[1], XmNitemCount, num_list);
	XtSetArg(wargs[2], XmNvisibleItemCount, num_list);
	XtSetValues(some_widgets->non_univ_list, wargs, 3);

	memset(delete_string, '\0', 801);

	strcpy(delete_string, tempString);
	add_to_the_list(some_widgets);          /* Fill the 'delete_list' listWidget with deleted items         */
	}


 memset(selected_string, '\0', sizeof(selected_string));


 numberOfSubgroupsSelected = 0;

 for(i = 0; i <= sub_group_num; i++)
	if(some_widgets->head[i] != NULL) XtSetSensitive(some_widgets->head[i]->parent_widget, TRUE);

 XtSetSensitive(some_widgets->begin_widget, TRUE);
 XtSetSensitive(some_widgets->reset_widget, FALSE);
 XtSetSensitive(some_widgets->revert_widget, TRUE);
 XtSetSensitive(some_widgets->deleteSegments_widget, FALSE);
 XtSetSensitive(some_widgets->keepSubset_widget, FALSE);


 add_overlays(some_widgets->main_canvas, some_widgets, NULL);

}




/* ****************************************************************************

	 check_multiple_selected()

	 Go through entire tree, if find a highlighted node
			increment counter,
			stop going down that branch.
   **************************************************************************** */

void check_multiple_selected (some_widgets, current)
	the_widget_struct       *some_widgets;
	node                    *current;
{
	node            *the_node;

 if((the_node = find_it(some_widgets->head[current->which_tree], current->e19.name)) == NULL) return;

 if(is_highlighted(the_node->segment_widget))
	{
	numberOfSubgroupsSelected++;
	return;
	}

if(current->left != NULL)       check_multiple_selected(some_widgets, current->left);
if(current->mid_left != NULL)   check_multiple_selected(some_widgets, current->mid_left);
if(current->center != NULL)     check_multiple_selected(some_widgets, current->center);
if(current->mid_right != NULL)  check_multiple_selected(some_widgets, current->mid_right);
if(current->right != NULL)      check_multiple_selected(some_widgets, current->right);
}


/* ****************************************************************************

	 is_highlighted()

   **************************************************************************** */

int is_highlighted(w)
	Widget          w;
{
	Arg             wargs[1];
	int             pixel;

 XtSetArg(wargs[0], XtNbackground, &pixel);
 XtGetValues(w, wargs, 1);

 if(pixel == application_default_foreground) return(TRUE);
 return(FALSE);
}

/* ****************************************************************************

	order_selected_string()

	Puts the segments in the selected_string
	(a global variable 801 words long) in down to upstream order
	using the node tree information.
	needed once we started allowing multiple selections to keep
	segments and make a subset.     (gfs -- 6/23/91)

   **************************************************************************** */

void order_selected_string(current_node)
      node    *current_node;
{
      char      *temp_string[100], *all_segments_in_order[100];
      char      *temp_selected_string;
      int       loc, i, j;




 for(i = 0; i < 100; i++)
	{
	temp_string[i] = (char *)malloc(9 * sizeof(char));
	memset(temp_string[i], '\0', 9);
	all_segments_in_order[i] = (char *)malloc(9 * sizeof(char));
	memset(all_segments_in_order[i], '\0', 9);
	}

/* fill all_segments_in_order with names of segments in down to upstream order. */

 loc = 0;
 make_list_of_segment_names(all_segments_in_order, &loc, current_node);

 j = 0;
 for(i = 0; i <= loc; i++)
	{
	temp_selected_string = selected_string;
	while(*temp_selected_string != '\0')
		{
		if(strncmp(temp_selected_string, all_segments_in_order[i], 8) == 0)
			{       /* have a match, copy name into temp_string             */
			strncpy(temp_string[j++], temp_selected_string, 8);
			break;
			}
		 else   {       /* no match, keep looking through selected string       */
			temp_selected_string += 8;
			}
		}
	}

 temp_selected_string = selected_string;        /* remember start of selected_string    */
 for(i = 0; i < j; i++)
	{       /* copy in segment names in proper order                                */
	strncpy(selected_string, temp_string[i], 8);
	selected_string += 8;
	}

 selected_string = temp_selected_string;        /* reset start of selected_string       */

}



/* ************************************************************************************

	 Make_list_of_segment_names is like concat_the_ids except it does't
	 post a window property, just creates a list of names. gfs 062391

   ************************************************************************************ */

void    make_list_of_segment_names(list, loc, current)

	char    *list[100];
	int     *loc;
	node    *current;
{

	char    string[9];
	char    *blank = " ";
	int     i, length;



 memset(string, '\0', 9);
 strcpy(string, current->e19.name);
 if((length = strlen(string)) < 8)
	{
	 for(i = 1; i <= 8 - length; i++) strcat(string, blank);
	}

 strncpy(list[*loc], string, 8);
 (*loc)++;

 if (current->left != NULL)      make_list_of_segment_names(list, loc, current->left);
 if (current->mid_left != NULL)      make_list_of_segment_names(list, loc, current->mid_left);
 if (current->center != NULL)      make_list_of_segment_names(list, loc, current->center);
 if (current->mid_right != NULL)      make_list_of_segment_names(list, loc, current->mid_right);
 if (current->right != NULL)      make_list_of_segment_names(list, loc, current->right);
}


void set_time_zone_code_atom(display)
	Display         *display;
{
 int   a_in, a_ipr, a_ipu, a_ioerr, a_iodbug;
 int   a_metric, a_iumgen, a_iumsac, a_iumapi;
 int   a_nhopdb, a_nhocal, a_local, a_nlstz;
 char   a_inptzc[5], a_modtzc[5];
 int   a_noutz, a_noutds, a_modwrn;
 int   a_nosnow, a_nofrze, a_iupwe, a_isac_snow, a_iupsc;
 int   a_iprsac, a_iprsnw, a_icrtro, a_iprhy, a_ifpr;
 int   a_idarun, a_ihrrun, a_ldacpd, a_lhrcpd, a_ldarun, a_lhrrun;
 int   a_now[5];

 RETRIEVE_HCL_TECHS_ARGS(&a_in, &a_ipr, &a_ipu, &a_ioerr, &a_iodbug,
			 &a_metric, &a_iumgen, &a_iumsac, &a_iumapi,
			 &a_nhopdb, &a_nhocal, &a_local, &a_nlstz,
			 a_inptzc, a_modtzc,
			 &a_noutz, &a_noutds, &a_modwrn,
			 &a_nosnow, &a_nofrze, &a_iupwe, &a_isac_snow, &a_iupsc,
			 &a_iprsac, &a_iprsnw, &a_icrtro, &a_iprhy, &a_ifpr,
			 &a_idarun, &a_ihrrun, &a_ldacpd, &a_lhrcpd, &a_ldarun, &a_lhrrun,
			 a_now);


 a_inptzc[4] = '\0';
 a_modtzc[4] = '\0';

 XChangeProperty
	 (
	 display,
	 DefaultRootWindow(display),
	 IFPA_time_zone_code,
	 IFPA_time_zone_code_type,
	 8,
	 PropModeReplace,
	 a_inptzc,
	 strlen(a_inptzc)
	 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/run_partial.c,v $";
 static char rcs_id2[] = "$Id: run_partial.c,v 1.15 2006/04/07 13:30:29 aivo Exp $";}
/*  ===================================================  */

}
