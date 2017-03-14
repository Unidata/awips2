
/* ***************************************************************************************************


	create_interface.c

				Creates the graphical user interface for the NWS River Forecast System

	Coded by:               Tom Adams
	Affiliation:            NOAA/NWS/Office of Hydrology/HRL
	Date:                   08/??/90
	Latest Major Change:    09/28/92, 03/22/93
        Changed:                04/03/95 - dp - reads parameter file to decide which overlays to turn on
                                07/21/95 - dp - added option to save gif files
                                04/30/97 - dp - add ModsViewer button
                                03/02/01 - av - add SAC and SNOW button

   *************************************************************************************************** */



#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "run_partial.h"
#include "globals.h"            /* Added 10/02/92       */
#include "struct_defs.h"        /* Added 10/02/92       */
#include "ifp_help.h"
#include "help.h"
#include "IFP_version_date.h"   /* Added 02/01/93       */

static void     extract_date_changes();
static void     set_time_zone_code_atom();



extern void startSnow();
extern void startSac();
extern void start_dhm_gridoutput_configuration();
Widget          sac_widget, snow_widget;/* to be used by run_subs */
typedef struct
	{
	int     time_change;
	int     time;
	char    time_zone[5];
	}       date_data;





static xs_menu_struct OverlayData[] =
	{
	{"States",                    add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Counties",                  add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Rivers",                    add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Cities",                    add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Basin boundaries",          add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Forecast points",           add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Current Forecast Group",    add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Forecast Group boundaries", add_overlays, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	};


static xs_menu_struct PrefItems[] =
	{
	{"Colors...",              choose_color,      NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Run_menu_struct[] =
	{
	{"Begin" , run_nwsrfs, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Rerun" , rerun_segment, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Next" , run_next_segment, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Continue" , continue_to_next_TulPlot, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Go to segment" , go_to_selected_segment, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"New Forecast Group..." , popup_FGroup_selectionDialog, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Quit" , exit_nwsrfs, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
	};


static xs_menu_struct Display_menu_struct[] =
	{
	{"Deleted segments" , show_deleted_segments, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Run segments" , show_run_segments, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Operations table" , show_operations_table, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Rating curve" , show_rating_curve, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Forecast Group topology" , show_ForecastGroup_topology, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Current Run-time Mods" , show_current_mods, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Save GIF File" , set_save_gif, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Geography", NULL, NULL, TRUE, PUSH_BUTTON, OverlayData, XtNumber(OverlayData), ""},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"SAC" , startSac, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"SNOW" , startSnow, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
	};


static xs_menu_struct Techniques_menu_struct[] =
	{
	{"Universal" , set_universal, NULL, TRUE, PUSH_BUTTON},
	{"Non-universal" , set_non_universal, NULL, TRUE, PUSH_BUTTON}
	};

static xs_menu_struct Options_menu_struct[] =
	{
	{"Keep" , make_forecast_group_subset, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Delete" , set_the_segments, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Reset" , reset_the_segments, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Single segment when Begin" , set_run_multiple, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Revert to original Forecast Group topology" , revert_to_original_tree, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Revert to original map view" , revert_to_default_view, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Set dates..." , set_run_dates, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Tools", show_tools, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Preferences", NULL, NULL, TRUE, PUSH_BUTTON, PrefItems, XtNumber(PrefItems), ""},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Techniques" , NULL, NULL, TRUE, PUSH_BUTTON, Techniques_menu_struct, XtNumber(Techniques_menu_struct), ""},
	{(char *) NULL , (void *) NULL, (caddr_t) NULL, TRUE, (int) NULL, (struct _menu_struct *) NULL, 0, (char *) NULL},
	{"Configure DHM Grid Output" , start_dhm_gridoutput_configuration, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
	};

static xs_menu_struct Mods_menu_struct[] =
	{
	{"Tulsa plot" , show_Tulsa_plot, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Time-series table" , show_TimeSeries_table, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"TS plot" , show_plot_TS, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Other mods" , show_other_mods, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL},
	{"Mods viewer" , set_show_mods_viewer, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL}
	};

static xs_menu_struct HelpStruct[] =
	{
	{"Online Help..." , popup_help_window, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Version..." , popup_version_window, NULL, TRUE, TOGGLE_BUTTON, NULL, 0, NULL}
	};








/* ****************************************************************************

	create_run_interface()

	Purpose:        Creates the main menubar and pulldown menus
	Code Type:      Interface

	Called from:    main()


   **************************************************************************** */

void create_run_interface(some_widgets)
	the_widget_struct       *some_widgets;
{
	Widget          form, menu_bar, run_info_shell;
	Widget          run_Control_mainMenuItem, run_Options_mainMenuItem;
	Widget          run_Display_mainMenuItem, run_Help_mainMenuItem;
	Widget          run_Mods_mainMenuItem;
	Widget          begin_widget, rerun_widget, next_widget, revert_widget, submenu_widget;
	Widget          go_to_widget;
	Widget          techniques_widget, universal_widget, non_universal_widget;
	Widget          reset_widget, runMultiple_widget, makeSubset_widget, continue_widget;
	Widget          showRunSegments_widget, setDates_widget, deleteSegments_widget;
	Widget          showRatingCurve_widget, showOperationsTable_widget;
	Widget          showTulsaPlot_widget, showTimeSeriesTable_widget, quit_widget;
	Widget          showPlotTS_widget;
	Widget          showDeletedSegments_widget, showOtherMods_widget, showModsViewer_widget;
	Widget          help_shell;
	Widget          Help_cascade;
        

	int             i;
	int             j;
	int             n;
        int             len, len2;

	Arg             wargs[9];

	Dimension       scrollBar_height;
	Dimension       form_height;
	Dimension       tree_width;
	Dimension       shell_height;
	Dimension       shell_width;
	Dimension       fg_label_height;
	Dimension       tree_height;
	Dimension       parent_margin;
	Position        tree_position;

	help_struct     *help_data;
	help_cb_struct  *context_help;

	xs_menu_widget_struct   *controlStruct;
	xs_menu_widget_struct   *optionsStruct;
	xs_menu_widget_struct   *displayStruct;
	xs_menu_widget_struct   *modsStruct;
	xs_menu_widget_struct   *HelpData_struct;

	char    *single = "Single segment when Begin";
	char    *multiple = "Go to segment when Begin";

	char    help_pathname[80];

	int     save_gif_temp;



 numberOfSubgroupsSelected = 0;
 NWSRFS_has_begun = FALSE;

 SingleSegment = malloc(sizeof(char) * (strlen(single) + 1));
 GoToSegment   = malloc(sizeof(char) * (strlen(multiple) + 1));
 strcpy(SingleSegment, single);
 strcpy(GoToSegment, multiple);

/* call routine to get the path name for help files */
 memset(help_pathname, '\0', 80);
 len = strlen("ifp_help_dir");
 get_apps_defaults("ifp_help_dir", &len, help_pathname, &len2);
 strcat(help_pathname, "/ifp_map/");

 /* Create the Help PopupShell & it's children; 'popupHelp_shell' is global...          */
 popupHelp_shell = create_applicationShell1(global_toplevel, help_pathname);

/* Create the popup shell widget for displaying thr dates & segments for the run...     */

 run_info_shell = XtCreatePopupShell
			(
			"run_info_shell",
			transientShellWidgetClass,
			global_toplevel,
			NULL,
			0
			);
 some_widgets->run_info_shell = run_info_shell;

 XtSetArg(wargs[0], XmNrubberPositioning, TRUE);
 form = XtCreateManagedWidget("run_form", xmFormWidgetClass, global_toplevel, wargs, 1);
 some_widgets->form_widget = form;


/*      Create a transientShell for the context sensitive help window...                */
 n = 0;
 XtSetArg(wargs[0], XtNallowShellResize, TRUE); n++;
/* XtSetArg(wargs[0], XtNgeometry, "300x200+500+300"); n++;     */
 help_shell = XtCreatePopupShell("run_help_shell", transientShellWidgetClass, form, wargs, n);



 XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
 XtSetArg(wargs[1], XmNleftAttachment, XmATTACH_FORM);
 XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
 menu_bar = XmCreateMenuBar(form, "run_menuBar", wargs, 3);
 XtManageChild(menu_bar);
 some_widgets->menuBar = menu_bar;


/*      Create the pulldown main menus...                                               */
 run_Control_mainMenuItem = XmCreatePulldownMenu(menu_bar, "run_Control_mainMenuItem", NULL, 0);
 some_widgets->control_mainMenuItem = run_Control_mainMenuItem;

 run_Options_mainMenuItem = XmCreatePulldownMenu(menu_bar, "run_Options_mainMenuItem", NULL, 0);
 some_widgets->options_mainMenuItem = run_Options_mainMenuItem;

 run_Display_mainMenuItem = XmCreatePulldownMenu(menu_bar, "run_Display_mainMenuItem", NULL, 0);
 some_widgets->display_mainMenuItem = run_Display_mainMenuItem;

 run_Mods_mainMenuItem = XmCreatePulldownMenu(menu_bar, "run_Mods_mainMenuItem", NULL, 0);
 some_widgets->mods_mainMenuItem = run_Mods_mainMenuItem;

 run_Help_mainMenuItem = XmCreatePulldownMenu(menu_bar, "run_Help_mainMenuItem", NULL, 0);

/* -----------------------------------------------------------------------------------  */
/*                      Create cascade buttons the main menu items...                   */
/* -----------------------------------------------------------------------------------  */

 XtSetArg(wargs[0], XmNsubMenuId, run_Control_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("File", XmSTRING_DEFAULT_CHARSET));
 XtSetArg(wargs[2], XmNmnemonic, 'F');
 run_cascade[0] = XmCreateCascadeButton(menu_bar, "run_Control_cascade", wargs, 3);
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "run_Control_cascade";
 XtAddEventHandler(run_cascade[0], EnterWindowMask, FALSE, help_event_handler, context_help);

 XtSetArg(wargs[0], XmNsubMenuId, run_Options_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Options", XmSTRING_DEFAULT_CHARSET));
 XtSetArg(wargs[2], XmNmnemonic, 'O');
 run_cascade[1] = XmCreateCascadeButton(menu_bar, "run_Options_cascade", wargs, 3);
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "run_Options_cascade";
 XtAddEventHandler(run_cascade[1], EnterWindowMask, FALSE, help_event_handler, context_help);

 XtSetArg(wargs[0], XmNsubMenuId, run_Display_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Display", XmSTRING_DEFAULT_CHARSET));
 XtSetArg(wargs[2], XmNmnemonic, 'D');
 run_cascade[2] = XmCreateCascadeButton(menu_bar, "run_Display_cascade", wargs, 3);
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "run_Display_cascade";
 XtAddEventHandler(run_cascade[2], EnterWindowMask, FALSE, help_event_handler, context_help);

 XtSetArg(wargs[0], XmNsubMenuId, run_Mods_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Modifications", XmSTRING_DEFAULT_CHARSET));
 XtSetArg(wargs[2], XmNmnemonic, 'M');
 run_cascade[3] = XmCreateCascadeButton(menu_bar, "run_Mods_cascade", wargs, 3);
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "run_Mods_cascade";
 XtAddEventHandler(run_cascade[3], EnterWindowMask, FALSE, help_event_handler, context_help);
 XtManageChildren(run_cascade, 4);

 XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET));
 XtSetArg(wargs[1], XmNmnemonic, 'H');
 XtSetArg(wargs[2], XmNsubMenuId, run_Help_mainMenuItem);
 Help_cascade = XmCreateCascadeButton(menu_bar, "run_Help_cascade", wargs, 3);
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "run_Help_cascade";
 XtAddEventHandler(Help_cascade, EnterWindowMask, FALSE, help_event_handler, context_help);


 help_data = (help_struct *) malloc(sizeof(help_struct));
 help_data->parent = global_toplevel;
 help_data->message_widget_name = "run_Help_mainMenuItem";

 XtVaSetValues(menu_bar, XmNmenuHelpWidget, Help_cascade, NULL);
 XtManageChild(Help_cascade);

/* -----------------------------------------------------------------------------------  */



 /* -----------------------------------------------------------------------------------  */
 /*      Create pulldown menu items for 'File'...                                        */
 /* -----------------------------------------------------------------------------------  */

 for(i = 0; i < XtNumber(Run_menu_struct); i++) Run_menu_struct[i].data = (caddr_t) some_widgets;
 controlStruct =
	(xs_menu_widget_struct *)xs_create_menu_buttons("", run_Control_mainMenuItem, Run_menu_struct, XtNumber(Run_menu_struct));

 j = 0;
 for(i = 0; i < XtNumber(Run_menu_struct); i++)
	{
	if(Run_menu_struct[i].name != NULL)
		{
		context_help = (help_cb_struct *)malloc(sizeof(help_cb_struct));
		context_help->top_help_shell = help_shell;
		context_help->widget_name = Run_menu_struct[i].name;
		XtAddEventHandler(controlStruct->widget_array[j++]->parent, EnterWindowMask,
				  FALSE, help_event_handler, context_help);
		}
	}



 /* -----------------------------------------------------------------------------------  */
 /*      Create pulldown menu items for 'Options'...                                     */
 /* -----------------------------------------------------------------------------------  */


 /* ------------ Techniques Submenu ----------- */
 for(i = 0; i < XtNumber(Techniques_menu_struct); i++) Techniques_menu_struct[i].data = (caddr_t) some_widgets;

 /* ----------- Preferences Submenu ----------- */
 for(i = 0; i < XtNumber(PrefItems); i++) PrefItems[i].data = (caddr_t) some_widgets;

 /* ------------ Options Mainmenu ------------- */
 for(i = 0; i < XtNumber(Options_menu_struct); i++) Options_menu_struct[i].data = (caddr_t) some_widgets;
 optionsStruct =
	(xs_menu_widget_struct *)xs_create_menu_buttons("", run_Options_mainMenuItem, Options_menu_struct, XtNumber(Options_menu_struct));

 j = 0;
 for(i = 0; i < XtNumber(Options_menu_struct); i++)
	{
	if(Options_menu_struct[i].name != NULL)
		{
		context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
		context_help->top_help_shell = help_shell;
		context_help->widget_name = Options_menu_struct[i].name;
		XtAddEventHandler(optionsStruct->widget_array[j++]->parent, EnterWindowMask,
				  FALSE, help_event_handler, context_help);
		}
	}

 for(i = 0; i < XtNumber(Techniques_menu_struct); i++)
	{
	context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
	context_help->top_help_shell = help_shell;
	context_help->widget_name = Techniques_menu_struct[i].name;
	XtAddEventHandler(optionsStruct->widget_array[9]->widget_array[i]->parent, EnterWindowMask,
			  FALSE, help_event_handler, context_help);
	}


 for(i = 0; i < XtNumber(PrefItems); i++)
	{
	context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
	context_help->top_help_shell = help_shell;
	context_help->widget_name = PrefItems[i].name;
	XtAddEventHandler(optionsStruct->widget_array[8]->widget_array[i]->parent, EnterWindowMask,
			  FALSE, help_event_handler, context_help);
	}


 /* -----------------------------------------------------------------------------------  */
 /*      Create pulldown menu items for 'Display'...                                     */
 /* -----------------------------------------------------------------------------------  */

 rad_data = (draw_struct *) malloc(sizeof(draw_struct));
 memset(rad_data, 0, sizeof(draw_struct));


 /* ------------- Overlay Submenu ------------- */
 for(i = 0; i < XtNumber(OverlayData); i++) OverlayData[i].data = (caddr_t) some_widgets;

 /* ------------- Display Mainmenu ------------ */
 for(i = 0; i < XtNumber(Display_menu_struct); i++) Display_menu_struct[i].data = (caddr_t) some_widgets;


 displayStruct =
     (xs_menu_widget_struct *)xs_create_menu_buttons("", run_Display_mainMenuItem, Display_menu_struct, XtNumber(Display_menu_struct));

 j = 0;
 for(i = 0; i < XtNumber(Display_menu_struct); i++)
	{
	if(Display_menu_struct[i].name != NULL)
		{
		context_help = (help_cb_struct *)malloc(sizeof(help_cb_struct));
		context_help->top_help_shell = help_shell;
		context_help->widget_name = Display_menu_struct[i].name;
		XtAddEventHandler(displayStruct->widget_array[j++]->parent, EnterWindowMask,
				  FALSE, help_event_handler, context_help);
		}
	}
 /**Ai Vo begin**/
 sac_widget = displayStruct->widget_array[8]->parent;
 snow_widget = displayStruct->widget_array[9]->parent;
 some_widgets->sac_widget = sac_widget;
 some_widgets->snow_widget = snow_widget;
 
 /**Ai Vo end**/  
 
 
 /* Add call to ReadParameters to read the ifp_param.dat file to
    decide which buttons to turn on initially then set the buttons. 
    dp - 3 April 95
    Moved call to here to set the Save GIF File toggle - dp - 07/21/95
 */
 ReadParameters();

 if(save_gif) XmToggleButtonSetState(displayStruct->widget_array[6]->parent, TRUE, FALSE);
 /* initially post for no save of gif files - reset after Begin */
 save_gif_temp = FALSE;
 post_save_gif_atom(displayStruct->widget_array[6]->parent, save_gif_temp);

 for(i = 0; i < XtNumber(OverlayData); i++)
	{
	context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
	context_help->top_help_shell = help_shell;
	context_help->widget_name = OverlayData[i].name;
	XtAddEventHandler(displayStruct->widget_array[7]->widget_array[i]->parent, EnterWindowMask,
			  FALSE, help_event_handler, context_help);
	}



 /* ----------------------------------------------------------------------------------- */
 /*      Create pulldown menu items for 'Modifications'...                              */
 /* ----------------------------------------------------------------------------------- */

 for(i = 0; i < XtNumber(Mods_menu_struct); i++) Mods_menu_struct[i].data = (caddr_t) some_widgets;
 modsStruct =
	(xs_menu_widget_struct *)xs_create_menu_buttons("", run_Mods_mainMenuItem, Mods_menu_struct, XtNumber(Mods_menu_struct));

 for(i = 0; i < XtNumber(Mods_menu_struct); i++)
	{
	context_help = (help_cb_struct *)malloc(sizeof(help_cb_struct));
	context_help->top_help_shell = help_shell;
	context_help->widget_name = Mods_menu_struct[i].name;
	XtAddEventHandler(modsStruct->widget_array[i]->parent, EnterWindowMask,
			  FALSE, help_event_handler, context_help);
	}


 /* -----------------------------------------------------------------------------------  */
 /*      Create pulldown menu items for 'Help'...                                        */
 /* -----------------------------------------------------------------------------------  */

 HelpStruct[0].data = (caddr_t) "RUN_MAP";      /* Initialize data for Online Help...           */
 for(i = 1; i < XtNumber(Mods_menu_struct); i++) HelpStruct[i].data = (caddr_t) some_widgets;
 HelpData_struct     =  (xs_menu_widget_struct *)xs_create_menu_buttons("", run_Help_mainMenuItem, HelpStruct, XtNumber(HelpStruct));

 for(i = 0; i < XtNumber(HelpStruct); i++)
	{
	context_help = (help_cb_struct *)malloc(sizeof(help_cb_struct));
	context_help->top_help_shell = help_shell;
	context_help->widget_name = HelpStruct[i].name;
	XtAddEventHandler(modsStruct->widget_array[i]->parent, EnterWindowMask,
			  FALSE, help_event_handler, context_help);
	}



/* ----------------------------------------------------------------------
	states_button             0  States
	counties_button           1  Counties
	rivers_button             2  Rivers
	cities_button             3  Cities
	basin_button              4  Basin boundaries
	points_button             5  Forecast points
	FcstGroup_button          6  Current Forecast Group
	FcstGroupBoundary_button  7  Forecast Group boundaries
   ---------------------------------------------------------------------- */



 some_widgets->states_widget              = displayStruct->widget_array[7]->widget_array[0]->parent;
 some_widgets->county_widget              = displayStruct->widget_array[7]->widget_array[1]->parent;
 some_widgets->rivers_widget              = displayStruct->widget_array[7]->widget_array[2]->parent;
 some_widgets->cities_widget              = displayStruct->widget_array[7]->widget_array[3]->parent;
 some_widgets->basin_boundaries_widget    = displayStruct->widget_array[7]->widget_array[4]->parent;
 some_widgets->forecastPoints_widget      = displayStruct->widget_array[7]->widget_array[5]->parent;
 some_widgets->FcstGroup_widget           = displayStruct->widget_array[7]->widget_array[6]->parent;
 some_widgets->FcstGroupBoundaries_widget = displayStruct->widget_array[7]->widget_array[7]->parent;

 /* set buttons according to settings found in earlier call to ReadParameters */  
 if (istate)  XmToggleButtonSetState(some_widgets->states_widget, TRUE, FALSE);
 if (icity)   XmToggleButtonSetState(some_widgets->cities_widget, TRUE, FALSE);
 if (iriver)  XmToggleButtonSetState(some_widgets->rivers_widget, TRUE, FALSE);
 if (ibound)  XmToggleButtonSetState(some_widgets->basin_boundaries_widget, TRUE, FALSE);
 if (icounty) XmToggleButtonSetState(some_widgets->county_widget, TRUE, FALSE);
/* ------------------------------------------------------------------------------------ */

 begin_widget = controlStruct->widget_array[0]->parent;
 if(begin_widget != NULL)
	{
	some_widgets->begin_widget = begin_widget;
	XtVaSetValues(begin_widget,
			XtVaTypedArg,   XmNacceleratorText, XmRString, "Ctrl + B", strlen("Ctrl + B")+1,
			XmNaccelerator, "Ctrl<KeyPress>B",
			XmNsensitive,   FALSE,
			NULL);

	}


 rerun_widget = controlStruct->widget_array[1]->parent;
 if(rerun_widget != NULL)
	{
	some_widgets->rerun_widget = rerun_widget;
	XtVaSetValues(rerun_widget,
		     XmNaccelerator,     "Ctrl<KeyPress>R",
		     XmNacceleratorText, XmStringCreateLtoR("Ctrl + R", XmSTRING_DEFAULT_CHARSET),
		     XmNsensitive,       FALSE,
		     NULL);
	}

 next_widget = controlStruct->widget_array[2]->parent;
 if(next_widget != NULL)
	{
	some_widgets->next_widget = next_widget;
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>N");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + N", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(next_widget, wargs, 2);
	XtSetSensitive(next_widget, FALSE);
	}


 continue_widget = controlStruct->widget_array[3]->parent;
 if(continue_widget != NULL)
	{
	some_widgets->continue_widget = continue_widget;
	XtVaSetValues(continue_widget,
		      XmNaccelerator,     "Ctrl<KeyPress>O",
		      XmNacceleratorText, XmStringCreateLtoR("Ctrl + O", XmSTRING_DEFAULT_CHARSET),
		      XmNsensitive,       FALSE,
		      NULL);

	/*XtSetSensitive(continue_widget, FALSE);*/
	}


 go_to_widget = controlStruct->widget_array[4]->parent;
 if(go_to_widget != NULL)
	{
	some_widgets->go_to_widget = go_to_widget;
	XtVaSetValues(go_to_widget,
		      XmNaccelerator,     "Ctrl<KeyPress>G",
		      XmNacceleratorText, XmStringCreateLtoR("Ctrl + G", XmSTRING_DEFAULT_CHARSET),
		      XmNsensitive,       FALSE,
		      NULL);

	/*XtSetSensitive(go_to_widget, FALSE);*/
	}

 /* New Forecast Group widget...                        */
 some_widgets->new_ForecastGroup_widget = controlStruct->widget_array[5]->parent;
 XtVaSetValues(some_widgets->new_ForecastGroup_widget,
		 XtVaTypedArg,   XmNacceleratorText, XmRString, "Ctrl + F", strlen("Ctrl + F")+1,
		 XmNaccelerator, "Ctrl<KeyPress>F",
		 XmNsensitive,   FALSE,
		 NULL);


 quit_widget = controlStruct->widget_array[6]->parent;
 if(quit_widget != NULL)
	{
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>Q");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + Q", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(quit_widget, wargs, 2);
	}


 makeSubset_widget = optionsStruct->widget_array[0]->parent;
 if(makeSubset_widget != NULL)
	{
	some_widgets->keepSubset_widget = makeSubset_widget;
	XtSetSensitive(makeSubset_widget, FALSE);
	}

 deleteSegments_widget = optionsStruct->widget_array[1]->parent;
 if(deleteSegments_widget != NULL)
	{
	some_widgets->deleteSegments_widget = deleteSegments_widget;
	XtSetSensitive(deleteSegments_widget, FALSE);
	}

 reset_widget = optionsStruct->widget_array[2]->parent;
 if(reset_widget != NULL)
	{
	some_widgets->reset_widget = reset_widget;
	XtSetSensitive(some_widgets->reset_widget, FALSE);
	}


 runMultiple_widget = optionsStruct->widget_array[3]->parent;
 if(runMultiple_widget != NULL)
	{
	some_widgets->run_multiple_widget = runMultiple_widget;
	XtSetSensitive(some_widgets->run_multiple_widget, TRUE);
	}


 revert_widget = optionsStruct->widget_array[4]->parent;
 if(revert_widget != NULL)
	{
	some_widgets->revert_widget = revert_widget;
	XtVaSetValues(revert_widget,
		     XmNsensitive,   FALSE,
		     NULL);
	}


 setDates_widget = optionsStruct->widget_array[6]->parent;
 if(setDates_widget != NULL)
	{
	some_widgets->setDates_widget = setDates_widget;
	XtVaSetValues(setDates_widget,
		     XmNsensitive,   FALSE,
		     NULL);
	}

 /* Set the 'Techniques' widget to insensitive, until the Forecast Group schematic      */
 /* is created...                                                                       */
 some_widgets->techniques_widget = optionsStruct->widget_array[9]->parent;
 XtSetSensitive(some_widgets->techniques_widget, FALSE);


 universal_widget = optionsStruct->widget_array[9]->widget_array[0]->parent;
 if(universal_widget != NULL)
	{
	some_widgets->universal_widget = universal_widget;
	XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Universal...", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(universal_widget, wargs, 1);
	}

 non_universal_widget = optionsStruct->widget_array[9]->widget_array[1]->parent;
 some_widgets->non_universal_widget = non_universal_widget;
 if(non_universal_widget != NULL)
	{
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>U");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + U", XmSTRING_DEFAULT_CHARSET));
	XtSetArg(wargs[2], XmNlabelString, XmStringCreate("Non-universal...", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(non_universal_widget, wargs, 3);
	}

 showDeletedSegments_widget = displayStruct->widget_array[0]->parent;
 if(showDeletedSegments_widget != NULL)
	{
	some_widgets->showDeletedSegments_widget = showDeletedSegments_widget;
	XtVaSetValues(showDeletedSegments_widget,
		     XmNaccelerator,     "Ctrl<KeyPress>D",
		     XmNacceleratorText, XmStringCreateLtoR("Ctrl + D", XmSTRING_DEFAULT_CHARSET),
		     XmNsensitive,       FALSE,
		     NULL);
	}

 showRunSegments_widget = displayStruct->widget_array[1]->parent;
 if(showRunSegments_widget != NULL)
	{
	some_widgets->showRunSegments_widget = showRunSegments_widget;
	XtVaSetValues(showRunSegments_widget,
		     XmNaccelerator,     "Ctrl<KeyPress>S",
		     XmNacceleratorText, XmStringCreateLtoR("Ctrl + S", XmSTRING_DEFAULT_CHARSET),
		     XmNsensitive,       FALSE,
		     NULL);
	}


 showOperationsTable_widget = displayStruct->widget_array[2]->parent;
 if(showOperationsTable_widget != NULL)
	{
	some_widgets->showOperationsTable_widget = showOperationsTable_widget;
	XtSetSensitive(showOperationsTable_widget, FALSE);
	}


 showRatingCurve_widget = displayStruct->widget_array[3]->parent;
 if(showRatingCurve_widget != NULL)
	{
	some_widgets->showRatingCurve_widget = showRatingCurve_widget;
	XtSetSensitive(showRatingCurve_widget, FALSE);
	}

 /*     Set the Forecast Group Topology Widget to Insensitive...        */
 some_widgets->showFGroup_Topology_widget = displayStruct->widget_array[4]->parent;
 XtSetSensitive(displayStruct->widget_array[4]->parent, FALSE);

 /*     Set the Current Run-time Mods Widget to Insensitive...          */
 some_widgets->showCurrentMods_widget = displayStruct->widget_array[5]->parent;
 XtSetSensitive(displayStruct->widget_array[5]->parent, FALSE);


 showTulsaPlot_widget = modsStruct->widget_array[0]->parent;
 if(showTulsaPlot_widget != NULL)
	{
	some_widgets->showTulsaPlot_widget = showTulsaPlot_widget;
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>T");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + T", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(showTulsaPlot_widget, wargs, 2);
	XtSetSensitive(showTulsaPlot_widget, FALSE);
	}

 showTimeSeriesTable_widget = modsStruct->widget_array[1]->parent;
 if(showTimeSeriesTable_widget != NULL)
	{
	some_widgets->showTimeSeriesTable_widget = showTimeSeriesTable_widget;
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>E");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + E", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(showTimeSeriesTable_widget, wargs, 2);
	XtSetSensitive(showTimeSeriesTable_widget, FALSE);
	}

 showPlotTS_widget = modsStruct->widget_array[2]->parent;
 if(showPlotTS_widget != NULL)
	{
	some_widgets->showPlotTS_widget = showPlotTS_widget;
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>P");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + P", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(showPlotTS_widget, wargs, 2);
	XtSetSensitive(showPlotTS_widget, FALSE);
	}

 showOtherMods_widget = modsStruct->widget_array[3]->parent;
 if(showOtherMods_widget != NULL)
	{
	some_widgets->showOtherMods_widget = showOtherMods_widget;
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>M");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + M", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(showOtherMods_widget, wargs, 2);
	XtSetSensitive(showOtherMods_widget, FALSE);
	}

/* Set the Mods Viewer button insensitive and add an accelerator - dp 04/30/97 */
 showModsViewer_widget = modsStruct->widget_array[4]->parent;
 if(showModsViewer_widget != NULL)
 {
	some_widgets->showModsViewer_widget = showModsViewer_widget;
	XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>V");
	XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + V", XmSTRING_DEFAULT_CHARSET));
	XtSetValues(showModsViewer_widget, wargs, 2);
	XtSetSensitive(showModsViewer_widget, FALSE);
 }

/*      Help Menu Items                                 */
 some_widgets->version_ToggleButton = HelpData_struct->widget_array[1]->parent; /* Version ToggleButton */


 create_IFP_Mapping_interface(form, menu_bar, some_widgets);
 create_Version_Dialog(some_widgets, IFP_version_and_date);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/create_interface.c,v $";
 static char rcs_id2[] = "$Id: create_interface.c,v 1.6 2006/04/07 13:29:34 aivo Exp $";}
/*  ===================================================  */

}


