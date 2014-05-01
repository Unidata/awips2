
/* ************************************************************************************************************

	univ_techs.c :
			source file containing the code to create the 'Universal' & 'Non-universal'
			"techniques" popup windows to set various units and time-zone preferences
			for input, computation, and output (display) in NWSRFS and the Interactive
			Forecast Program (ie, the graphical user interface to NWSRFS).

	Coded by     :  Tom Adams (NWS/Office of Hydrology/Hydrologic Research Laboratory)
	Date         :  3/12/91
	Revised      :


   ************************************************************************************************************ */



#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "ifp_help.h"
#include "help.h"
#include "c_call_f/retrieve_hcl_techs_args.h"
#include "c_call_f/fctzc.h"
extern xs_menu_widget_struct *xs_create_menu_buttons(title, menu, menulist,
nitems);
void post_univ_techniques_atoms(Widget);
static xs_menu_struct Input_Standard_menu_struct[] =
	{
	{"EST" , set_Input_EST, NULL, TRUE, PUSH_BUTTON},
	{"CST" , set_Input_CST, NULL, TRUE, PUSH_BUTTON},
	{"MST" , set_Input_MST, NULL, TRUE, PUSH_BUTTON},
	{"PST" , set_Input_PST, NULL, TRUE, PUSH_BUTTON},
	{"AST" , set_Input_AST, NULL, TRUE, PUSH_BUTTON},
	{"HST" , set_Input_HST, NULL, TRUE, PUSH_BUTTON},
	{"NST" , set_Input_NST, NULL, TRUE, PUSH_BUTTON}
	};

static xs_menu_struct Input_Daylight_menu_struct[] =
	{
	{"EDT" , set_Input_EDT, NULL, TRUE, PUSH_BUTTON},
	{"CDT" , set_Input_CDT, NULL, TRUE, PUSH_BUTTON},
	{"MDT" , set_Input_MDT, NULL, TRUE, PUSH_BUTTON},
	{"PDT" , set_Input_PDT, NULL, TRUE, PUSH_BUTTON},
	{"ADT" , set_Input_ADT, NULL, TRUE, PUSH_BUTTON},
	{"HDT" , set_Input_HDT, NULL, TRUE, PUSH_BUTTON},
	{"NDT" , set_Input_NDT, NULL, TRUE, PUSH_BUTTON}
	};

static xs_menu_struct Input_timeZone_menu_struct[] =
	{
	{"Z" , set_Input_Z, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Standard" , NULL, NULL, TRUE, PUSH_BUTTON, Input_Standard_menu_struct, XtNumber(Input_Standard_menu_struct), ""},
	{"Daylight Savings" , NULL, NULL, TRUE, PUSH_BUTTON, Input_Daylight_menu_struct, XtNumber(Input_Daylight_menu_struct), ""}
	};


static xs_menu_struct Output_Standard_menu_struct[] =
	{
	{"EST" , set_Output_EST, NULL, TRUE, PUSH_BUTTON},
	{"CST" , set_Output_CST, NULL, TRUE, PUSH_BUTTON},
	{"MST" , set_Output_MST, NULL, TRUE, PUSH_BUTTON},
	{"PST" , set_Output_PST, NULL, TRUE, PUSH_BUTTON},
	{"AST" , set_Output_AST, NULL, TRUE, PUSH_BUTTON},
	{"HST" , set_Output_HST, NULL, TRUE, PUSH_BUTTON},
	{"NST" , set_Output_NST, NULL, TRUE, PUSH_BUTTON}
	};

static xs_menu_struct Output_Daylight_menu_struct[] =
	{
	{"EDT" , set_Output_EDT, NULL, TRUE, PUSH_BUTTON},
	{"CDT" , set_Output_CDT, NULL, TRUE, PUSH_BUTTON},
	{"MDT" , set_Output_MDT, NULL, TRUE, PUSH_BUTTON},
	{"PDT" , set_Output_PDT, NULL, TRUE, PUSH_BUTTON},
	{"ADT" , set_Output_ADT, NULL, TRUE, PUSH_BUTTON},
	{"HDT" , set_Output_HDT, NULL, TRUE, PUSH_BUTTON},
	{"NDT" , set_Output_NDT, NULL, TRUE, PUSH_BUTTON}
	};

static xs_menu_struct Output_timeZone_menu_struct[] =
	{
	{"Z" , set_Output_Z, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Standard" , NULL, NULL, TRUE, PUSH_BUTTON, Output_Standard_menu_struct, XtNumber(Output_Standard_menu_struct), ""},
	{"Daylight Savings" , NULL, NULL, TRUE, PUSH_BUTTON, Output_Daylight_menu_struct, XtNumber(Output_Daylight_menu_struct), ""}
	};


static xs_menu_struct Mods_Standard_menu_struct[] =
	{
	{"EST" , set_Mods_EST, NULL, TRUE, PUSH_BUTTON},
	{"CST" , set_Mods_CST, NULL, TRUE, PUSH_BUTTON},
	{"MST" , set_Mods_MST, NULL, TRUE, PUSH_BUTTON},
	{"PST" , set_Mods_PST, NULL, TRUE, PUSH_BUTTON},
	{"AST" , set_Mods_AST, NULL, TRUE, PUSH_BUTTON},
	{"HST" , set_Mods_HST, NULL, TRUE, PUSH_BUTTON},
	{"NST" , set_Mods_NST, NULL, TRUE, PUSH_BUTTON}
	};

static xs_menu_struct Mods_Daylight_menu_struct[] =
	{
	{"EDT" , set_Mods_EDT, NULL, TRUE, PUSH_BUTTON},
	{"CDT" , set_Mods_CDT, NULL, TRUE, PUSH_BUTTON},
	{"MDT" , set_Mods_MDT, NULL, TRUE, PUSH_BUTTON},
	{"PDT" , set_Mods_PDT, NULL, TRUE, PUSH_BUTTON},
	{"ADT" , set_Mods_ADT, NULL, TRUE, PUSH_BUTTON},
	{"HDT" , set_Mods_HDT, NULL, TRUE, PUSH_BUTTON},
	{"NDT" , set_Mods_NDT, NULL, TRUE, PUSH_BUTTON}
	};

static xs_menu_struct Mods_timeZone_menu_struct[] =
	{
	{"Z" , set_Mods_Z, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Standard" , NULL, NULL, TRUE, PUSH_BUTTON, Mods_Standard_menu_struct, XtNumber(Mods_Standard_menu_struct), ""},
	{"Daylight Savings" , NULL, NULL, TRUE, PUSH_BUTTON, Mods_Daylight_menu_struct, XtNumber(Mods_Daylight_menu_struct), ""}
	};





/* *****************************************************************************

	create_Universal_Tech_popup()

   ***************************************************************************** */

void create_Universal_Tech_popup(the_widget_struct *some_widgets)
{

	Arg             wargs[12];
	Display         *display;
	Widget          popup, form, rc_widget, pushButton_rc_widget;
	Widget          timeZone_frame_widget, units_frame_widget, misc_frame_widget;
	Widget          input_separator, mods_separator, output_separator, separator_widget;
	Widget          ok_widget, cancel_widget, help_widget;

	int             n, pixel, backgroundColor;
	int             i;
	int             menuItem, sub_menuItem;

	char            *output_time_zone_code;
	char            string[5];
	char            *first_blank;

	Widget          timeZone_bb, timeZone_separator_widget;
	Widget          input_menu, output_menu, mods_menu;
	Widget          input_control, output_control, mods_control;
	Widget          history_widget;

	Widget          units_bb, units_separator_widget;
	Widget          units_rc_widget, units_generalFrame, units_modsFrame;
	Widget          sac_rc_widget, api_rc_widget, gen_rc_widget;
	Widget          units_generalFrame_bb, units_modsFrame_bb, bb_for_radioBox_rc;
	Widget          units_general_radioBox, units_sac_radioBox, units_api_radioBox, units_gen_radioBox;
	Widget          english_toggleButton, metric_toggleButton;
	Widget          sac_english_toggleButton, sac_metric_toggleButton;
	Widget          api_english_toggleButton, api_metric_toggleButton;
	Widget          general_english_toggleButton, general_metric_toggleButton;
	Widget          sac_heading_frame, api_heading_frame, gen_heading_frame;
	Widget          modsUnits_rc_widget, modsHeadings_rc_widget;


	Widget          misc_bb, misc_separator_widget;
	Widget          warning_widget, futurePrecip_widget;
	Widget          help_shell;

	help_struct             *help_data;
	help_cb_struct          *context_help;
	xs_menu_widget_struct   *input_widget_array;
	xs_menu_widget_struct   *output_widget_array;
	xs_menu_widget_struct   *mods_widget_array;



output_time_zone_code = (char *) malloc(5);

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Universal Techniques", XmSTRING_DEFAULT_CHARSET));
popup = XtCreatePopupShell
			(
			"universal_shell",
			transientShellWidgetClass,
			some_widgets->tree_shell,
			NULL,
			0
			);
some_widgets->universalTech_popup_shell = popup;

pixel = get_pixel_by_name(popup, "yellow");

/*      Create a transientShell for the context sensitive help window...                                */
help_shell = XtCreatePopupShell("non_univ_tech_help_shell", transientShellWidgetClass, global_toplevel, NULL, 0);


n = 0;
form = XtCreateManagedWidget
		(
		"universal_form",
		xmFormWidgetClass,
		popup,
		wargs,
		n
		);


n = 0;
XtSetArg(wargs[n], XmNnumColumns, 3); n++;
XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
rc_widget = XtCreateManagedWidget
		(
		"universal_rc_widget",
		xmRowColumnWidgetClass,
		form,
		wargs,
		n
		);


n = 0;
XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
XtSetArg(wargs[n], XmNtopWidget, rc_widget); n++;
XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
separator_widget = XtCreateManagedWidget
		(
		"universal_top_separator_widget",
		xmSeparatorWidgetClass,
		form,
		wargs,
		n
		);

n = 0;
XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
XtSetArg(wargs[n], XmNtopWidget, separator_widget); n++;
XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
pushButton_rc_widget = XtCreateManagedWidget
		(
		"pushButton_rc_widget",
		xmBulletinBoardWidgetClass,
		form,
		wargs,
		n
		);

n = 0;
XtSetArg(wargs[n], XtNheight, 300); n++;
timeZone_frame_widget = XtCreateManagedWidget
		(
		"timeZone_frame_widget",
		xmFrameWidgetClass,
		rc_widget,
		wargs,
		n
		);

n = 0;
XtSetArg(wargs[n], XtNheight, 300); n++;
units_frame_widget = XtCreateManagedWidget
		(
		"units_frame_widget",
		xmFrameWidgetClass,
		rc_widget,
		wargs,
		n
		);

n = 0;
XtSetArg(wargs[n], XtNheight, 300); n++;
misc_frame_widget = XtCreateManagedWidget
		(
		"misc_frame_widget",
		xmFrameWidgetClass,
		rc_widget,
		wargs,
		n
		);



n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Set", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNwidth, 100); n++;
ok_widget = XtCreateManagedWidget
		(
		"universal_ok_widget",
		xmPushButtonWidgetClass,
		pushButton_rc_widget,
		wargs,
		n
		);
XtAddCallback(ok_widget, XmNactivateCallback, pop_down_shell, popup);
XtAddCallback(ok_widget, XmNactivateCallback, change_univ_Techniques_struct, NULL);
XtAddCallback(ok_widget, XmNactivateCallback, update_Dates_timeZone_code, NULL);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "universal_ok_widget";
XtAddEventHandler(ok_widget, EnterWindowMask, FALSE, help_event_handler, context_help);
some_widgets->universalTech_ok_widget = ok_widget;

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Cancel", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNwidth, 100); n++;
cancel_widget = XtCreateManagedWidget
		(
		"universal_cancel_widget",
		xmPushButtonWidgetClass,
		pushButton_rc_widget,
		wargs,
		n
		);
XtAddCallback(cancel_widget, XmNactivateCallback, pop_down_shell, popup);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "universal_cancel_widget";
XtAddEventHandler(cancel_widget, EnterWindowMask, FALSE, help_event_handler, context_help);

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNwidth, 100); n++;
help_widget = XtCreateManagedWidget
		(
		"universal_help_widget",
		xmPushButtonWidgetClass,
		pushButton_rc_widget,
		wargs,
		n
		);

help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = popup;
help_data->message_widget_name = "universal_help_widget";

XtAddCallback(help_widget, XmNactivateCallback, popup_help_window, "UNIV_TECHNIQUES");

XtSetArg(wargs[0], XtNbackground, &backgroundColor);
XtGetValues(ok_widget, wargs, 1);
XtSetArg(wargs[0], XmNborderColor, backgroundColor);
XtSetValues(ok_widget, wargs, 1);
XtSetArg(wargs[0], XmNborderColor, backgroundColor);
XtSetValues(cancel_widget, wargs, 1);
XtSetArg(wargs[0], XmNborderColor, backgroundColor);
XtSetValues(help_widget, wargs, 1);

/* -----------------------------      Time Zone widgets     --------------------------------- */


timeZone_bb = XtCreateManagedWidget
		(
		"timeZone_bb",
		xmBulletinBoardWidgetClass,
		timeZone_frame_widget,
		NULL,
		0
		);

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Time Zone", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("time_zone_heading", xmLabelWidgetClass, timeZone_bb, wargs, 1);

timeZone_separator_widget = XtCreateManagedWidget
		(
		"timeZone_separator_widget",
		xmSeparatorWidgetClass,
		timeZone_bb,
		NULL,
		0
		);

 XtCreateManagedWidget("Input", xmLabelWidgetClass, timeZone_bb, NULL, 0);
 XtCreateManagedWidget("Output", xmLabelWidgetClass, timeZone_bb, NULL, 0);
 XtCreateManagedWidget("Mods", xmLabelWidgetClass, timeZone_bb, NULL, 0);


 XtSetArg(wargs[0], XmNwidth, 100);
 input_menu = XmCreatePulldownMenu(timeZone_bb, "input_tz_menu", wargs, 1);

 XtSetArg(wargs[0], XmNwidth, 100);
 output_menu = XmCreatePulldownMenu(timeZone_bb, "output_tz_menu", wargs, 1);

 XtSetArg(wargs[0], XmNwidth, 100);
 mods_menu = XmCreatePulldownMenu(timeZone_bb, "mods_tz_menu", wargs, 1);


 some_widgets->inputFrame_label = input_menu;
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "inputFrame_label";
 XtAddEventHandler(input_menu, EnterWindowMask, FALSE, help_event_handler, context_help);


 some_widgets->outputFrame_label = output_menu;
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "outputFrame_label";
 XtAddEventHandler(output_menu, EnterWindowMask, FALSE, help_event_handler, context_help);

 some_widgets->modsFrame_label = mods_menu;
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "modsFrame_label";
 XtAddEventHandler(mods_menu, EnterWindowMask, FALSE, help_event_handler, context_help);



 /*      Create pulldown menu items for 'Input Time_Zone'...                            */

 memset(string, '\0', 5);
 if((first_blank = strstr(universalTechniques->input_time_zone_code, " ")) != NULL)
	strncpy(string, universalTechniques->input_time_zone_code,
		first_blank - universalTechniques->input_time_zone_code);

 for(i = 0; i < XtNumber(Input_timeZone_menu_struct); i++)
	{
	Input_timeZone_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(string, Input_timeZone_menu_struct[i].name) == 0)
		{
		menuItem = 0;
		sub_menuItem = 0;
		}
	}
 for(i = 0; i < XtNumber(Input_Standard_menu_struct); i++)
	{
	Input_Standard_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(string, Input_Standard_menu_struct[i].name) == 0)
		{
		menuItem = 1;
		sub_menuItem = i;
		}
	}
 for(i = 0; i < XtNumber(Input_Daylight_menu_struct); i++)
	{
	Input_Daylight_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(string, Input_Daylight_menu_struct[i].name) == 0)
		{
		menuItem = 2;
		sub_menuItem = i;
		}
	}

 input_widget_array =
	(xs_menu_widget_struct *)xs_create_menu_buttons("", input_menu, Input_timeZone_menu_struct, XtNumber(Input_timeZone_menu_struct));


 if(menuItem == 0) history_widget = input_widget_array->widget_array[menuItem]->parent;
 else history_widget = input_widget_array->widget_array[menuItem]->widget_array[sub_menuItem]->parent;

 /*              Create a  Option Menu                                                  */
 n = 0;
 XtSetArg(wargs[n], XmNmenuHistory, history_widget); n++;
 XtSetArg(wargs[n], XmNwhichButton, (unsigned int) 1); n++;
 XtSetArg(wargs[n], XmNsubMenuId, input_menu); n++;
 input_control = XmCreateOptionMenu(timeZone_bb, "input_tz_control", wargs, n);
 XtManageChild(input_control);


 /*      Create pulldown menu items for 'Output Time_Zone'...                           */

 strcpy(output_time_zone_code, get_output_time_zone_code());

 for(i = 0; i < XtNumber(Output_timeZone_menu_struct); i++)
	{
	Output_timeZone_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(output_time_zone_code, Output_timeZone_menu_struct[i].name) == 0)
		{
		menuItem = 0;
		sub_menuItem = 0;
		}
	}
 for(i = 0; i < XtNumber(Output_Standard_menu_struct); i++)
	{
	Output_Standard_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(output_time_zone_code, Output_Standard_menu_struct[i].name) == 0)
		{
		menuItem = 1;
		sub_menuItem = i;
		}
	}
 for(i = 0; i < XtNumber(Output_Daylight_menu_struct); i++)
	{
	Output_Daylight_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(output_time_zone_code, Output_Daylight_menu_struct[i].name) == 0)
		{
		menuItem = 2;
		sub_menuItem = i;
		}
	}

 output_widget_array =
	(xs_menu_widget_struct *)xs_create_menu_buttons("", output_menu, Output_timeZone_menu_struct, XtNumber(Output_timeZone_menu_struct));


 if(menuItem == 0) history_widget = output_widget_array->widget_array[menuItem]->parent;
 else history_widget = output_widget_array->widget_array[menuItem]->widget_array[sub_menuItem]->parent;

 /*              Create a  Option Menu                                                  */
 n = 0;
 XtSetArg(wargs[n], XmNmenuHistory, history_widget); n++;
 XtSetArg(wargs[n], XmNwhichButton, (unsigned int) 1); n++;
 XtSetArg(wargs[n], XmNsubMenuId, output_menu); n++;
 output_control = XmCreateOptionMenu(timeZone_bb, "output_tz_control", wargs, n);
 XtManageChild(output_control);


 /*      Create pulldown menu items for 'Mods Time_Zone'...                             */

 memset(string, '\0', 5);
 if((first_blank = strstr(universalTechniques->mod_time_zone_code, " ")) != NULL)
	strncpy(string, universalTechniques->mod_time_zone_code,
		first_blank - universalTechniques->mod_time_zone_code);

 for(i = 0; i < XtNumber(Mods_timeZone_menu_struct); i++)
	{
	Mods_timeZone_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(string, Mods_timeZone_menu_struct[i].name) == 0)
		{
		menuItem = 0;
		sub_menuItem = 0;
		}
	}
 for(i = 0; i < XtNumber(Mods_Standard_menu_struct); i++)
	{
	Mods_Standard_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(string, Mods_Standard_menu_struct[i].name) == 0)
		{
		menuItem = 1;
		sub_menuItem = i;
		}
	}
 for(i = 0; i < XtNumber(Mods_Daylight_menu_struct); i++)
	{
	Mods_Daylight_menu_struct[i].data = (caddr_t) some_widgets;
	if(strcmp(string, Mods_Daylight_menu_struct[i].name) == 0)
		{
		menuItem = 2;
		sub_menuItem = i;
		}
	}

 mods_widget_array =
      (xs_menu_widget_struct *)xs_create_menu_buttons("", mods_menu, Mods_timeZone_menu_struct, XtNumber(Mods_timeZone_menu_struct));

 if(menuItem == 0) history_widget = mods_widget_array->widget_array[menuItem]->parent;
 else history_widget = mods_widget_array->widget_array[menuItem]->widget_array[sub_menuItem]->parent;

 /*              Create a  Option Menu                                                  */
 n = 0;
 XtSetArg(wargs[n], XmNmenuHistory, history_widget); n++;
 XtSetArg(wargs[n], XmNwhichButton, (unsigned int) 1); n++;
 XtSetArg(wargs[n], XmNsubMenuId, mods_menu); n++;
 mods_control = XmCreateOptionMenu(timeZone_bb, "mods_tz_control", wargs, n);
 XtManageChild(mods_control);



/* ---------------------------------      Units widgets     ---------------------------------- */


units_bb = XtCreateManagedWidget
		(
		"units_bb",
		xmBulletinBoardWidgetClass,
		units_frame_widget,
		NULL,
		0
		);

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Units", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("units_heading", xmLabelWidgetClass, units_bb, wargs, 1);

units_separator_widget = XtCreateManagedWidget
		(
		"units_separator_widget",
		xmSeparatorWidgetClass,
		units_bb,
		NULL,
		0
		);



units_generalFrame = XtCreateManagedWidget("units_generalFrame", xmFrameWidgetClass, units_bb, NULL, 0);
units_modsFrame = XtCreateManagedWidget("units_modsFrame", xmFrameWidgetClass, units_bb, NULL, 0);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("General", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("general_heading", xmLabelWidgetClass, units_bb, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Run-time Modifications", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("mods_heading", xmLabelWidgetClass, units_bb, wargs, 1);



units_generalFrame_bb = XtCreateManagedWidget
			(
			"units_generalFrame_bb",
			xmBulletinBoardWidgetClass,
			units_generalFrame,
			NULL,
			0
			);

units_modsFrame_bb = XtCreateManagedWidget
			(
			"units_modsFrame_bb",
			xmBulletinBoardWidgetClass,
			units_modsFrame,
			NULL,
			0
			);



modsHeadings_rc_widget = XtCreateManagedWidget("modsHeadings_rc_widget", xmRowColumnWidgetClass, units_modsFrame_bb, NULL, 0);
modsUnits_rc_widget = XtCreateManagedWidget("modsUnits_rc_widget", xmRowColumnWidgetClass, units_modsFrame_bb, NULL, 0);


units_general_radioBox = XmCreateRadioBox(units_generalFrame_bb, "units_general_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "units_general_radioBox";
XtAddEventHandler(units_general_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);

XtManageChild(units_general_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("English", XmSTRING_DEFAULT_CHARSET));
english_toggleButton = (Widget)XmCreateToggleButtonGadget(units_general_radioBox, "english_toggleButton", wargs, 2);
XtManageChild(english_toggleButton);
XtAddCallback(english_toggleButton, XmNvalueChangedCallback, english_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("Metric", XmSTRING_DEFAULT_CHARSET));
metric_toggleButton = (Widget)XmCreateToggleButtonGadget(units_general_radioBox, "metric_toggleButton", wargs, 2);
XtManageChild(metric_toggleButton);
XtAddCallback(metric_toggleButton, XmNvalueChangedCallback, metric_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNset, TRUE);
if(universalTechniques->metric_units == METRIC_UNITS) XtSetValues(metric_toggleButton, wargs, 1);
else XtSetValues(english_toggleButton, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("English", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("mods_english_heading", xmLabelWidgetClass, modsHeadings_rc_widget, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Metric", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("mods_metric_heading", xmLabelWidgetClass, modsHeadings_rc_widget, wargs, 1);


/* --------------------------------- General units ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("General", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("gen_heading", xmLabelWidgetClass, modsUnits_rc_widget, wargs, 1);

units_gen_radioBox = XmCreateRadioBox(modsUnits_rc_widget, "units_gen_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "units_gen_radioBox";
XtAddEventHandler(units_gen_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(units_gen_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR(" ", XmSTRING_DEFAULT_CHARSET));
general_english_toggleButton = (Widget)XmCreateToggleButtonGadget(units_gen_radioBox, "general_english_toggleButton", wargs, 2);
XtManageChild(general_english_toggleButton);
XtAddCallback(general_english_toggleButton, XmNvalueChangedCallback, general_english_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR(" ", XmSTRING_DEFAULT_CHARSET));
general_metric_toggleButton = (Widget)XmCreateToggleButtonGadget(units_gen_radioBox, "general_metric_toggleButton", wargs, 2);
XtManageChild(general_metric_toggleButton);
XtAddCallback(general_metric_toggleButton, XmNvalueChangedCallback, general_metric_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNset, TRUE);
if(universalTechniques->mod_units == METRIC_UNITS) XtSetValues(general_metric_toggleButton, wargs, 1);
else XtSetValues(general_english_toggleButton, wargs, 1);


/* ------------------------------------- SAC units ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("SAC", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("sac_heading", xmLabelWidgetClass, modsUnits_rc_widget, wargs, 1);

units_sac_radioBox = XmCreateRadioBox(modsUnits_rc_widget, "units_sac_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "units_sac_radioBox";
XtAddEventHandler(units_sac_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(units_sac_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR(" ", XmSTRING_DEFAULT_CHARSET));
sac_english_toggleButton = (Widget)XmCreateToggleButtonGadget(units_sac_radioBox, "sac_english_toggleButton", wargs, 2);
XtManageChild(sac_english_toggleButton);
XtAddCallback(sac_english_toggleButton, XmNvalueChangedCallback, sac_english_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR(" ", XmSTRING_DEFAULT_CHARSET));
sac_metric_toggleButton = (Widget)XmCreateToggleButtonGadget(units_sac_radioBox, "sac_metric_toggleButton", wargs, 2);
XtManageChild(sac_metric_toggleButton);
XtAddCallback(sac_metric_toggleButton, XmNvalueChangedCallback, sac_metric_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNset, TRUE);
if(universalTechniques->mod_sac_units == METRIC_UNITS) XtSetValues(sac_metric_toggleButton, wargs, 1);
else XtSetValues(sac_english_toggleButton, wargs, 1);

/* ------------------------------------- API units ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("API", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("api_heading", xmLabelWidgetClass, modsUnits_rc_widget, wargs, 1);

units_api_radioBox = XmCreateRadioBox(modsUnits_rc_widget, "units_api_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "units_api_radioBox";
XtAddEventHandler(units_api_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(units_api_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR(" ", XmSTRING_DEFAULT_CHARSET));
api_english_toggleButton = (Widget)XmCreateToggleButtonGadget(units_api_radioBox, "api_english_toggleButton", wargs, 2);
XtManageChild(api_english_toggleButton);
XtAddCallback(api_english_toggleButton, XmNvalueChangedCallback, api_english_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR(" ", XmSTRING_DEFAULT_CHARSET));
api_metric_toggleButton = (Widget)XmCreateToggleButtonGadget(units_api_radioBox, "api_metric_toggleButton", wargs, 2);
XtManageChild(api_metric_toggleButton);
XtAddCallback(api_metric_toggleButton, XmNvalueChangedCallback, api_metric_toggleButton_callback, NULL);

XtSetArg(wargs[0], XmNset, TRUE);
if(universalTechniques->mod_api_units == METRIC_UNITS) XtSetValues(api_metric_toggleButton, wargs, 1);
else XtSetValues(api_english_toggleButton, wargs, 1);




/* -----------------------------      Miscellaneous widgets     ------------------------------ */


misc_bb = XtCreateManagedWidget
		(
		"misc_bb",
		xmBulletinBoardWidgetClass,
		misc_frame_widget,
		NULL,
		0
		);

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Miscellaneous", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("misc_heading", xmLabelWidgetClass, misc_bb, wargs, 1);

misc_separator_widget = XtCreateManagedWidget
		(
		"misc_separator_widget",
		xmSeparatorWidgetClass,
		misc_bb,
		NULL,
		0
		);



XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("Mods Warning", XmSTRING_DEFAULT_CHARSET));
warning_widget = (Widget)XmCreateToggleButton(misc_bb, "warning_widget", wargs, 2);
XtManageChild(warning_widget);
XtAddCallback(warning_widget, XmNvalueChangedCallback, warning_callback, NULL);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "warning_widget";
XtAddEventHandler(warning_widget, EnterWindowMask, FALSE, help_event_handler, context_help);


if(universalTechniques->mod_warning == ON) XtSetArg(wargs[0], XmNset, TRUE);
else XtSetArg(wargs[0], XmNset, FALSE);
XtSetValues(warning_widget, wargs, 1);

/* Changed the widget used for the Future precip technique from Toggle button
   to a scale to work with the new way FUTPRECP technique works. 
   The resources for minimum, maximum, and y are stored in the resource file.
   dp - 14 June 95
*/
n=0;
XtSetArg(wargs[n], XmNtitleString,
	 XmStringCreate("Future Precipitation", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XmNvalue, universalTechniques->future_precip); n++;
XtSetArg(wargs[n], XmNprocessingDirection, XmMAX_ON_RIGHT); n++;
XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
XtSetArg(wargs[n], XmNshowValue, TRUE); n++;
	 
futurePrecip_widget = XmCreateScale(misc_bb, "futurePrecip_widget", wargs, n);
XtManageChild(futurePrecip_widget);
XtAddCallback(futurePrecip_widget, XmNvalueChangedCallback, futurePrecip_callback, NULL);
	 
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "futurePrecip_widget";
XtAddEventHandler(futurePrecip_widget, EnterWindowMask, FALSE, help_event_handler, context_help);

}


/* ************************************************************************

	post_menu_handler()

   ************************************************************************ */

void post_menu_handler(Widget w, Widget menu, XEvent *event)
{

	Arg             wargs[1];
	int             button;

XtSetArg(wargs[0], XmNwhichButton, &button);
XtGetValues(menu, wargs, 1);

if(event->xbutton.button == button)
	{
	XmMenuPosition(menu, (XButtonPressedEvent *)event);
	XtManageChild(menu);
	}

}



/* ************************************************************************

	create_univ_Techniques_struct()

   ************************************************************************ */

void create_univ_Techniques_struct(the_widget_struct *someWidgets)
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



 universalTechniques = (univ_techniques_struct *) malloc(sizeof(univ_techniques_struct));
 temp_univ_Techniques = (univ_techniques_struct *) malloc(sizeof(univ_techniques_struct));

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

 /*     Fill 'MODUNITS' value...                */
 universalTechniques->mod_units = a_iumgen;

 /*     Fill 'MODSACUN' value...                */
 universalTechniques->mod_sac_units = a_iumsac;

 /*     Fill 'MODAPIUN' value...                */
 universalTechniques->mod_api_units = a_iumapi;

 /*     Fill 'METRIC' value...                  */
 universalTechniques->metric_units = a_metric;

 /*     Fill 'MODWARN' value...                 */
 universalTechniques->mod_warning = a_modwrn;

 /*     Fill 'FUTPRECP' value...                */
 universalTechniques->future_precip = a_ifpr;

 /*     Fill 'NOUTDS' value...                  */
 universalTechniques->output_daylight_savings = a_noutds;

 /*     Fill 'NOUTZ' value...                   */
 universalTechniques->output_time_zone = a_noutz;

 /*     Fill 'INPTZC' value...                  */
 strcpy(universalTechniques->input_time_zone_code, a_inptzc);

 /*     Fill 'MODTZC' value...                  */
 strcpy(universalTechniques->mod_time_zone_code, a_modtzc);

 *temp_univ_Techniques = *universalTechniques;
}

/* ************************************************************************

	post_univ_techniques_atoms()

   ************************************************************************ */

void post_univ_techniques_atoms(Widget w)
{

   Display    *display;
   Window     root;
   
display = XtDisplay(w);
root = DefaultRootWindow(display);
/* Now set the window properties for the units */

XChangeProperty(
	display,
	root,
	IFPA_general_units,
	IFPA_general_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(universalTechniques->metric_units),
	sizeof(int)
	);

XChangeProperty(
	display,
	root,
	IFPA_mods_general_units,
	IFPA_mods_general_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(universalTechniques->mod_units),
	sizeof(int)
	);

XChangeProperty(
	display,
	root,
	IFPA_mods_API_units,
	IFPA_mods_API_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(universalTechniques->mod_api_units),
	sizeof(int)
	);

XChangeProperty(
	display,
	root,
	IFPA_mods_SAC_units,
	IFPA_mods_SAC_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(universalTechniques->mod_sac_units),
	sizeof(int)
	);


}



/* ************************************************************************

	get_output_time_zone_code()
		retrieves the 'output_time_zone_code' from:

		universalTechniques->output_daylight_savings &
		universalTechniques->output_time_zone;

		-- returns a string (pointer) for displaying the time
		zone code;

   ************************************************************************ */

char *get_output_time_zone_code()
{

	char            code_string[4];
	char            *timeZoneCode;
	char            *string;
	char            *first_blank;



 timeZoneCode = (char *) malloc(5);
 string = (char *) malloc(5);
 memset(timeZoneCode, '\0', 5);
 memset(string, '\0', 5);

 FCTZC(&(universalTechniques->output_time_zone), &(universalTechniques->output_daylight_savings), code_string);

 strncpy(string, code_string, 4);
 if((first_blank = strstr(string, " ")) == NULL)
	strncpy(timeZoneCode, string, 4);
 else   strncpy(timeZoneCode, string, first_blank - string);


 return(timeZoneCode);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/univ_techs.c,v $";
 static char rcs_id2[] = "$Id: univ_techs.c,v 1.5 2006/04/07 13:30:51 aivo Exp $";}
/*  ===================================================  */

}

