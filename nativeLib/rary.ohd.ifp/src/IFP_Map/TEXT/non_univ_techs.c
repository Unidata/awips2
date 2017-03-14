
/* ************************************************************************************************************

	non_univ_techs.c :
			source file containing the code to create the 'Non-universal'"techniques"
			popup windows to set various units and time-zone preferences for input,
			computation, and output (display) in NWSRFS and the Interactive Forecast
			Program (ie, the graphical user interface to NWSRFS).

	Coded by     :  Tom Adams (NWS/Office of Hydrology/Hydrologic Research Laboratory)
	Date         :  3/12/91
	Revised      :  AV added SACSNOW techniques  3/07/01    


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






/* *****************************************************************************

	create_nonUniversal_Tech_popup()

   ***************************************************************************** */

void create_nonUniversal_Tech_popup(some_widgets)
	the_widget_struct       *some_widgets;
{

	Arg             wargs[15];
	Display         *display;
	Widget          popup, form, rc_widget, pushButton_rc_widget, bb;

	Widget          timeZone_frame_widget, units_frame_widget, misc_frame_widget;
	Widget          input_separator, mods_separator, output_separator, separator_widget;
	Widget          pushButton_bb_widget, ok_widget, cancel_widget, help_widget;
	int             n, pixel, backgroundColor;
	int             i;

	Widget          snowModel_rc_widget, applyTo_rc_widget, applyTo_Frame, snowModel_Frame;
	Widget          snow_Frame_bb, applyTo_Frame_bb;
	Widget          rc_for_snow_headings;
	Widget          applyTo_radioBox, all_toggleButton, selected_toggleButton;

	Widget          snow_radioBox, snow_on_toggleButton, snow_off_toggleButton, snow_noChange_toggleButton;
	Widget          frost_radioBox, frost_on_toggleButton, frost_off_toggleButton, frost_noChange_toggleButton;
	Widget          upsc_radioBox, upsc_on_toggleButton, upsc_off_toggleButton, upsc_noChange_toggleButton;
	Widget          upwe_radioBox, upwe_on_toggleButton, upwe_off_toggleButton, upwe_noChange_toggleButton;
	Widget          sac_snow_radioBox, sac_snow_on_toggleButton, sac_snow_off_toggleButton, sac_snow_noChange_toggleButton;

	Widget          scroll_window, list;
	Widget          help_shell;

	help_struct     *help_data;
	help_cb_struct  *context_help;


non_univ_techs_All_segs_selected = TRUE;

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Universal Techniques", XmSTRING_DEFAULT_CHARSET));
popup = XtCreatePopupShell
			(
			"non_universal_shell",
			transientShellWidgetClass,
			some_widgets->tree_shell,
			NULL,
			0
			);
some_widgets->non_universalTech_popup_shell = popup;

pixel = get_pixel_by_name(popup, "yellow");

/*      Create a transientShell for the context sensitive help window...                                */
help_shell = XtCreatePopupShell("univ_tech_help_shell", transientShellWidgetClass, global_toplevel, NULL, 0);



n = 0;
form = XtCreateManagedWidget
		(
		"non_universal_form",
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
bb = XtCreateManagedWidget
		(
		"bb_for_select_buttons",
		xmBulletinBoardWidgetClass,
		form,
		wargs,
		n
		);


n = 0;
XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
XtSetArg(wargs[n], XmNtopWidget, bb); n++;
XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
separator_widget = XtCreateManagedWidget
		(
		"non_univ_top_separator_widget",
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
pushButton_bb_widget = XtCreateManagedWidget
		(
		"pushButton_bb_widget",
		xmBulletinBoardWidgetClass,
		form,
		wargs,
		n
		);





/* --------------------------------- Create Toggle buttons & container widgets -------------------------------- */


XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Snow Model Techniques", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("general_heading", xmLabelWidgetClass, bb, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Segments", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("segments_heading", xmLabelWidgetClass, bb, wargs, 1);

applyTo_Frame = XtCreateManagedWidget("applyTo_Frame", xmFrameWidgetClass, bb, NULL, 0);
snowModel_Frame = XtCreateManagedWidget("snowModel_Frame", xmFrameWidgetClass, bb, NULL, 0);



snow_Frame_bb = XtCreateManagedWidget
			(
			"snow_Frame_bb",
			xmBulletinBoardWidgetClass,
			snowModel_Frame,
			NULL,
			0
			);

/* Create a list widget with the segment names for the current NWSRFS run...                    */

XtSetArg(wargs[0], XmNscrolledWindowMarginWidth, 0);
XtSetArg(wargs[1], XmNscrollingPolicy, XmAUTOMATIC);
XtSetArg(wargs[2], XtNmappedWhenManaged, FALSE);
scroll_window = XtCreateManagedWidget("non_univ_swindow", xmScrolledWindowWidgetClass, bb, wargs, 3);
some_widgets->non_univ_swindow = scroll_window;

XtSetArg(wargs[0], XmNwidth, 100);
list = XtCreateManagedWidget("non_univ_list", xmListWidgetClass, scroll_window, wargs, 1);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "non_univ_list";
XtAddEventHandler(list, EnterWindowMask, FALSE, help_event_handler, context_help);
some_widgets->non_univ_list = list;
XtAddCallback(list, XmNextendedSelectionCallback, set_snowModel_buttons_for_selectedSegment, some_widgets);


/* Create RowColumn widget to hold the headings for the Snow Model...                           */
rc_for_snow_headings = XtCreateManagedWidget("rc_for_snow_headings", xmBulletinBoardWidgetClass, snow_Frame_bb, NULL, 0);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("On", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("on_heading", xmLabelWidgetClass, rc_for_snow_headings, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Off", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("off_heading", xmLabelWidgetClass, rc_for_snow_headings, wargs, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("No Change", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("noChange_heading", xmLabelWidgetClass, rc_for_snow_headings, wargs, 1);

/* Create the RowColumn widgets to hold the labels & radio boxes...                             */
snowModel_rc_widget = XtCreateManagedWidget("snowModel_rc_widget", xmRowColumnWidgetClass, snow_Frame_bb, NULL, 0);
applyTo_rc_widget = XtCreateManagedWidget("applyTo_rc_widget", xmRowColumnWidgetClass, applyTo_Frame, NULL, 0);


/* --------------------------------- 'Apply to' buttons ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Apply to:", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("applyTo_label", xmLabelWidgetClass, applyTo_rc_widget, wargs, 1);

applyTo_radioBox = XmCreateRadioBox(applyTo_rc_widget, "applyTo_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "applyTo_radioBox";
XtAddEventHandler(applyTo_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(applyTo_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("All", XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[2], XmNset, TRUE);
all_toggleButton = (Widget)XmCreateToggleButtonGadget(applyTo_radioBox, "all_toggleButton", wargs, 3);
some_widgets->all_toggleButton = all_toggleButton;
XtManageChild(all_toggleButton);
XtAddCallback(all_toggleButton, XmNvalueChangedCallback, hide_nonUniversal_segmentList, some_widgets);
XtAddCallback(all_toggleButton, XmNvalueChangedCallback, set_snowModel_buttons_to_default, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("Selected", XmSTRING_DEFAULT_CHARSET));
selected_toggleButton = (Widget)XmCreateToggleButtonGadget(applyTo_radioBox, "selected_toggleButton", wargs, 2);
some_widgets->selected_toggleButton = selected_toggleButton;
XtManageChild(selected_toggleButton);
XtAddCallback(selected_toggleButton, XmNvalueChangedCallback, show_nonUniversal_segmentList, some_widgets);
XtAddCallback(selected_toggleButton, XmNvalueChangedCallback, set_snowModel_buttons_to_noChange, some_widgets);

/* ------------------------------------- 'Snow' buttons ------------------------------------ */

XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Snow", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("snow_heading", xmLabelWidgetClass, snowModel_rc_widget, wargs, 1);

snow_radioBox = XmCreateRadioBox(snowModel_rc_widget, "snow_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "snow_radioBox";
XtAddEventHandler(snow_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(snow_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
snow_on_toggleButton = (Widget)XmCreateToggleButtonGadget(snow_radioBox, "snow_on_toggleButton", wargs, 2);
some_widgets->snowButtons.on = snow_on_toggleButton;
XtManageChild(snow_on_toggleButton);
XtAddCallback(snow_on_toggleButton, XmNvalueChangedCallback, snow_on_cb, some_widgets);


XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
snow_off_toggleButton = (Widget)XmCreateToggleButtonGadget(snow_radioBox, "snow_off_toggleButton", wargs, 2);
some_widgets->snowButtons.off = snow_off_toggleButton;
XtManageChild(snow_off_toggleButton);
XtAddCallback(snow_off_toggleButton, XmNvalueChangedCallback, snow_off_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[2], XmNset, TRUE);
snow_noChange_toggleButton = (Widget)XmCreateToggleButtonGadget(snow_radioBox, "snow_noChange_toggleButton", wargs, 3);
some_widgets->snowButtons.no_change = snow_noChange_toggleButton;
XtManageChild(snow_noChange_toggleButton);
XtAddCallback(snow_noChange_toggleButton, XmNvalueChangedCallback, snow_no_change_cb, some_widgets);


/* ------------------------------------- 'Frost' buttons ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("Frost", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("frost_heading", xmLabelWidgetClass, snowModel_rc_widget, wargs, 1);

frost_radioBox = XmCreateRadioBox(snowModel_rc_widget, "frost_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "frost_radioBox";
XtAddEventHandler(frost_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(frost_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
frost_on_toggleButton = (Widget)XmCreateToggleButtonGadget(frost_radioBox, "frost_on_toggleButton", wargs, 2);
some_widgets->frostButtons.on = frost_on_toggleButton;
XtManageChild(frost_on_toggleButton);
XtAddCallback(frost_on_toggleButton, XmNvalueChangedCallback, frost_on_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
frost_off_toggleButton = (Widget)XmCreateToggleButtonGadget(frost_radioBox, "frost_off_toggleButton", wargs, 2);
some_widgets->frostButtons.off = frost_off_toggleButton;
XtManageChild(frost_off_toggleButton);
XtAddCallback(frost_off_toggleButton, XmNvalueChangedCallback, frost_off_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[2], XmNset, TRUE);
frost_noChange_toggleButton = (Widget)XmCreateToggleButtonGadget(frost_radioBox, "frost_noChange_toggleButton", wargs, 3);
some_widgets->frostButtons.no_change = frost_noChange_toggleButton;
XtManageChild(frost_noChange_toggleButton);
XtAddCallback(frost_noChange_toggleButton, XmNvalueChangedCallback, frost_no_change_cb, some_widgets);


/* ------------------------------------- 'UPSC' buttons ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("UPSC", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("upsc_heading", xmLabelWidgetClass, snowModel_rc_widget, wargs, 1);

upsc_radioBox = XmCreateRadioBox(snowModel_rc_widget, "upsc_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "upsc_radioBox";
XtAddEventHandler(upsc_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(upsc_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
upsc_on_toggleButton = (Widget)XmCreateToggleButtonGadget(upsc_radioBox, "upsc_on_toggleButton", wargs, 2);
some_widgets->upscButtons.on = upsc_on_toggleButton;
XtManageChild(upsc_on_toggleButton);
XtAddCallback(upsc_on_toggleButton, XmNvalueChangedCallback, upsc_on_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
upsc_off_toggleButton = (Widget)XmCreateToggleButtonGadget(upsc_radioBox, "upsc_off_toggleButton", wargs, 2);
some_widgets->upscButtons.off = upsc_off_toggleButton;
XtManageChild(upsc_off_toggleButton);
XtAddCallback(upsc_off_toggleButton, XmNvalueChangedCallback, upsc_off_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[2], XmNset, TRUE);
upsc_noChange_toggleButton = (Widget)XmCreateToggleButtonGadget(upsc_radioBox, "upsc_noChange_toggleButton", wargs, 3);
some_widgets->upscButtons.no_change = upsc_noChange_toggleButton;
XtManageChild(upsc_noChange_toggleButton);
XtAddCallback(upsc_noChange_toggleButton, XmNvalueChangedCallback, upsc_no_change_cb, some_widgets);

/* ------------------------------------- 'UPWE' buttons ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("UPWE", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("upwe_heading", xmLabelWidgetClass, snowModel_rc_widget, wargs, 1);

upwe_radioBox = XmCreateRadioBox(snowModel_rc_widget, "upwe_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "upwe_radioBox";
XtAddEventHandler(upwe_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(upwe_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
upwe_on_toggleButton = (Widget)XmCreateToggleButtonGadget(upwe_radioBox, "upwe_on_toggleButton", wargs, 2);
some_widgets->upweButtons.on = upwe_on_toggleButton;
XtManageChild(upwe_on_toggleButton);
XtAddCallback(upwe_on_toggleButton, XmNvalueChangedCallback, upwe_on_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
upwe_off_toggleButton = (Widget)XmCreateToggleButtonGadget(upwe_radioBox, "upwe_off_toggleButton", wargs, 2);
some_widgets->upweButtons.off = upwe_off_toggleButton;
XtManageChild(upwe_off_toggleButton);
XtAddCallback(upwe_off_toggleButton, XmNvalueChangedCallback, upwe_off_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[2], XmNset, TRUE);
upwe_noChange_toggleButton = (Widget)XmCreateToggleButtonGadget(upwe_radioBox, "upwe_noChange_toggleButton", wargs, 3);
some_widgets->upweButtons.no_change = upwe_noChange_toggleButton;
XtManageChild(upwe_noChange_toggleButton);
XtAddCallback(upwe_noChange_toggleButton, XmNvalueChangedCallback, upwe_no_change_cb, some_widgets);

/* added 2/2/01 aivo */
/* ------------------------------------- 'sac_snow' buttons ------------------------------------ */
XtSetArg(wargs[0], XmNlabelString, XmStringCreateLtoR("SAC/SNOW STATE", XmSTRING_DEFAULT_CHARSET));
XtCreateManagedWidget("sac_snow_heading", xmLabelWidgetClass, snowModel_rc_widget, wargs, 1);

sac_snow_radioBox = XmCreateRadioBox(snowModel_rc_widget, "sac_snow_radioBox", NULL, 0);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "sac_snow_radioBox";
XtAddEventHandler(sac_snow_radioBox, EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChild(sac_snow_radioBox);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
sac_snow_on_toggleButton = (Widget)XmCreateToggleButtonGadget(sac_snow_radioBox, "sac_snow_on_toggleButton", wargs, 2);
some_widgets->sac_snowButtons.on = sac_snow_on_toggleButton;
XtManageChild(sac_snow_on_toggleButton);
XtAddCallback(sac_snow_on_toggleButton, XmNvalueChangedCallback, sac_snow_on_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);
XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
sac_snow_off_toggleButton = (Widget)XmCreateToggleButtonGadget(sac_snow_radioBox, "sac_snow_off_toggleButton", wargs, 2);
some_widgets->sac_snowButtons.off = sac_snow_off_toggleButton;
XtManageChild(sac_snow_off_toggleButton);
XtAddCallback(sac_snow_off_toggleButton, XmNvalueChangedCallback, sac_snow_off_cb, some_widgets);

XtSetArg(wargs[0], XmNselectColor, pixel);

XtSetArg(wargs[1], XmNlabelString, XmStringCreateLtoR("       ", XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[2], XmNset, TRUE);
sac_snow_noChange_toggleButton = (Widget)XmCreateToggleButtonGadget(sac_snow_radioBox, "sac_snow_noChange_toggleButton", wargs, 3);
some_widgets->sac_snowButtons.no_change = sac_snow_noChange_toggleButton;
XtSetSensitive(sac_snow_noChange_toggleButton, FALSE);
XtManageChild(sac_snow_noChange_toggleButton);
XtAddCallback(sac_snow_noChange_toggleButton, XmNvalueChangedCallback, sac_snow_no_change_cb, some_widgets);

/*-------------------------------------------------------------------------------------------- */

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Apply", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNwidth, 100); n++;
ok_widget = XtCreateManagedWidget
		(
		"non_univ_ok_widget",
		xmPushButtonWidgetClass,
		pushButton_bb_widget,
		wargs,
		n
		);
XtAddCallback(ok_widget, XmNactivateCallback, set_non_univ_techniques, some_widgets);
XtAddCallback(ok_widget, XmNactivateCallback, pop_down_shell, popup);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "non_univ_ok_widget";
XtAddEventHandler(ok_widget, EnterWindowMask, FALSE, help_event_handler, context_help);
some_widgets->non_universalTech_ok_widget = ok_widget;

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Cancel", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNwidth, 100); n++;
cancel_widget = XtCreateManagedWidget
		(
		"non_univ_cancel_widget",
		xmPushButtonWidgetClass,
		pushButton_bb_widget,
		wargs,
		n
		);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "non_univ_cancel_widget";
XtAddEventHandler(cancel_widget, EnterWindowMask, FALSE, help_event_handler, context_help);
XtAddCallback(cancel_widget, XmNactivateCallback, pop_down_shell, popup);

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNwidth, 100); n++;
help_widget = XtCreateManagedWidget
		(
		"non_univ_help_widget",
		xmPushButtonWidgetClass,
		pushButton_bb_widget,
		wargs,
		n
		);

help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = popup;
help_data->message_widget_name = "non_univ_help_dialog";

XtAddCallback(help_widget, XmNactivateCallback, popup_help_window, "NON_UNIV_TECHNIQUES");
XtSetArg(wargs[0], XtNbackground, &backgroundColor);
XtGetValues(ok_widget, wargs, 1);
XtSetArg(wargs[0], XmNborderColor, backgroundColor);
XtSetValues(ok_widget, wargs, 1);
XtSetArg(wargs[0], XmNborderColor, backgroundColor);
XtSetValues(cancel_widget, wargs, 1);
XtSetArg(wargs[0], XmNborderColor, backgroundColor);
XtSetValues(help_widget, wargs, 1);




}


/* ************************************************************************

	post_menu_handler()

   ************************************************************************

void post_menu_handler(w, menu, event)
	Widget          w;
	Widget          menu;
	XEvent          *event;
{

	Arg             wargs[1];
	int             button;

XtSetArg(wargs[0], XmNwhichButton, &button);
XtGetValues(menu, wargs, 1);

if(event->xbutton.button == button)
	{
	XmMenuPosition(menu, event);
	XtManageChild(menu);
	}

}



*/



/* ************************************************************************

	non_univ_Techniques_init()
		create and initialize a global 'non_univ_techniques_struct'
		to be used as a temporary structure to hold changes to
		non-universal techniques for selected segments...

   ************************************************************************ */

void non_univ_Techniques_init()
{

 non_univ_Techniques =
	(non_univ_techniques_struct *) realloc(non_univ_Techniques, sizeof(non_univ_techniques_struct));

 non_univ_Techniques->snow = NO_CHANGE;
 non_univ_Techniques->frost = NO_CHANGE;
 non_univ_Techniques->upsc = NO_CHANGE;
 non_univ_Techniques->upwe = NO_CHANGE;
 non_univ_Techniques->sac_snow = NO_CHANGE;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/non_univ_techs.c,v $";
 static char rcs_id2[] = "$Id: non_univ_techs.c,v 1.3 2006/04/07 13:30:15 aivo Exp $";}
/*  ===================================================  */

}
