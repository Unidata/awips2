

/* *************************************************************************************************************

	seed.c
		Begins execution of NWS/Office of Hydrology
		Interactive Forecast Program, the Graphical
		User Interface for the NWSRFS

	Originally coded by Tom Adams NOAA/NWS/OH/HRL, 05/03/90:        Xt Toolkit & Xw widgets
	Revised          by Tom Adams NOAA/NWS/OH/HRL, 12/  /90:        Xt Toolkit & xm (Motif) widgets
	Revised          by Tom Adams NOAA/NWS/OH/HRL, 01/22/90:        changed List widget
	Revised          by Tom Adams NOAA/NWS/OH/HRL, 05/20/91:        added context sensitive help & carryover
									date selection
	Revised          by Tom Adams NOAA/NWS/OH/HRL, 06/26/91:        added check for X root window property
									to see if Start IFP is already running
        Revised          by DHM Team, 11/16/2006:                       added code to check return status of
                                                                        call to get_ofs_data script
	Project:        P  R  O  T  E  U  S

   ************************************************************************************************************* */
#include <unistd.h>
#include <sys/types.h>

#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "globals.h"
#include "struct_defs.h"
#include "ifp_help.h"
#include "help.h"
#include "c_call_f/number_of_fgroups.h"
#include "c_call_f/read_fcstandco_groupnames.h"
#include "c_call_f/get_ofs_default_tzc.h"
#include "c_call_f/set_ofs_lock.h"
#include "c_call_f/free_ofs_lock.h"
#include "c_call_f/ofs_carryover_dates.h"
#include "c_call_f/fcobbl.h"

#define CANCEL_START_IFP                0
#define USE_PREVIOUS_IFP_FILES          1
#define COPY_CURRENT_OFS_FILES          2
#define CO_DATE_LENGTH                 22

static void list_fgs();
static void fill_FGList();
static void handleFGList();
static void handleFGSelection();
static void leave_IFP();
static void load_FGroup_data();
void reset_interface_components(the_widget_struct *);

void    fill_carryover_dates();
int     get_carryover_dates();

void    handle_carryover_date_selection();
void    write_fgName_and_startDate();
void    copy_current_ofs_files();
void    use_previous_ifp_files();
void    highlight_default_carryover_date();
void    set_New_ForecastGroup_button_sensitive();
void    reset_OptionMenu(Widget, Widget, XmAnyCallbackStruct *);

extern void get_files_copied_date(char *);
extern void get_mods_copied_date(char *);

typedef struct
	{
	int     time_change;
	int     time;
	char    time_zone[5];
	}       date_data;
	
typedef struct
	{
	Widget                 fgList; 
	xs_menu_widget_struct  *option_menu;
	}	fgList_struct;	


 xs_menu_struct Option_struct[] =
	{
	{"Use copy of current OFS files" , copy_current_ofs_files, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Use previous IFP files" , use_previous_ifp_files, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static char *month_char[] = {"NONE", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
				     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

int     whichDataToGet;
int     *carryover_julianHour;

date    *carryover_date = NULL;

Widget  prevFGWidget, prevCGWidget;

char    theSelectedFGName[9];
char    *theSelectedCarryoverDate = NULL;

int     which_carryover_date;




void make_ForecastGroup_selectionBox(parent, some_widgets)
	Widget                  parent;
	the_widget_struct       *some_widgets;
{

	Widget          shell;

	int     numFGs;                 /*      Number of Forecast Groups...                    */
	char    *fgIDs;                 /*      Forecast Group IDs, 8-characters long, max.     */
	char    *cgIDs;                 /*      Carryover Group IDs, 8-characters long, max.    */
	char    *buffer;
	char    *next_newline;




 shell = XtCreatePopupShell("ForecastGroup_selectionBox", transientShellWidgetClass, parent, NULL, 0);
 some_widgets->FcstGroup_selectionBoxShell = shell;

 /* Find out how many forecast groups there are so we can malloc space for them...              */

 NUMBER_OF_FGROUPS(&numFGs);

 fgIDs = (char *) malloc(8 * numFGs + 1);
 cgIDs = (char *) malloc(8 * numFGs + 1);

 memset(fgIDs, '\0', 8 * numFGs + 1);
 memset(cgIDs, '\0', 8 * numFGs + 1);

 READ_FCSTANDCO_GROUPNAMES(fgIDs, cgIDs);
 list_fgs(numFGs, fgIDs, cgIDs, shell, some_widgets);

 some_widgets->ForecastGroupNames = fgIDs;
 some_widgets->NumForecastGroups = numFGs;

}






void list_fgs(numFGs, fgIDs, cgIDs, shell, some_widgets)
	int                     numFGs;         /*      Number of Forecast Groups...                    */
	char                    fgIDs[][8];     /*      Forecast Group IDs, 8-characters long, max.     */
	char                    cgIDs[][8];     /*      Carryover Group IDs, 8-characters long, max.    */
	Widget                  shell;
	the_widget_struct       *some_widgets;
{

	Arg             wargs[15];
	int             background_color;
	int             i;
	int             n;

	char            label[70];

	Widget          sw_for_fg_list;
	Widget          bboard, start_ifp, quit_ifp, fg_list;
	Widget          rc_for_fg_list;
	Widget          carryover_date_sw;
	Widget          carryover_date_list;
	Widget          sub_menu;
	Widget          choose_option_popup_menu;
	Widget          separator, top_separator;
	Widget          form;
	Widget          help_shell;
	Widget          help_button;
	Widget          mid_separator;
	Widget          files_date_label;
	Widget          mods_date_label;


	xs_menu_widget_struct   *option_widget_list;
	help_struct             *help_data;
	help_cb_struct          *context_help;
	fgList_struct           *fg_list_struct;





 whichDataToGet = COPY_CURRENT_OFS_FILES;


/*      Create a transientShell for the context sensitive help window...                                */
help_shell = XtCreatePopupShell("startIFP_help_shell", transientShellWidgetClass,
			       some_widgets->toplevel, NULL, 0);

theSelectedCarryoverDate = (char *) malloc(CO_DATE_LENGTH);

form = XtVaCreateManagedWidget("ifp_form_widget", xmFormWidgetClass, shell,
			      XmNresizable, FALSE,
			      NULL);

bboard = XtVaCreateManagedWidget("ifp_bboard", xmBulletinBoardWidgetClass, form,
				XmNtopAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);


top_separator = XtVaCreateManagedWidget("ifp_top_separator", xmSeparatorWidgetClass, form,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, bboard,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNrightAttachment, XmATTACH_FORM,
				       NULL);


 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, top_separator); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftOffset, 42); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 sub_menu = XmCreatePulldownMenu(form, "option_submenu", wargs, n);

 /* Fill the 'data' portion of 'Option_struct'...                                               */
 for(i = 0; i < XtNumber(Option_struct); i++) Option_struct[i].data = (caddr_t) NULL;

 /*  Create the Main menu pane.                  */
 option_widget_list =
	 (xs_menu_widget_struct *)xs_create_menu_buttons("", sub_menu, Option_struct, XtNumber(Option_struct));
	 
 fg_list_struct = (fgList_struct *) malloc(sizeof(fgList_struct));
 fg_list_struct->option_menu = option_widget_list;
 
 /*  Set the Use Previous Files button to insensitive until it's determined it can be used */
 XtSetSensitive(option_widget_list->widget_array[1]->parent, FALSE);

 for(i = 0; i < XtNumber(Option_struct); i++)
	{
	context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
	context_help->top_help_shell = help_shell;

	if(i == 0)      context_help->widget_name = "option_1";
	else            context_help->widget_name = "option_2";

	XtAddEventHandler(option_widget_list->widget_array[i]->parent, EnterWindowMask,
				FALSE, help_event_handler, context_help);
	}

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, top_separator); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftOffset, 115); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNmenuHistory, option_widget_list->widget_array[0]->parent); n++;
 XtSetArg(wargs[n], XmNwhichButton, (unsigned int) 1); n++;
 XtSetArg(wargs[n], XmNsubMenuId, sub_menu); n++;
 choose_option_popup_menu = XmCreateOptionMenu(form, "choose_option_popup_menu", wargs, n);
 XtManageChild(choose_option_popup_menu);

/* Added the following 3 widgets to show the date files last copied from OFS
 * and the date mods last copied to OFS.  dp - 10 Oct. 1995
 */
mid_separator = XtVaCreateManagedWidget("ifp_mid_separator", xmSeparatorWidgetClass, form,
				        XmNtopAttachment,   XmATTACH_WIDGET,
				        XmNtopWidget,       choose_option_popup_menu,
				        XmNleftAttachment,  XmATTACH_FORM,
				        XmNrightAttachment, XmATTACH_FORM,
				        NULL);

memset(label, '\0', 70);
get_files_copied_date(label);

n=0;
XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
XtSetArg(wargs[n], XmNtopWidget, mid_separator); n++;
XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNlabelString, 
         XmStringCreate(label, XmSTRING_DEFAULT_CHARSET)); n++;
files_date_label = XtCreateManagedWidget("files_date", xmLabelWidgetClass, form, wargs, n);         
some_widgets->files_date_label = files_date_label;

memset(label, '\0', 70);
get_mods_copied_date(label);

n=0;
XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
XtSetArg(wargs[n], XmNtopWidget, files_date_label); n++;
XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
XtSetArg(wargs[n], XmNlabelString, 
         XmStringCreate(label, XmSTRING_DEFAULT_CHARSET)); n++;
mods_date_label = XtCreateManagedWidget("mods_date", xmLabelWidgetClass, form, wargs, n);         
some_widgets->mods_date_label = mods_date_label;

separator = XtVaCreateManagedWidget("ifp_separator", xmSeparatorWidgetClass, form,
				   XmNtopAttachment,   XmATTACH_WIDGET,
				   XmNtopWidget,       mods_date_label,
				   XmNleftAttachment,  XmATTACH_FORM,
				   XmNrightAttachment, XmATTACH_FORM,
				   NULL);


memset(label, '\0', 20);
strcpy(label, "Load");
start_ifp = XtVaCreateManagedWidget("start_ifp", xmPushButtonWidgetClass, form,
		XmNlabelString,      XmStringCreate(label, "charSet1"),
		XmNshowAsDefault,    TRUE,
		XmNtopAttachment,    XmATTACH_WIDGET,
		XmNtopWidget,        separator,
		XmNleftAttachment,   XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset,       50,
		XmNtopOffset,        10,
		XmNbottomOffset,     10,
		XmNuserData,         option_widget_list,
		NULL);
XtAddCallback(start_ifp, XmNactivateCallback, (XtCallbackProc)load_FGroup_data, some_widgets);
XtAddCallback(start_ifp, XmNactivateCallback, (XtCallbackProc)reset_OptionMenu, choose_option_popup_menu);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "start_ifp";
XtAddEventHandler(start_ifp, EnterWindowMask, FALSE, help_event_handler, context_help);
XmProcessTraversal(start_ifp, XmTRAVERSE_CURRENT);

sw_for_fg_list      = XtVaCreateManagedWidget("sw_for_fg_list", xmScrolledWindowWidgetClass, bboard, NULL);
carryover_date_sw   = XtVaCreateManagedWidget("carryover_date_sw", xmScrolledWindowWidgetClass, bboard, NULL);
carryover_date_list = XtVaCreateManagedWidget("carryover_date_list", xmListWidgetClass, carryover_date_sw,
					     XmNselectionPolicy, XmBROWSE_SELECT,
					     NULL);
XtAddCallback(carryover_date_list, XmNbrowseSelectionCallback, handle_carryover_date_selection, NULL);

context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "carryover_date_list";
XtAddEventHandler(carryover_date_list, EnterWindowMask, FALSE,
help_event_handler, context_help);
XtManageChild(carryover_date_list);


rc_for_fg_list = XtVaCreateManagedWidget("rc_for_fg_list", xmRowColumnWidgetClass, sw_for_fg_list,
					XmNnumColumns,  2,
					XmNpacking,     XmPACK_COLUMN,
					XmNorientation, XmVERTICAL,
					NULL);
/* add argument to fill_FGList - dp 23 Sept. 1997 */
fill_FGList(rc_for_fg_list, fgIDs, cgIDs, numFGs, carryover_date_list, help_shell, some_widgets,
            fg_list_struct);

XtVaGetValues(bboard, XmNbackground, &background_color, NULL);

/* Create a 'Cancel' button to exit the IFP     */
 quit_ifp = XtVaCreateManagedWidget("cancel_ifp", xmPushButtonWidgetClass, form,
				   XmNlabelString,      XmStringCreate("Cancel", "charSet1"),
				   XmNsensitive,        FALSE,
				   NULL);
 context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
 context_help->top_help_shell = help_shell;
 context_help->widget_name = "Cancel";
 XtAddEventHandler(quit_ifp, EnterWindowMask, FALSE, help_event_handler, context_help);
 XtAddCallback(quit_ifp, XmNactivateCallback, leave_IFP, shell);
 XtAddCallback(quit_ifp, XmNactivateCallback, set_New_ForecastGroup_button_sensitive, some_widgets);

 some_widgets->FcstGroup_selectionBoxCancel = quit_ifp;

XtVaSetValues(quit_ifp,
	    XmNborderWidth,      4,
	    XmNborderColor,      background_color,
	    XmNtopAttachment,    XmATTACH_WIDGET,
	    XmNtopWidget,        separator,
	    XmNleftAttachment,   XmATTACH_WIDGET,
	    XmNleftWidget,       start_ifp,
	    XmNbottomAttachment, XmATTACH_FORM,
	    XmNleftOffset,       20,
	    XmNtopOffset,        10,
	    XmNbottomOffset,     10,
	    NULL);


help_button = XtVaCreateManagedWidget("start_ifp_help_button", xmPushButtonWidgetClass, form,
				     XmNlabelString,      XmStringCreate("Help", "charSet1"),
				     XmNborderWidth, 4,
				     XmNborderColor, background_color,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     XmNtopWidget, separator,
				     XmNleftAttachment, XmATTACH_WIDGET,
				     XmNleftWidget, quit_ifp,
				     XmNbottomAttachment, XmATTACH_FORM,
				     XmNleftOffset, 20,
				     XmNtopOffset, 10,
				     XmNbottomOffset, 10,
				     NULL);


help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = global_toplevel;
help_data->message_widget_name = "start_ifp_help_button";

XtAddCallback(help_button, XmNactivateCallback, popup_help_window, "FORECAST_GROUP_AND_CARRYOVER");


XtVaCreateManagedWidget("fgLabel", xmLabelWidgetClass, bboard,
		XmNlabelString, XmStringCreateLtoR("Forecast\nGroup", "charSet1"),
		NULL);

XtVaCreateManagedWidget("cgLabel", xmLabelWidgetClass, bboard,
		XmNlabelString, XmStringCreateLtoR("Carryover\nGroup", "charSet1"),
		NULL);


XtVaCreateManagedWidget("cdLabel", xmLabelWidgetClass, bboard,
		XmNlabelString, XmStringCreateLtoR("Carryover dates", "charSet1"),
		NULL);


XtVaCreateManagedWidget("ifp_message2", xmLabelWidgetClass, bboard,
		XmNlabelString,
		XmStringCreateLtoR(
		  "NWS River Forecast System\nInteractive Forecast Program",
		  "charSet1"), 
		XtNx, 40,
		NULL);

}



/* *****************************************************************************

	fill_FGList()

   ***************************************************************************** */

void fill_FGList(parent, fgID, cgID, num_ForecastGroups, cd_List, help_shell, some_widgets, 
                 fg_list_struct)
	Widget          parent;
	char            fgID[][8];              /*      Forecast Group IDs, 8-characters long, max.     */
	char            cgID[][8];              /*      Carryover Group IDs, 8-characters long, max.    */
	int             num_ForecastGroups;
	Widget          cd_List;                /*      Carryover-date List widget...                   */
	Widget          help_shell;
	the_widget_struct       *some_widgets;
	fgList_struct    *fg_list_struct;
{

	Widget          fg_list, cg_list;
	char            *FGname, *CGname;
	XmString        *xmFGids, *xmCGids;
	int             k;
	Arg             wargs[5];

	FcstGroupSelectionDialogStruct *dataStruct;
	help_cb_struct  *context_help;



FGname = (char *)malloc(9);
CGname = (char *)malloc(9);

memset(FGname, '\0', 9);
memset(CGname, '\0', 9);

dataStruct = (FcstGroupSelectionDialogStruct *) malloc(sizeof(FcstGroupSelectionDialogStruct));

dataStruct->carryoverDate_list = cd_List;
dataStruct->carryover_dates = NULL;

some_widgets->dataStruct = dataStruct;

xmFGids = (XmString *) XtMalloc(sizeof(XmString) * num_ForecastGroups);
xmCGids = (XmString *) XtMalloc(sizeof(XmString) * num_ForecastGroups);

for(k = 0; k < num_ForecastGroups; k++)
	{
	strncpy(FGname, fgID[k], 8);
	xmFGids[k] = XmStringCreateLtoR(FGname, "charSet1");
	strncpy(CGname, cgID[k], 8);
	xmCGids[k] = XmStringCreateLtoR(CGname, "charSet1");
	}


fg_list = XtVaCreateManagedWidget("fg_list", xmListWidgetClass, parent,
				 XmNuserData,            some_widgets,
				 XmNitems,               xmFGids,
				 XmNitemCount,           num_ForecastGroups,
				 XmNvisibleItemCount,    num_ForecastGroups,
				 XmNselectionPolicy,     XmBROWSE_SELECT,
				 XmNdoubleClickInterval, 1,
				 NULL);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "fg_list";
XtAddEventHandler(fg_list, EnterWindowMask, FALSE, help_event_handler, context_help);

fg_list_struct->fgList = fg_list;
dataStruct->forecastGroup_list = fg_list;

cg_list = XtVaCreateManagedWidget("cg_list", xmListWidgetClass, parent,
				 XmNitems,               xmCGids,
				 XmNitemCount,           num_ForecastGroups,
				 XmNvisibleItemCount,    num_ForecastGroups,
				 XmNselectionPolicy,     XmBROWSE_SELECT,
				 XmNdoubleClickInterval, 1,
				 NULL);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "cg_list";
XtAddEventHandler(cg_list, EnterWindowMask, FALSE, help_event_handler, context_help);

dataStruct->carryoverGroup_list = cg_list;

XtAddCallback(fg_list, XmNbrowseSelectionCallback, handleFGList, cg_list);
XtAddCallback(cg_list, XmNbrowseSelectionCallback, handleFGList, fg_list);

/*      Pass 'fg_list' to the callback to set the forecast group name from the item selected...         */
XtAddCallback(fg_list, XmNbrowseSelectionCallback, handleFGSelection, fg_list_struct);
XtAddCallback(cg_list, XmNbrowseSelectionCallback, handleFGSelection, fg_list_struct);

/*      Pass 'cg_list' to the callback to set the carryover date for the selected forecast group...     */
XtAddCallback(fg_list, XmNbrowseSelectionCallback, fill_carryover_dates, dataStruct);
XtAddCallback(cg_list, XmNbrowseSelectionCallback, fill_carryover_dates, dataStruct);

 XmStringFree((XmString)xmFGids);
 XmStringFree((XmString)xmCGids);


}




/* ***************************************************************************

	void handleFGList()
		selects the companion widget to the selected list widget

   *************************************************************************** */

void handleFGList(selectedWidget, companionWidget, call_data)
	Widget                          selectedWidget;
	Widget                          companionWidget;
	XmListCallbackStruct            *call_data;
{

XmListSelectPos(companionWidget, call_data->item_position, FALSE);

}




/* ************************************************************************************

	handleFGSelection()
		obtains the forecast group name from the forecast group list widget
		and copies it to the global variable 'theSelectedFGName'

   ************************************************************************************ */

void  handleFGSelection(selectedWidget, fg_list_struct, call_data)
	Widget                          selectedWidget;
	fgList_struct                   *fg_list_struct;
	XmListCallbackStruct            *call_data;
{

	XmString        *xmFGName;
	char            *fgName_string;
	Arg             wargs[2];

	the_widget_struct       *some_widgets;
	int             use_ok, i;
	char            fgName_no_blanks[9];


 XtVaGetValues(fg_list_struct->fgList, XmNselectedItems, &xmFGName, NULL);
 XmStringGetLtoR(*xmFGName, "charSet1", &fgName_string);

 strcpy(theSelectedFGName, fgName_string);
 
 /* Check if it's possible to use previous files with this forecast group 
  * If so, set the button so it's sensitive. - dp 23 Sept. 1997
  */
 memset(fgName_no_blanks, '\0', 9);
 strcpy(fgName_no_blanks, fgName_string);
 for(i = 0; i < 8; i++)     /* Remove trailing blanks */
    if(fgName_no_blanks[i] == ' ')
    {
       fgName_no_blanks[i] = '\0';
       break;
    }

 use_ok = ok_use_previous_files(fgName_no_blanks);
 if(use_ok)
    XtSetSensitive(fg_list_struct->option_menu->widget_array[1]->parent, TRUE);
 else
    XtSetSensitive(fg_list_struct->option_menu->widget_array[1]->parent, FALSE);

 if(FGBasin_ID != NULL) free(FGBasin_ID);
 FGBasin_ID = (char **)map_areas_in_fg(theSelectedFGName, &NumBasinsInCurrFcstGroup);



 XtVaGetValues(fg_list_struct->fgList, XmNuserData, &some_widgets, NULL);
 strcpy(some_widgets->selected_ForecastGroupName, fgName_string);


 /* ------------------------------------------------------------------- */
 /*     Highlight the selected Forecast Group basins...                 */
 /* ------------------------------------------------------------------- */

 if(FGBasin_ID != NULL && NumBasinsInCurrFcstGroup != 0)
	 {
	 if(rad_data->currentForecastGroup_on)
		 {
		 show_currentForecastGroup(some_widgets);
		 }
	 XmToggleButtonSetState(some_widgets->FcstGroup_widget, TRUE, FALSE);
	 add_overlays(selectedWidget, some_widgets, NULL);
	 }

 XtVaSetValues(some_widgets->ForecastGroup_label,
	       XtVaTypedArg, XmNlabelString, XmRString, some_widgets->selected_ForecastGroupName,
	       strlen(some_widgets->selected_ForecastGroupName)+1,
	       NULL);

 /* Free (XmString *) xmFGName and (char *) fgName_string to avoid leaving      */
 /* holes in memory...                                                          */

 /*  Commented out the following XmStringFree because it was crashing the 
     IFP_Map program on the HPs when you chose several different forecast groups 
     before loading - dp - 31 Aug. 1993 --- THIS NEEDS TO GET FIXED LATER!!
 */

 /*XmStringFree(*xmFGName);*/
 free(fgName_string);

}




/* ************************************************************************************

	leave_IFP()
		exits from startifp...

   ************************************************************************************ */

void  leave_IFP(w, shell, call_data)
	Widget                  w;
	Widget                  shell;
	XmAnyCallbackStruct     *call_data;
{

 XtPopdown(shell);

}



/* ************************************************************************************

	load_FGroup_data()
		Begins the Interactive Forecast Program & puts the selected forecast
		group name on the DefaultRootWindow as a window property...

   ************************************************************************************ */

void  load_FGroup_data(w, some_widgets, call_data)
	Widget                  w;
	the_widget_struct       *some_widgets;
	XmAnyCallbackStruct     *call_data;
        
{

	Display         *display;
	Window          root;

	char            time_zone[5];
	int             IFP_is_already_running;
	char            script_command[120];
	char            lock_type[6];       /* holds type of lock (read or write) */
	int             icond;              /* flag for condition set by lock program */
    int             len, len2;          /* length of strings for get_apps_defaults */
    int             status;             /* return status of get_ofs_data script */
    pid_t			wdialog_pid;  
    char            sys_killcmd[80];  
 
 display = XtDisplay(w);
 root = DefaultRootWindow(display);

/* Replaced the following ill-formed test (causing segmentation violations) - jgg  8/21/01
 if((strcmp(theSelectedFGName, NULL) != 0) && (strcmp(theSelectedCarryoverDate, NULL) != 0))*/
 
 if((theSelectedFGName[0] != '\0') && (theSelectedCarryoverDate != NULL))
	{       /*      Both a Forecast Group and a Carryover Date must be selected...          */

	wdialog_pid = fork_working_dialog();

	/*      Add the Forecast Group name and Carryover Date as window properties to the      */
	/*      root window with calls to 'XChangeProperty'...                                  */

	XChangeProperty
		(
		display,
		root,
		IFPA_forecast_group,
		IFPA_forecast_group_type,
		8,
		PropModeReplace,
		theSelectedFGName,
		8
		);

	/*      The carryover date and the 'run_start_date' are identical...                    */
	XChangeProperty
		(
		display,
		root,
		IFPA_run_start_date,
		IFPA_run_start_date_type,
		8,
		PropModeReplace,
		(unsigned char *)&carryover_date[which_carryover_date],
		sizeof(date)
		);


/*
 * get the default time zone code from NWSRFS OFS files
 */
	memset(time_zone, '\0', 5);

	GET_OFS_DEFAULT_TZC(time_zone);

	XChangeProperty
		(
		display,
		root,
		IFPA_time_zone_code,
		IFPA_time_zone_code_type,
		8,
		PropModeReplace,
		time_zone,
		strlen(time_zone)
		);


	XtSetSensitive(some_widgets->begin_widget, TRUE);
	XtSetSensitive(some_widgets->showDeletedSegments_widget, TRUE);
	XtSetSensitive(some_widgets->showFGroup_Topology_widget, TRUE);
	XtSetSensitive(some_widgets->setDates_widget, TRUE);
	XtSetSensitive(some_widgets->techniques_widget, TRUE);

	XtRemoveEventHandler(some_widgets->main_canvas, ButtonPressMask, FALSE,
			     select_ForecastGroup, some_widgets);
	XtAddEventHandler(some_widgets->main_canvas, ButtonPressMask, FALSE,
			  select_basin, some_widgets);

	/* In the future the 'State' & 'Notify' arguments will be set from              */
	/* a user Preferences structure...                                              */
	XmToggleButtonSetState(some_widgets->showFGroup_Topology_widget, TRUE, FALSE);

	XtPopdown(some_widgets->FcstGroup_selectionBoxShell);
	XFlush(display);

	/* call routine to get the path name for script files */
	memset(script_command, '\0', 120);
        len = strlen("ifp_scripts_dir");
	get_apps_defaults("ifp_scripts_dir", &len, script_command, &len2);
	strcat(script_command, "/get_ofs_data");

	if(whichDataToGet == COPY_CURRENT_OFS_FILES)
		{ /*--------------------------------------------------------------------*/
		  /* Call SET_OFS_LOCK to put a lock on the ofs files to be copied.     */
		  /*    If successful (icond=0); continue.                              */
		  /*    If not (icond>0); stop the program.                             */
		  /*--------------------------------------------------------------------*/

		memset(lock_type, '\0', 6);
		strcpy(lock_type, "read");
		SET_OFS_LOCK(lock_type, &icond);
		if(icond > 0)
			{
			printf("Exiting IFP_Map: could not get copy of OFS files\n");
			exit(1);
			}
		 /* check return status of call to get_ofs_data script
		    non-zero status causes IFP to exit */
		status = system(script_command);
		/* if status is not good.  Exit IFP_Map. Kill the working_dialog
           process then exit 
        */
		if (status != 0)
        {
		    printf("Exiting IFP_Map: could not get copy of OFS files\n");
            sprintf(sys_killcmd,"kill -9 %ld",wdialog_pid);
            system(sys_killcmd);
		    exit(status);
		}
		
		FREE_OFS_LOCK(&icond);
		}

	/* call routine to get the path name for script files */
	memset(script_command, '\0', 120);
        len = strlen("ifp_scripts_dir");
	get_apps_defaults("ifp_scripts_dir", &len, script_command, &len2);

	strcat(script_command, "/start_ifp_map_script");

	system(script_command);

	create_FcstGroup_schematic(theSelectedFGName, some_widgets);
	zoom_forecastGroup(some_widgets);
	reset_interface_components(some_widgets);
	}
else    {       /*   Forecast group name is NULL - no selection has been made   */
	XBell(display, 100);
	}

}


/* ************************************************************************************

	fill_carryover_dates()

   ************************************************************************************ */

void  fill_carryover_dates(selectedWidget, start_ifp_data, call_data)
	Widget                          selectedWidget;
	FcstGroupSelectionDialogStruct  *start_ifp_data;
	XmListCallbackStruct            *call_data;
{

	Arg             wargs[5];
	int             num_items;
	int             i;
	int             n;

	char            *string;
	char            *first_blank;
	char            *carryoverGroupName;
	char            *forecastGroupName;

	XmString        **xmstr_cg_items;
	XmString        **xmstr_fg_items;

 /*     Get the number of date items for the previous carryover group...                        */
 XtSetArg(wargs[0], XmNitemCount, &num_items);
 XtGetValues(start_ifp_data->carryoverDate_list, wargs, 1);

 /*     Clear the previous dates from the 'carryoverDate_list' widget...                        */
 for(i = 1; i <= num_items; i++) XmListDeletePos(start_ifp_data->carryoverDate_list, 0);

 carryoverGroupName = (char *) malloc(15);
 memset(carryoverGroupName, '\0', 15);
 forecastGroupName = (char *) malloc(15);
 memset(forecastGroupName, '\0', 15);
 memset(theSelectedCarryoverDate, '\0', CO_DATE_LENGTH);

 /*     Extract the 'carryoverGroupName' and 'forecastGroupName' from the List widgets...       */
 n = 0;
 XtSetArg(wargs[n], XmNitems, &xmstr_cg_items); n++;
 XtGetValues(start_ifp_data->carryoverGroup_list, wargs, n);
 XmStringGetLtoR((XmString)xmstr_cg_items[call_data->item_position - 1], "charSet1", &string);

 if((first_blank = strstr(string, " ")) != NULL)
	strncpy(carryoverGroupName, string, first_blank - string);
 else   strncpy(carryoverGroupName, string, 8);

 n = 0;
 XtSetArg(wargs[n], XmNitems, &xmstr_fg_items); n++;
 XtGetValues(start_ifp_data->forecastGroup_list, wargs, n);
 XmStringGetLtoR((XmString)xmstr_fg_items[call_data->item_position - 1], "charSet1", &string);

 if((first_blank = strstr(string, " ")) != NULL)
	strncpy(forecastGroupName, string, first_blank - string);
 else   strncpy(forecastGroupName, string, 8);

 num_items = get_carryover_dates(carryoverGroupName, forecastGroupName, start_ifp_data);

 /*     Fill the 'carryoverDate_list' widget with the carryover dates...                        */
 n = 0;
 XtSetArg(wargs[n], XmNitemCount, num_items); n++;
 XtSetArg(wargs[n], XmNvisibleItemCount, num_items); n++;
 XtSetArg(wargs[n], XmNitems, start_ifp_data->carryover_dates); n++;
 XtSetValues(start_ifp_data->carryoverDate_list, wargs, n);

 highlight_default_carryover_date(start_ifp_data->carryoverDate_list,
				  start_ifp_data->carryover_dates, num_items);

}


/* ************************************************************************************

	get_carryover_dates()
		read the carryover dates from a file for the carryover group given
		by 'carryoverGroupName'; each carryover date must be on its own
		line (in Julian Hours as calculated in NWSRFS); the first value
		is the number of dates...

   ************************************************************************************ */

int get_carryover_dates(char *carryoverGroupName, char *forecastGroupName, 
                               FcstGroupSelectionDialogStruct  *start_ifp_data)
	/*char                            *carryoverGroupName;
	char                            *forecastGroupName;
	FcstGroupSelectionDialogStruct  *start_ifp_data;*/
{
	int             julda, julhr;
	int             day;
	int             month;
	int             year;
	int             hour;
	int             zondum, dlsdum;
	int             i;

	char            date_string[CO_DATE_LENGTH];

	char            tz_code[5];
        

  int   num_co_dates;
  int   co_dates[20], co_status[20];
  char  co_group_padded[8];
  char  f_group_padded[8];

  memset(co_group_padded, ' ', 8);
  for(i = 0; i < strlen(carryoverGroupName); i++)
     co_group_padded[i] = carryoverGroupName[i];

  memset(f_group_padded, ' ', 8);
  for(i = 0; i < strlen(forecastGroupName); i++)
     f_group_padded[i] = forecastGroupName[i];

  OFS_CARRYOVER_DATES(co_group_padded, f_group_padded, &num_co_dates, co_dates, co_status);
/*
 * now have number of carryover dates, the dates in julian hours since 1/1/1900,
 *  and a status for each date (i.e., incomplete/volatile   = 0,
 *                                    complete/volatile     = 1,
 *                                    incomplete/protected  = 2,
 *                                    complete/protected    = 3)
 *
 * Sort the dates in ascending order to make list easier to read.
 *  FCOBBL expects julian days, internal hours, and the number of dates
 *  as arguments.  If we give is julian hours instead of days, the values
 *  of the first argument can never be equal to each other so it will never
 *  look at the second argument (normally hours) to sort on.  FCOBBL will
 *  just swap the order of the second argument consistently with the
 *  order of the first argument.  So, we can pass the co_status as the
 *  second argument and FCOBBL will keep it in the same order as the dates.
 */
  FCOBBL(co_dates, co_status, &num_co_dates);

 memset(tz_code, '\0', 5);
 GET_OFS_DEFAULT_TZC(tz_code);

 if(carryover_date) free(carryover_date);

 carryover_date = (date *) malloc(sizeof(date) * num_co_dates);       /* Create space for the array of date structs   */
 carryover_julianHour = (int *) malloc(sizeof(int) * num_co_dates);   /* Make space for the date array in julian hrs. */

 /* if(start_ifp_data->carryover_dates == NULL) --AV 9/18/01 this check does not seem right?.-- */
 if(start_ifp_data->carryover_dates != NULL)
	XtFree((char *)start_ifp_data->carryover_dates);

 start_ifp_data->carryover_dates = (XmString *) XtMalloc(sizeof(XmString) * num_co_dates);

 /*     Fill the global arrays 'carryover_date' and 'carryover_julianHour'    */
 for(i = 0; i < num_co_dates; i++)
	{
	carryover_julianHour[i] = co_dates[i];

	get_month_day_year_hour_tzc(co_dates[i], &julda, &julhr,
				    &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);

	carryover_date[i].day = day;
	carryover_date[i].month = month;
	carryover_date[i].year = year;
	carryover_date[i].hour = hour;
	strcpy(carryover_date[i].time_zone, tz_code);

	sprintf(date_string, "%s %2d, %4d %2d, %s", month_char[month],
						    day,
						    year,
						    hour,
						    tz_code);

	if(co_status[i] == 2 || co_status[i] == 4)
		start_ifp_data->carryover_dates[i] =
			XmStringSegmentCreate(date_string, "charSet2", XmSTRING_DIRECTION_L_TO_R, FALSE);
	else    start_ifp_data->carryover_dates[i] =
			XmStringSegmentCreate(date_string, "charSet1", XmSTRING_DIRECTION_L_TO_R, FALSE);
	}

 return(num_co_dates);

}




/* ************************************************************************************

	handle_carryover_date_selection()
		gets the string of the selected carryover date from the list
		widget to a global variable

   ************************************************************************************ */

void  handle_carryover_date_selection(selectedWidget, client_data, call_data)
	Widget                          selectedWidget;
	caddr_t                         *client_data;
	XmListCallbackStruct            *call_data;
{

	char    *selected_date;


 which_carryover_date = call_data->item_position - 1;

 XmStringGetLtoR(call_data->item, "charSet1", &selected_date);

/*  printf("The selected Carryover date is: %s\n", selected_date);      */
 strcpy(theSelectedCarryoverDate, selected_date);

}



/* ***************************************************************************

	copy_current_ofs_files()

   *************************************************************************** */

void copy_current_ofs_files(w, client_data, call_data)
	Widget          w;
	caddr_t         *client_data;
	caddr_t         *call_data;
{

	whichDataToGet = COPY_CURRENT_OFS_FILES;

}



/* ***************************************************************************

	use_previous_ifp_files()

   *************************************************************************** */

void use_previous_ifp_files(w, client_data, call_data)
	Widget          w;
	caddr_t         *client_data;
	caddr_t         *call_data;
{

	whichDataToGet = USE_PREVIOUS_IFP_FILES;

}



/* ***************************************************************************

	highlight_default_carryover_date()

   *************************************************************************** */

void highlight_default_carryover_date(carryoverDate_list, carryover_dates, num_items)
	Widget          carryoverDate_list;
	XmString        *carryover_dates;
	int             num_items;
{

	char            string[50];
	char            *date_string;

	time_t          tp;
	int             i;
	int             length;
	int             the_month;
	int             the_day;
	int             the_year;
	int             the_hour;

	struct tm       *time_tm;

 time(&tp);
 time_tm = localtime(&tp);


 the_month = time_tm->tm_mon + 1;
 the_day = time_tm->tm_mday - 5;
 the_year = 1900 + time_tm->tm_year;
 the_hour = time_tm->tm_hour;


 /* Check that we haven't wrapped around the beginning of the month      */
 if(the_day < 1)
	 {       /*      Have wrapped the month...                       */
	 if(--the_month < 1)
		 {       /* Have wrapped the year...                     */
		 the_year--;
		 the_month = 12;
		 }
	 the_day = days_in_month(the_month, the_year) + the_day;
	 }

 sprintf(string, "%s %2d", month_char[the_month], the_day);

 length = strlen(string);

 for(i = 0; i < num_items; i++)
	{
	XmStringGetLtoR(carryover_dates[i], "charSet1", &date_string);
	if(strncmp(date_string, string, length) == 0)
		{
		XmListSelectPos(carryoverDate_list, i + 1, TRUE);
		break;
		}
	}
}





/* ************************************************************************************

	set_New_ForecastGroup_button_sensitive()

   ************************************************************************************ */

void set_New_ForecastGroup_button_sensitive(w, data, call_data)
	Widget                  w;
	the_widget_struct       *data;
	XmAnyCallbackStruct     *call_data;
{

 XtSetSensitive(data->new_ForecastGroup_widget, TRUE);

}


/* ************************************************************************************

	void reset_OptionMenu()

	The first item in the option_widget_list

   ************************************************************************************ */

void reset_OptionMenu(Widget w, Widget option_menu, XmAnyCallbackStruct *call_data)
{

 xs_menu_widget_struct   *option_widget_list;

/*Replaced the following ill-formed test (causing segmentation violations) - jgg  8/21/01
 if((strcmp(theSelectedFGName, NULL) != 0) && (strcmp(theSelectedCarryoverDate, NULL) != 0))*/
 if((theSelectedFGName[0] != '\0') && (theSelectedCarryoverDate != NULL))
 {
 XtVaGetValues(w, XmNuserData, &option_widget_list, NULL);
 XtVaSetValues(option_menu, XmNmenuHistory, option_widget_list->widget_array[0]->parent, NULL);

 copy_current_ofs_files(option_widget_list->widget_array[0]->parent, NULL, NULL);
 }
}


/* ************************************************************************************

	void reset_interface_components()


   ************************************************************************************ */

void reset_interface_components(the_widget_struct *data)
{

 XtSetSensitive(data->run_multiple_widget, TRUE);
 XtVaSetValues(data->run_multiple_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, SingleSegment, strlen(SingleSegment)+1,
	       NULL);

 XtSetSensitive(data->universal_widget, TRUE);     /* added by gfs 9/2/93 */
 XtSetSensitive(data->non_universal_widget, TRUE); /* added by gfs 9/2/93 */

 XtSetSensitive(run_cascade[1], TRUE);             /* added by gfs 9/2/93 */
 XtSetSensitive(run_cascade[3], TRUE);             /* added by gfs 9/2/93 */

 free(data->previous_segment);
 data->previous_segment = NULL;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/seed.c,v $";
 static char rcs_id2[] = "$Id: seed.c,v 1.8 2006/04/07 13:30:37 aivo Exp $";}
/*  ===================================================  */

}
