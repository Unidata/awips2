
/* ************************************************************************************************************

	select_next.c :
			source file containing the code to create a popup list for selecting a segment
			to run in the NWSRFS and the Interactive Forecast Program (the graphical user
			interface to NWSRFS). The list contains the names of segments already run during
			the current forecast run; 'select_next' is used by the forecaster to make a new
			run for a segment that, say, has produced downstream consequences that require
			an adjustment -> this means that the segments downstream from the selected segment
			will be rerun...

	Coded by     :  Tom Adams (NWS/Office of Hydrology/Hydrologic Research Laboratory)
	Date         :  4/09/91
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


void    set_nextSegment();
void    IFP_Map_popdown_shell();
void    flag_NextSegment_selected();






/* *****************************************************************************

	create_selectNext_popup()

   ***************************************************************************** */

void create_selectNext_popup(some_widgets)
	the_widget_struct       *some_widgets;
{

	Widget          shell, list_form, list_menuBar;
	Widget          help_button, list_cascade[1];
	Widget          list_Control_mainMenuItem, list_Help_mainMenuItem;
	Widget          sw_for_list, listWidget, close_selectList_widget;
	Widget          select_widget;
	Widget          help_shell;

	Arg             wargs[10];
	Display         *display;
	int             i;
	int             j;
	char            modCommand[9];

	help_struct     *help_data;
	help_cb_struct  *context_help;

	xs_menu_widget_struct   *selectStruct;


static xs_menu_struct Select_next_display[] =
	{
	{"Select" , set_nextSegment, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{NULL , NULL, NULL, TRUE, 0, NULL, 0, NULL},
	{"Close" , IFP_Map_popdown_shell, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
	};


display = XtDisplay(global_toplevel);


XtSetArg(wargs[0], XmNmappedWhenManaged, FALSE);
shell = XtCreateManagedWidget
		(
		"mods_list",
		topLevelShellWidgetClass,
		global_toplevel,
		wargs,
		1
		);
some_widgets->select_next_shellWidget = shell;

/*      Create a transientShell for the context sensitive help window...                                */
help_shell = XtCreatePopupShell("select_help_shell", transientShellWidgetClass, global_toplevel, NULL, 0);

XtSetArg(wargs[0], XmNrubberPositioning, TRUE);
list_form = XtCreateManagedWidget("sn_list_form", xmFormWidgetClass, shell, wargs, 1);

XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
XtSetArg(wargs[1], XmNleftAttachment, XmATTACH_FORM);
XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
list_menuBar = XmCreateMenuBar(list_form, "sn_list_menuBar", wargs, 3);
XtManageChild(list_menuBar);



/* Create the Mods main menu items...                                           */
list_Control_mainMenuItem = XmCreatePulldownMenu(list_menuBar, "sn_list_Control_mainMenuItem", NULL, 0);

XtSetArg(wargs[0], XmNsubMenuId, list_Control_mainMenuItem);
XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Control", XmSTRING_DEFAULT_CHARSET));
list_cascade[0] = XmCreateCascadeButton(list_menuBar, "sn_list_Mods_cascade", wargs, 2);
context_help = (help_cb_struct *) malloc(sizeof(help_cb_struct));
context_help->top_help_shell = help_shell;
context_help->widget_name = "sn_list_Mods_cascade";
XtAddEventHandler(list_cascade[0], EnterWindowMask, FALSE, help_event_handler, context_help);
XtManageChildren(list_cascade, 1);

XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET));
XtSetArg(wargs[1], XmNmnemonic, 'H');
list_Help_mainMenuItem = XmCreateCascadeButton(list_menuBar, "sn_list_Help_mainMenuItem", wargs, 2);

help_data = (help_struct *) malloc(sizeof(help_struct));
help_data->parent = global_toplevel;
help_data->message_widget_name = "sn_list_Help_mainMenuItem";

XtAddCallback(list_Help_mainMenuItem, XmNactivateCallback, create_help_Dialog, help_data);
XtManageChild(list_Help_mainMenuItem);

/*      Create pulldown menu items for 'Control'...                                     */
for(i = 0; i < XtNumber(Select_next_display); i++) Select_next_display[i].data = (caddr_t) some_widgets;
selectStruct =
       (xs_menu_widget_struct *)xs_create_menu_buttons("", list_Control_mainMenuItem, Select_next_display, XtNumber(Select_next_display));

j = 0;
for(i = 0; i < XtNumber(Select_next_display); i++)
	{
	if(Select_next_display[i].name != NULL)
		{
		context_help = (help_cb_struct *)malloc(sizeof(help_cb_struct));
		context_help->top_help_shell = help_shell;
		context_help->widget_name = Select_next_display[i].name;
		XtAddEventHandler(selectStruct->widget_array[j++]->parent, EnterWindowMask,
				  FALSE, help_event_handler, context_help);
		}
	}



XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_WIDGET);
XtSetArg(wargs[1], XmNtopWidget, list_menuBar);
XtSetArg(wargs[2], XmNleftAttachment, XmATTACH_FORM);
XtSetArg(wargs[3], XmNrightAttachment, XmATTACH_FORM);
XtSetArg(wargs[4], XmNbottomAttachment, XmATTACH_FORM);
sw_for_list = XtCreateManagedWidget("nextSegment_list_sw", xmScrolledWindowWidgetClass, list_form, wargs, 5);

XtSetArg(wargs[0], XmNselectionPolicy, XmSINGLE_SELECT);
XtSetArg(wargs[1], XmNscrollBarDisplayPolicy, XmAUTOMATIC);
XtSetArg(wargs[2], XmNtraversalOn, TRUE);
listWidget = XtCreateManagedWidget("selectNext_list", xmListWidgetClass, sw_for_list, wargs, 3);

some_widgets->selectNext_listWidget = listWidget;

select_widget = selectStruct->widget_array[0]->parent;
some_widgets->select_widget = select_widget;

XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>S");
XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + S", XmSTRING_DEFAULT_CHARSET));
XtSetValues(select_widget, wargs, 2);

close_selectList_widget = selectStruct->widget_array[1]->parent;
XtSetArg(wargs[0], XmNaccelerator, "Ctrl<KeyPress>K");
XtSetArg(wargs[1], XmNacceleratorText, XmStringCreateLtoR("Ctrl + K", XmSTRING_DEFAULT_CHARSET));
XtSetValues(close_selectList_widget, wargs, 2);

XtAddCallback(listWidget, XmNdefaultActionCallback, select_nextSegment, some_widgets);
XtAddCallback(listWidget, XmNsingleSelectionCallback, flag_NextSegment_selected, some_widgets);

XtRealizeWidget(shell);

}





/* ****************************************************************************************

	select_nextSegment()


   **************************************************************************************** */

void    select_nextSegment(w, some_widgets, call_data)
	Widget                  w;
	the_widget_struct       *some_widgets;
	caddr_t                 *call_data;
{

	Display         *display;
	Window          root;

	char            *first_blank;



 display = XtDisplay(w);
 root = DefaultRootWindow(display);


 XChangeProperty(       /* Window change property for the next forecast segment name...         */
	display,
	root,
	IFPA_goto_upstream_segment,
	IFPA_goto_upstream_segment_type,
	8,
	PropModeReplace,
	(unsigned char *) some_widgets->segment_selected,
	strlen(some_widgets->segment_selected)
	);



/*      Remove the blank spaces from the end of 'string' returned by 'XmStringGetLtoR'...       */
 if((first_blank = strstr(some_widgets->segment_selected, " ")) != NULL) *first_blank = '\0';

/* Pass non-universal techniques to other NWSRFS/IFP components         */
/*      through an X Window property...                                 */
 non_univ_techs_to_window_property(some_widgets->segment_selected, w, some_widgets);

}

/* ****************************************************************************************



   **************************************************************************************** */

void    IFP_Map_popdown_shell(w, some_widgets, call_data)
	Widget                  w;
	the_widget_struct       *some_widgets;
	XmAnyCallbackStruct     *call_data;
{

XtUnmapWidget(some_widgets->select_next_shellWidget);

}

/* ****************************************************************************************

	flag_NextSegment_selected()
		sets a global flag indicating which segment in the nextSegment
		 list was selected...

   **************************************************************************************** */

void    flag_NextSegment_selected(w, some_widgets, call_data)
	Widget                  w;
	the_widget_struct       *some_widgets;
	XmListCallbackStruct     *call_data;
{

 selectNext_itemPosition = call_data->item_position - 1;

}


/* ****************************************************************************************

	set_nextSegment()


   **************************************************************************************** */

void    set_nextSegment(w, some_widgets, call_data)
	Widget                  w;
	the_widget_struct       *some_widgets;
	XmAnyCallbackStruct    *call_data;
{

	Arg             wargs[1];
	Display         *display;
	Window          root;

	XmString        *xmstringItems;
	XmString        *xmstring_Segments;
	XmString        xmstr_newSegment;

	char            *string;
	char            *first_blank;
	char            *segment_name;

	int             number_of_Segments;
	int             itemPosition;
	int             j;


 display = XtDisplay(w);
 root = DefaultRootWindow(display);

 segment_name = (char *) malloc(9);
 memset(segment_name, '\0', 9);

 XtSetArg(wargs[0], XmNitems, &xmstringItems);
 XtGetValues(some_widgets->selectNext_listWidget, wargs, 1);

 XmStringGetLtoR(xmstringItems[selectNext_itemPosition], XmSTRING_DEFAULT_CHARSET, &string);

 if(string != NULL)
	{
	XChangeProperty(        /* Window change property for the next forecast segment name... */
		display,
		root,
		IFPA_goto_upstream_segment,
		IFPA_goto_upstream_segment_type,
		8,
		PropModeReplace,
		(unsigned char *) string,
		strlen(string)
		);

	xmstr_newSegment = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);

	/*      Remove the blank spaces from the end of 'string' returned by 'XmStringGetLtoR'...       */
	if((first_blank = strstr(string, " ")) != NULL)
		strncpy(segment_name, string, first_blank - string);

        /* The following 2 lines added to handle case where                    */
	/*  the segment name is 8 characters long and therefore there is no    */
	/*  first_blank found.  The string must still be copied into the       */
	/*  segment_name to be used by non_univ_techs_to_window_property.      */
	/* Changed by gfs - hrl - 20 Aug 1994.                                 */
	else
		strncpy(segment_name, string, 8);

	/* Pass non-universal techniques to other NWSRFS/IFP components         */
	/*      through an X Window property...                                 */
	non_univ_techs_to_window_property(segment_name, w, some_widgets);

	XtSetSensitive(some_widgets->next_widget, TRUE);
	inLastSegment = FALSE;

	XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
	XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
	XtGetValues(some_widgets->selectNext_listWidget, wargs, 2);

	itemPosition = 0;
	for(j = 0; j < number_of_Segments; j++)
		{
		itemPosition++;
		if(XmStringCompare(xmstring_Segments[j], xmstr_newSegment) == TRUE) break;
		}

	for(j = itemPosition; j <= number_of_Segments; j++)
		{
		XmListDeletePos(some_widgets->selectNext_listWidget, 0);
		}

/*        if(itemPosition == 1) XtSetSensitive(some_widgets->selectNext_widget, FALSE); */
	}
 else   printf("No segment has been selected... try again\n");
 XtUnmapWidget(some_widgets->select_next_shellWidget);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/select_next.c,v $";
 static char rcs_id2[] = "$Id: select_next.c,v 1.2 2006/04/07 13:30:41 aivo Exp $";}
/*  ===================================================  */

}



