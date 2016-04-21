
/* ***************************************************************************************

	create_op_table.c
		create a scrolled list in a application shell to display
		the NWSRFS operations table for the current segment;
		also allows the user to show the parameter values for the
		selected operation...


	Coded by:       Tom Adams
	Affiliation:    NWS/Office of Hydrology/Hydrologic Research Laboratory
	Date:           9/17/91
	Modified:       9/17/91
    Modified:       9/07/94 - to add new operations to list 
                                   in create_list_items function - gfs - hrl


   *************************************************************************************** */


#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "mods.h"
#include "mod_struct.h"
#include "mods_info.h"
#include "ifp_globals.h"
#include "ifp_help.h"
#include "ifp_struct.h"
#include "help.h"
#include "c_call_f/write_parameters.h" /*--Added by AV --*/
#include "Mods_flags.h"



typedef struct
	{
	XmString        *list_items;
	int             num_operations;
	}       opTable_list_struct;



typedef struct
	{
	Widget          list;
	Widget          shell;
	float           *p_float;
	int             *t_int;
	float           *ts_float;
	float           *c_float;
	}       opTable_data_struct;



static int      loct_array[MAX_OPERATIONS];
static Widget   op_show_button;
int             selectedOperation_position;
XmFontList      fontlist;

void                    show_op_values_from_list();
void                    show_op_values();
void                    select_operation();
opTable_list_struct     *create_list_items();
void                    show_parameters();
static Widget           create_viewer();


Widget create_opTable_viewer(top_widget, p_float, p_char, t_int, ts_float, c_float)
	Widget          top_widget;
	float           *p_float;
	char            p_char[][4];
	int             *t_int;
	float           *ts_float;
	float           *c_float;
{

	Widget          shell;
	Widget          swindow;
	Widget          segName_label;
	Widget          row_col_widget;
	Widget          list;

	Arg             wargs[8];
	int             n;
	int             type, format, nitems, left;
	long            offset = 0;

	char            label[9];
	char            *currentSegment;

	XmString        *xmstr_listItems;

	Display         *display;
	Window          root;


	opTable_list_struct     *data;
	opTable_data_struct     *data_struct;

        Font          font_id;
        GC            gc;
        int           mask;
        XGCValues     gcv;
         
        XmFontListEntry entry;

 shell = XtCreatePopupShell("operationsTable_viewer", topLevelShellWidgetClass, top_widget, NULL, 0);

 display = XtDisplay(shell);
 root = DefaultRootWindow(display);
 
 entry = XmFontListEntryLoad(display,
                             "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-1",
                             XmFONT_IS_FONT, "textlistfont");                             
 fontlist = XmFontListAppendEntry(NULL,entry);
  
 n = 0;
 XtSetArg(wargs[n], XmNorientation, XmVERTICAL); n++;
 XtSetArg(wargs[n], XmNnumColumns, 1); n++;
 row_col_widget = XtCreateManagedWidget
		(
		"opTable_row_col_widget",
		xmFormWidgetClass,
		shell,
		wargs,
		n
		);



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
	&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&currentSegment
	) == Success && type == IFPA_current_segment_type){ /*  We're Ok...     */ }


 n = 0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate(currentSegment, XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 segName_label = XtCreateManagedWidget
		(
		"opTable_segName_label",
		xmLabelWidgetClass,
		row_col_widget,
		wargs,
		n
		);


 n = 0;
 XtSetArg(wargs[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, segName_label); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 swindow = XtCreateManagedWidget
		(
		"opTable_scrolled_window",
		xmScrolledWindowWidgetClass,
		row_col_widget,
		wargs,
		n
		);


 data = create_list_items(p_char, t_int);

 data_struct = (opTable_data_struct *) malloc(sizeof(opTable_data_struct));
 data_struct->p_float = p_float;
 data_struct->t_int = t_int;
 data_struct->ts_float = ts_float;
 data_struct->c_float = c_float;
 data_struct->shell = shell;


 n = 0;
 XtSetArg(wargs[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
 XtSetArg(wargs[n], XmNvisibleItemCount, data->num_operations); n++;
 XtSetArg(wargs[n], XmNitems, data->list_items); n++;
 XtSetArg(wargs[n], XmNitemCount, data->num_operations); n++;
 XtSetArg(wargs[n], XmNfontList, fontlist); n++;
 list = XtCreateManagedWidget
		(
		"opTable_list",
		xmListWidgetClass,
		swindow,
		wargs,
		n
		);

 data_struct->list = list;
 XtAddCallback(list, XmNsingleSelectionCallback, select_operation, NULL);
 XtAddCallback(list, XmNdefaultActionCallback, show_op_values_from_list, data_struct);



 n = 0;
 strcpy(label, "Show");
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate(label, XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, swindow); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNshowAsDefault, (short) 1); n++;
 op_show_button = XtCreateManagedWidget
		(
		"opTable_show_button",
		xmPushButtonWidgetClass,
		row_col_widget,
		wargs,
		n
		);
 XtAddCallback(op_show_button, XmNactivateCallback, show_op_values, data_struct);
 XtSetKeyboardFocus(row_col_widget, op_show_button);
 XtSetSensitive(op_show_button, FALSE);


 XtPopup(shell, XtGrabNone);
 
 return(shell);

}



/* ********************************************************************

	show_op_values_from_list()


   ******************************************************************** */

void show_op_values_from_list(w, data_struct, call_data)
	Widget                  w;
	opTable_data_struct     *data_struct;
	XmListCallbackStruct    *call_data;
{

	char            *string;
	int             loct;



 XtSetSensitive(op_show_button, TRUE);  /* 'op_show_button' is global...        */

 XmStringGetLtoR(call_data->item, XmSTRING_DEFAULT_CHARSET, &string);

 loct = loct_array[call_data->item_position - 1];

 show_parameters(string, loct, data_struct);


}


/* ********************************************************************

	show_op_values()


   ******************************************************************** */

void show_op_values(w, data_struct, call_data)
	Widget                  w;
	opTable_data_struct     *data_struct;
	XmAnyCallbackStruct     *call_data;
{

	char            *string;
	int             loct;
	Arg             wargs[1];
	XmString        *xmstr_items;



 XtSetSensitive(op_show_button, TRUE);  /* 'op_show_button' is global...        */

 XtSetArg(wargs[0], XmNitems, &xmstr_items);
 XtGetValues(data_struct->list, wargs, 1);
 XmStringGetLtoR(xmstr_items[selectedOperation_position - 1], XmSTRING_DEFAULT_CHARSET, &string);

 loct = loct_array[selectedOperation_position - 1];

 show_parameters(string, loct, data_struct);


}



/* ********************************************************************

	select_operation()


   ******************************************************************** */

void select_operation(w, client_data, call_data)
	Widget                  w;
	caddr_t                 *client_data;
	XmListCallbackStruct    *call_data;
{


 XtSetSensitive(op_show_button, TRUE);  /* 'op_show_button' is global...        */
 selectedOperation_position = call_data->item_position;

}



/* ********************************************************************

	create_list_items()


   ******************************************************************** */

opTable_list_struct *create_list_items(p_char, t_int)
	char    p_char[][4];
	int     *t_int;
{

	int             i, j, k;
	int             length;
	int             loct, operation_number;
	int             num_i;
	int             number_of_operations;

	char            list_names[MAX_OPERATIONS * 2][10];
	char            **list_types, **list_opnames, **seg_optypes;
	char            op_string[26];

	XmString        *xmstr_operations;

	opTable_list_struct     *data;




 data = (opTable_list_struct *) malloc(sizeof(opTable_list_struct));


 for(i = 0; i < MAX_OPERATIONS * 2; i++) memset(list_names[i], '\0', 10);


/*      fill list_names with operation types and names  */

 i = 0;
 k = 0;
 loct = 1;

 while ((operation_number = t_int[loct - 1]) != -1 && i < MAX_OPERATIONS * 2)
	{
	loct_array[k] = loct;

	strcpy(list_names[i++], operation_types[operation_number - 1]);
	if(operation_number == 4)                       /* operation names not defined for      */
		memset(list_names[i++], ' ', 8);        /* CLEAR-TS operations                  */
	else    strncpy(list_names[i++], p_char[t_int[loct - 1 + 2] -1 - 5], 8);

	loct = t_int[loct-1 + 1];
	k++;
	}

 num_i = i;
 number_of_operations = i/2;

/* Set up lists of      1) different types of operations used (seg_optypes)             */
/*                      2) optypes of all operations in segment (list_types)            */
/*                      3) opnames of all operations in segment (list_names)            */

 seg_optypes = (char **) malloc(number_of_operations * sizeof(char *));
 list_types = (char **) malloc(number_of_operations * sizeof(char *));
 list_opnames = (char **) malloc(number_of_operations * sizeof(char *));
 for(i = 0; i < number_of_operations; i++)
	{
	seg_optypes[i] = (char *) malloc(sizeof(char) * 10);
	list_types[i] = (char *) malloc(sizeof(char) * 10);
	list_opnames[i] = (char *) malloc(sizeof(char) * 10);
	strcpy(list_types[i], list_names[i * 2]);
	strcpy(list_opnames[i], list_names[i * 2 + 1]);
	}


/*      Allocate space for the time-series XmString arrays...                                   */
 xmstr_operations = (XmString *)XtMalloc(sizeof(XmString) * number_of_operations);


 j = 0;
 i = 0;
 while(j < num_i)       /* Fill the XmString arrays for the time-series                         */
			/*      'Type' & 'Name' List widgets...                                 */
	{
	/* Pad the Operation name with blanks to put in space separators between it & the       */
	/*      operation type -- the font used must have fixed character spacing to make       */
	/*      it look right...                                                                */
	length = strlen(list_names[j]);

	strcpy(op_string, list_names[j]);
	for(k = 0; k < 9 - length; k++) strcat(op_string, " ");
	j++;

	length = strlen(list_names[j]);         /* Pad the Operation type with blanks, too, to  */
	strcat(op_string, list_names[j]);       /*      make life easier for us later...        */
	for(k = 0; k < 9 - length; k++) strcat(op_string, " ");

	xmstr_operations[i] = XmStringCreateLtoR(op_string, XmSTRING_DEFAULT_CHARSET);

	j++;
	i++;
	}


 data->num_operations = number_of_operations;
 data->list_items = xmstr_operations;


 return(data);

}



/* ********************************************************************

	show_parameters()


   ******************************************************************** */

void show_parameters(operation, loct, data_struct)
	char                    *operation;
	int                     loct;
	opTable_data_struct     *data_struct;
{

	Widget          textWindow;
	char            *buffer;
	char            *filename = "/.prp_temp";
	char            input_file_name[51];
	char            title[50];
	int             i;
	int             length;




 memset(input_file_name, '\0', 51);

 strcpy(input_file_name, (char *)getenv("HOME"));
 strcat(input_file_name, filename);
 length = strlen(input_file_name);

 for(i = 0; i < 50 - length; i++) strcat(input_file_name, " ");

 if(loct < 1) loct = 1;

 WRITE_PARAMETERS
		(
		input_file_name,
		data_struct->p_float,
		data_struct->t_int,
		data_struct->ts_float,
		data_struct->c_float,
		&loct
		);


 for(i = 0; i < 50; i++)
	if(input_file_name[i] == ' ')
		{
		input_file_name[i] = '\0';
		break;
		}


 buffer = (char *)GetSource(input_file_name);

/* Remove carriage control characters...        */
 if(buffer[0] == '\r') buffer[0] = ' ';
 i = 0;
 while(buffer[i] != '\0')
 {
   if(buffer[i] == '\n' && buffer[i+1] == '\r') buffer[i+1] = ' ';
   i++;
 }

 strcpy(title, "Parameters for Operation ");
 strcat(title, operation);

 textWindow = create_viewer(title, data_struct->shell);
 XmTextSetString(textWindow, buffer);

}






/* *****************************************************************************

	create_viewer()

   ***************************************************************************** */

Widget create_viewer(char *title, Widget parent_shell)
{

	Widget          shell, viewer_form, viewer_menuBar;
	Widget          help_button, viewer_cascade[1];
	Widget          viewer_Control_mainMenuItem, viewer_Help_mainMenuItem;
	Widget          viewer_sw, textWidget, viewer_close_widget;
	Widget          help_shell;

	Arg             wargs[10];
	Display         *display;
	int             i, n;

	help_struct             *help_data;
	xs_menu_widget_struct   *viewer_DisplayStruct;
	viewer_struct           *viewer_data;
	help_cb_struct          *help_cb;

	static xs_menu_struct   Viewer_display[] =
					{
					{"Close" , popdown_shell, NULL,
					 TRUE, PUSH_BUTTON, NULL, 0, NULL},
					};

 display = XtDisplay(parent_shell);

 XtSetArg(wargs[0], XmNtitle, title);
 shell = XtCreatePopupShell
		(
		"opTable_viewer",
		transientShellWidgetClass,
		parent_shell,
		wargs,
		1
		);


 help_shell = XtCreatePopupShell("opTable_viewer_help_shell", transientShellWidgetClass, shell, NULL, 0);


 XtSetArg(wargs[0], XmNrubberPositioning, TRUE);
 viewer_form = XtCreateManagedWidget("opTable_viewer_form", xmFormWidgetClass, shell, wargs, 1);

 XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
 XtSetArg(wargs[1], XmNleftAttachment, XmATTACH_FORM);
 XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
 viewer_menuBar = XmCreateMenuBar(viewer_form, "opTable_viewer_menuBar", wargs, 3);
 XtManageChild(viewer_menuBar);



/* Create the Mods main menu items...                                           */
 viewer_Control_mainMenuItem = XmCreatePulldownMenu(viewer_menuBar, "opTable_viewer_Control_mainMenuItem", NULL, 0);

 XtSetArg(wargs[0], XmNsubMenuId, viewer_Control_mainMenuItem);
 XtSetArg(wargs[1], XmNlabelString, XmStringCreate("Control", XmSTRING_DEFAULT_CHARSET));
 viewer_cascade[0] = (Widget )XmCreateCascadeButton(viewer_menuBar, "opTable_viewer_cascade", wargs, 2);
 XtManageChildren(viewer_cascade, 1);

 XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET));
 XtSetArg(wargs[1], XmNmnemonic, 'H');
 viewer_Help_mainMenuItem = (Widget )XmCreateCascadeButton(viewer_menuBar, "opTable_viewer_Help_mainMenuItem", wargs, 2);

 help_data = (help_struct *) malloc(sizeof(help_struct));
 help_data->parent = shell;
 help_data->message_widget_name = "opTable_viewer_Help_mainMenuItem";

 XtAddCallback(viewer_Help_mainMenuItem, XmNactivateCallback, popup_help_window, "OP_TABLE_VIEWER");
 XtManageChild(viewer_Help_mainMenuItem);

/* move help button to conform to Motif style guide */
 XtSetArg(wargs[0], XmNmenuHelpWidget, viewer_Help_mainMenuItem);
 XtSetValues(viewer_menuBar, wargs, 1);

 XtSetArg(wargs[0], XmNleftAttachment, XmATTACH_FORM);
 XtSetArg(wargs[1], XmNrightAttachment, XmATTACH_FORM);
 XtSetArg(wargs[2], XmNbottomAttachment, XmATTACH_FORM);
 XtSetArg(wargs[3], XmNeditMode, XmMULTI_LINE_EDIT);
 XtSetArg(wargs[4], XmNeditable, FALSE);
 XtSetArg(wargs[5], XmNscrollVertical, TRUE);
 XtSetArg(wargs[6], XmNscrollHorizontal, TRUE);
 XtSetArg(wargs[7], XmNfontList, fontlist);;
 textWidget = XmCreateScrolledText(viewer_form, "opTable_text_viewer", wargs, 8);

 XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_WIDGET);
 XtSetArg(wargs[1], XmNtopWidget, viewer_menuBar);
 XtSetValues(XtParent(textWidget), wargs, 2);


/*      Create pulldown menu items for 'Control'...                                     */
 for(i = 0; i < XtNumber(Viewer_display); i++) Viewer_display[i].data = (caddr_t) shell;
 viewer_DisplayStruct =
		(xs_menu_widget_struct *)xs_create_menu_buttons("", viewer_Control_mainMenuItem, Viewer_display, XtNumber(Viewer_display));


 n = 0;
 viewer_close_widget = viewer_DisplayStruct->widget_array[0]->parent;
 XtSetArg(wargs[n], XmNaccelerator, "Ctrl<KeyPress>K"); n++;
 XtSetArg(wargs[n], XmNacceleratorText, XmStringCreateLtoR("Ctrl + K", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetValues(viewer_close_widget, wargs, n);

 XtManageChild(textWidget);

 XtPopup(shell, XtGrabNone);
 return(textWidget);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/creatOpTable.c,v $";
 static char rcs_id2[] = "$Id: creatOpTable.c,v 1.13 2006/05/03 13:14:57 aivo Exp $";}
/*  ===================================================  */

}
