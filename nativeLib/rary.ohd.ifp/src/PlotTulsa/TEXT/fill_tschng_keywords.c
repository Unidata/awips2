/* **********************************************************************
   File: fill_tschng_keywords.c

	choose_ts_popup
		the popup menu for getting information for choose_ts for
		NWSRFS
   ********************************************************************** */

#include "libXifp.h"
#include "libXs.h"
#include "Mods_initStruct.h"
#include "Mods_opTSDataStruct.h"
#include "ifp_globals.h"
#include "ifp_help.h"
#include "ifp_struct.h"
#include "blank_pixmap.h"
#include "check_pixmap.h"
#include "Mods_tschngKeyword.h"
#include "Mods_everythingStruct.h"
#include "Mods_flags.h"


#define FIRST                             1
#define LAST                              2
#define OPTYPE                            3
#define NONE_PREVIOUSLY_SELECTED         -1



typedef struct
	{
	Widget                  bboard;                /* bulletin board widget */
	Widget                  operation_label;
	Widget                  type_label;
	Widget                  name_label;
	Widget                  sw_widget;             /* Scrolled window widget...*/
	Widget                  name_widget;           /* List widget for operation Names... */
	Widget                  type_widget;           /* List widget for operation Types... */
	Widget                  tsName_select;         /* Label widget for ts name */
	int                     number_of_operations;
	int                     operation_selected[MAX_TS][MAX_OPERATIONS];
	char                    *current_ts;           /* current time series pointer */
	char                    **mod_ts_names;        /* address of modified time series names pointer */
	int                     num_mod_ts;            /* Number of modified time series used in the current
							  forcast program. */
	int                     mod_ts_index[MAX_TS];  /* modified time series index array */
	combined_struct         *ptm_data;             /* plot Tulsa tabular info runtime mods data pointer */
	xs_menu_widget_struct   *ts_widget_array;      /* time series widget array */
	xs_menu_widget_struct   **type_widget_array;
	int                     current_ts_index;      /* Time series floating point data */
	float                   *ts_float;             /* Time series floating point data */
	char                    **ts_char;             /* address of time series character data pointer */
	TS_INFO                 *ts_info;              /* Time series information structure  */
	xs_menu_struct          *Choose_TStype_struct;
	Widget                  tsMods_select;         /* time series modify select widget */
	int                     num_ts_types;          /* number of the types of time series available */
	int                     keyword[MAX_TS];       /* keyword for time series selection */
	char                    **seg_optypes;         /* address of types of operations used pointer */
	int                     num_seg_optypes;       /* number of the different types of operations used */
	char                    **list_types;          /* types of all operations in the segment */
	char                    **list_opnames;        /* list of operation names */
	int                     *start_run;
	int                     *valid_date;           /* end of observed period */
	}       list_widget_struct;


typedef struct
	{
	char                    tsType[MAX_CHARS_IN_TSMOD_KEYWORD];  /* time series type */
	Widget                  *do_chooseType_widget;
	Widget                  tsMods_toplevel;
	list_widget_struct      *list;
	Mods_everythingStruct   *everythingData;
	}       choose_type_DataStruct;


typedef struct
	{
	choose_type_DataStruct  *data_struct;
	Widget                  check_label;
	Widget                  *timeSeries_label;
	int                     whichTimeSeries;
	}       timeSeries_struct;





int     stay_in_choose_ts;     /* TRUE, FALSE flag */
int     which_TimeSeries;
int     previous_TimeSeries;   /* previous time series selected flag */
int     number_of_TimeSeries;  /* number of modified time series     */
int     *TimeSeries_OK;        /* time series OK flag pointer        */

Pixmap  blank_pixmap;
Pixmap  check_pixmap;

Widget  done_selection;
Widget  *currentTS_checkLabel;



void    save_TStype_selection();         /* callback function to handle activating (pushing) the 'Save' button */
void    handle_list_widget_selection();  /* callback function that handles the selection of a time-series type or
					    name */
void    set_operations();                /* clears the operations type and name lists, and selects the ones
					    associated with the current time series. */
int     find_num_op_selected();          /* finds the number of operations of an optype selected */
void    Create_A2_mods();                /* Fills in the mods structure for A2 type mods (TSCHNG) */
void    handle_Keyword_selection();
void    handle_BeforeOp_selection();




void fill_tschng_keywords(p_float, p_char, ts_float, ts_char, t_int, first_time, 
                          ts_info, ptm_data, start_run, end_obs_data, locp, data)
	float            *p_float, *ts_float;
	char             p_char[][4], ts_char[][4];
	int              *t_int, *first_time, *start_run, *end_obs_data;
	TS_INFO          ts_info[];
	combined_struct  *ptm_data;
	int              *locp;
	Mods_everythingStruct *data;
{
	char            *argv[1];
	char            label[MAX_CHARS_IN_TSMOD_KEYWORD];
	char            current_time_series[17], delta_t_char[3];
	char            **mod_ts_names;
	char            list_names[MAX_OPERATIONS*2][10];
	char            **list_types, **list_opnames, **seg_optypes;
	char            tul_plot_name[9];

	Widget          tsMods_toplevel, bboard, choose_op_popup_menu;
	Widget          help_widget, tsMods_select, tsMods_select_frame;
	Widget          sw_for_list, rc_for_list;       /* Scrolled Window & RowColumn widgets */
	Widget          type_list, name_list;           /* List widgets for the time-series */
	Widget          choose_ts_menu;
	Widget          tsName_label, tsName_widget, tsName_select;
	Widget          choose_sw;
	Widget          choose_rc;
	Widget          *currentTS_label;
	Widget          *checkLabel_frame;
	Widget          *sub_menu;
	Widget          select_bb;
	Widget          menu_buttons[3];
	Widget          timeSeries_bb;

	int             argc = 1;
	int             pixel, i;
	int             background_color;
	int             screen;
	int             number_of_active, loct, operation_number;
	int             j, k, m, n, num_i;
	int             repeat;
	int             No_Default_Keyword;
	int             tul_plot_op_num;

	Arg             wargs[10];
	XFontStruct     *font;
	XmString        *xmname, *xmtype;
	Display         *display;
	Window          window;
	Window          root;
	XEvent          event;

	timeSeries_struct       *ts_display_data;
	help_struct             *help_data;
	list_widget_struct      list;
	xs_menu_struct          ts_menu[MAX_TS];
	xs_menu_widget_struct   *ts_widget_array;
	xs_menu_widget_struct   *type_widget_array;

	choose_type_DataStruct  *dataStruct;


/*******************  end of declarations *********************************************************/


previous_TimeSeries = NONE_PREVIOUSLY_SELECTED;




list.ts_float = ts_float;
list.ts_char  = (char **)ts_char;
list.ts_info  = ts_info;
list.start_run = start_run;
list.valid_date = end_obs_data;
list.Choose_TStype_struct = Choose_TStype_struct;

/* Create array of name, data_type, and delta_t info strings for modified
   time series */
mod_ts_names = (char **)malloc(ptm_data->plot->num_ts * sizeof(char *));
for(i=0; i<ptm_data->plot->num_ts; i++)
   mod_ts_names[i] = (char*)malloc(sizeof(char)*17);

k=0;
for(i=0; i<ptm_data->plot->num_ts; i++)
/* for(j=0; j<*ptm_data->plot->num_pts; j++)*/ 
   for(j=0; j<ptm_data->plot->end[i]; j++)  
      if (ptm_data->plot->ts_array[i][j] !=
	  ptm_data->plot->orig_ts_array[i][j])
      {
	 list.mod_ts_index[k] = i;
	 strcpy(mod_ts_names[k], ts_info[i].ts_id);

	 for (m = strlen(mod_ts_names[k]); m < 9; m++)
	    strcat(mod_ts_names[k], " ");

	 strcat(mod_ts_names[k], ts_info[i].data_type);

	 for (m = strlen(mod_ts_names[k]); m < 14; m++)
	    strcat(mod_ts_names[k], " ");

	 sprintf(delta_t_char, "%2d", ts_info[i].delta_t);
	 strcat(mod_ts_names[k], delta_t_char);
	 /*printf("mod_ts_names[k] = %s\n", mod_ts_names[k]);*/
	 k++;
	 break;
      }
list.num_mod_ts = k;

/* if there are no modified time series return to calling program */
if(list.num_mod_ts == 0)
{
/*   printf("no modified time series\n");  */
   return;
}
for(i = 0; i < MAX_OPERATIONS*2; i++)
		memset(list_names[i], '\0', 10);

/*      fill list_names with operation types and names  */

i = 0;
loct = 1;

while ((operation_number = t_int[loct-1]) != -1 && i < MAX_OPERATIONS * 2)
	{
	strcpy(list_names[i++], operation_types[operation_number-1]);
	if(operation_number == 4)                       /* operation names not defined for      */
		memset(list_names[i++], ' ', 8);        /* CLEAR-TS operations                  */
	else
		strncpy(list_names[i++], p_char[t_int[loct-1 + 2]-1 - 5], 8);

	loct = t_int[loct-1 + 1];
	}

num_i = i;
list.number_of_operations = i/2;

/* Set up lists of 1) different types of operations used (seg_optypes)
		   2) optypes of all operations in segment (list_types)
		   3) opnames of all operations in segment (list_names)
*/
seg_optypes = (char **)malloc(list.number_of_operations * sizeof(char *));
list_types = (char **)malloc(list.number_of_operations * sizeof(char *));
list_opnames = (char **)malloc(list.number_of_operations * sizeof(char *));
for(i=0; i<list.number_of_operations; i++)
{
   seg_optypes[i] = (char*)malloc(sizeof(char)*10);
   memset(seg_optypes[i], '\0', 10);
   list_types[i] = (char*)malloc(sizeof(char)*10);
   list_opnames[i] = (char*)malloc(sizeof(char)*10);
   strcpy(list_types[i], list_names[i*2]);
   strcpy(list_opnames[i], list_names[i*2+1]);
}

strcpy(seg_optypes[0], list_names[0]);
k=1;
for(i=1; i<list.number_of_operations; i++)
{
   repeat = FALSE;
   for(j=0; j<i; j++)
      if(strcmp(list_names[i*2], seg_optypes[j]) != 0)
	 continue;
      else
      {
	 repeat = TRUE;
	 break;
      }
   if(repeat==FALSE)
   {
      strcpy(seg_optypes[k], list_names[i*2]);
      k++;
   }
}
list.list_types = list_types;
list.list_opnames = list_opnames;
list.seg_optypes = seg_optypes;
list.num_seg_optypes = k;

/*      Allocate space for the time-series XmString arrays...                                   */
xmtype = (XmString *)XtMalloc(sizeof(XmString) * list.number_of_operations);
xmname = (XmString *)XtMalloc(sizeof(XmString) * list.number_of_operations);

j = 0;
i = 0;
while(j < num_i)       /* Fill the XmString arrays for the time-series                         */
		       /*      'Type' & 'Name' List widgets...                                 */
	{
	xmtype[i] = XmStringCreate(list_names[j], XmSTRING_DEFAULT_CHARSET);
	j++;
	xmname[i] = XmStringCreate(list_names[j], XmSTRING_DEFAULT_CHARSET);
	j++;
	i++;
	}

/* Zero out operation_selected and keyword arrays to start */
for(i=0; i<list.num_mod_ts; i++)
{
   list.keyword[i] = 0;
   for(j=0; j<list.number_of_operations; j++)
      list.operation_selected[i][j] = 0;
}

/* Determine which Tulsa Plot operation you are in */
memset(tul_plot_name, '\0', 9);
strncpy(tul_plot_name, p_char[*locp-1-5], 8);
for(i=0; i<list.number_of_operations; i++)
   if(strncmp(list_types[i], "PLOT-TUL", 8) == 0)
      if(strcmp(list_opnames[i], tul_plot_name) == 0)
	 tul_plot_op_num = i;

/* Set the default keyword for the modified time series (if any).
   For output time series (3) set default operation selected to
   current Tulsa Plot operation.
*/
for(i=0; i<list.num_mod_ts; i++)
   if(ts_info[list.mod_ts_index[i]].ts_type == 1)
      list.keyword[i] = FIRST;
   else if(ts_info[list.mod_ts_index[i]].ts_type == 3)
   {
      list.keyword[i] = OPTYPE;
      list.operation_selected[i][tul_plot_op_num] = 1;
   }
   else if(ts_info[list.mod_ts_index[i]].ts_type == 4)
      list.keyword[i] = OPTYPE;

list.current_ts_index = 0;

/* printf("number of operations = %d\n", list.number_of_operations);            */

/*     Initialize the Intrinsics and create a BulletinBoard widget as a base    */

/*        printf("first_time = %d\n", *first_time);                             */
if((*first_time)++ < 1)
	crwdgt();

dataStruct = (choose_type_DataStruct *)malloc(sizeof(choose_type_DataStruct));
/*dataStruct->everythingData = (Mods_everythingStruct *)malloc(sizeof(Mods_everythingStruct));*/
dataStruct->everythingData = data;

XtSetArg(wargs[0], XtNgeometry, "700x440+200+200");
tsMods_toplevel = XtCreateApplicationShell("tsMods_toplevel", topLevelShellWidgetClass, wargs, 1);


 display = XtDisplay(tsMods_toplevel);
 root = DefaultRootWindow(display);
 screen = DefaultScreen(display);


dataStruct->tsMods_toplevel = tsMods_toplevel;

bboard = XtCreateManagedWidget("tschng_bb", xmBulletinBoardWidgetClass, tsMods_toplevel, NULL, 0);

list.bboard = bboard;

list.num_ts_types = XtNumber(Choose_TStype_struct);

n = 0;
XtSetArg(wargs[n], XtNx, 450); n++;
XtSetArg(wargs[n], XtNy, 100); n++;
XtSetArg(wargs[n], XtNwidth, 230); n++;
XtSetArg(wargs[n], XtNheight, 300); n++;
XtSetArg(wargs[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
XtSetArg(wargs[n], XtNmappedWhenManaged, FALSE); n++;
sw_for_list = XtCreateManagedWidget
		(
		"sw_for_list",
		xmScrolledWindowWidgetClass,
		bboard,
		wargs,
		n
		);
list.sw_widget = sw_for_list;

XtSetArg(wargs[0], XmNorientation, XmHORIZONTAL);
XtSetArg(wargs[1], XmNnumColumns, 2);
rc_for_list = XtCreateManagedWidget
		(
		"rc_for_list",
		xmRowColumnWidgetClass,
		sw_for_list,
		wargs,
		2
		);


XtSetArg(wargs[0], XmNselectionPolicy, XmMULTIPLE_SELECT);
XtSetArg(wargs[1], XmNvisibleItemCount, list.number_of_operations);
XtSetArg(wargs[2], XmNdoubleClickInterval, 1);
XtSetArg(wargs[3], XmNitems, xmtype);
XtSetArg(wargs[4], XmNitemCount, list.number_of_operations);
type_list = XtCreateManagedWidget
		(
		"type_list",
		xmListWidgetClass,
		rc_for_list,
		wargs,
		5
		);


XtSetArg(wargs[0], XmNselectionPolicy, XmMULTIPLE_SELECT);
XtSetArg(wargs[1], XmNvisibleItemCount, list.number_of_operations);
XtSetArg(wargs[2], XmNdoubleClickInterval, 1);
XtSetArg(wargs[3], XmNitems, xmname);
XtSetArg(wargs[4], XmNitemCount, list.number_of_operations);
name_list = XtCreateManagedWidget
		(
		"name_list",
		xmListWidgetClass,
		rc_for_list,
		wargs,
		5
		);

list.type_widget = type_list;
list.name_widget = name_list;

dataStruct->list = &list;

XtAddCallback(type_list, XmNmultipleSelectionCallback, handle_list_widget_selection, dataStruct);
XtAddCallback(name_list, XmNmultipleSelectionCallback, handle_list_widget_selection, dataStruct);

XtSetArg(wargs[0], XtNx, 100);
XtSetArg(wargs[1], XtNy, 390);
XtSetArg(wargs[2], XtNwidth, 100);
strcpy(label, "Done");
XtSetArg(wargs[3], XmNlabelString, XmStringCreate(label, XmSTRING_DEFAULT_CHARSET));
done_selection = XtCreateManagedWidget
		(
		"done_selection",
		xmPushButtonWidgetClass,
		bboard,
		wargs,
		4
		);
XtAddCallback(done_selection, XmNactivateCallback, save_TStype_selection, dataStruct);
XtSetSensitive(done_selection, FALSE);


XtSetArg(wargs[0], XmNbackground, &background_color);
XtGetValues(bboard, wargs, 1);

n = 0;
XtSetArg(wargs[n], XmNborderColor, background_color); n++;
XtSetArg(wargs[n], XmNborderWidth, 3); n++;
XtSetArg(wargs[n], XtNx, 250); n++;
XtSetArg(wargs[n], XtNy, 390); n++;
XtSetArg(wargs[n], XtNwidth, 100); n++;
strcpy(label, "Help");
XtSetArg(wargs[n], XmNlabelString, XmStringCreate(label, XmSTRING_DEFAULT_CHARSET)); n++;
help_widget = XtCreateManagedWidget
		(
		"help_widget",
		xmPushButtonWidgetClass,
		bboard,
		wargs,
		n
		);


 help_data = (help_struct *) malloc(sizeof(help_struct));
 help_data->parent = tsMods_toplevel;
 help_data->message_widget_name = "help_widget";
 XtAddCallback(help_widget, XmNactivateCallback, create_help_Dialog, help_data);


/*  Create label of Current Time Series */
n = 0;
XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Time Series", XmSTRING_DEFAULT_CHARSET)); n++;
tsName_label = XtCreateManagedWidget
		(
		"tsName_label",
		xmLabelWidgetClass,
		bboard,
		wargs,
		n
		);

/*   Create time series name's frame and label widgets */
choose_sw = XtCreateManagedWidget
		(
		"choose_sw",
		xmScrolledWindowWidgetClass,
		bboard,
		NULL,
		0
		);

choose_rc = XtCreateManagedWidget
		(
		"choose_rc",
		xmRowColumnWidgetClass,
		choose_sw,
		NULL,
		0
		);


 dataStruct->do_chooseType_widget = (Widget *) malloc(sizeof(Widget) * list.num_mod_ts);
 checkLabel_frame = (Widget *) malloc(sizeof(Widget) * list.num_mod_ts);
 currentTS_checkLabel = (Widget *) malloc(sizeof(Widget) * list.num_mod_ts);
 currentTS_label = (Widget *) malloc(sizeof(Widget) * list.num_mod_ts);
 TimeSeries_OK = (int *) malloc(sizeof(int) * list.num_mod_ts);
 sub_menu = (Widget *) malloc(sizeof(Widget) * list.num_mod_ts);
 list.type_widget_array = (xs_menu_widget_struct **) malloc(sizeof(xs_menu_widget_struct) * list.num_mod_ts);

 pixel = get_pixel_by_name(bboard, "yellow");

 number_of_TimeSeries = list.num_mod_ts;

 for(i = 0; i < list.num_mod_ts; i++) TimeSeries_OK[i] = FALSE;



 blank_pixmap = XCreatePixmapFromBitmapData
			(
			display,
			root,
			blank_pixmap_bits,
			blank_pixmap_width,
			blank_pixmap_height,
			BlackPixel(display, screen),
			background_color,
			DefaultDepth(display, screen)
			);

 check_pixmap = XCreatePixmapFromBitmapData
			(
			display,
			root,
			check_pixmap_bits,
			check_pixmap_width,
			check_pixmap_height,
			BlackPixel(display, screen),
			background_color,
			DefaultDepth(display, screen)
			);


	/* zz */

/* Create a check label, time series label, and  option menu  as children of the RowColumn widget       */
/*      for each time series changed; we should make exactly 'list.num_mod_ts' of them...               */

 for(j = 0; j < list.num_mod_ts; j++)
	{
	Do_First = FALSE;
	Do_Last = FALSE;
	Do_Before_operation = FALSE;

	No_Default_Keyword = FALSE;

	ts_display_data = (timeSeries_struct *) malloc(sizeof(timeSeries_struct));

	ts_display_data->whichTimeSeries = j;
	ts_display_data->data_struct = dataStruct;
	ts_display_data->timeSeries_label = currentTS_label;


	n = 0;
	XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
	timeSeries_bb = XmCreateRowColumn(choose_rc, "timeSeries_bb", wargs, n);
	XtManageChild(timeSeries_bb);

	checkLabel_frame[j] = (Widget )XmCreateFrame(timeSeries_bb, "checkLabel_frame", NULL, 0);
	XtManageChild(checkLabel_frame[j]);

	n = 0;
	XtSetArg(wargs[n], XmNlabelType, XmPIXMAP); n++;
	XtSetArg(wargs[n], XmNlabelPixmap, blank_pixmap); n++;
	currentTS_checkLabel[j] = XmCreateLabel(checkLabel_frame[j], "choose_checkLabel", wargs, n);
	XtManageChild(currentTS_checkLabel[j]);
	ts_display_data->check_label = currentTS_checkLabel[j];

	n = 0;
	XtSetArg(wargs[n], XmNlabelString, XmStringCreateLtoR(mod_ts_names[j], XmSTRING_DEFAULT_CHARSET)); n++;
	XtSetArg(wargs[n], XmNborderWidth, 1); n++;
	XtSetArg(wargs[n], XmNborderColor, background_color); n++;
	currentTS_label[j] = XmCreateLabel(timeSeries_bb, "timeSeries_label", wargs, n);
	XtManageChild(currentTS_label[j]);

	select_bb = XmCreateBulletinBoard(timeSeries_bb, "select_bb", NULL, 0);

	sub_menu[j] = XmCreatePulldownMenu(select_bb, "chooseTS_submenu", NULL, 0);



	/* Set proper keyword options for each time series...                                   */
	mods_keywords(list.ts_info[list.mod_ts_index[j]].ts_id,
		      list.ts_info[list.mod_ts_index[j]].data_type,
		      list.ts_info[list.mod_ts_index[j]].delta_t, list.ts_float,
		      list.ts_char, list.Choose_TStype_struct,
		      list.num_ts_types);

	for(i = 0; i < list.num_ts_types; i++)
		{
		if(i == 0) Do_First = list.Choose_TStype_struct[i].active;
		else if(i == 1) Do_Last = list.Choose_TStype_struct[i].active;
		else Do_Before_operation = list.Choose_TStype_struct[i].active;
		}

	if(Do_First)
		{
		if(Do_Last)
			{ /* Keywords: "First", "Last", & "Before Operation"... */

			No_Default_Keyword = TRUE;

			for(i = 0; i < XtNumber(Choose_TStype_struct); i++)
				{
				/*callback_data = (callback_dataStruct *) malloc(sizeof(callback_dataStruct));*/
				Choose_TStype_struct[i].data = (caddr_t) dataStruct;
				}

			/*  Create the Main menu pane.                  */
			type_widget_array =
				(xs_menu_widget_struct *)xs_create_menu_buttons("", sub_menu[j],
							Choose_TStype_struct, XtNumber(Choose_TStype_struct));

			for(i = 0; i < XtNumber(Choose_TStype_struct); i++)
				{
				if(i < (XtNumber(Choose_TStype_struct) - 1))
					/* Add callback to place a check for 'Before' & 'Last' selection        */
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_Keyword_selection,
							ts_display_data);
				else    /* A callback that removes the check for 'Before operation' selection   */
					{
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_BeforeOp_selection,
							ts_display_data);
					XtAddCallback(type_widget_array->widget_array[i]->parent, XmNactivateCallback,
						      set_operations, &list);
					}
				}
			}
		else    { /* Keywords: "First" & "Before Operation"...          */
			for(i = 0; i < XtNumber(Choose_TStype_struct_FB); i++)
				Choose_TStype_struct_FB[i].data = (caddr_t) dataStruct;


			XtSetArg(wargs[0], XmNlabelPixmap, check_pixmap);
			XtSetValues(currentTS_checkLabel[j], wargs, 1);

			TimeSeries_OK[j] = TRUE;
			list.keyword[j] = FIRST;
			if(list.num_mod_ts == 1 ) XtSetSensitive(done_selection, TRUE);

			/*  Create the Main menu pane.                  */
			type_widget_array =
				(xs_menu_widget_struct *)xs_create_menu_buttons("", sub_menu[j],
							Choose_TStype_struct_FB, XtNumber(Choose_TStype_struct_FB));

			for(i = 0; i < XtNumber(Choose_TStype_struct_FB); i++)
				{
				if(i < (XtNumber(Choose_TStype_struct_FB) - 1))
					/* Add callback to place a check for 'Before' & 'Last' selection        */
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_Keyword_selection,
							ts_display_data);
				else    /* A callback that removes the check for 'Before operation' selection   */
					{
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_BeforeOp_selection,
							ts_display_data);
					XtAddCallback(type_widget_array->widget_array[i]->parent, XmNactivateCallback,
						      set_operations, &list);
					}
				}
			}
		}
	else if(Do_Last)
		{
		if(Do_First)
			{ /* Keywords: "First", "Last", & "Before Operation"... */

			No_Default_Keyword = TRUE;

			for(i = 0; i < XtNumber(Choose_TStype_struct); i++)
				Choose_TStype_struct[i].data = (caddr_t) dataStruct;

			/*  Create the Main menu pane.                  */
			type_widget_array =
				 (xs_menu_widget_struct *)xs_create_menu_buttons("", sub_menu[j],
							Choose_TStype_struct, XtNumber(Choose_TStype_struct));

			for(i = 0; i < XtNumber(Choose_TStype_struct); i++)
				{
				if(i < (XtNumber(Choose_TStype_struct) - 1))
					/* Add callback to place a check for 'Before' & 'Last' selection        */
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_Keyword_selection,
							ts_display_data);
				else    /* A callback that removes the check for 'Before operation' selection   */
					{
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_BeforeOp_selection,
							ts_display_data);
					XtAddCallback(type_widget_array->widget_array[i]->parent, XmNactivateCallback,
						      set_operations, &list);
					}
				}
			}
		else    { /* Keywords: "Last" & "Before Operation"...           */
			for(i = 0; i < XtNumber(Choose_TStype_struct_BL); i++)
				Choose_TStype_struct_BL[i].data = (caddr_t) dataStruct;


			XtSetArg(wargs[0], XmNlabelPixmap, check_pixmap);
			XtSetValues(currentTS_checkLabel[j], wargs, 1);

			TimeSeries_OK[j] = TRUE;
			list.keyword[j] = OPTYPE;
			if(list.num_mod_ts == 1 ) XtSetSensitive(done_selection, TRUE);


			/*  Create the Main menu pane.                  */
			type_widget_array =
				(xs_menu_widget_struct *)xs_create_menu_buttons("", sub_menu[j],
							Choose_TStype_struct_BL, XtNumber(Choose_TStype_struct_BL));

			for(i = 0; i < XtNumber(Choose_TStype_struct_BL); i++)
				{
				if(i >= (XtNumber(Choose_TStype_struct_BL) - 1))

					/* Add callback to place a check for 'Before' & 'Last' selection        */
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_Keyword_selection,
							ts_display_data);
				else    /* A callback that removes the check for 'Before operation' selection   */
					{
					XtAddCallback(type_widget_array->widget_array[i]->parent,
							XmNarmCallback,
							handle_BeforeOp_selection,
							ts_display_data);
					XtAddCallback(type_widget_array->widget_array[i]->parent, XmNactivateCallback,
						      set_operations, &list);
					}
				}
			}
		}
	else    {         /* Keyword: "Before Operation"...                     */
		for(i = 0; i < XtNumber(Choose_TStype_struct_B); i++)
			Choose_TStype_struct_B[i].data = (caddr_t) dataStruct;

		/*  Create the Main menu pane.                  */
		type_widget_array =
			(xs_menu_widget_struct *)xs_create_menu_buttons("", sub_menu[j],
						Choose_TStype_struct_B, XtNumber(Choose_TStype_struct_B));

		for(i = 0; i < XtNumber(Choose_TStype_struct_B); i++)
			{
			XtAddCallback(type_widget_array->widget_array[i]->parent, XmNarmCallback,
				      handle_BeforeOp_selection, ts_display_data);
			XtAddCallback(type_widget_array->widget_array[i]->parent, XmNactivateCallback,
				      set_operations, &list);
			}
		}





	list.type_widget_array[j] = type_widget_array;



	/*  Create the Menu Manager with a "yellow" border   */
	n = 0;
	if(No_Default_Keyword)
		{
		XtSetArg(wargs[n], XmNlabelString, XmStringCreate("None", XmSTRING_DEFAULT_CHARSET));
		n++;
		}
	else    {
		XtSetArg(wargs[n], XmNmenuHistory, type_widget_array->widget_array[0]->parent);
		n++;
		}
	XtSetArg(wargs[n], XmNwhichButton, (unsigned int) 1); n++;
	XtSetArg(wargs[n], XmNsubMenuId, sub_menu[j]); n++;
	choose_op_popup_menu = XmCreateOptionMenu(select_bb, "choose_op_popup_menu", wargs, n);
	XtManageChild(choose_op_popup_menu);


	dataStruct->do_chooseType_widget[j] = choose_op_popup_menu;
	list.tsMods_select = choose_op_popup_menu;


	pixel = get_pixel_by_name(tsMods_toplevel, "yellow");
	XtSetArg(wargs[0], XtNborderColor, pixel);
	XtSetValues(choose_op_popup_menu, wargs, 1);

	XtManageChild(select_bb);

	}



list.ts_widget_array = NULL;
list.ptm_data = ptm_data;
list.mod_ts_names = mod_ts_names;


n = 0;
XtSetArg(wargs[n], XmNlabelString,
	 XmStringCreate("Set time series Mods keywords", XmSTRING_DEFAULT_CHARSET)); n++;
pixel = get_pixel_by_name(tsMods_toplevel, "black");
XtSetArg(wargs[n], XtNborderColor, pixel); n++;
pixel = get_pixel_by_name(tsMods_toplevel, "navy");
XtSetArg(wargs[n], XtNforeground, pixel); n++;
XtCreateManagedWidget
		(
		"message1",
		xmLabelWidgetClass,
		bboard,
		wargs,
		n
		);

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Keyword", XmSTRING_DEFAULT_CHARSET)); n++;
XtCreateManagedWidget
		(
		"message2",
		xmLabelWidgetClass,
		bboard,
		wargs,
		n
		);

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Operations", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNx, 505); n++;
XtSetArg(wargs[n], XtNy, 50); n++;
XtSetArg(wargs[n], XtNmappedWhenManaged, FALSE); n++;
list.operation_label = XtCreateManagedWidget
		       (
		       "operation_label",
		       xmLabelWidgetClass,
		       bboard,
		       wargs,
		       n
		       );

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Type", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNx, 480); n++;
XtSetArg(wargs[n], XtNy, 70); n++;
XtSetArg(wargs[n], XtNmappedWhenManaged, FALSE); n++;
list.type_label = XtCreateManagedWidget
		  (
		  "type_label",
		  xmLabelWidgetClass,
		  bboard,
		  wargs,
		  n
		  );

n = 0;
XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Name", XmSTRING_DEFAULT_CHARSET)); n++;
XtSetArg(wargs[n], XtNx, 580); n++;
XtSetArg(wargs[n], XtNy, 70); n++;
XtSetArg(wargs[n], XtNmappedWhenManaged, FALSE); n++;
list.name_label = XtCreateManagedWidget
		  (
		  "name_label",
		  xmLabelWidgetClass,
		  bboard,
		  wargs,
		  n
		  );



XtRealizeWidget(tsMods_toplevel);
stay_in_choose_ts = TRUE;

while(stay_in_choose_ts)
     {
	XtNextEvent(&event);
	XtDispatchEvent(&event);
     }       /** end of event loop **/

}       /* ----------------------------  E N D  O F  M A I N  -------------------------- */





/* ************************  Define callbacks for each menu entry. *********************  */




/* *************************************************************************************

	do_first()

   ************************************************************************************* */

void do_first(w, dataStruct, call_data)
	Widget                  w;
	choose_type_DataStruct  *dataStruct;
	caddr_t                 call_data;
{
	Arg             wargs[1];
	int             i, numOperationsSelected;
	XmString        *typeArray, *nameArray;




dataStruct->list->keyword[which_TimeSeries] = FIRST;
strcpy(dataStruct->tsType, Choose_TStype_struct[0].name);

XmListDeselectAllItems(dataStruct->list->type_widget);
XmListDeselectAllItems(dataStruct->list->name_widget);

for(i = 0; i < dataStruct->list->number_of_operations; i++)
   dataStruct->list->operation_selected[which_TimeSeries][i] = 0;

XUnmapSubwindows(XtDisplay(dataStruct->list->sw_widget), XtWindow(dataStruct->list->sw_widget));
XUnmapWindow(XtDisplay(dataStruct->list->sw_widget), XtWindow(dataStruct->list->sw_widget));
XUnmapWindow(XtDisplay(dataStruct->list->operation_label), XtWindow (dataStruct->list->operation_label));
XUnmapWindow(XtDisplay(dataStruct->list->type_label), XtWindow (dataStruct->list->type_label));
XUnmapWindow(XtDisplay(dataStruct->list->name_label), XtWindow (dataStruct->list->name_label));

}



/* *************************************************************************************

	do_last()

   ************************************************************************************* */

void do_last(w, dataStruct, call_data)
	Widget                  w;
	choose_type_DataStruct  *dataStruct;
	caddr_t                 call_data;
{
	Arg             wargs[1];
	int             i;


/*dataStruct->list->keyword[dataStruct->list->current_ts_index] = LAST;*/
dataStruct->list->keyword[which_TimeSeries] = LAST;
strcpy(dataStruct->tsType, Choose_TStype_struct[1].name);


XmListDeselectAllItems(dataStruct->list->type_widget);
XmListDeselectAllItems(dataStruct->list->name_widget);

for(i=0; i<dataStruct->list->number_of_operations; i++)
   dataStruct->list->operation_selected[which_TimeSeries][i] = 0;

XUnmapSubwindows(XtDisplay(dataStruct->list->sw_widget), XtWindow(dataStruct->list->sw_widget));
XUnmapWindow(XtDisplay(dataStruct->list->sw_widget), XtWindow(dataStruct->list->sw_widget));
XUnmapWindow(XtDisplay(dataStruct->list->operation_label), XtWindow (dataStruct->list->operation_label));
XUnmapWindow(XtDisplay(dataStruct->list->type_label), XtWindow (dataStruct->list->type_label));
XUnmapWindow(XtDisplay(dataStruct->list->name_label), XtWindow (dataStruct->list->name_label));
}



/* *************************************************************************************

	do_opType()

   ************************************************************************************* */

void do_opType(w, dataStruct, call_data)
	Widget                  w;
	choose_type_DataStruct  *dataStruct;
	caddr_t                 call_data;
{
	Arg             wargs[1];



dataStruct->list->keyword[which_TimeSeries] = OPTYPE;
strcpy(dataStruct->tsType, Choose_TStype_struct[2].name);


XMapSubwindows(XtDisplay(dataStruct->list->sw_widget), XtWindow(dataStruct->list->sw_widget));
XMapWindow(XtDisplay(dataStruct->list->sw_widget), XtWindow(dataStruct->list->sw_widget));
XMapWindow(XtDisplay(dataStruct->list->operation_label), XtWindow (dataStruct->list->operation_label));
XMapWindow(XtDisplay(dataStruct->list->type_label), XtWindow (dataStruct->list->type_label));
XMapWindow(XtDisplay(dataStruct->list->name_label), XtWindow (dataStruct->list->name_label));

}




/* ************************************************************************************

	save_TStype_selection()
		callback function to handle activating (pushing) the 'Save' button


   ************************************************************************************ */

void save_TStype_selection(w, data, call_data)
	Widget                  w;
	choose_type_DataStruct  *data;
	XmAnyCallbackStruct     *call_data;
{
	int             numOperationsSelected, *num_sel, total_sel;
	int             i, j, k;
	int             data_points;
	Arg             wargs[1];
	char            *string_type, *string_name;
	XmString        *typeArray, *nameArray;
	char            *keywd, *optype, *opname;




keywd = (char*)malloc(6 * sizeof(char));
optype = (char*)malloc(9 * sizeof(char));
opname = (char*)malloc(9 * sizeof(char));
num_sel = (int*)malloc(data->list->num_seg_optypes * sizeof(int));


for(i=0; i<data->list->num_mod_ts; i++)
{
   if(data->list->keyword[i] == 0)
   {
      printf("error: no keyword set for %s %s\n",
	      data->list->ts_info[data->list->mod_ts_index[i]].ts_id,
	      data->list->ts_info[data->list->mod_ts_index[i]].data_type);
      return;
   }
   else if(data->list->keyword[i] == OPTYPE)
   {
      /* Fill num_sel array and check to make sure at least
	 one operation was selected for each ts */
      total_sel = 0;
      for(j=0; j<data->list->num_seg_optypes; j++)
      {
	 num_sel[j] = find_num_op_selected(data, j, i);
	 if (num_sel[j] == -1)  /* all of that type were selected */
	    total_sel += 1;
	 else
	    total_sel += num_sel[j];
      }

      if(total_sel == 0)
      {
	 printf("error: no operations selected for %s %s\n",
		 data->list->ts_info[data->list->mod_ts_index[i]].ts_id,
		 data->list->ts_info[data->list->mod_ts_index[i]].data_type);
	 return;
      }
   }
}

for(i=0; i<data->list->num_mod_ts; i++)
{
   data_points = data->list->ptm_data->plot->end[data->list->mod_ts_index[i]];
       
   if(data->list->keyword[i] == FIRST || data->list->keyword[i] == LAST)
   {
      strcpy(optype," ");
      strcpy(opname," ");
      if(data->list->keyword[i] == FIRST)
	 strcpy(keywd, "FIRST");
      else if(data->list->keyword[i] == LAST)
	 strcpy(keywd, "LAST");

      Create_A2_mods(data->list->ptm_data->plot->orig_ts_array,
		     data->list->ptm_data->plot->ts_array,
		     data->list->ptm_data->plot->num_ts,
                     data_points,
		     data->list->start_run,
		     data->list->valid_date,
		     data->list->ptm_data->seg_name,
		     data->list->ts_info,
		     data->list->mod_ts_index[i],
		     keywd, optype, opname, data->everythingData);
   }
   else if(data->list->keyword[i] == OPTYPE)
   {

      for(j=0; j<data->list->num_seg_optypes; j++)
      {
	 num_sel[j] = find_num_op_selected(data, j, i);
	 if(num_sel[j] == -1)
	 {
	     strcpy(optype, data->list->seg_optypes[j]);
	     strcpy(keywd," ");
	     strcpy(opname," ");

	     Create_A2_mods(data->list->ptm_data->plot->orig_ts_array,
			    data->list->ptm_data->plot->ts_array,
			    data->list->ptm_data->plot->num_ts,
                            data_points,
			    data->list->start_run,
			    data->list->valid_date,
			    data->list->ptm_data->seg_name,
			    data->list->ts_info,
			    data->list->mod_ts_index[i],
			    keywd, optype, opname, data->everythingData);
	 }
	 else if(num_sel[j] > 0)
	    for(k=0; k<data->list->number_of_operations; k++)
	       if(strcmp(data->list->seg_optypes[j], data->list->list_types[k]) == 0 &&
		  data->list->operation_selected[i][k] == 1)
	       {
		  strcpy(keywd," ");
		  strcpy(optype, data->list->list_types[k]);
		  strcpy(opname, data->list->list_opnames[k]);
		  Create_A2_mods(data->list->ptm_data->plot->orig_ts_array,
			         data->list->ptm_data->plot->ts_array,
			         data->list->ptm_data->plot->num_ts,
                                 data_points,
			         data->list->start_run,
			         data->list->valid_date,
			         data->list->ptm_data->seg_name,
			         data->list->ts_info,
			         data->list->mod_ts_index[i],
			         keywd, optype, opname, data->everythingData);
	       }
      }      /* end of for loop */
   }     /* end of else OPTYPE */
}     /* end of for loop */

 XtDestroyWidget(data->tsMods_toplevel);
 XFlush(XtDisplay(data->tsMods_toplevel));
 stay_in_choose_ts = FALSE;

}

/* ***********************************************************************************

	handle_list_widget_selection()
		callback function that handles the selection of a time-series
		'Type' or 'Name'...


   *********************************************************************************** */

void handle_list_widget_selection(w, dataStruct, call_data)
	Widget                  w;
	choose_type_DataStruct  *dataStruct;
	XmListCallbackStruct    *call_data;
{

	char            *widget_string, selected_operation_type[9];
	Arg             wargs[3];

	int             current_pos, i, j, numTypes = 0;
	int             num_selectedItems;
	int             numberOK = 0;

	XmString        *xmType_array, *xmName_array;



	XtSetArg(wargs[0], XmNitems, &xmType_array);
	XtGetValues(dataStruct->list->type_widget, wargs, 1);
	XtSetArg(wargs[0], XmNitems, &xmName_array);
	XtGetValues(dataStruct->list->name_widget, wargs, 1);

if(w == dataStruct->list->name_widget)   /*  A time-series 'Name' has been selected...*/
	{                               /*  Change one row only...*/

	if(dataStruct->list->operation_selected
			     [dataStruct->list->current_ts_index][call_data->item_position - 1] == 0)
		{                       /*  The item is not currently highlighted, so highlight */
					/*      its companion 'Type' item...  */
		XmListSelectPos(dataStruct->list->type_widget, call_data->item_position, FALSE);
		dataStruct->list->operation_selected
				  [dataStruct->list->current_ts_index][call_data->item_position - 1] = 1;
		}
	else    {                       /*  The item & its companion are highlighted, so deselect them  */
		XmListDeselectPos(dataStruct->list->type_widget, call_data->item_position);
		dataStruct->list->operation_selected
				  [dataStruct->list->current_ts_index][call_data->item_position - 1] = 0;
		}
	}
else    {       /*  A time-series 'Type' has been selected, so handle it & all other items of  */
		/*      the same type...   */

	if(dataStruct->list->operation_selected
			     [dataStruct->list->current_ts_index][call_data->item_position - 1] == 0)
		{                       /*  The item is not currently selected, so select its */
					/*      companion & all others of the same type... */
		XmListSelectPos(dataStruct->list->name_widget, call_data->item_position, FALSE);
		dataStruct->list->operation_selected
				  [dataStruct->list->current_ts_index][call_data->item_position - 1] = 1;

		for(j = 0; j < dataStruct->list->number_of_operations; j++)
			{
			if(XmStringCompare(call_data->item, xmType_array[j]) == TRUE)
				{
				if( j + 1 != call_data->item_position)
					{
					if(dataStruct->list->operation_selected
							     [dataStruct->list->current_ts_index][j] == 0)
						{            /*  Only select it if it is currently not selected  */
						XmListSelectPos(dataStruct->list->type_widget, j + 1, FALSE);
						XmListSelectPos(dataStruct->list->name_widget, j + 1, FALSE);
						dataStruct->list->operation_selected
								  [dataStruct->list->current_ts_index][j] = 1;
						}
					}
				}
			}
		}
	else    {       /*  The item & other items of the same type and their companions are currently  */
			/*      selected, so deselect them...                                           */
		XmListDeselectPos(dataStruct->list->name_widget, call_data->item_position);
		dataStruct->list->operation_selected
				  [dataStruct->list->current_ts_index][call_data->item_position - 1] = 0;

		for(j = 0; j < dataStruct->list->number_of_operations; j++)
			{
			if(XmStringCompare(call_data->item, xmType_array[j]) == TRUE)
				{
				if( j + 1 != call_data->item_position)
					{
					if(dataStruct->list->operation_selected
							     [dataStruct->list->current_ts_index][j] == 1)
						{            /*  Only unselect it if it is currently selected  */
						XmListDeselectPos(dataStruct->list->type_widget, j + 1);
						XmListDeselectPos(dataStruct->list->name_widget, j + 1);
						dataStruct->list->operation_selected
								  [dataStruct->list->current_ts_index][j] = 0;
						}
					}
				}
			}
		}
	}                                       /*  end else            */


 XtSetArg(wargs[0], XmNselectedItemCount, &num_selectedItems);
 XtGetValues(w, wargs, 1);

 if(num_selectedItems)
	{
	XtSetArg(wargs[0], XmNlabelPixmap, check_pixmap);
	XtSetValues(currentTS_checkLabel[which_TimeSeries], wargs, 1);

	TimeSeries_OK[which_TimeSeries] = TRUE;

	for(i = 0; i < number_of_TimeSeries; i++) numberOK += TimeSeries_OK[i];
	if(numberOK == number_of_TimeSeries) XtSetSensitive(done_selection, TRUE);
	}
 else   {
	XtSetArg(wargs[0], XmNlabelPixmap, blank_pixmap);
	XtSetValues(currentTS_checkLabel[which_TimeSeries], wargs, 1);

	TimeSeries_OK[which_TimeSeries] = FALSE;
	XtSetSensitive(done_selection, FALSE);
	}

}                                               /*  end of the function */


/* ***********************************************************************************************

	set_operations()
		    clears the operations type and name lists, and selects the ones
		    associated with the current time series.

   *********************************************************************************************** */

void set_operations(w, list, call_data)
	Widget                    w;
	list_widget_struct        *list;
	XmAnyCallbackStruct       *call_data;
{

	Arg     wargs[10];
	int     i, j, n;
	char    label[MAX_CHARS_IN_TSMOD_KEYWORD];




 list->current_ts_index = which_TimeSeries;

 /* Clear out list */
 XmListDeselectAllItems(list->type_widget);
 XmListDeselectAllItems(list->name_widget);

 /* Select operations previously chosen */
 for(j = 0; j < list->number_of_operations; j++)
	 if(list->operation_selected[list->current_ts_index][j] == 1)
		 {
		 XmListSelectPos(list->type_widget, j+1, FALSE);
		 XmListSelectPos(list->name_widget, j+1, FALSE);
		 }

}



/* ***********************************************************************************

	find_num_op_selected()
		finds the number of operations of an optype selected and
		returns a -1 if all are selected and the number selected
		if not.

   *********************************************************************************** */

int find_num_op_selected(data, index, ts_num)
	choose_type_DataStruct  *data;
	int                     index, ts_num;
{

	int   num_selected, all_selected;
	int   i, j;
	char  string;



 num_selected = 0;
 all_selected = TRUE;

 for(i = 0; i < data->list->number_of_operations; i++)
	{
	if(strcmp(data->list->list_types[i], data->list->seg_optypes[index]) == 0)
		{
		if(data->list->operation_selected[ts_num][i] == 1) num_selected += 1;
		else all_selected = FALSE;
		}
	}


 if(all_selected == TRUE) return(-1);
 else return(num_selected);

}



/* ***********************************************************************************

	handle_Keyword_selection()

   *********************************************************************************** */

void handle_Keyword_selection(w, ts_display_data, call_data)
	Widget                  w;
	timeSeries_struct       *ts_display_data;
	XmAnyCallbackStruct     *call_data;
{

	Arg             wargs[1];
	int             i;
	int             numberOK = 0;
	int             pixel;
	int             background;




 which_TimeSeries = ts_display_data->whichTimeSeries;


 if(previous_TimeSeries != which_TimeSeries)
	{
	pixel = get_pixel_by_name(w, "yellow");

	/* Highlight the current time series label...                   */
	XtSetArg(wargs[0], XmNborderColor, pixel);
	XtSetValues(ts_display_data->timeSeries_label[which_TimeSeries], wargs, 1);

	if(previous_TimeSeries != NONE_PREVIOUSLY_SELECTED)
		{
		/* Get the background color of the previous time series label...*/
		XtSetArg(wargs[0], XmNbackground, &background);
		XtGetValues(ts_display_data->timeSeries_label[previous_TimeSeries], wargs, 1);

		/* Unhighlight the previous time series label...                */
		XtSetArg(wargs[0], XmNborderColor, background);
		XtSetValues(ts_display_data->timeSeries_label[previous_TimeSeries], wargs, 1);
		}
	}


 XtSetArg(wargs[0], XmNlabelPixmap, check_pixmap);
 XtSetValues(ts_display_data->check_label, wargs, 1);

 TimeSeries_OK[ts_display_data->whichTimeSeries] = TRUE;

 for(i = 0; i < number_of_TimeSeries; i++) numberOK += TimeSeries_OK[i];
 if(numberOK == number_of_TimeSeries) XtSetSensitive(done_selection, TRUE);


 previous_TimeSeries = which_TimeSeries;

}



/* ***********************************************************************************

	handle_BeforeOp_selection()

   *********************************************************************************** */

void handle_BeforeOp_selection(w, ts_display_data, call_data)
	Widget                  w;
	timeSeries_struct       *ts_display_data;
	XmAnyCallbackStruct     *call_data;
{

	Arg             wargs[1];
	int             num_selectedItems;
	int             j;
	int             total_selected = 0;
	int             *num_selected;
	int             pixel;
	int             background;




 which_TimeSeries = ts_display_data->whichTimeSeries;


 if(previous_TimeSeries != which_TimeSeries)
	{
	pixel = get_pixel_by_name(w, "yellow");

	/* Highlight the current time series label...                   */
	XtSetArg(wargs[0], XmNborderColor, pixel);
	XtSetValues(ts_display_data->timeSeries_label[which_TimeSeries], wargs, 1);

	if(previous_TimeSeries != NONE_PREVIOUSLY_SELECTED)
		{
		/* Get the background color of the previous time series label...*/
		XtSetArg(wargs[0], XmNbackground, &background);
		XtGetValues(ts_display_data->timeSeries_label[previous_TimeSeries], wargs, 1);

		/* Unhighlight the previous time series label...                */
		XtSetArg(wargs[0], XmNborderColor, background);
		XtSetValues(ts_display_data->timeSeries_label[previous_TimeSeries], wargs, 1);
		}
	}


 num_selected = (int *) malloc(ts_display_data->data_struct->list->num_seg_optypes * sizeof(int));

 if(ts_display_data->data_struct->list->keyword[which_TimeSeries] == OPTYPE)
	 {       /* Fill num_selected array and check to make sure at least      */
		 /*   one operation was selected for each ts                     */
	 total_selected = 0;
	 for(j = 0; j < ts_display_data->data_struct->list->num_seg_optypes; j++)
		 {
		 num_selected[j] = find_num_op_selected(ts_display_data->data_struct, j, which_TimeSeries);
		 if (num_selected[j] == -1) total_selected++;    /* All of that type were selected       */
		 else total_selected += num_selected[j];
		 }
	 }

 
 if(total_selected == 0)
	{
	XtSetArg(wargs[0], XmNlabelPixmap, blank_pixmap);
	XtSetValues(ts_display_data->check_label, wargs, 1);

	TimeSeries_OK[ts_display_data->whichTimeSeries] = FALSE;
	XtSetSensitive(done_selection, FALSE);

	TimeSeries_OK[ts_display_data->whichTimeSeries] = FALSE;
	}

 previous_TimeSeries = which_TimeSeries;



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/fill_tschng_keywords.c,v $";
 static char rcs_id2[] = "$Id: fill_tschng_keywords.c,v 1.12 2006/05/03 13:11:45 aivo Exp $";}
/*  ===================================================  */

}




