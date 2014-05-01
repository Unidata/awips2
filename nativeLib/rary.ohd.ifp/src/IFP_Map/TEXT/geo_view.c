
#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "drawa.h"
#include "libXs.h"
#include "help.h"
#include "ifp_atoms.h"


static void     init_DrawData();
extern char     **map_areas_in_fg();


/* ********************** Beginning of IFP Mapping Interface *********************** */



/* **********************************************************************

	void create_IFP_Mapping_interface()

	Purpose:        To create the query popup window
	Code Type:      Interface

	Called from:    create_run_interface()


   ********************************************************************** */

void create_IFP_Mapping_interface(form, menu_bar, some_widgets)
	Widget                  form;
	Widget                  menu_bar;
	the_widget_struct       *some_widgets;
{

	Widget          scroll_window;
	Widget          canvas;
	Widget          locator_rc;
	Widget          top_rc;
	Widget          bottom_rc;
	Widget          scale_rc;
	Widget          latitude_rc;
	Widget          longitude_rc;
	Widget          distance_rc;

	char            *blanks = " ";  /* Spacing to reserve room in RowColumn widgets...      */

	int             n;
	int             bg;
	int             i;
	int             j;

	Display         *display;

	MappingWidgetStruct     *mappingStruct;



/* ------------------------------------------------------------------------------------ */

 display = XtDisplay(form);

 read_color_list();
 ReadParameters();

 zoom_factor = 0.0;
 NumBasinsInCurrFcstGroup = 0;
 FGBasin_ID = NULL;


 mappingStruct = (MappingWidgetStruct *) malloc(sizeof(MappingWidgetStruct));
 some_widgets->mappingStruct = mappingStruct;

 rad_data->maximum_columns = MAXX;
 rad_data->maximum_rows = MAXY;

 rad_data->data_array = (short int **)malloc((rad_data->maximum_columns)*sizeof(short int *));

 for (i=0; i<rad_data->maximum_columns; i++)
    rad_data->data_array[i] = (short int *)malloc((rad_data->maximum_rows)*sizeof(short int));

 rad_data->levels = (int *)malloc(17*sizeof(int));
 rad_data->gc = (GC *)malloc(17*sizeof(GC));

 rad_data->origin.x = XOR;
 rad_data->origin.y = YOR;


 rad_data->states_on = istate;
 rad_data->county_on = icounty;
 rad_data->rivers_on = iriver;
 rad_data->basins_on = ibound;
 rad_data->cities_on = icity;


/* ------------------------------------------------------------------------------------ */

 initialize_data(some_widgets);

/* ------------------------------------------------------------------------------------ */


 locator_rc = XtVaCreateManagedWidget("locator_rc", xmRowColumnWidgetClass, form,
				     XmNorientation,      XmVERTICAL,
				     XmNtopAttachment,    XmATTACH_WIDGET,
				     XmNtopWidget,        menu_bar,
				     XmNrightAttachment,  XmATTACH_FORM,
				     XmNleftAttachment,   XmATTACH_FORM,
				     NULL);



 top_rc = XtVaCreateManagedWidget("Top_rc", xmRowColumnWidgetClass, locator_rc,
				  XmNpacking,          XmPACK_COLUMN,
				  XmNorientation,      XmHORIZONTAL,
				  NULL);

 bottom_rc = XtVaCreateManagedWidget("Bottom_rc", xmRowColumnWidgetClass, locator_rc,
				     XmNpacking,          XmPACK_COLUMN,
				     XmNorientation,      XmHORIZONTAL,
				     NULL);

 latitude_rc = XtVaCreateManagedWidget("Latitude_rc", xmRowColumnWidgetClass, top_rc,
				       XmNpacking,          XmPACK_COLUMN,
				       XmNorientation,      XmHORIZONTAL,
				       NULL);

 XtVaCreateManagedWidget("Latitude_label", xmLabelWidgetClass, latitude_rc,
			 XtVaTypedArg, XmNlabelString, XmRString, "Latitude:", strlen("Latitude:")+1,
			 NULL);

 some_widgets->Latitude_widget = XtVaCreateManagedWidget("Latitude_value", xmLabelWidgetClass, latitude_rc,
							XtVaTypedArg, XmNlabelString, XmRString, blanks, strlen(blanks)+1,
							NULL);


 longitude_rc = XtVaCreateManagedWidget("Longitude_rc", xmRowColumnWidgetClass, top_rc,
					XmNpacking,          XmPACK_COLUMN,
					XmNorientation,      XmHORIZONTAL,
					NULL);

 XtVaCreateManagedWidget("Longitude_label", xmLabelWidgetClass, longitude_rc,
			XtVaTypedArg, XmNlabelString, XmRString, "Longitude:", strlen("Longitude:")+1,
			NULL);

 some_widgets->Longitude_widget = XtVaCreateManagedWidget("Longitude_value", xmLabelWidgetClass, longitude_rc,
							 XtVaTypedArg, XmNlabelString, XmRString, blanks, strlen(blanks)+1,
							 NULL);



 distance_rc = XtVaCreateManagedWidget("Distance_rc", xmRowColumnWidgetClass, top_rc,
				       XmNpacking,          XmPACK_COLUMN,
				       XmNorientation,      XmHORIZONTAL,
				       NULL);

 XtVaCreateManagedWidget("Distance_label", xmLabelWidgetClass, distance_rc,
			XtVaTypedArg, XmNlabelString, XmRString, "Distance:", strlen("Distance:")+1,
			NULL);

 some_widgets->Distance_widget =
		XtVaCreateManagedWidget("Distance_value", xmLabelWidgetClass, distance_rc,
				       XtVaTypedArg, XmNlabelString, XmRString, blanks, strlen(blanks)+1,
				       NULL);


 scale_rc = XtVaCreateManagedWidget("Scale_rc", xmRowColumnWidgetClass, bottom_rc,
				    XmNorientation,      XmHORIZONTAL,
				    NULL);

 XtVaCreateManagedWidget("Scale_label", xmLabelWidgetClass, scale_rc,
			 XtVaTypedArg, XmNlabelString, XmRString, "Scale 1:", strlen("Scale 1:")+1,
			 NULL);

 some_widgets->Scale_TextField =
		XtVaCreateManagedWidget("Scale_TextField", xmTextFieldWidgetClass, scale_rc,
					XmNmaxLength,           8,
					XtVaTypedArg, XmNlabelString, XmRString, blanks, strlen(blanks)+1,
					NULL);
/* XtAddCallback(some_widgets->Scale_TextField, XmNmodifyVerifyCallback, check_scale_value, some_widgets); */
 XtAddCallback(some_widgets->Scale_TextField, XmNactivateCallback, do_scale_change, some_widgets);


 some_widgets->ForecastGroup_label =
		XtVaCreateManagedWidget("ForecastGroup_label", xmLabelWidgetClass, bottom_rc,
					XtVaTypedArg, XmNlabelString, XmRString, blanks, strlen(blanks)+1,
					NULL);


/* Add scrolled window below menubar as child of form...        */

  scroll_window = XtVaCreateManagedWidget("window", xmScrolledWindowWidgetClass, form,
					 XmNscrollingPolicy,  XmAUTOMATIC,
					 XmNtopAttachment,    XmATTACH_WIDGET,
					 XmNtopWidget,        locator_rc,
					 XmNrightAttachment,  XmATTACH_FORM,
					 XmNleftAttachment,   XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					 NULL);
 some_widgets->drawArea_SWindow = scroll_window;


/* Create drawing area as child of the scrolled window...       */

 canvas = XtVaCreateManagedWidget("canvas",xmDrawingAreaWidgetClass, scroll_window,
				 XtVaTypedArg, XmNbackground, XmRString, color_list[1], strlen(color_list[1])+1, NULL);
 some_widgets->main_canvas = canvas;



 rbdata.gc = IFP_Map_xs_create_xor_gc(canvas, "yellow", LineOnOffDash);
 rbdata.w  = canvas;

 init_DrawData(canvas, rad_data, some_widgets);

 XtAddCallback(canvas, XmNresizeCallback, fill_pixmap, some_widgets);
 XtAddCallback(canvas, XmNexposeCallback, copy_area, rad_data);

 some_widgets->mappingStruct->tools_shell = create_toolbox(global_toplevel, some_widgets);
 create_query_popup(some_widgets);


}       /* --------------- Return to calling function... -------------- */





/* ******************************************************************

	init_DrawData()

	Purpose:        initialize Pixmaps to NULL and create a graphics
			context for the DrawingArea widget...

	Code Type:      Application

	Called from:    create_IFP_Mapping_interface()


   ****************************************************************** */

void init_DrawData(w, data, widget_struct)
	Widget                  w;
	draw_struct             *data;
	the_widget_struct       *widget_struct;
{

	int             i;
	Display         *display;
	XGCValues       gcv;
	int             mask = GCForeground;




 display = XtDisplay(w);

 widget_struct->overlays = data;
 data->w = w;

 XtVaGetValues(w, XmNwidth, &data->width, XmNheight, &data->height, NULL);

 for (i = 0; i < data->num_levels; i++)
	{
	gcv.foreground = get_pixel_by_name(w, color_list[i]);
	data->gc[i] = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
	}


 data->gc[0] = XCreateGC(display, DefaultRootWindow(display), 0, NULL);
 data->pix = (Pixmap)NULL;
 data->pixbase = (Pixmap)NULL;
 data->highlightedArea_pixmap = (Pixmap)NULL;

 data->Basin_gc = NULL;

/*----------------------------------
 data->maximum_columns = MAXX;
 data->maximum_rows = MAXY;
 data->origin.x = XOR;
 data->origin.y = YOR;
  ----------------------------------*/

 data->center.x = data->origin.x + data->maximum_columns/2;
 data->center.y = data->origin.y - data->maximum_rows/2;




}


/* **********************************************************************

	void create_query_popup()

	Purpose:        To create the query popup window
	Code Type:      Interface

	Called from:    create_IFP_Mapping_interface()


   ********************************************************************** */

void create_query_popup(widget_struct)
	the_widget_struct       *widget_struct;
{

   Widget       shell, rc;



 shell = XtCreatePopupShell("Query_shell", transientShellWidgetClass, widget_struct->main_canvas, NULL, 0);
 widget_struct->mappingStruct->query_shell = shell;

 rc = XtVaCreateManagedWidget("loc_bb", xmRowColumnWidgetClass, shell, NULL);

 widget_struct->mappingStruct->MAP_Basin_label = XtVaCreateManagedWidget("Query_MAP_Basin_label", xmLabelWidgetClass, rc, NULL);
 widget_struct->mappingStruct->County_label    = XtVaCreateManagedWidget("Query_county_label", xmLabelWidgetClass, rc, NULL);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/geo_view.c,v $";
 static char rcs_id2[] = "$Id: geo_view.c,v 1.2 2006/04/07 13:29:53 aivo Exp $";}
/*  ===================================================  */

}
