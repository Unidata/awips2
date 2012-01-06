
/* ******************************************************************************************

	events.c
		contains event handlers for the IFP mapping functions


	Coded by:       Tom Adams
	Date:           09/14/92
	Affiliation:    NOAA/NWS/Office of Hydrology/HRL

	Functions in events.c:
				void   locate()
				void   close_locate()
				void   select_basin()
				void   track_locator()
				void   show_location()
				void   clear_location()
				void   start_ScaleTool_rb()
				void   track_ScaleTool_rb()
				void   end_ScaleTool_rb()
				point  convert_screen_coords_to_HRAP()
				float  calculate_distance_from_LatLong()
				XPoint *RegionFound()
				void   start_CircleTool_rb()
				void   track_CircleTool_rb()
				void   end_CircleTool_rb()
				void   select_ForecastGroup()
				void   get_ForecastGroup_ID()
				void   CallSegmentWidgetCallbacks()


   ****************************************************************************************** */



#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"
#include "drawa.h"

static point    convert_screen_coords_to_HRAP();
static float    calculate_distance_from_LatLong();
static XPoint   *RegionFound();
static void     CallSegmentWidgetCallbacks();




/* **************************************************************************

	void locate();

	Functions called:
			RegionFound();
			HrapToLatLong();

   ******************************************** BEGIN locate ***************** */

void locate(w, widget_struct, event)
   Widget               w;
   the_widget_struct    *widget_struct;
   XEvent               *event;
{

	int          array_pos;
	int          x;
	int          y;
	int          i;
	char         str[80];
	char         segmentName[9];
	Display      *display;

	XPoint       *bpoints;
	XPoint       *cpoints;

	node         *segment_node;


 x = event->xbutton.x;
 y = event->xbutton.y;


 display = XtDisplay(w);

 /*---------------------------------------------------------------------*/
 /*     check to see if we're in the right window...                    */
 /*---------------------------------------------------------------------*/

 if (XtWindow(w) != event->xbutton.window)
	{
	XBell(display, 100);
	return;
	}


 /* -------------- MAP Basins -------------- */
 if((bpoints = RegionFound(x, y, rad_data, mapbasin, nummap, &array_pos)) != NULL)
	{
	strcpy(segmentName, mapbasin[array_pos]->id);
	sprintf(str,"MAP Basin: %s\n", segmentName);
	}
 else strcpy(str, "MAP Basin: Not Found");

 XtVaSetValues(widget_struct->mappingStruct->MAP_Basin_label,
	       XtVaTypedArg, XmNlabelString, XmRString, str, strlen(str)+1,
	       NULL);



 /* --------------- Counties --------------- */
 if((cpoints = RegionFound(x, y, rad_data, county, numcounty, &array_pos)) != NULL)
	sprintf(str,"County: %s\n", county[array_pos]->id);
 else strcpy(str, "County: Not Found");

 XtVaSetValues(widget_struct->mappingStruct->County_label,
	       XtVaTypedArg, XmNlabelString, XmRString, str, strlen(str)+1,
	       NULL);

 free(bpoints);
 free(cpoints);

 if(widget_struct->tree_shell != NULL)
	{
	for(i = 0; i <= sub_group_num; i++)
		if((segment_node = find_it(widget_struct->head[i], segmentName)) != NULL) break;

	if(segment_node != NULL) XtPopup(segment_node->popup_shell, XtGrabNone);
	else XBell(display, 100);
	}
 else   {
	XtPopup(widget_struct->mappingStruct->query_shell, XtGrabNone);
	XtAddEventHandler(w, ButtonReleaseMask, FALSE, close_locate, widget_struct);
	}

}




/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/stage3_callbacks.c                       */
/*  FUNCTION NAME:   close_locate()                                        */
/*       FUNCTION:   close locator shell                                   */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) stage3_interface

Functions called:
   none

******************************************** BEGIN close_locate ************/

void close_locate(w, widget_struct, event)
   Widget               w;
   the_widget_struct    *widget_struct;
   XEvent               *event;
{

 XtRemoveEventHandler(widget_struct->main_canvas, ButtonReleaseMask, FALSE, close_locate, widget_struct);
 XtPopdown(widget_struct->mappingStruct->query_shell);

}





/* ************************************************************************

	void select_basin()


   ************************************************************************ */

void select_basin(w, widget_struct, event)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XEvent                  *event;
{

	Display         *display;
	XPoint          *points;
	XGCValues       gcv;
	XtCallbackList  CallbackList;

	int             numbasin;
	int             mask = GCForeground;
	int             array_pos;
	int             i;

	overlay_struct  **basin;
	node            *found;


 display = XtDisplay(w);

 /*--------------------------------------------------------------*/
 /*     Create graphics context, if not already created...       */
 /*--------------------------------------------------------------*/

 if(rad_data->Basin_gc == NULL)
	{
	gcv.foreground = get_pixel_by_name(w, color_list[18]);
	rad_data->Basin_gc = (GC *) malloc(sizeof(GC));
	*rad_data->Basin_gc = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
	}


 /*--------------------------------------------------------------*/
 /*     Find if we've clicked in a basin & fill it with the      */
 /*     selected color (above)...                                */
 /*--------------------------------------------------------------*/

 basin = mapbasin;
 numbasin = nummap;

 if((points = RegionFound(event->xbutton.x, event->xbutton.y, rad_data, basin, numbasin, &array_pos)) != NULL)
	{       /* We've found the region the user has clicked in...            */

	/*----------------------------------------------*/
	/* Test if the Forecast Group data has          */
	/* been loaded; if it has, tree_shell != NULL   */
	/*----------------------------------------------*/

	if(widget_struct->tree_shell != NULL)
		{
		/*----------------------------------------------*/
		/* Find the 'node' structure corresponding to   */
		/* the NWSRFS Segment of the MAP basin...       */
		/*----------------------------------------------*/

		for(i = 0; i <= sub_group_num; i++)
			if((found = find_it(widget_struct->head[i], basin[array_pos]->SegmentID)) != NULL) break;

		if(found == NULL)
			{       /* The user is trying to select a segment (basin) that either is not    */
				/* in the current Forecast Group or is in one that has been deleted     */
				/* from the Forecast Group for this NWSRFS run only...                  */

			printf("Can't find %s\n", basin[array_pos]->id);

			XBell(XtDisplay(w), 100);
			return;
			}
		else    {
			if(!XtIsSensitive(found->parent_widget))
				{       /* The user is trying to select a segment (basin) in a tree     */
					/* that's inactive, so don't allow it...                        */

				printf("The parent tree widget is insensitive\n");

				XBell(XtDisplay(w), 100);
				return;
				}

			/*----------------------------------------------*/
			/* Call the callbacks as if the user had        */
			/* clicked on a segment PushButton widget (a    */
			/* child of 'tree_shell')...                    */
			/*----------------------------------------------*/

			CallSegmentWidgetCallbacks(found->segment_widget, widget_struct, found);
			/* printf("%s selected\n", found->e19.name);    */
			}
		}
	else    {       /*----------------------------------------------*/
			/* The Forecast Group data has not been read    */
			/* so, simply highlight the selected segment    */
			/* MAP basins...                                */
			/*----------------------------------------------*/

		XFillPolygon(display, XtWindow(rad_data->w), *rad_data->Basin_gc,
			     points, basin[array_pos]->npts, Nonconvex, CoordModeOrigin);
		XFillPolygon(display, rad_data->pix, *rad_data->Basin_gc,
			     points, basin[array_pos]->npts, Nonconvex, CoordModeOrigin);
		}
	}

 if(points != NULL) free(points);

}



/* **************************************************************************

	void track_locator();


   ************************************************************************** */

void track_locator(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{

	int          x, y;
	point        hrap;
	HRAP         LatLong;
	char         string[20];





 x  = event->xbutton.x;
 y  = event->xbutton.y;

 hrap = convert_screen_coords_to_HRAP(rad_data, x, y);

 LatLong = HrapToLatLong(hrap);

 sprintf(string,"%.4f", LatLong.y);
 XtVaSetValues(widget_struct->Latitude_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);

 sprintf(string,"%.4f", LatLong.x);
 XtVaSetValues(widget_struct->Longitude_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);


}




/* **************************************************************************

	void show_location();


   ************************************************************************** */

void show_location(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{

	int          x, y;
	point        hrap;
	HRAP         LatLong;
	char         string[20];





 x  = event->xbutton.x;
 y  = event->xbutton.y;

 hrap = convert_screen_coords_to_HRAP(rad_data, x, y);

 LatLong = HrapToLatLong(hrap);

 sprintf(string,"%.4f", LatLong.y);
 XtVaSetValues(widget_struct->Latitude_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);

 sprintf(string,"%.4f", LatLong.x);
 XtVaSetValues(widget_struct->Longitude_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);


}




/* **************************************************************************

	void clear_location();


   ************************************************************************** */

void clear_location(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{

	char    *string = " ";




 XtVaSetValues(widget_struct->Latitude_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);

 XtVaSetValues(widget_struct->Longitude_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);


}




/* **************************************************************************

	void start_ScaleTool_rb();


   ************************************************************************** */

void start_ScaleTool_rb(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{

/* if (event->xbutton.button != 1) return;      */

/*  puts("Inside 'start_ScaleTool_rb()'...");   */

 widget_struct->ScaleToolData->last.x = widget_struct->ScaleToolData->start.x = event->xbutton.x;
 widget_struct->ScaleToolData->last.y = widget_struct->ScaleToolData->start.y = event->xbutton.y;

 /*
 XDrawLine(XtDisplay(w), XtWindow(w),
		widget_struct->ScaleToolData->gc,
		0, 0,
		widget_struct->ScaleToolData->last.x,  widget_struct->ScaleToolData->last.y);
 */

 XDrawLine(XtDisplay(w), XtWindow(w),
		widget_struct->ScaleToolData->gc,
		widget_struct->ScaleToolData->start.x, widget_struct->ScaleToolData->start.y,
		widget_struct->ScaleToolData->last.x,  widget_struct->ScaleToolData->last.y);

}






/* **************************************************************************

	void track_ScaleTool_rb();


   ************************************************************************** */

void track_ScaleTool_rb(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{

	HRAP         LatLong_start;
	HRAP         LatLong_end;
	char         string[20];
	point        hrap;
	float        distance;




 /*------------------------------------------------------------------*/
 /* Draw once to clear the previous line.                            */
 /*------------------------------------------------------------------*/

 XDrawLine(XtDisplay(w), XtWindow(w),
		widget_struct->ScaleToolData->gc,
		widget_struct->ScaleToolData->start.x, widget_struct->ScaleToolData->start.y,
		widget_struct->ScaleToolData->last.x,  widget_struct->ScaleToolData->last.y);

 /*------------------------------------------------------------------*/
 /* Update the end points.                                           */
 /*------------------------------------------------------------------*/

 widget_struct->ScaleToolData->last.x  = event->xbutton.x;
 widget_struct->ScaleToolData->last.y  = event->xbutton.y;

 /*------------------------------------------------------------------*/
 /* Draw the new line.                                               */
 /*------------------------------------------------------------------*/

 XDrawLine(XtDisplay(w), XtWindow(w),
		widget_struct->ScaleToolData->gc,
		widget_struct->ScaleToolData->start.x, widget_struct->ScaleToolData->start.y,
		widget_struct->ScaleToolData->last.x,  widget_struct->ScaleToolData->last.y);



 /*------------------------------------------------------------------*/
 /* Update Distance in the Label widget...                           */
 /*------------------------------------------------------------------*/

 hrap = convert_screen_coords_to_HRAP(rad_data, widget_struct->ScaleToolData->start.x, widget_struct->ScaleToolData->start.y);
 LatLong_start = HrapToLatLong(hrap);

 hrap = convert_screen_coords_to_HRAP(rad_data, widget_struct->ScaleToolData->last.x, widget_struct->ScaleToolData->last.y);
 LatLong_end = HrapToLatLong(hrap);

 distance = calculate_distance_from_LatLong(&LatLong_start, &LatLong_end);

 sprintf(string,"%.1f", distance);
 XtVaSetValues(widget_struct->Distance_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);

}



/* **************************************************************************

	void end_ScaleTool_rb();


   ************************************************************************** */

void end_ScaleTool_rb(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{

	char    *string = " ";



 XDrawLine(XtDisplay(w), XtWindow(w),
		widget_struct->ScaleToolData->gc,
		widget_struct->ScaleToolData->start.x, widget_struct->ScaleToolData->start.y,
		widget_struct->ScaleToolData->last.x,  widget_struct->ScaleToolData->last.y);

 /*------------------------------------------------------------------*/
 /* Update the end points.                                           */
 /*------------------------------------------------------------------*/
/*
 widget_struct->ScaleToolData->last.x  = event->xbutton.x;
 widget_struct->ScaleToolData->last.y  = event->xbutton.y;
*/

 widget_struct->ScaleToolData->start.x  = 0;
 widget_struct->ScaleToolData->start.y  = 0;
 widget_struct->ScaleToolData->last.x  = 0;
 widget_struct->ScaleToolData->last.y  = 0;

 XtVaSetValues(widget_struct->Distance_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);


}







/* ******************************************************************

	point convert_screen_coords_to_HRAP()


   ****************************************************************** */

point convert_screen_coords_to_HRAP(data, x, y)
	draw_struct  *data;
	int          x;
	int          y;
{

	int          x_pixels_per_bin, y_pixels_per_bin;
	point        hrap;




 /*---------------------------------------------------------------------*/
 /*     find location in HRAP coordinates                               */
 /*---------------------------------------------------------------------*/

 x_pixels_per_bin = (float)data->width/(float)data->maximum_columns;
 y_pixels_per_bin = (float)data->height/(float)data->maximum_rows;

 if (x_pixels_per_bin > y_pixels_per_bin) x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin) y_pixels_per_bin = x_pixels_per_bin;


 hrap.x = data->origin.x + (x/x_pixels_per_bin);
 hrap.y = data->origin.y + (data->maximum_rows - y/y_pixels_per_bin);

 return(hrap);

}



/* ******************************************************************

	float calculate_distance_from_LatLong()


   ****************************************************************** */

float calculate_distance_from_LatLong(LatLong_start, LatLong_end)
	HRAP    *LatLong_start;
	HRAP    *LatLong_end;
{

	float   distance;
	double  angle;          /* Angle (in Radians) between the two points along a great circle...    */
	double  theta1;         /* Longitude (in Radians) of the starting point...                      */
	double  theta2;         /* Longitude (in Radians) of the ending point...                        */
	double  phi1;           /* Latitude (in Radians) of the starting point...                       */
	double  phi2;           /* Latitude (in Radians) of the ending point...                         */

 theta1 = (double)(PI*(double)LatLong_start->x/(double)180.);
 phi1   = (double)(PI*(double)LatLong_start->y/(double)180.);

 theta2 = (double)(PI*(double)LatLong_end->x/(double)180.);
 phi2   = (double)(PI*(double)LatLong_end->y/(double)180.);

 angle = acos((cos(phi1)*cos(theta1)*cos(phi2)*cos(theta2) +
	       cos(phi1)*sin(theta1)*cos(phi2)*sin(theta2) +
	       sin(phi1)*sin(phi2))/
	       (sqrt(cos(phi1)*cos(theta1)*cos(phi1)*cos(theta1) +
		     cos(phi1)*sin(theta1)*cos(phi1)*sin(theta1) +
		     sin(phi1)*sin(phi1)) *
		sqrt(cos(phi2)*cos(theta2)*cos(phi2)*cos(theta2) +
		     cos(phi2)*sin(theta2)*cos(phi2)*sin(theta2) +
		     sin(phi2)*sin(phi2))));


/* ----------------------------------------------------------------------

  Added by gfs - 9/26/92 to check for NaNQ value returned from acos()
  some times when numinator and denominator of argument equal to 1.00...

  isnan() function returns a non-zero value if its argument is NaNQ,
  otherwise, returns a zero

  '3956.' is the approx. mean radius of the Earth, in miles...

   ---------------------------------------------------------------------- */

 if(isnan(angle) == 0) distance = angle * (double)3956.0;
 else distance = (double)0.0;


 return(distance);
}



/* ************************************************************************

	XPoint *RegionFound()


   ************************************************************************ */

XPoint *RegionFound(x, y, data, array, Number, array_position)
	int                     x;
	int                     y;
	draw_struct             *data;
	overlay_struct          **array;
	int                     Number;
	int                     *array_position;
{

	int             x_pixels_per_bin;
	int             y_pixels_per_bin;
	int             i;
	int             j;
	int             maxpts = 0;
	int             mask = GCForeground;
	int             region_found = FALSE;
	int             ForecastGroupFound = FALSE;

	float           value;

	char            *temp_ptr;
	char            FGroup_ID[9];

	XPoint          *points;
	Region          tempRegion;





 /*---------------------------------------------------------------------*/
 /*     find location in HRAP coordinates                               */
 /*---------------------------------------------------------------------*/

 x_pixels_per_bin = (float)data->width/(float)data->maximum_columns;
 y_pixels_per_bin = (float)data->height/(float)data->maximum_rows;

 if (x_pixels_per_bin > y_pixels_per_bin) x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin) y_pixels_per_bin = x_pixels_per_bin;


 /*--------------------------------------------------------------*/
 /*     Find if we've clicked in a basin & fill it with the      */
 /*     selected color (above), if we have...                    */
 /*--------------------------------------------------------------*/

 points = (XPoint *) malloc(50 * sizeof(XPoint));


 for(i = 0; i < Number; i++)
	if (array[i]->npts > maxpts) maxpts = array[i]->npts;

 for (i = 0; i < Number; i++)
	{
	if(array[i]->npts > 0)
		{
		points = (XPoint *) realloc(points, array[i]->npts * sizeof(XPoint));

		for (j = 0; j < array[i]->npts; j++)
			{
			points[j].x = (array[i]->hrap[j].x - data->origin.x) * x_pixels_per_bin;
			points[j].y = (data->maximum_rows - (array[i]->hrap[j].y - data->origin.y)) * y_pixels_per_bin;
			}
		tempRegion = XPolygonRegion(points, array[i]->npts, EvenOddRule);

		if(XPointInRegion(tempRegion, x, y))
			{
			region_found = *array_position = i;
			XDestroyRegion(tempRegion);
			break;
			}
		else XDestroyRegion(tempRegion);
		}
	}


 if(region_found) return(points);
 else return(NULL);

}




/* **************************************************************************

	void start_CircleTool_rb();


   ************************************************************************** */

void start_CircleTool_rb(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{



 /* Set angles to draw full circles...                                  */

  widget_struct->CircleToolData->circle[0].angle1 = widget_struct->CircleToolData->circle[1].angle1 = 0;
  widget_struct->CircleToolData->circle[0].angle2 = widget_struct->CircleToolData->circle[1].angle2 = 360 * 64;

 /* Set center of circle (this will not change for current circle),     */
 /* and initial values for upper left corner and width and height       */
 /* of bounding box.                                                    */

  widget_struct->CircleToolData->center_x = event->xbutton.x;
  widget_struct->CircleToolData->center_y = event->xbutton.y;

  widget_struct->CircleToolData->circle[0].x  =  widget_struct->CircleToolData->circle[1].x = event->xbutton.x;
  widget_struct->CircleToolData->circle[0].y  =  widget_struct->CircleToolData->circle[1].y = event->xbutton.y;

  widget_struct->CircleToolData->circle[0].width  =  widget_struct->CircleToolData->circle[1].width = 0;
  widget_struct->CircleToolData->circle[0].height  =  widget_struct->CircleToolData->circle[1].height = 0;

 /* Draw circle - should just be a dot...                               */

  XDrawArc(XtDisplay(w), XtWindow(w),
	   widget_struct->CircleToolData->gc,
	   widget_struct->CircleToolData->circle[1].x, widget_struct->CircleToolData->circle[1].y,
	   widget_struct->CircleToolData->circle[1].width, widget_struct->CircleToolData->circle[1].height,
	   widget_struct->CircleToolData->circle[1].angle1, widget_struct->CircleToolData->circle[1].angle2);
}



/* **************************************************************************

	void track_CircleTool_rb()


   ************************************************************************** */

void track_CircleTool_rb(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{


	HRAP         LatLong_start;
	HRAP         LatLong_end;
	char         string[20];
	point        hrap;
	float        distance;

	short        radius;
	double       sqrt_argument;



  /* Move previous circle from location 1 to 0 in widget_struct->CircleToolData->circle array...       */

  widget_struct->CircleToolData->circle[0].x = widget_struct->CircleToolData->circle[1].x;
  widget_struct->CircleToolData->circle[0].y = widget_struct->CircleToolData->circle[1].y;
  widget_struct->CircleToolData->circle[0].width = widget_struct->CircleToolData->circle[1].width;
  widget_struct->CircleToolData->circle[0].height = widget_struct->CircleToolData->circle[1].height;

 /* Fill location 1 in circle array based on new mouse location.        */
 /* Find the radius of the new circle based on center and               */
 /*  current mouse location.                                            */

  sqrt_argument = (event->xbutton.x - widget_struct->CircleToolData->center_x) *
		  (event->xbutton.x - widget_struct->CircleToolData->center_x)
		  +
		  (event->xbutton.y - widget_struct->CircleToolData->center_y) *
		  (event->xbutton.y - widget_struct->CircleToolData->center_y);

  radius = (short) sqrt(sqrt_argument);

 /* Compute new x, y, width, and height based on circle center          */
 /*  and new radius.                                                    */

  widget_struct->CircleToolData->circle[1].x = widget_struct->CircleToolData->center_x - radius;
  widget_struct->CircleToolData->circle[1].y = widget_struct->CircleToolData->center_y - radius;
  widget_struct->CircleToolData->circle[1].width = 2 * radius;
  widget_struct->CircleToolData->circle[1].height = 2 * radius;

 /* Draw both circles.  Circle 0 gets erased, circle 1 gets drawn.      */

  XDrawArcs(XtDisplay(w), XtWindow(w), widget_struct->CircleToolData->gc,
	    widget_struct->CircleToolData->circle, 2);


 /*------------------------------------------------------------------*/
 /* Update Distance in the Label widget...                           */
 /*------------------------------------------------------------------*/

 hrap = convert_screen_coords_to_HRAP(rad_data, widget_struct->CircleToolData->center_x, widget_struct->CircleToolData->center_y);
 LatLong_start = HrapToLatLong(hrap);

 hrap = convert_screen_coords_to_HRAP(rad_data, event->xbutton.x, event->xbutton.y);
 LatLong_end = HrapToLatLong(hrap);

 distance = calculate_distance_from_LatLong(&LatLong_start, &LatLong_end);

 sprintf(string,"%.1f", distance);
 XtVaSetValues(widget_struct->Distance_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);


}

/* **************************************************************************

	void end_CircleTool_rb()


   ************************************************************************** */

void end_CircleTool_rb(w, widget_struct, event)
	Widget               w;
	the_widget_struct    *widget_struct;
	XEvent               *event;
{

	char    *string = " ";



/* ---------------------------------------------------- */
/*      Erase circle...                                 */
/* ---------------------------------------------------- */

  XDrawArc(XtDisplay(w), XtWindow(w),
	   widget_struct->CircleToolData->gc,
	   widget_struct->CircleToolData->circle[1].x, widget_struct->CircleToolData->circle[1].y,
	   widget_struct->CircleToolData->circle[1].width, widget_struct->CircleToolData->circle[1].height,
	   widget_struct->CircleToolData->circle[1].angle1, widget_struct->CircleToolData->circle[1].angle2);


/* ---------------------------------------------------- */
/*      Reset the distance display to blank...          */
/* ---------------------------------------------------- */

 XtVaSetValues(widget_struct->Distance_widget,
	       XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
	       NULL);
}




/* ************************************************************************

	void select_ForecastGroup()


   ************************************************************************ */

void select_ForecastGroup(w, widget_struct, event)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XEvent                  *event;
{

   Display      *display;
   XPoint       *points;
   XGCValues    gcv;
   XmString     xmstr_FGroupID;

   int          numbasin;
   int          mask = GCForeground;
   int          array_pos;

   overlay_struct      **basin;



 display = XtDisplay(w);


 /*--------------------------------------------------------------*/
 /*     Create graphics context, if not already created...       */
 /*--------------------------------------------------------------*/

 if(rad_data->Basin_gc == NULL)
	{
	gcv.foreground = get_pixel_by_name(w, color_list[18]);
	rad_data->Basin_gc = (GC *) malloc(sizeof(GC));
	*rad_data->Basin_gc = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
	}


 /*--------------------------------------------------------------*/
 /*     Get the Forecast Group name (ID) and read in a           */
 /*     file containing the Basin (Segment) IDs and highlight    */
 /*     them by calling 'XmListSelectItem(...)' from the         */
 /*     Forecast Group & Carryover date selection dialog...      */
 /*--------------------------------------------------------------*/

 basin = mapbasin;
 numbasin = nummap;

 if((points = RegionFound(event->xbutton.x, event->xbutton.y, rad_data, basin, numbasin, &array_pos)) != NULL)
	{
	get_ForecastGroup_ID(widget_struct, basin[array_pos]->id);

	xmstr_FGroupID = XmStringCreate(widget_struct->selected_ForecastGroupName, "charSet1");
	XmListSelectItem(widget_struct->dataStruct->forecastGroup_list, xmstr_FGroupID, TRUE);
	XmStringFree(xmstr_FGroupID);
	}

 if(points != NULL) free(points);

}






/* ************************************************************************

	void get_ForecastGroup_ID()


   ************************************************************************ */

void get_ForecastGroup_ID(widget_struct, BasinName)
	the_widget_struct       *widget_struct;
	char                    *BasinName;
{

	int     i;
	int     j;
	int     ForecastGroupFound = FALSE;

	char    *temp_ptr;
	char    FGroup_ID[9];




 memset(widget_struct->selected_ForecastGroupName, '\0', 9);
 temp_ptr = widget_struct->ForecastGroupNames;

 for(i = 0; i < widget_struct->NumForecastGroups; i++)
	 {
	 if(FGBasin_ID != NULL) free(FGBasin_ID);

	 memset(FGroup_ID, '\0', 9);
	 strncpy(FGroup_ID, temp_ptr, 8);
	 FGBasin_ID = (char **)map_areas_in_fg(FGroup_ID, &NumBasinsInCurrFcstGroup);
	 for(j = 0; j < NumBasinsInCurrFcstGroup; j++)
		 {
		 if(strcmp(BasinName, FGBasin_ID[j]) == 0)
			 {
			 ForecastGroupFound = TRUE;
			 break;
			 }
		 }

	 if(ForecastGroupFound)
		 {
		 strcpy(widget_struct->selected_ForecastGroupName, FGroup_ID);
		 break;
		 }

	 temp_ptr += 8;
	 }

}


/* ************************************************************************

	void CallSegmentWidgetCallbacks()


   ************************************************************************ */

void CallSegmentWidgetCallbacks(w, widget_struct, current)
	Widget                  w;
	the_widget_struct       *widget_struct;
	node                    *current;
{

	XmString                xmStr;
	XmString                xmSingleString;
	tree_data_struct        *data;



 if(NWSRFS_has_begun)
	{
	data = (tree_data_struct *) malloc(sizeof(tree_data_struct));
	data->dataStruct = widget_struct;
	data->branch = current;

	handle_segment_selected(w, widget_struct, NULL);
	tell_which_tree(w, data, NULL);
	}
 else   {
	xmSingleString = XmStringCreateSimple("Single segment when Begin");

	XtVaGetValues(widget_struct->run_multiple_widget, XmNlabelString, &xmStr, NULL);

	if(XmStringCompare(xmStr, xmSingleString) == TRUE)
		{
		data = (tree_data_struct *) malloc(sizeof(tree_data_struct));
		data->dataStruct = widget_struct;
		data->branch = current;

		amend_seg_list(w, data, NULL);
		post_change(w, current->e19.name, NULL);
		}
	else    handle_segment_selected(w, widget_struct, NULL);

	XmStringFree(xmStr);
	XmStringFree(xmSingleString);
	}



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/events.c,v $";
 static char rcs_id2[] = "$Id: events.c,v 1.3 2006/04/07 13:29:48 aivo Exp $";}
/*  ===================================================  */

}
