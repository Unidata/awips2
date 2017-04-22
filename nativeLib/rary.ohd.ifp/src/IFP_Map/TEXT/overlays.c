/*=========================================================================*/
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   add_overlays()                     */
/*                                      show_states()                      */
/*                                      show_county()                      */
/*                                      show_cities_and_towns()            */
/*                                      show_basin_boundaries()            */
/*                                      show_rivers()                      */
/*                                                                         */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "drawa.h"







/* **************************************************************************

	void add_overlays()

		callback function that calls the drawing routines for the
		selected overlays; we know which overlays are selected from
		the corresponding ToggleButton states (on/off).

		Also, if a Forecast Group has been selected and the data for
		it have been loaded (creating the topologial tree diagram
		for the Forecast Group), any selected segments (and their
		MAP basins) are also redrawn...

	Functions called:
	   show_states
	   show_rivers
	   show_basin_boundaries
	   show_cities_and_towns
	   show_county

	Global variables:
	   (see above)

   ************************************************************************** */

void add_overlays(w, widget_struct, call_data)
	Widget                          w;
	the_widget_struct               *widget_struct;
	XmToggleButtonCallbackStruct    *call_data;
{

	int             i;
	Dimension       width, height;
	char            currentSegment[9];
	char            *first_blank;
	node            *found;





 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);
 XCopyArea(XtDisplay(w), widget_struct->overlays->pixbase, widget_struct->overlays->pix,
	  widget_struct->overlays->gc[0], 0, 0, width, height, 0, 0);

/*--------------------------------------*/
/*      Current Forecast Group          */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->FcstGroup_widget))
		show_currentForecastGroup(widget_struct);

/*--------------------------------------*/
/*      MAP Basins highlight            */
/*--------------------------------------*/
 if(call_data != NULL)
	{
	if(sub_group_num >= 0)
		{       /*--------------------------------------*/
			/*      Forecast Group data is loaded   */
			/*--------------------------------------*/

		for(i = 0; i <= sub_group_num; i++)
			{
			if(strlen(selected_string) != 0) highlight_MAPBasins(widget_struct->head[i], widget_struct);
			else    {
				if(NWSRFS_has_begun)
					set_MAPBasin_Subnodes_selected(widget_struct->head[i], widget_struct);
				}
			}

		/*----------------------------------------------------------------------*/
		/*      We need to get the new clip area for the current segment        */
		/*      in case the user selects another Segment MAP basin...           */
		/*----------------------------------------------------------------------*/

		if(strlen(widget_struct->current_segment) != 0)
			{
			memset(currentSegment, '\0', 9);
			if((first_blank = strstr(widget_struct->current_segment, " ")) != NULL)
				strncpy(currentSegment, widget_struct->current_segment, first_blank - widget_struct->current_segment);
			else    strcpy(currentSegment, widget_struct->current_segment);

			for(i = 0; i <= sub_group_num; i++)
			       if((found = find_it(widget_struct->head[i], currentSegment)) != NULL) break;

			set_MAPBasin_selected(found, widget_struct->overlays);
			}
		}
	}

/*--------------------------------------*/
/*      States                          */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->states_widget))
		show_states(widget_struct);

/*--------------------------------------*/
/*      Rivers                          */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->rivers_widget))
		show_rivers(widget_struct);

/*--------------------------------------*/
/*      MAP Basin boundaries            */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->basin_boundaries_widget))
		show_basin_boundaries(widget_struct);

/*--------------------------------------*/
/*      Forecst Group boundaries        */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->FcstGroupBoundaries_widget))
		show_forecastGroup_boundaries(widget_struct);

/*--------------------------------------*/
/*      Cities & Towns                  */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->cities_widget))
		show_cities_and_towns(widget_struct);

/*--------------------------------------*/
/*      Counties                        */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->county_widget))
		show_county(widget_struct);

/*--------------------------------------*/
/*      Forecast Points                 */
/*--------------------------------------*/
 if (XmToggleButtonGetState(widget_struct->forecastPoints_widget))
		show_forecast_points(widget_struct);



}

/********************************************* END add_overlays ************/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/overlays.c                               */
/*  FUNCTION NAME:   show_states                                           */
/*       FUNCTION:   callback to toggle states overlay                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (states) button

Functions called:
   add_overlays
   get_pixel_by_name

Global variables:
   (see above)

Local variables:
   w - Widget structure;
   data - deref draw_struct structure;
   call_data - deref caddr_t structure;
   i, j - integers;
   n - integer; index (incrementor) for wargs array
   maxpts - integer;
   points - deref XPoint structure;
   gc - GC structure;
   dpy - deref Display structure;
   width - Dimension structure;
   height - Dimension structure;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5
   x, y - integer;
   mask - integer;
   gcv - XGCValues structure;

******************************************** BEGIN show_states *************/

void show_states(widget_struct)
	the_widget_struct       *widget_struct;
{

	int                     i, j, n, maxpts;
	XPoint                  *points;
	GC                      gc;
	Display                 *dpy;
	Dimension               width, height;
	Arg                     wargs[5];
	int                     x, y;
	int                     mask = GCForeground;
	XGCValues               gcv;

	draw_struct             *data;



 data = widget_struct->overlays;
 dpy = XtDisplay(widget_struct->main_canvas);

 /*--------------------------------------------------------------*/
 /*     determine size of display area                           */
 /*--------------------------------------------------------------*/

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 /*--------------------------------------------------------------*/
 /*     determine number of points to be plotted & allocate space*/
 /*--------------------------------------------------------------*/

 maxpts = 0;
 for(i = 0; i < numstates; i++)
	{
	if (state[i]->npts > maxpts) maxpts = state[i]->npts;
	}


 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context for states overlay               */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(widget_struct->main_canvas,color_list[17]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     draw states                                              */
 /*--------------------------------------------------------------*/

 for (i=0;i<numstates;i++)
    {
    for (j=0;j<state[i]->npts;j++)
       {
       points[j].x = (state[i]->hrap[j].x - data->origin.x) * x;
       points[j].y = (data->maximum_rows - (state[i]->hrap[j].y - data->origin.y))*y;
       }
    XDrawLines(dpy, data->pix, gc, points, state[i]->npts, CoordModeOrigin);
    }

 if (XtIsRealized(widget_struct->main_canvas))
	XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

}

/********************************************* END show_states *************/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/overlays.c                               */
/*  FUNCTION NAME:   show_county                                           */
/*       FUNCTION:   callback to toggle county overlay                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (county) button

Functions called:
   add_overlays
   get_pixel_by_name

Global variables:
   (see above)

Local variables:
   w - Widget structure;
   data - deref draw_struct structure;
   call_data - deref caddr_t structure;
   i, j - integer;
   n - integer; index (incrementor) for wargs array
   maxpts - integer;
   points - XPoint structure;
   gc - GC structure;
   dpy - deref Display structure;
   width - Dimension structure;
   height - Dimension structure;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5
   x, y - integer;
   mask - integer;
   gcv - XGCValues structure;

******************************************** BEGIN show_county *************/

void show_county(widget_struct)
	the_widget_struct       *widget_struct;
{

	int          i, j, n, maxpts;
	XPoint       *points;
	GC           gc;
	Display      *dpy;
	Dimension    width, height;
	Arg          wargs[5];
	int          x, y;
	int          mask = GCForeground;
	XGCValues    gcv;

	draw_struct  *data;



 data = widget_struct->overlays;

 dpy = XtDisplay(widget_struct->main_canvas);

 /*--------------------------------------------------------------*/
 /*     determine size of display area                           */
 /*--------------------------------------------------------------*/

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 /*--------------------------------------------------------------*/
 /*     determine number of points to be plotted & allocate space*/
 /*--------------------------------------------------------------*/

 maxpts = 0;
 for(i=0;i<numcounty;i++)
      if (county[i]->npts > maxpts)
	  maxpts = county[i]->npts;

 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context for states overlay               */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(widget_struct->main_canvas, color_list[24]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     draw states                                              */
 /*--------------------------------------------------------------*/

 for (i=0;i<numcounty;i++)
    {
    for (j=0;j<county[i]->npts;j++)
       {
       points[j].x = (county[i]->hrap[j].x - data->origin.x) * x;
       points[j].y = (data->maximum_rows - (county[i]->hrap[j].y - data->origin.y))*y;
       }
    XDrawLines(dpy, data->pix, gc, points, county[i]->npts, CoordModeOrigin);
    }

 if (XtIsRealized(widget_struct->main_canvas))
    XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

}

/********************************************* END show_county *************/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/overlays.c                               */
/*  FUNCTION NAME:   show_cities_and_towns                                 */
/*       FUNCTION:   callback to toggle cities and towns overlay           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (cities) button

Functions called:
   add_overlays
   get_pixel_by_name
   LatLongToHrap

Global variables:
   (see above)

Local variables:
   w - Widget structure;
   data - deref draw_struct structure;
   call_data - deref caddr_t structure;
   i, j - integer;
   n - integer; index (incrementor) for wargs array
   xloc - integer;
   yloc - integer;
   hpt - HRAP structure;
   cityfile - deref FILE structure;
   xlat - float;
   xlong - float;
   str - stack deref (array) character; dimensioned 30;
   city - stack deref (array) character; dimensioned 30;
   gc - GC structure;
   dpy - deref Display structure;
   width - Dimension structure;
   height - Dimension structure;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5
   x, y - integer;
   mask - integer;
   gcv - XGCValues structure;
   cityfile_name - stack deref (array) character; dimensioned 120;
   rfc_name - stack deref (array) character; dimensioned 9;

******************************************** BEGIN show_cities_and_towns ***/

void show_cities_and_towns(widget_struct)
	the_widget_struct       *widget_struct;
{

	int          i, j, n, xloc, yloc;
	HRAP         hpt;
	FILE         *cityfile;
	float        xlat, xlong;
	char         str[30], city[30];
	GC           gc;
	Display      *dpy;
	Dimension    width, height;
	Arg          wargs[5];
	int          x,y;
	int          mask = GCForeground;
	XGCValues    gcv;

	draw_struct  *data;
	char         cityfile_name[120];
	char         rfc_name[9];
        int          len, len2;

 data = widget_struct->overlays;

 dpy = XtDisplay(widget_struct->main_canvas);

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y) x = y;
 else if (y > x) y = x;


 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(widget_struct->main_canvas,color_list[21]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     open appropriate data file based on display size         */
 /*--------------------------------------------------------------*/

 memset(cityfile_name, '\0', 120);
 memset(rfc_name, '\0', 9);

 /* call routine to get the overlays directory path and rfc name*/
 len = strlen("geo_data");
 get_apps_defaults("geo_data", &len, cityfile_name, &len2);
 len = strlen("ifp_rfc");
 get_apps_defaults("ifp_rfc", &len, rfc_name, &len2);

 strcat(cityfile_name, "/");
 strcat(cityfile_name, rfc_name);

 if(data->maximum_columns < 131) strcat(cityfile_name, "/ascii/town_zoom.dat");
 else                            strcat(cityfile_name, "/ascii/town.dat");

 if( (cityfile = fopen(cityfile_name, "r")) != NULL)
 {

    /*--------------------------------------------------------------*/
    /*     display city names at appropriate location               */
    /*     a period is affixed to the beginning of the              */
    /*     name to locate the city on the display                   */
    /*--------------------------------------------------------------*/

    for(;;)
    {
	j = fscanf(cityfile, "%s %f %f", city, &xlat, &xlong);
	if (j == EOF) break;
	hpt = LatLongToHrap(xlat, xlong);
	xloc = (hpt.x - data->origin.x) * x;
	yloc = (data->maximum_rows - (hpt.y - data->origin.y))*y;
	strcpy(str,".");
	strcat(str, city);
	XDrawString(XtDisplay(widget_struct->main_canvas),data->pix,gc,xloc,yloc,str,strlen(str));
    }
    fclose(cityfile);
 }
 else
    printf("Problems opening %s !\n ", cityfile_name);

 /*--------------------------------------------------------------*/
 /*     create expose event if appropriate                       */
 /*--------------------------------------------------------------*/

 if (XtIsRealized(widget_struct->main_canvas))
    XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);


}

/********************************************* END show_cities_and_towns ***/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/overlays.c                               */
/*  FUNCTION NAME:   show_basin_boundaries                                 */
/*       FUNCTION:   callback to toggle basin boundaries overlay           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (basins) button

Functions called:
   add_overlays
   get_pixel_by_name

Global variables:
   (see above)
Local variables:
   w - Widget structure;
   data - deref draw_struct structure;
   call_data - deref caddr_t structure;
   i, j - integer;
   n - integer; index (incrementor) for wargs array
   maxpts - integer;
   points - deref XPoint structure;
   gc - GC structure;
   dpy - deref Display structure;
   width - Dimension structure;
   height - Dimension structure;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5
   x, y - integer;
   mask - integer;
   gcv - XGCValues structure;
   numbasin - integer;
   basin - deref overlay_struct structure;

******************************************** BEGIN show_basin_boundaries ***/

void show_basin_boundaries(widget_struct)
	the_widget_struct       *widget_struct;
{

	int                  i, j, n, maxpts;
	XPoint               *points;
	GC                   gc;
	Display              *dpy;
	Dimension            width, height;
	Arg                  wargs[5];
	int                  x, y;
	int                  mask = GCForeground;
	XGCValues            gcv;
	int                  numbasin;
	overlay_struct       **basin;

	draw_struct          *data;



 data = widget_struct->overlays;

 dpy = XtDisplay(widget_struct->main_canvas);

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 basin = mapbasin;

 /*--------------------------------------------------------------*/
 /*     determine maximum number of points to display & allocate */
 /*     space                                                    */
 /*--------------------------------------------------------------*/

 maxpts = 0;
 numbasin = nummap;

 for(i = 0; i < numbasin; i++)
    if (basin[i]->npts > maxpts) maxpts = basin[i]->npts;


 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;

 if (x > y) x = y;
 else if (y > x) y = x;


 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(widget_struct->main_canvas, color_list[18]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     display basin boundaries                                 */
 /*--------------------------------------------------------------*/

 for (i = 0; i < numbasin; i++)
	{
	for (j = 0; j < basin[i]->npts; j++)
		{
		points[j].x = (basin[i]->hrap[j].x - data->origin.x) * x;
		points[j].y = (data->maximum_rows - (basin[i]->hrap[j].y - data->origin.y))*y;
		}
	XDrawLines(dpy, data->pix, gc, points, basin[i]->npts, CoordModeOrigin);
	}

 /*--------------------------------------------------------------*/
 /*     create expose event if display widget has already been   */
 /*     realized                                                 */
 /*--------------------------------------------------------------*/

 if (XtIsRealized(widget_struct->main_canvas))
	XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

}




/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/overlays.c                               */
/*  FUNCTION NAME:   show_rivers                                           */
/*       FUNCTION:   callback to toggle rivers overlay                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) overlay (rivers) button

Functions called:
   add_overlays
   get_pixel_by_name

Global variables:
   (see above)

Local variables:
   w - Widget structure;
   data - deref draw_struct structure;
   call_data - deref caddr_t structure;
   i, j - integer;
   n - integer; index (incrementor) for wargs array
   maxpts - integer;
   points - deref XPoint structure;
   gc - GC structure;
   dpy - deref Display structure;
   width - Dimension structure;
   height - Dimension structure;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 5
   x, y - integer;
   minorder - integer;
   mask - integer;
   gcv - XGCValues structure;

******************************************** BEGIN show_rivers *************/

void show_rivers(widget_struct)
	the_widget_struct       *widget_struct;
{

	int          i, j, n, maxpts;
	XPoint       *points;
	GC           gc;
	Display      *dpy;
	Dimension    width, height;
	int          x, y, minorder;
	int          mask = GCForeground;
	XGCValues    gcv;

	draw_struct  *data;



 data = widget_struct->overlays;

 dpy = XtDisplay(widget_struct->main_canvas);

 /*--------------------------------------------------------------*/
 /*     determine dimension of display area                      */
 /*--------------------------------------------------------------*/

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 /*--------------------------------------------------------------*/
 /*     if basins are on, copy the base pixmap without overlays  */
 /*     onto active pixmap, add other overlays and create        */
 /*     an expose event to re-display                            */
 /*--------------------------------------------------------------*/

/* --------------------------------------------------------------
 if (data->rivers_on == 1)
    {
    data->rivers_on = FALSE;
    XCopyArea(dpy, data->pixbase, data->pix, data->gc[0], 0, 0, width, height, 0, 0);
    add_overlays(data);
    XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

    return;
    }
 data->rivers_on = 1;
   -------------------------------------------------------------- */

 /*--------------------------------------------------------------*/
 /*     allocate space for number of points to display           */
 /*--------------------------------------------------------------*/

 maxpts = 0;
 for(i=0;i<numrivers;i++)
    if (river[i]->npts > maxpts)
       maxpts = river[i]->npts;
 points = (XPoint *)malloc(maxpts*sizeof(XPoint));

 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 /*--------------------------------------------------------------*/
 /*     create graphics context                                  */
 /*--------------------------------------------------------------*/

 gcv.foreground = get_pixel_by_name(widget_struct->main_canvas,color_list[19]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     determine minimum stream order to display                */
 /*--------------------------------------------------------------*/

 if (zoom_factor < 3) minorder = 3;
 else minorder = 1;


 /*--------------------------------------------------------------*/
 /*     display rivers                                           */
 /*--------------------------------------------------------------*/

 for (i = 0; i < numrivers; i++)
    {
    /* add code so if order is -1, river segment will be displayed
     * dp - 19 Nov. 1997
     */
    if (river[i]->order == -1 || river[i]->order >= minorder)
       {
       for (j=0;j<river[i]->npts;j++)
	  {
	  points[j].x = (river[i]->hrap[j].x - data->origin.x) * x;
	  points[j].y = (data->maximum_rows - (river[i]->hrap[j].y - data->origin.y))*y;
	  }
       XDrawLines(dpy, data->pix, gc, points, river[i]->npts, CoordModeOrigin);
       }
    }

 /*--------------------------------------------------------------*/
 /*     create expose event if display widget has already        */
 /*     been realized                                            */
 /*--------------------------------------------------------------*/

 if (XtIsRealized(widget_struct->main_canvas))
	XCopyArea(dpy, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);

}




/* ********************************************************************

	show_currentForecastGroup()

   ******************************************************************** */

void show_currentForecastGroup(widget_struct)
	the_widget_struct       *widget_struct;
{

	int             i, j, n, maxpts;
	int             k;
	int             x, y;
	int             mask = GCForeground;
	int             numbasin;
	int             MAPBasinFound;

	char            *blank;
	char            temp1[9];
	char            temp2[9];

	GC              gc;
	Display         *display;
	Dimension       width, height;
	XGCValues       gcv;
	Pixel           color;

	XPoint          *points;
	overlay_struct  **basin;
	draw_struct     *data;



 data = widget_struct->overlays;

 display = XtDisplay(widget_struct->main_canvas);

 if(sub_group_num >= 0)
	{       /* If the Forecast Group data has been loaded 'sub_group_num' > 0, and this     */
		/* will highlight the Forecast Group basins fastest...                          */

	color = get_pixel_by_name(widget_struct->main_canvas, color_list[20]);
	if(widget_struct->overlays->Basin_gc == NULL)
	       {
	       widget_struct->overlays->Basin_gc = (GC *) malloc(sizeof(GC));
	       *widget_struct->overlays->Basin_gc =
			      XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
	       }

	XSetForeground(display, *widget_struct->overlays->Basin_gc, color);

	for(i = 0; i <= sub_group_num; i++)
		if(widget_struct->head[i] != NULL)
			set_MAPBasin_Subnodes_selected(widget_struct->head[i], widget_struct);

	color = get_pixel_by_name(data->w, color_list[18]);
	XSetForeground(display, *widget_struct->overlays->Basin_gc, color);

	}
 else   {       /* Otherwise, we have to highlight the Forecast Group basins using an           */
		/* exhaustive search through the basin array...                                 */

	memset(temp1, '\0', 9);
	memset(temp2, '\0', 9);

	/*--------------------------------------------------------------*/
	/*     determine dimension of display area                      */
	/*--------------------------------------------------------------*/

	XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);


	/*--------------------------------------------------------------*/
	/*     determine maximum number of points to display & allocate */
	/*     space                                                    */
	/*--------------------------------------------------------------*/

	maxpts = 0;
	basin = mapbasin;
	numbasin = nummap;

	for(i = 0; i < numbasin; i++)
	   if (basin[i]->npts > maxpts) maxpts = basin[i]->npts;


	points = (XPoint *)malloc(maxpts*sizeof(XPoint));

	/*--------------------------------------------------------------*/
	/*     determine number of pixels per hrap bin                  */
	/*--------------------------------------------------------------*/

	x = (float)width/(float)data->maximum_columns;
	y = (float)height/(float)data->maximum_rows;

	if (x > y) x = y;
	else if (y > x) y = x;


	/*--------------------------------------------------------------*/
	/*     create graphics context                                  */
	/*--------------------------------------------------------------*/

	gcv.foreground = get_pixel_by_name(widget_struct->main_canvas, color_list[20]);
	gc = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);

	/*--------------------------------------------------------------*/
	/*     display basin boundaries                                 */
	/*--------------------------------------------------------------*/

	for (k = 0; k < NumBasinsInCurrFcstGroup; k++)
	       {
	       /* It's not certain that there will be a match in the data between      */
	       /* FGBasin_ID[] and basin[] in all cases, so let's check...             */
	       MAPBasinFound = FALSE;

	       /* Make sure there are no trailing blanks...                            */
	       strcpy(temp1, FGBasin_ID[k]);
	       if((blank = strchr(temp1, ' ')) != (char *)NULL) *blank = '\0';

	       for (i = 0; i < numbasin; i++)
		      {
		      /* Make sure there are no trailing blanks...                     */
		      strcpy(temp2, basin[i]->id);
		      if((blank = strchr(temp2, ' ')) != (char *)NULL) *blank = '\0';

		      if(strcmp(temp1, temp2) == 0)
			       {
			       /* printf("%s, %s.\n", FGBasin_ID[k], basin[i]->id);    */
			       MAPBasinFound = TRUE;
			       break;
			       }
		      }

	       if(MAPBasinFound)
		       {
		       for (j = 0; j < basin[i]->npts; j++)
			       {
			       points[j].x = (basin[i]->hrap[j].x - data->origin.x) * x;
			       points[j].y = (data->maximum_rows - (basin[i]->hrap[j].y - data->origin.y))*y;
			       }
		       XFillPolygon(display, data->pix, gc, points, basin[i]->npts, Nonconvex, CoordModeOrigin);
		       }
	       }
	} /* End else ( sub_group_num < 0, ==> the node struct for the Forecast Group topology  */
	  /* has not been created, so 'set_MAPBasin_Subnodes_selected()' can't be called...     */

 /*--------------------------------------------------------------*/
 /*     create expose event if display widget has already been   */
 /*     realized                                                 */
 /*--------------------------------------------------------------*/

 if (XtIsRealized(widget_struct->main_canvas))
    XCopyArea(display, data->pix, XtWindow(widget_struct->main_canvas), data->gc[0], 0, 0, width, height, 0, 0);




/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/overlays.c,v $";
 static char rcs_id2[] = "$Id: overlays.c,v 1.3 2002/02/11 19:18:27 dws Exp $";}
/*  ===================================================  */

}


