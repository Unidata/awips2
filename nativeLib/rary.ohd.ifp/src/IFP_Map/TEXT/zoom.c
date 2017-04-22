
/* ********************************************************************************************

	zoom.c

		Contains functions for handling zooming

		Coded by:       Tom Adams
		Affiliation:    DOC/NOAA/NWS/Office of Hydrology/Hydrologic Research Lab.
		Date:           08/17/92


	zoom.c contains functions:

				void       revert_to_default_view()
				void       zoom_in()
				void       zoom_out()
				void       zoom()
				point      get_RFC_center()
				int        get_map_scale()
				ZoomStruct *NewZoomStruct()
				ZoomStruct *GetLastZoomLevel()


   ******************************************************************************************** */

/* ------------------------- INCLUDE FILES ------------------------- */

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"
#include "libXs.h"
#include "drawa.h"
#include "math.h"

/* ----------------------------------------------------------------- */



static point      get_RFC_center();
static int        get_map_scale();
static ZoomStruct *NewZoomStruct();
static ZoomStruct *GetLastZoomLevel();





/* ****************************************************************************

	revert_to_default_view()
		sets the oringinal image size at the default projection
		and scale.

   **************************************************************************** */

void revert_to_default_view(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{

	int             x_pixels_per_bin;
	int             y_pixels_per_bin;
	Dimension       width;
	Dimension       height;
	point           center;         /* (x,y) center of the zoom area in HRAP units...       */


 if(zoom_factor == 1.0) return;

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 x_pixels_per_bin = (float)width/(float)widget_struct->overlays->maximum_columns;
 y_pixels_per_bin = (float)height/(float)widget_struct->overlays->maximum_rows;

 if (x_pixels_per_bin > y_pixels_per_bin) x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin) y_pixels_per_bin = x_pixels_per_bin;


 /*-------------------------------------------------------------------------*/
 /*     determine size of selected zoom area and malloc necessary space     */
 /*-------------------------------------------------------------------------*/

 widget_struct->overlays->maximum_columns = MAXX;
 widget_struct->overlays->maximum_rows = MAXY;


 /*-------------------------------------------------------------------------*/
 /*     set zoom origin                                                     */
 /*-------------------------------------------------------------------------*/

 widget_struct->overlays->origin.x = XOR;
 widget_struct->overlays->origin.y = YOR;

 /*-------------------------------------------------------------------------*/
 /*     Reset the zoom data to NULL since we've zoomed to the initial scale */
 /*-------------------------------------------------------------------------*/

 if(widget_struct->zoom != NULL)
	{
	/* puts("Free the zoom struct..."); */
	free(widget_struct->zoom);
	widget_struct->zoom = (ZoomStruct *) NULL;
	}

 /*-------------------------------------------------------------------------*/
 /*     Find the center of the RFC region and zoom so that the scrolled     */
 /*     window completely contains the RFC boundary at its largest size...  */
 /*-------------------------------------------------------------------------*/

 center = get_RFC_center(widget_struct, widget_struct->overlays);

 zoom(&center, widget_struct, widget_struct->overlays);

 if(strlen(selected_string) != 0)
		highlight_MAPBasins(widget_struct->head[whichTree_index], widget_struct);

}






/* ****************************************************************************

	zoom_in()
		the center of a rectangular selected area is passed to the
		function and the selected area is re-scaled to some maximum
		size that will be contained within the ScrolledWindow

   **************************************************************************** */

void zoom_in(center, widget_struct, data)
	point                   *center;        /* (x,y) center of the zoom area in HRAP units...       */
	the_widget_struct       *widget_struct;
	draw_struct             *data;
{

	ZoomStruct      *ThisZoomLevel;




 /*-----------------------------------------------------------------------------*/
 /*     Keep track of the data that defines the old level of zoom, so that      */
 /*     the user can zoom out using the Zoom-out tool...                        */
 /*-----------------------------------------------------------------------------*/

 ThisZoomLevel = NewZoomStruct(widget_struct);

 ThisZoomLevel->origin.x     = data->origin.x;
 ThisZoomLevel->origin.y     = data->origin.y;
 ThisZoomLevel->center.x     = center->x;
 ThisZoomLevel->center.y     = center->y;
 ThisZoomLevel->upperLeft.x  = rbdata.start_x;
 ThisZoomLevel->upperLeft.y  = rbdata.start_y;
 ThisZoomLevel->lowerRight.x = rbdata.last_x;
 ThisZoomLevel->lowerRight.y = rbdata.last_y;
 ThisZoomLevel->rows         = data->maximum_rows;
 ThisZoomLevel->columns      = data->maximum_columns;

 zoom(center, widget_struct, data);

}



/* ****************************************************************************

	zoom_out()
		reinitialize the data to zoom to the previous zoom level
		by calling zoom_in()


   **************************************************************************** */

void zoom_out(widget_struct)
	the_widget_struct       *widget_struct;
{

	point           center;         /* (x,y) center of the zoom area in HRAP units...       */
	ZoomStruct      *last_level;
	ZoomStruct      *prev_level;


 last_level = GetLastZoomLevel(widget_struct->zoom);
 if((prev_level = last_level->prevLevel) == (ZoomStruct *) NULL)
	{       /* The user wants to return to the default view, showing the entire River       */
		/* Forecast Center area of responsibility...                                    */
	revert_to_default_view(widget_struct->toplevel, widget_struct, NULL);
	return;
	}

 widget_struct->overlays->origin.x = prev_level->origin.x;
 widget_struct->overlays->origin.y = prev_level->origin.y;

 center.x = prev_level->center.x;
 center.y = prev_level->center.y;

 rbdata.start_x = prev_level->upperLeft.x;
 rbdata.start_y = prev_level->upperLeft.y;
 rbdata.last_x  = prev_level->lowerRight.x;
 rbdata.last_y  = prev_level->lowerRight.y;

 widget_struct->overlays->maximum_rows    = prev_level->rows;
 widget_struct->overlays->maximum_columns = prev_level->columns;

 free(last_level);
 prev_level->nextLevel = (ZoomStruct *) NULL;

 zoom(&center, widget_struct, widget_struct->overlays);

}




/* **************************************************************************

	point get_RFC_center()


   ************************************************************************** */

point get_RFC_center(widget_struct, data)
	the_widget_struct       *widget_struct;
	draw_struct             *data;
{

	int             i;
	int             j;
	int             numbasin;
	int             numpoints;
	float           x;
	float           y;

	Dimension       width;
	Dimension       height;

	point           center;
	point           upperLeft;
	point           lowerRight;

	overlay_struct  **basin;



 upperLeft.x  =  10000;
 upperLeft.y  = -10000;

 lowerRight.x = -10000;
 lowerRight.y =  10000;

 basin = bound;
 if (basin == (overlay_struct **) NULL) numpoints = 0;
 else numpoints = bound[0]->npts;


 /*-------------------------------------------------------------*/
 /*     Find the upper-left and lower-right verticies of the    */
 /*     bounding rectangle...                                   */
 /*-------------------------------------------------------------*/


 for(j = 0; j < numpoints; j++)
	 {
	 if(basin[0]->hrap[j].x < upperLeft.x) upperLeft.x = basin[0]->hrap[j].x;
	 if(basin[0]->hrap[j].y > upperLeft.y) upperLeft.y = basin[0]->hrap[j].y;

	 if(basin[0]->hrap[j].x > lowerRight.x) lowerRight.x = basin[0]->hrap[j].x;
	 if(basin[0]->hrap[j].y < lowerRight.y) lowerRight.y = basin[0]->hrap[j].y;
	 }

 center.x = upperLeft.x + abs(lowerRight.x - upperLeft.x)/2;
 center.y = upperLeft.y - abs(lowerRight.y - upperLeft.y)/2;


 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 XtVaGetValues(data->w, XmNwidth, &width, XmNheight, &height, NULL);

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;

 if (x > y) x = y;
 else if (y > x) y = x;


 /*-------------------------------------------------------------*/
 /*     Needed for subsequent functions; convert HRAP           */
 /*     coordinates to DrawingArea widget pixel units...        */
 /*-------------------------------------------------------------*/

 rbdata.start_x = (float)(upperLeft.x - data->origin.x) * x;
 rbdata.start_y = (float)(data->maximum_rows - (upperLeft.y - data->origin.y))*y;
 rbdata.last_x  = (float)(lowerRight.x - data->origin.x) * x;
 rbdata.last_y  = (float)(data->maximum_rows - (lowerRight.y - data->origin.y))*y;


 /*--------------------------------------------------------------*/


 return(center);

}



/* ****************************************************************************

	int get_map_scale()

		Calculate the actual scale of the map drawing; if there is a
		more elegant way of getting Screen_Resolution (pixels/cm), do
		it!!

		The returned value will, practically, always be significantly
		greater than 1, since we will not be viewing the earth at
		scales close to approaching reading a newspaper - let alone
		seeing one...

   **************************************************************************** */

int get_map_scale(center, PixelsPerBin)
	point           *center;                /* (x,y) center of the zoom area in HRAP units...       */
	int             PixelsPerBin;
{

	float           OnePlus_Sin60 = 1.8660254;
	float           BinWidth;               /* Distance in kilometers...                            */
	float           factor = 4.7625;        /* HRAP bin width at 60 N Lat & 102 W Long...           */
	int             scale;
	double          phi;                    /* Latitude (radians)...                                */
	HRAP            LatLong;



 LatLong  = HrapToLatLong(*center);
 phi      = (double)(PI*(double)LatLong.y/(double)180.);       /* Convert to radians...                */
 BinWidth = factor/(OnePlus_Sin60/(1. + sin(phi)));

 scale    = (BinWidth * (float)CMS_PER_KM * (float)Screen_Resolution)/(float)PixelsPerBin;


 return(scale);

}



/* ****************************************************************************

	ZoomStruct *NewZoomStruct()

		make space for a new ZoomStruct to keep track of data
		at the new zoom level...

   **************************************************************************** */

ZoomStruct *NewZoomStruct(widget_struct)
	the_widget_struct       *widget_struct;
{

	ZoomStruct      *level;
	ZoomStruct      *ThisZoomLevel;
	ZoomStruct      *prev;



 ThisZoomLevel = (ZoomStruct *) malloc(sizeof(ZoomStruct));

 if(widget_struct->zoom == (ZoomStruct *) NULL)
	{       /*------------------------------------------------------*/
		/*      This is the base zoom level, so point to the    */
		/*      space just malloc'ed ...                        */
		/*------------------------------------------------------*/

	widget_struct->zoom = ThisZoomLevel;
	ThisZoomLevel->prevLevel = (ZoomStruct *) NULL;
	}
 else   {       /*------------------------------------------------------*/
		/*      The user has zoomed previously, so find the     */
		/*      end of the linked list, and point to the space  */
		/*      just malloc'ed ...                              */
		/*------------------------------------------------------*/

	prev = GetLastZoomLevel(widget_struct->zoom);
	prev->nextLevel = ThisZoomLevel;
	ThisZoomLevel->prevLevel = prev;
	}

 ThisZoomLevel->nextLevel = (ZoomStruct *) NULL;

 return(ThisZoomLevel);         /* Return a pointer for the space just  */
				/* malloc'ed, so it can be filled...    */
}


/* ****************************************************************************

	ZoomStruct *GetLastZoomLevel()


   **************************************************************************** */

ZoomStruct *GetLastZoomLevel(level)
	ZoomStruct      *level;
{

	ZoomStruct      *ptr = level;
	ZoomStruct      *prev;


 if(level->nextLevel == (ZoomStruct *) NULL) return(level);

 while(ptr != NULL)
	 {
	 prev = ptr;
	 ptr = ptr->nextLevel;
	 }

 return(prev);

}



/* ****************************************************************************

	zoom()
		the center of a rectangular selected area is passed to the
		function and the selected area is re-scaled to some maximum
		size that will be contained within the ScrolledWindow

   **************************************************************************** */

void zoom(center, widget_struct, data)
	point                   *center;        /* (x,y) center of the zoom area in HRAP units...       */
	the_widget_struct       *widget_struct;
	draw_struct             *data;
{
	Widget          SWinHorzScrollBar;
	Widget          SWinVertScrollBar;

	char            string[20];

	int             HorzScrollBar_valueReturn;
	int             HorzScrollBar_sliderSizeReturn;
	int             HorzScrollBar_incrementReturn;
	int             HorzScrollBar_pageIncrementReturn;
	int             HorzScrollBar_maximum;

	int             VertScrollBar_valueReturn;
	int             VertScrollBar_sliderSizeReturn;
	int             VertScrollBar_incrementReturn;
	int             VertScrollBar_pageIncrementReturn;
	int             VertScrollBar_maximum;

	int             x_pixels_per_bin;
	int             y_pixels_per_bin;
	int             pixels_per_bin;
	int             pixels_per_bin_Y;
	int             x;
	int             y;
	int             i;
	int             prev_origin_x;
	int             prev_origin_y;
	int             delta_x;
	int             delta_y;
	int             map_scale;

	Dimension       width, height;          /* Width & Height of the DrawingArea widget             */
	Dimension       sw_width, sw_height;    /* Width & Height of the ScrolledWindow widget,         */
						/* which is the parent of the DrawingArea widget...     */




 /*-----------------------------------------------------------------------------*/
 /*     get size of pixels in original display                                  */
 /*-----------------------------------------------------------------------------*/

 XtVaGetValues(widget_struct->drawArea_SWindow,
	      XmNwidth,               &sw_width,
	      XmNheight,              &sw_height,
	      XmNhorizontalScrollBar, &SWinHorzScrollBar,
	      XmNverticalScrollBar,   &SWinVertScrollBar,
	      NULL);

 XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

 XtVaGetValues(SWinHorzScrollBar, XmNmaximum, &HorzScrollBar_maximum, NULL);
 XmScrollBarGetValues(SWinHorzScrollBar,
		     &HorzScrollBar_valueReturn,
		     &HorzScrollBar_sliderSizeReturn,
		     &HorzScrollBar_incrementReturn,
		     &HorzScrollBar_pageIncrementReturn);

 XtVaGetValues(SWinVertScrollBar, XmNmaximum, &VertScrollBar_maximum, NULL);
 XmScrollBarGetValues(SWinVertScrollBar,
		     &VertScrollBar_valueReturn,
		     &VertScrollBar_sliderSizeReturn,
		     &VertScrollBar_incrementReturn,
		     &VertScrollBar_pageIncrementReturn);


 x_pixels_per_bin = (float)width/(float) data->maximum_columns;
 y_pixels_per_bin = (float)height/(float) data->maximum_rows;

 if (x_pixels_per_bin > y_pixels_per_bin) x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin) y_pixels_per_bin = x_pixels_per_bin;

 delta_x = abs(rbdata.last_x-rbdata.start_x);
 delta_y = abs(rbdata.last_y-rbdata.start_y);


 /*-----------------------------------------------------------------------------*/
 /*     determine size of selected zoom area...                                 */
 /*-----------------------------------------------------------------------------*/

 pixels_per_bin = (float)sw_width * (float)x_pixels_per_bin/(float)delta_x;
 pixels_per_bin_Y = (float)sw_height * (float)y_pixels_per_bin/(float)delta_y;

 if(pixels_per_bin > pixels_per_bin_Y) pixels_per_bin = pixels_per_bin_Y;

 if(pixels_per_bin == 0) pixels_per_bin = 1;   /* added: dp - 29 Dec. 93 */

 data->maximum_columns = width/pixels_per_bin;
 data->maximum_rows = height/pixels_per_bin;

 data->origin.x = center->x - data->maximum_columns/2;
 data->origin.y = center->y - data->maximum_rows/2;

 /*-----------------------------------------------------------------------------*/
 /*     Calculate how far we've zoomed in:                                      */
 /*                                                                             */
 /*                          (# Pixels per HRAP bin currently)                  */
 /*            zoom_factor = ---------------------------------                  */
 /*                          (# Pixels per HRAP bin originally)                 */
 /*                                                                             */
 /*-----------------------------------------------------------------------------*/

 zoom_factor = (float) MAXX /(float) data->maximum_columns;

 printf("Zoom Factor: %f\n", zoom_factor);

 map_scale = get_map_scale(center, pixels_per_bin);
 sprintf(string,"%d", map_scale);
 XtVaSetValues(widget_struct->Scale_TextField, XmNvalue, string, NULL);


 /*-----------------------------------------------------------------------------*/
 /*     create the pixmap and draw to the window...                             */
 /*-----------------------------------------------------------------------------*/

 fill_pixmap(widget_struct->main_canvas, widget_struct, NULL);

 XmScrollBarSetValues(SWinHorzScrollBar,
		     HorzScrollBar_maximum/2 - HorzScrollBar_sliderSizeReturn/2,
		     HorzScrollBar_sliderSizeReturn,
		     HorzScrollBar_incrementReturn,
		     HorzScrollBar_pageIncrementReturn,
		     TRUE);

 XmScrollBarSetValues(SWinVertScrollBar,
		     VertScrollBar_maximum/2 - VertScrollBar_sliderSizeReturn/2,
		     VertScrollBar_sliderSizeReturn,
		     VertScrollBar_incrementReturn,
		     VertScrollBar_pageIncrementReturn,
		     TRUE);

 if(widget_struct->tree_shell != NULL)
	{
	for(i = 0; i <= sub_group_num; i++)
			highlight_MAPBasins(widget_struct->head[i], widget_struct);
	}

 data->center.x = center->x;
 data->center.y = center->y;

 /*-----------------------------------------------------------------------------*/
 /*     re-initialize the zoom-box verticies to (0,0)...                        */
 /*-----------------------------------------------------------------------------*/
 rbdata.start_x = rbdata.last_x = 0;
 rbdata.start_y = rbdata.last_y = 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/zoom.c,v $";
 static char rcs_id2[] = "$Id: zoom.c,v 1.1 1995/09/08 14:56:04 page Exp $";}
/*  ===================================================  */

}

