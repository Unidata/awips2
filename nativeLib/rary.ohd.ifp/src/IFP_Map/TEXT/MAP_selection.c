
/* ******************************************************************************************

	MAP_selection.c
		contains functions for handling the selection, and highlighting of MAP
		basins within the selected forecast group for the NWSRFS run


	Coded by:       Tom Adams
	Date:           11/06/92
	Affiliation:    NOAA/NWS/Office of Hydrology/HRL

	Functions in MAP_selection.c :

			void    set_MAPBasin_selected();
			void    reset_previous_MAPBasin_selected();
			XPoint  **get_MAPBasin_boundaries();
			void    highlight_MAPBasin_selected();
			void    highlight_MAPBasins();
			void    reset_MAPBasins_ifSubnodeSelected();
			void    set_MAPBasin_Subnodes_selected();
			Region  get_NodeRegion();
			void    Reset_ForecastGroupRegion();


   ****************************************************************************************** */



#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "drawa.h"


static  XPoint **get_MAPBasin_boundaries();




/* ************************************************************************************

	void set_MAPBasin_selected()



   ************************************************************************************ */

void set_MAPBasin_selected(node *head, draw_struct *data)
{

	int             i;
	int             j;

	XPoint          **points;
	Region          tempRegion;
	Region          CombinedRegion;
	Region          IntersectRegion;
	XRectangle      boundingRectangle;
	Display         *display;


 display = XtDisplay(data->w);

 /*--------------------------------------------------------------*/
 /*     For each MAP basin in the selected segment, color its    */
 /*     interior, and union the Regions together; also, get      */
 /*     the bounding rectangle for the entire segment area...    */
 /*--------------------------------------------------------------*/

 if((points = get_MAPBasin_boundaries(head, data)) == NULL) return;

 CombinedRegion = XCreateRegion();      /* Create a Region of zero size...      */

 for (i = 0; i < head->e19.num_MAP_basins; i++)
    if (points[i] != NULL)         /* added check for points[i] == NULL  - dp 042995 */
    {
        tempRegion = XPolygonRegion(points[i], head->MAP_data[i]->npts, EvenOddRule);
        XUnionRegion(tempRegion, CombinedRegion, CombinedRegion);
    }

 head->segment_region = CombinedRegion;

 /* If only one segment area has been selected, or if this is the       */
 /* first of two or more segment areas to be selected, save the         */
 /* region; otherwise, union this region with others saved previously   */

 if(data->selectedRegion == NULL) data->selectedRegion = CombinedRegion;
 else XUnionRegion(CombinedRegion, data->selectedRegion, data->selectedRegion);

 /* Get the bounding rectangle...                                       */

 XClipBox(data->selectedRegion, &boundingRectangle);


 data->zoom_factor = zoom_factor;

 /* Destroy the old pixmap, if it exists, and save the new one...       */

 if(data->highlightedArea_pixmap != (Pixmap)NULL) XFreePixmap(display, data->highlightedArea_pixmap);
 data->highlightedArea_pixmap = XCreatePixmap(display, DefaultRootWindow(display),
					      boundingRectangle.width, boundingRectangle.height,
					      DefaultDepthOfScreen(XtScreen(data->w)));


 /* Copy the area before it's drawn to, so we can revert back to it     */
 /* later without need for a complete redraw...                         */
 
 XCopyArea(display, data->pix, data->highlightedArea_pixmap, data->gc[0],
	  boundingRectangle.x, boundingRectangle.y, boundingRectangle.width, boundingRectangle.height,
	  0, 0);


 free(points);

}




/* ************************************************************************************

	void reset_previous_MAPBasin_selected()



   ************************************************************************************ */

void reset_previous_MAPBasin_selected(data)
	draw_struct     *data;
{

	XRectangle      boundingRectangle;
	Display         *display;

 
 if(zoom_factor != data->zoom_factor)
	{
	if(data->selectedRegion != NULL)
          {
           XDestroyRegion(data->selectedRegion);
           data->selectedRegion = NULL;
          }
	return;
	}

 display = XtDisplay(data->w);

 if(data->selectedRegion != NULL)
         XClipBox(data->selectedRegion, &boundingRectangle);

 /* Copy the area before it's drawn to, so we can revert back to it     */
 /* later without need for a complete redraw...                         */

 XCopyArea(display, data->highlightedArea_pixmap, data->pix, data->gc[0],
	  0, 0, boundingRectangle.width, boundingRectangle.height,
	  boundingRectangle.x, boundingRectangle.y);
 
 XCopyArea(display, data->pix, XtWindow(data->w),
		data->gc[0], 0, 0, data->width, data->height, 0, 0);

/* +++ XSetRegion(display, data->gc[0], None); +++ */
 if(data->selectedRegion != NULL){
 
     XDestroyRegion(data->selectedRegion);
     data->selectedRegion = NULL;
 }

}



/* ************************************************************************************

	XPoint **get_MAPBasin_boundaries()



   ************************************************************************************ */

XPoint **get_MAPBasin_boundaries(head, data)
	node            *head;
	draw_struct     *data;
{

	int             x_pixels_per_bin;
	int             y_pixels_per_bin;
	int             i;
	int             j;
	int             all_NULL;

	XPoint          **points = NULL;


 /* If there are no MAP basins, there's nothing to do...                */
 
 if(head->e19.num_MAP_basins <= 0) return (NULL);
 else if(head->e19.num_MAP_basins > 10000) return (NULL);


 /*---------------------------------------------------------------------*/
 /*     find location in HRAP coordinates                               */
 /*---------------------------------------------------------------------*/

 x_pixels_per_bin = (float)data->width/(float)data->maximum_columns;
 y_pixels_per_bin = (float)data->height/(float)data->maximum_rows;

 if (x_pixels_per_bin > y_pixels_per_bin) x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin) y_pixels_per_bin = x_pixels_per_bin;


 points = (XPoint **) malloc(sizeof(XPoint *) * head->e19.num_MAP_basins);
 all_NULL = TRUE;

 /*--------------------------------------------------------------*/
 /*     For each MAP basin in the selected segment, color its    */
 /*     interior, and union the Regions together; also, get      */
 /*     the bounding rectangle for the entire segment area...    */
 /*--------------------------------------------------------------*/

/* printf("Number MAP basins: %d\n", head->e19.num_MAP_basins); */

 for (i = 0; i < head->e19.num_MAP_basins; i++)
	{
	if(head->MAP_data[i] != NULL)
		{
		all_NULL = FALSE;
		points[i] = (XPoint *) malloc(head->MAP_data[i]->npts * sizeof(XPoint));

		for (j = 0; j < head->MAP_data[i]->npts; j++)
			{
			points[i][j].x = (head->MAP_data[i]->hrap[j].x - data->origin.x)
						* x_pixels_per_bin;
			points[i][j].y = (data->maximum_rows - (head->MAP_data[i]->hrap[j].y - data->origin.y))
						* y_pixels_per_bin;
			}
		}
	else points[i] = NULL;
	}

 if(all_NULL == TRUE) return (NULL);
 else return(points);

}




/* ************************************************************************************

	void highlight_MAPBasin_selected()



   ************************************************************************************ */

void highlight_MAPBasin_selected(head, data)
	node            *head;
	draw_struct     *data;
{

	int             i;
	int             mask = GCForeground;

	Pixel           color;
	XPoint          **points;
	XGCValues       gcv;
	XGCValues       gcValues;
	Display         *display;


 /*--------------------------------------------------------------*/
 /*     If the current segment Region is NULL, either it's not   */
 /*     selected or not viewable, so don't highlight it...       */
 /*--------------------------------------------------------------*/

 if(head == (node *)NULL) return;
 if((points = get_MAPBasin_boundaries(head, data)) == NULL) return;

 display = XtDisplay(data->w);


 /*--------------------------------------------------------------*/
 /*     Create a graphics context, if it's not already created...*/
 /*--------------------------------------------------------------*/

 if(data->Basin_gc == NULL)
 {
	gcv.foreground = get_pixel_by_name(data->w, color_list[18]);
	data->Basin_gc = (GC *) malloc(sizeof(GC));
	*data->Basin_gc = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
 }

 XGetGCValues(display, *data->Basin_gc, GCForeground, &gcValues);

 for (i = 0; i < head->e19.num_MAP_basins; i++)
	if (points[i] != NULL)               /* added check for points[i] == NULL  - dp 042995 */
	{
	XFillPolygon(display, XtWindow(data->w), *data->Basin_gc,
		     points[i], head->MAP_data[i]->npts, Nonconvex, CoordModeOrigin);
	XFillPolygon(display, data->pix, *data->Basin_gc,
		     points[i], head->MAP_data[i]->npts, Nonconvex, CoordModeOrigin);
	}

 if((sub_group_num >= 0)  && !NWSRFS_has_begun)
	{
	XSetLineAttributes(display, *data->Basin_gc, 2, LineSolid, CapButt, JoinRound);
	XSetForeground(display, *data->Basin_gc,
		       get_pixel_by_name(data->w, flood_color_levels[head->status]));
	}
 else   {
	XSetLineAttributes(display, *data->Basin_gc, 1, LineSolid, CapButt, JoinRound);
	XSetForeground(display, *data->Basin_gc, get_pixel_by_name(data->w, color_list[18]));
	}

 for (i = 0; i < head->e19.num_MAP_basins; i++)
	if (points[i] != NULL)                  /* added check for points[i] == NULL  - dp 042995 */
	{
	XDrawLines(display, XtWindow(data->w), *data->Basin_gc, points[i], head->MAP_data[i]->npts, CoordModeOrigin);
	XDrawLines(display, data->pix, *data->Basin_gc, points[i], head->MAP_data[i]->npts, CoordModeOrigin);
	}

 XSetForeground(display, *data->Basin_gc, gcValues.foreground);

 free(points);

}





/* ****************************************************************************

	 void highlight_MAPBasins()

   **************************************************************************** */

void highlight_MAPBasins(current, widget_struct)
	node                    *current;
	the_widget_struct       *widget_struct;
{
	int             i, length;
	char            seg_selected[9];
	char            CurrentSegment[9];
	char            *firstBlank;
	char            *blank = " ";
	char            string[9];
	XGCValues       gcv;
	int             mask = GCForeground;
	Pixel           pixel;
	Display         *display;



 display = XtDisplay(widget_struct->overlays->w);
 
 strcpy(string, current->e19.name);
 length = strlen(string);
 for(i = 0; i < 8 - length; i++) strcat(string, blank);

 /* Highlight the segment MAP basins if selected...     */

 if(NWSRFS_has_begun)
	{
	/*------------------------------------------------------*/
	/* We want to make sure that the MAP basins are filled  */
	/* with the appropriate color...                        */
	/*------------------------------------------------------*/

	strcpy(CurrentSegment, widget_struct->current_segment);
	if((firstBlank = strstr(CurrentSegment, " ")) != NULL) *firstBlank = '\0';

	if(strcmp(current->e19.name, CurrentSegment) == 0)
			pixel = get_pixel_by_name(widget_struct->overlays->w, flood_color_levels[SELECTED]);
	else            
			pixel = get_pixel_by_name(widget_struct->overlays->w, flood_color_levels[current->computed_status]);

	if(widget_struct->overlays->Basin_gc == (GC *)NULL)
	       {
	       gcv.foreground = pixel;
	       widget_struct->overlays->Basin_gc = (GC *) malloc(sizeof(GC));
	       *widget_struct->overlays->Basin_gc = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
	       }
	XSetForeground(XtDisplay(current->segment_widget), *widget_struct->overlays->Basin_gc, pixel);

	}

 if(strstr(selected_string, string) != (char *)NULL)
		highlight_MAPBasin_selected(current, widget_struct->overlays);

 if(current->left != (node *)NULL)      highlight_MAPBasins(current->left, widget_struct);
 if(current->mid_left != (node *)NULL)  highlight_MAPBasins(current->mid_left, widget_struct);
 if(current->center != (node *)NULL)    highlight_MAPBasins(current->center, widget_struct);
 if(current->mid_right != (node *)NULL) highlight_MAPBasins(current->mid_right, widget_struct);
 if(current->right != (node *)NULL)     highlight_MAPBasins(current->right, widget_struct);

}




/* ****************************************************************************

	 void reset_MAPBasins_ifSubnodeSelected()

   **************************************************************************** */

void reset_MAPBasins_ifSubnodeSelected(data, current)
	draw_struct     *data;
	node            *current;
{
	int             i, length;
	char            seg_selected[9];
	char            *blank = " ";
	char            string[9];




 if(current->left != NULL)
	{
	strcpy(string, current->left->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) != NULL)
		{
		if(data->selectedRegion != NULL) reset_previous_MAPBasin_selected(data);
		}
	else reset_MAPBasins_ifSubnodeSelected(data, current->left);
	}
 if(current->mid_left != NULL)
	{
	strcpy(string, current->mid_left->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) != NULL)
		{
		if(data->selectedRegion != NULL) reset_previous_MAPBasin_selected(data);
		}
	else reset_MAPBasins_ifSubnodeSelected(data, current->mid_left);
	}
 if(current->center != NULL)
	{
	strcpy(string, current->center->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) != NULL)
		{
		if(data->selectedRegion != NULL) reset_previous_MAPBasin_selected(data);
		}
	else reset_MAPBasins_ifSubnodeSelected(data, current->center);
	}
 if(current->mid_right != NULL)
	{
	strcpy(string, current->mid_right->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) != NULL)
		{
		if(data->selectedRegion != NULL) reset_previous_MAPBasin_selected(data);
		}
	else reset_MAPBasins_ifSubnodeSelected(data, current->mid_right);
	}
 if(current->right != NULL)
	{
	strcpy(string, current->right->e19.name);
	length = strlen(string);
	for(i = 0; i < 8 - length; i++) strcat(string, blank);

	if(strstr(selected_string, string) != NULL)
		{
		if(data->selectedRegion != NULL) reset_previous_MAPBasin_selected(data);
		}
	else reset_MAPBasins_ifSubnodeSelected(data, current->right);
	}
}


/* ************************************************************************************

	void set_MAPBasin_Subnodes_selected()



   ************************************************************************************ */

void set_MAPBasin_Subnodes_selected(head, widget_struct)
	node                    *head;
	the_widget_struct       *widget_struct;
{

	XGCValues       gcv;
	Display         *display;
	int             mask = GCForeground;
	Pixel           pixel;
	char            CurrentSegment[9];
	char            *firstBlank;

	draw_struct     *data;



 display = XtDisplay(widget_struct->overlays->w);

 if(NWSRFS_has_begun)
	{
	/*------------------------------------------------------*/
	/* We want to make sure that the MAP basins are filled  */
	/* with the appropriate color...                        */
	/*------------------------------------------------------*/

	strcpy(CurrentSegment, widget_struct->current_segment);
	if((firstBlank = strstr(CurrentSegment, " ")) != NULL) *firstBlank = '\0';

	if(strcmp(head->e19.name, CurrentSegment) == 0)
			pixel = get_pixel_by_name(widget_struct->overlays->w, flood_color_levels[SELECTED]);
	else            
			pixel = get_pixel_by_name(widget_struct->overlays->w, flood_color_levels[head->computed_status]);

	if(widget_struct->overlays->Basin_gc == NULL)
	       {
	       gcv.foreground = pixel;
	       widget_struct->overlays->Basin_gc = (GC *) malloc(sizeof(GC));
	       *widget_struct->overlays->Basin_gc = XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
	       }
	XSetForeground(XtDisplay(head->segment_widget), *widget_struct->overlays->Basin_gc, pixel);
	}

 highlight_MAPBasin_selected(head, widget_struct->overlays);

 if(head->left != (node *) NULL)      set_MAPBasin_Subnodes_selected(head->left, widget_struct);
 if(head->mid_left != (node *) NULL)  set_MAPBasin_Subnodes_selected(head->mid_left, widget_struct);
 if(head->center != (node *) NULL)    set_MAPBasin_Subnodes_selected(head->center, widget_struct);
 if(head->mid_right != (node *) NULL) set_MAPBasin_Subnodes_selected(head->mid_right, widget_struct);
 if(head->right != (node *) NULL)     set_MAPBasin_Subnodes_selected(head->right, widget_struct);

}



/* ************************************************************************************

	Region get_NodeRegion()



   ************************************************************************************ */

Region get_NodeRegion(head, data)
	node            *head;
	draw_struct     *data;
{

	int             i;

	XPoint          **points;
	XPoint          CanvasPoints[4];

	Region          tempRegion;
	Region          CombinedRegion;
	Region          LeftRegion = NULL;
	Region          MidLeftRegion = NULL;
	Region          CenterRegion = NULL;
	Region          MidRightRegion = NULL;
	Region          RightRegion = NULL;
	Region          CanvasRegion;
	Region          intersection;





 /*--------------------------------------------------------------*/
 /*     If the current segment Region is NULL, either it's not   */
 /*     selected or not viewable, so don't highlight it...       */
 /*--------------------------------------------------------------*/

 if(head->segment_region == NULL) return(NULL);


 /*--------------------------------------------------------------*/
 /* Initialize the XPoint array that defines the DrawingArea     */
 /* widget's window as a polygon (rectangle)...                  */
 /*--------------------------------------------------------------*/

 CanvasPoints[0].x = 0;
 CanvasPoints[0].y = 0;
 CanvasPoints[1].x = data->width;
 CanvasPoints[1].y = 0;
 CanvasPoints[2].x = data->width;
 CanvasPoints[2].y = data->height;
 CanvasPoints[3].x = 0;
 CanvasPoints[3].y = data->height;


 /* Create a Region for the segment comprised by it's   */
 /* individual MAP basin regions and save it...         */

 CombinedRegion = XCreateRegion();      /* Create a Region of zero size...      */

 /*-------------------------------------------------------------*/
 /*     It's possible there are no boundary data for the        */
 /*     segment, so we can't create a region, etc.; so, go      */
 /*     to the subnodes...                                      */
 /*-------------------------------------------------------------*/

 if((points = get_MAPBasin_boundaries(head, data)) != NULL)
	{
	for (i = 0; i < head->e19.num_MAP_basins; i++)
	       {
	       tempRegion = XPolygonRegion(points[i], head->MAP_data[i]->npts, EvenOddRule);
	       XUnionRegion(tempRegion, CombinedRegion, CombinedRegion);
	       }

	if(tempRegion != NULL) XDestroyRegion(tempRegion);

	XDestroyRegion(head->segment_region);
	free(points);

	/*-------------------------------------------------------------*/
	/*     Test if the region for the current node (segment) lies  */
	/*     at least partially within the window of the DrawingArea */
	/*     widget; if not, destroy the region and return.          */
	/*     Otherwise, proceed with the subnodes...                 */
	/*-------------------------------------------------------------*/

	intersection = XCreateRegion();        /* Create a Region of zero size...      */
	CanvasRegion = XPolygonRegion(CanvasPoints, 4, EvenOddRule);
	XIntersectRegion(CombinedRegion, CanvasRegion, intersection);

	if(XEmptyRegion(intersection))
	       {
	       head->segment_region = XCreateRegion(); /* Create a Region of zero size */

	       XDestroyRegion(CanvasRegion);
	       XDestroyRegion(intersection);
	       XDestroyRegion(tempRegion);
	       XDestroyRegion(CombinedRegion);

	       return(NULL);
	       }

	if(CanvasRegion != NULL)   XDestroyRegion(CanvasRegion);
	if(intersection != NULL)   XDestroyRegion(intersection);
	}

 head->segment_region = CombinedRegion;



 /*-------------------------------------------------------------*/
 /*     Do the subnodes...                                      */
 /*-------------------------------------------------------------*/

 if(head->left != NULL)
	{
	if((LeftRegion = get_NodeRegion(head->left, data)) != NULL)
		{
		XUnionRegion(LeftRegion, CombinedRegion, CombinedRegion);
		XDestroyRegion(LeftRegion);
		}
	}

 if(head->mid_left != NULL)
	{
	if((MidLeftRegion = get_NodeRegion(head->mid_left, data)) != NULL)
		{
		XUnionRegion(MidLeftRegion, CombinedRegion, CombinedRegion);
		XDestroyRegion(MidLeftRegion);
		}
	}

 if(head->center != NULL)
	{
	if((CenterRegion = get_NodeRegion(head->center, data)) != NULL)
		{
		XUnionRegion(CenterRegion, CombinedRegion, CombinedRegion);
		XDestroyRegion(CenterRegion);
		}
	}

 if(head->mid_right != NULL)
	{
	if((MidRightRegion = get_NodeRegion(head->mid_right, data)) != NULL)
		{
		XUnionRegion(MidRightRegion, CombinedRegion, CombinedRegion);
		XDestroyRegion(MidRightRegion);
		}
	}

 if(head->right != NULL)
	{
	if((RightRegion = get_NodeRegion(head->right, data)) != NULL)
		{
		XUnionRegion(RightRegion, CombinedRegion, CombinedRegion);
		XDestroyRegion(RightRegion);
		}
	}

 return(CombinedRegion);

}



/* ************************************************************************************

	void Reset_ForecastGroupRegion()



   ************************************************************************************ */

void Reset_ForecastGroupRegion(widget_struct, data)
	the_widget_struct       *widget_struct;
	draw_struct             *data;
{

	int     i;

	Region  tempRegion;
	Region  CombinedRegion;

 
 if(data->ForecastGroupRegion != NULL) XDestroyRegion(data->ForecastGroupRegion);

 CombinedRegion = XCreateRegion();      /* Create a Region of zero size...      */

 for(i = 0; i <= sub_group_num; i++)
	{
	if((tempRegion = get_NodeRegion(widget_struct->head[i], data)) != NULL)
		{
		XUnionRegion(tempRegion, CombinedRegion, CombinedRegion);
		XDestroyRegion(tempRegion);
		}
	}

 data->ForecastGroupRegion = CombinedRegion;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/MAP_selection.c,v $";
 static char rcs_id2[] = "$Id: MAP_selection.c,v 1.4 2006/04/07 13:29:22 aivo Exp $";}
/*  ===================================================  */

}
