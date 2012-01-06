
/* **************************************************************************************

	handle_segments.c


	Coded by:               Tom Adams - 6/10/91

	Modifications by:       Tom Adams  (NWS/OH/HRL)


   ************************************************************************************** */


#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"





/* ************************************************************************

	highlight_current_segment()

   ************************************************************************ */


void highlight_current_segment (some_widgets, currentSegment, previousSegment)
	the_widget_struct       *some_widgets;
	char                    *currentSegment;
	char                    *previousSegment;
{

	int             i;
	node            *the_node;
	unsigned long   mask = 0;
	XGCValues       gcv;
	Display         *display;
	Pixel           color;




 display = XtDisplay(some_widgets->overlays->w);

/* replacing the following erroneous test for a null pointer - jgg 8/24/01
  if(strcmp(currentSegment, (char *)NULL) != 0)*/
 if(currentSegment != NULL)
	{
	for(i = 0; i <= sub_group_num; i++)
		if((the_node = find_it(some_widgets->head[i], currentSegment)) != NULL) break;

	invert_segment(currentSegment, some_widgets->head[i]);


	/*----------------------------------------------------------------------*/
	/*      Reset the gc foreground color to the color used to highlight    */
	/*      a user selected segment...                                      */
	/*----------------------------------------------------------------------*/

	 if(the_node != NULL)
		{
		if(some_widgets->overlays->Basin_gc == NULL)
		       {
		       some_widgets->overlays->Basin_gc = (GC *) malloc(sizeof(GC));
		       *some_widgets->overlays->Basin_gc =
				      XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
		       }
		color = get_pixel_by_name(some_widgets->overlays->w, flood_color_levels[SELECTED]);
		XSetForeground(display, *some_widgets->overlays->Basin_gc, color);

		highlight_MAPBasin_selected(the_node, some_widgets->overlays);
		set_MAPBasin_selected(the_node, some_widgets->overlays);
		}
	}
/* replacing the following erroneous test for a null pointer - jgg 8/24/01
 if(strcmp(previousSegment, (char *)NULL) != 0)*/
if(previousSegment != NULL)
	{
	for(i = 0; i <= sub_group_num; i++)
		if((the_node = find_it(some_widgets->head[i], previousSegment)) != NULL) break;

	invert_segment(previousSegment, some_widgets->head[i]);


	/*----------------------------------------------------------------------*/
	/*      Set the current segment's MAP basin color to the color          */
	/*      representing the computed flood level (NORMAL, ALERT, FLOOD,    */
	/*      UNKNOWN)...                                                     */
	/*----------------------------------------------------------------------*/

	 if(the_node != NULL)
		{
		if(some_widgets->overlays->Basin_gc == NULL)
		       {
		       some_widgets->overlays->Basin_gc = (GC *) malloc(sizeof(GC));
		       *some_widgets->overlays->Basin_gc =
				      XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
		       }
		color = get_pixel_by_name(some_widgets->overlays->w, flood_color_levels[the_node->computed_status]);
		XSetForeground(display, *some_widgets->overlays->Basin_gc, color);


		highlight_MAPBasin_selected(the_node, some_widgets->overlays);
		}
	}


}




/* ************************************************************************

	change_current_segment_status()

   ************************************************************************ */


void change_current_segment_status (some_widgets, segmentStatus)
	the_widget_struct       *some_widgets;
	seg_status              *segmentStatus;
{

	Widget          w;
	Display         *display;
	Window          root;

	char            segment_name[9];
	char            *first_blank;
	char            *currentSegment;
	char            *string;

	int             i;
	int             pixel;

	int             mask = 0;
	XGCValues       gcv;

	int             bg_pixel;
	int             fg_pixel;

	node            *found;



 w = some_widgets->main_canvas;

 currentSegment = (char *) malloc(9);

 display = XtDisplay(global_toplevel);
 root = DefaultRootWindow(display);


 switch(segmentStatus->status_id)
	{
	case 0:
		pixel = get_pixel_by_name(global_toplevel, flood_color_levels[UNKNOWN]);
		break;

	case 1:
		pixel = get_pixel_by_name(global_toplevel, flood_color_levels[NORMAL]);
		break;

	case 2:
		pixel = get_pixel_by_name(global_toplevel, flood_color_levels[ALERT]);
		break;

	case 3:
		pixel = get_pixel_by_name(global_toplevel, flood_color_levels[FLOOD]);
		break;

	default:
		printf("The received status is invalid!\n");
		return;
		break;
	}


 strcpy(segment_name, segmentStatus->segment_name);

 memset(currentSegment, '\0', 9);
 if((first_blank = strstr(segment_name, " ")) != NULL)
	{
	strncpy(currentSegment, segment_name, first_blank - segment_name);
	change_segment_color(some_widgets, currentSegment, pixel, segmentStatus->status_id);
	}
 else   {
	change_segment_color(some_widgets, segment_name, pixel, segmentStatus->status_id);
	strcpy(currentSegment, segment_name);
	}

/*----------------------------------------------------------------------*/
/*      Set the current segment's MAP basin color to the color          */
/*      representing the computed flood level (NORMAL, ALERT, FLOOD,    */
/*      UNKNOWN)...                                                     */
/*----------------------------------------------------------------------*/

 for(i = 0; i <= sub_group_num; i++)
	if((found = find_it(some_widgets->head[i], currentSegment)) != NULL) break;

 if(some_widgets->overlays->Basin_gc == NULL)
	{
	some_widgets->overlays->Basin_gc = (GC *) malloc(sizeof(GC));
	*some_widgets->overlays->Basin_gc =
		       XCreateGC(display, DefaultRootWindow(display), mask, &gcv);
	}
 XSetForeground(display, *some_widgets->overlays->Basin_gc,
		get_pixel_by_name(w, flood_color_levels[segmentStatus->status_id]));

 highlight_MAPBasin_selected(found, some_widgets->overlays);

/*----------------------------------------------------------------------*/
/*      Reset the gc foreground color to the color used to highlight    */
/*      a user selected segment...                                      */
/*----------------------------------------------------------------------*/

 XSetForeground(display, *some_widgets->overlays->Basin_gc,
		get_pixel_by_name(w, flood_color_levels[SELECTED]));


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/handle_segments.c,v $";
 static char rcs_id2[] = "$Id: handle_segments.c,v 1.3 2006/04/07 13:30:09 aivo Exp $";}
/*  ===================================================  */

}

