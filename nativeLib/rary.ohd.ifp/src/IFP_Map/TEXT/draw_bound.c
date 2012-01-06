/*=========================================================================*/
/*                    FILE PATH/NAME:   /home/lef/s3/draw_bound.c          */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   draw_bound()                       */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "drawa.h"




/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/draw_bound.c                             */
/*  FUNCTION NAME:   draw_bound()                                          */
/*       FUNCTION:   draw RFC boundary to pixmap                           */
/***************************************************************************

Function type:
   void

Called by function:
   fill_pixmap

Functions called:
   get_pixel_by_name

Global variables:
   bound - stack double-deref (array) overlay_struct structure; 
   color_list - stack double-deref character; dimensioned 25 on stack;
   numrfc - integer;

Local variables:
   w - Widget structure;
   data - deref draw_struct structure;
   x - unsigned integer;
   y - unsigned integer;
   i - integer;
   j - integer;
   points - stack deref (array) XPoint structure;
   mask - integer;
   gcv - XGCValues structure;
   gcbound - GC structure;
   dpy - deref Display structure;

******************************************** BEGIN draw_bound **************/

void draw_bound(w, data, x, y)
	Widget          w;
	draw_struct     *data;
	unsigned int    x;
	unsigned int    y;
{
	int             i;
	int             j;
	XPoint          *points;
	int             mask = GCForeground;
	XGCValues       gcv;
	GC              gcbound;
	Display         *dpy;



 /*--------------------------------------------------------------*/
 /*     create graphics context using appropriate color          */
 /*--------------------------------------------------------------*/

 dpy = XtDisplay(w);
 gcv.foreground = get_pixel_by_name(w, color_list[23]);
 gcbound = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 XSetLineAttributes(XtDisplay(w),
		    gcbound,
		    (unsigned int) 2,   /* width      */
		    LineSolid,          /* line_style */
		    0,               /* cap_style  */
		    0);              /* join_stype */


 /*--------------------------------------------------------------*/
 /*     draw each segment of rfc boundary                        */
 /*--------------------------------------------------------------*/

 points = (XPoint *) malloc(sizeof(XPoint) * bound[0]->npts);

 for(j = 0; j < bound[0]->npts; j++)
 {
    points[j].x = (bound[0]->hrap[j].x - data->origin.x) * x;
    points[j].y = (data->maximum_rows - (bound[0]->hrap[j].y - data->origin.y))*y;
    printf(""); /*AV added this to fix RFC boundary problem (Do not know why!!Could it be mem leaks??)*/
 }                    
 XDrawLines(dpy, data->pix, gcbound, points, bound[0]->npts, CoordModeOrigin);


 /*--------------------------------------------------------------*/
 /*     connect first and last points of boundary                */
 /*--------------------------------------------------------------*/

/*
 points[1].x = (bound[0].hrap[0].x - data->origin.x) * x;
 points[1].y = (data->maximum_rows - (bound[0].hrap[0].y - data->origin.y)) * y;
 XDrawLine(dpy, data->pix, gcbound, points[0].x, points[0].y, points[1].x, points[1].y);
*/

 free(points);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/draw_bound.c,v $";
 static char rcs_id2[] = "$Id: draw_bound.c,v 1.3 2006/04/07 13:29:40 aivo Exp $";}
/*  ===================================================  */

}

