/*=========================================================================*/
/*                         FILE NAME:  draw_bound.c                        */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   draw_bound()                       */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include "drawa.h"
#include "map_defines.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "overlay.h"
#include "stage3_globals.h"

/*~~~GLOBAL VARIABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

char           *color_list_overlays[40];

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/***************************************************************************/
/*  FUNCTION NAME:   draw_bound()                                          */
/*       FUNCTION:   draw RFC boundary to pixmap                           */
/***************************************************************************

Function type:
   void

Called by function:
   fill_pixmap_radar_site

Functions called:
   get_pixel_by_name

******************************************** BEGIN draw_bound **************/

void draw_bound ( Widget w, draw_struct * data, unsigned int x, unsigned int y )
{
   char         color [ COLOR_NAME_LENGTH + 1 ] ;
   Display      * dpy = NULL ;
   GC           gcbound;
   int          j;
   int          line_width ;
   int          mask = GCForeground;
   const char * pColor = NULL ;   
   XGCValues    gcv;
   XPoint       * points = NULL ;

 /*--------------------------------------------------------------*/
 /*     create graphics context using appropriate color          */
 /*--------------------------------------------------------------*/
 pColor = mGetOverlayColor ( M_RFC_BOUNDARY ) ; 
 
 /* Create a local copy of the constant point pColor. */
 memset ( color , '\0' , COLOR_NAME_LENGTH + 1 ) ;
 strcpy ( color , pColor ) ;
 gcv.foreground = get_pixel_by_name ( w , color ) ;

 /* Retrieve the line width of the overlay. */
 line_width = mGetOverlayLineWidth ( M_RFC_BOUNDARY ) ; 

 if ( line_width >= 0 )
 {
    gcv.line_width = line_width ;
 }
 else
 {
    gcv.line_width = 1 ;
 }

 dpy = XtDisplay(w);
 gcbound = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 /*--------------------------------------------------------------*/
 /*     draw each segment of rfc boundary                        */
 /*     geo_data overlay has 1 segment for rfc boundary          */
 /*--------------------------------------------------------------*/

 points = (XPoint *) malloc(sizeof(XPoint) * bound[0]->npts);

 if ( points == NULL )
 {
    flogMessage ( stderr , "\nIn routine \"draw_bound\":\n"
                       "Could not allocate %d bytes of memory for the\n"
                       "\"points\" array.  Cannot draw bounds.\n" ,
                       ( sizeof ( XPoint ) * bound [ 0 ] -> npts ) ) ;
    return ;
 }

 for (j = 0; j < bound[0]->npts; j++)
    {
    points[j].x = (bound[0]->hrap[j].x - data->origin.x) * x;
    points[j].y = (data->maximum_rows - (bound[0]->hrap[j].y - data->origin.y))*y;
    }
 XDrawLines(dpy, data->pix, gcbound, points, bound[0]->npts, CoordModeOrigin);

 /*--------------------------------------------------------------*/
 /*     connect first and last points of boundary                */
 /*--------------------------------------------------------------*/

 free(points);
 XFreeGC ( dpy , gcbound ) ;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

/********************************************* END draw_bound **************/
