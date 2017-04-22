/*=========================================================================*/
/*  FUNCTIONS CONTAINED IN THIS FILE:   mpe_start_rubber_band()            */
/*                                      mpe_track_rubber_band()            */
/*                                      mpe_end_rubber_band()              */
/*========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "map.h"
#include "map_convert.h"
#include "map_resource.h"
#include "post_functions.h"
#include "stage3_globals.h"
#include "stage3_interface.h"

/***************************************************************************/
/*  FUNCTION NAME:   mpe_start_rubber_band()                               */
/*       FUNCTION:   ----------------------------------------              */
/*                    ---------------------------------------              */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

Local variables:
   w - Widget structure;
   data - deref rubber_band_data structure;
   event - deref XEvent structure;
   gc_rect - GC structure;
   values - XGCValues structure;

******************************************** BEGIN start_rubber_band *******/

void mpe_start_rubber_band ( Widget w , XtPointer clientdata , 
                             XEvent * event ,
                             Boolean * continue_to_dispatch_return )
{

 Display * display = NULL ;
 int height ;
 int width ;
 Widget map_widget ;


 rubber_band_data  * data = ( rubber_band_data * ) clientdata ;

 /* Create an expose event to clear any polygons on the map. */
 map_widget = _get_map_widget ( 0 ) ;
 display = _get_map_display ( ) ;
 XClearArea ( display , XtWindow ( map_widget) , 0 , 0 , 1 , 1 , True ) ;   

 data->last_x = data->start_x = event->xbutton.x;
 data->last_y = data->start_y = event->xbutton.y;
 data->w = w;

 width = data->last_x - data->start_x ;
 height = data->last_y - data->start_y ;

 if ( width < 0 || height < 0 )
 {
    width = 0 ;
    height = 0 ;
 }

 XDrawRectangle(XtDisplay(w), XtWindow(w),
		data->gc,
		data->start_x, data->start_y,
		width , height ) ;

 data->rubber_band_zoom_mode = True ;

}

/********************************************* END mpe_start_rubber_band *****/



/***************************************************************************/
/*  FUNCTION NAME:   mpe_track_rubber_band()                               */
/*       FUNCTION:   tracks motion of mouse when right button is pressed   */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

Local variables:
   w - Widget structure;
   data - deref rubber_band_data structure;
   event - XEvent structure;

******************************************** BEGIN mpe_track_rubber_band ****/

void mpe_track_rubber_band( Widget w , XtPointer clientdata , XEvent * event ,
                            Boolean * continue_to_dispatch_return )
{

 int width ;
 int height ;

 rubber_band_data  * data = ( rubber_band_data * ) clientdata ;

 if ( data->rubber_band_zoom_mode == False ) return ;

 /*------------------------------------------------------------------*/
 /* Draw once to clear the previous line.                            */
 /*------------------------------------------------------------------*/
 width = data->last_x - data->start_x ;
 height = data->last_y - data->start_y ;

 if ( width < 0 || height < 0 )
 {
    width = 0 ;
    height = 0 ;
 }

 XDrawRectangle(XtDisplay(w), XtWindow(w), data->gc,
		data->start_x, data->start_y,
                width , height ) ;		

 /*------------------------------------------------------------------*/
 /* Update the end points.                                           */
 /*------------------------------------------------------------------*/

 data->last_x  = event->xbutton.x;
 data->last_y  = event->xbutton.y;

 width = data->last_x - data->start_x ;
 height = data->last_y - data->start_y ;

 if ( width < 0 || height < 0 )
 {
    width = 0 ;
    height = 0 ;
 }

 /*------------------------------------------------------------------*/
 /* Draw the new line.                                               */
 /*------------------------------------------------------------------*/

 XDrawRectangle(XtDisplay(w), XtWindow(w), data->gc,
		data->start_x, data->start_y,
                width , height ) ;
}

/***************************************** END mpe_track_rubber_band *******/



/***************************************************************************/
/*  FUNCTION NAME:   mpe_end_rubber_band()                                 */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:

Local variables:
   w - Widget structure;
   data - deref rubber_band_data structure;
   event - XEvent structure;

***************************************** BEGIN mpe_end_rubber_band *********/

void mpe_end_rubber_band( Widget w, XtPointer clientdata , XEvent * event ,
                          Boolean * continue_to_dispatch_return )
{
    rubber_band_data  * data = ( rubber_band_data * ) clientdata ;
    data->rubber_band_zoom_mode = False ;
    data->zoom_state = False ;
    data->use_rectangle = True ;
}

/***************************************** END mpe_end_rubber_band *********/
