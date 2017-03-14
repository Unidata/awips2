/*=========================================================================*/
/*                    FILE PATH/NAME:  STAGE3_SOURCE/copy_area.c           */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   copy_area()                        */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "stage3_interface.h"
#include "post_stage3.h"
#include "post_drawa.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/copy_area.c                              */
/*  FUNCTION NAME:   copy_area()                                           */
/*       FUNCTION:   copy portion of pixmap back to screen as a result of  */
/*                    an expose event                                      */
/***************************************************************************

Function type:
   void

Called by function:
   fill_pixmap

Functions called:
   none

Local variables:
   w - Widget structure; DrawingArea widget
   data - deref draw_struct structure;
   call_data - deref XmDrawingAreaCallbackStruct structure;
   event - deref XExposeEvent structure; Expose Event detailing area exposed

******************************************** BEGIN copy_area ***************/

void copy_area(w, data, call_data)
   Widget                       w;
   draw_struct                 *data;
   XmDrawingAreaCallbackStruct *call_data;
{
   XExposeEvent                *event = (XExposeEvent *) call_data->event;

 /*--------------------------------------------------------------*/
 /*     Extract the expose area from the event and copy          */
 /*     from the saved pixmap to the window.                     */
 /*--------------------------------------------------------------*/

 XCopyArea(XtDisplay(w), data->pix, XtWindow(w), data->gc[0],
	   event->x, event->y, event->width, event->height,
	   event->x, event->y);



}

/********************************************* END copy_area ***************/
