/*=========================================================================*/
/*                    FILE PATH/NAME:   /home/lef/s3/copy_area.c           */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   copy_area()                        */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"
#include "drawa.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/copy_area.c                              */
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
   w - Widget structure;
   data - deref draw_struct structure;
   call_data - deref XmDrawingAreaCallbackStruct structure;
   event - deref XExposeEvent structure;

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


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/copy_area.c,v $";
 static char rcs_id2[] = "$Id: copy_area.c,v 1.1 1995/09/08 14:55:05 page Exp $";}
/*  ===================================================  */

}

