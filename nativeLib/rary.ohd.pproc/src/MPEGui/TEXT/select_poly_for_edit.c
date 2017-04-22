/*=========================================================================*/
/*                         FILE NAME:  select_poly_for_edit.c              */
/*                                                                         */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <sys/stat.h>
#include <X11/cursorfont.h>

#include "draw_precip_poly_RFCW.h"
#include "mpe_log_utils.h"
#include "stage3_interface.h"
#include "stage3_globals.h"
#include "stage3.h"
#include "drawa.h"


/***************************************************************************/
/*  FUNCTION NAME:   select_poly_for_edit                                  */
/*       FUNCTION:    determine which polygon has been selected for edit   */
/*                    if inside more than one polygon, then innermost      */
/*                    polygon will be selected                             */
/***************************************************************************

Function type:
   void

Called by function:
   callback from mouse button press

Functions called:

********************************* BEGIN select_poly_for_edit *****************/

void select_poly_for_edit ( Widget w , XtPointer clientdata, XEvent * event ,
                            Boolean * continue_to_dispatch )
{
   rubber_poly_data * data = ( rubber_poly_data * ) clientdata ;
   int                  x , y ;
   double               dmin ;

 /*--------------------------------------------------------------*/
 /*     find location of cursor                                  */
 /*--------------------------------------------------------------*/

 x = event->xbutton.x;
 y = event->xbutton.y;

 /*--------------------------------------------------------------*/
 /*     initialize counters                                      */
 /*--------------------------------------------------------------*/

 dmin = 999;
 data->xpoly = -1;

 /*--------------------------------------------------------------*/
 /*     find polygon with boundary closest to button click       */
 /*--------------------------------------------------------------*/

/* for ( i = 0 ; i < data->npoly ; i++ )
 {
    for ( j=0 ; j < data->npoints [ i ] ; j++ )
    {
       dx = (double)(x - data->points[i][j].x);
       dy = (double)(y - data->points[i][j].y);
       d = sqrt(dx*dx + dy*dy);
       if (d < dmin)
       {
	  dmin = d;
	  data->xpoly = i;
       }
     }
  }

  if(dmin < 50)
   logMessage("polygon number %d selected for edit\n",data->xpoly);
  else
  {
   logMessage("button press did not occur near a polygon -- select edit option again\n");
    return;
  } */

  XtSetSensitive(editpoly_widget,FALSE);
  XtSetSensitive(deletepoly_widget,TRUE);
  XtSetSensitive(applyexit_widget,TRUE);
  XtSetSensitive(noapplyexit_widget,TRUE);
  

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
/********************************* END select_poly_for_edit *****************/

