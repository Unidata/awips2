
#include <sys/stat.h>
#include <X11/cursorfont.h>

#include "draw_precip_poly_RFCW.h"
#include "stage3_interface.h"
#include "stage3_globals.h"
#include "stage3.h"
#include "drawa.h"


/***************************************************************************/
/*  FUNCTION NAME:   read_edit_precip_value                                */
/*       FUNCTION:   read new precip value to be assigned to bins within   */
/*                   the draw precip polygon from the slider bar           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) ok button on slider bar popup

******************************************** BEGIN read_edit_precip_value*****/

void read_edit_precip_value(w, poly_struct, call_data)
   Widget       w;
   rubber_poly_data   *poly_struct;
   caddr_t      *call_data;
{
   int          value;

 XmScaleGetValue(poly_struct->shell, &value);
 draw_precip_values[poly_struct->xpoly] = (float)value/100.;
}
/******************************************** END read_edit_precip_value*****/

