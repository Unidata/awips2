#include "stage3_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "mpe_log_utils.h"
#include "drawa.h"
#include <X11/cursorfont.h>


/***************************************************************************/
/*  FUNCTION NAME:   select_callback()                                     */
/*       FUNCTION:   callback to determine selected data in list           */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) button press in list widget

Functions called:

******************************************** BEGIN select_callback *********/

void select_callback(w, client_data, call_data)
   Widget                w;
   int                  *client_data;
   XmListCallbackStruct *call_data;
{
   extern int dbg ; 
   char                 *text;

 XmStringGetLtoR(call_data->item, XmSTRING_DEFAULT_CHARSET, &text);
 *client_data = call_data->item_position;
 if (dbg) logMessage("%s iselect = %d\n",text,*client_data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

/********************************************* END select_callback *********/

