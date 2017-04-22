
#include "stage3_interface.h"
#include "stage3.h"
#include "drawa.h"
#include "stage3_globals.h"


/***************************************************************************/
/*  FUNCTION NAME:   gage_edit_missing                                     */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:

Called by function:

Functions called:

Local variables:

Modification History:

October 23, 2002    Bryon Lawrence     Removed line of code which would 
                                       sensitize the "Rerun MpeFieldGen"
                                       option on the "MPEcontrols" menu.

******************************************** BEGIN gage_edit_missing *******/

void gage_edit_missing(w, num, call_data)
   Widget w;
   int num;
   caddr_t *call_data;
{
 strcpy(gage[num].edit, "M");
}
/******************************************** END gage_edit_missing *******/
