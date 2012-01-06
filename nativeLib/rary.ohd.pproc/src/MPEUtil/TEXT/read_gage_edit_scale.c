

#include "stage3_interface.h"
#include "stage3.h"
#include "drawa.h"
#include "mpe_log_utils.h"
#include "stage3_globals.h"

/***************************************************************************/
/*  FUNCTION NAME:   read_gage_edit_scale                                  */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:

Called by function:

Functions called:

Local variables:

******************************************** BEGIN read_gage_edit_scale*****/

void read_gage_edit_scale(w, edit_struct, call_data)
   Widget       w;
   gage_edit_struct   *edit_struct;
   caddr_t      *call_data;
{
   extern int   dbg ;
   int          value;

 if (dbg) logMessage("in read gage edit scale\n");
 XmScaleGetValue(edit_struct->w, &value);
 sprintf(gage[edit_struct->num].edit,"%.2f", (float)value/100.);
 /*XtSetSensitive(rerun_stii_widget,TRUE);*/
 if (dbg) logMessage("gage # %d = %s value=%d\n",edit_struct->num,gage[edit_struct->num].edit,value);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
/******************************************** END read_gage_edit_scale*****/

