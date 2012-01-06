

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "stage3.h"
#include "stage3_interface.h"
#include "drawa.h"
#include "mpe_log_utils.h"
#include "stage3_globals.h"
#include "help.h"
#include <string.h>
#include <X11/cursorfont.h>
#include <math.h>


/******************************************************************/
/* FILE PATH/NAME:   st3_src/add_pseudo.c                         */
/*  FUNCTION NAME:   read_pseudo_scale                            */
/*       FUNCTION:   read value of pseudo gage from scale         */
/*******************************************************************

Function type:
   void

Called by function:

Functions called:
   none

***************************** BEGIN read_pseudo_scale ***********/
void read_pseudo_scale(w, pseudo, call_data)
   Widget       w;
   pseudo_struct *pseudo;
   caddr_t      *call_data;
{
   extern int   dbg ;
   int          value ;

 if (dbg) logMessage("in read pseudo scale");
 XmScaleGetValue(pseudo->scale_widget, &value);
 pseudo->value = (float)value/100.;
 if (dbg) logMessage("value of pseudo equals %f\n", pseudo->value);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

