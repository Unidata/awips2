
/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <X11/cursorfont.h>
#include <Xm/Protocols.h>
#include <time.h>
/*
#include "libXs.h"
#include "map.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "post_functions.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "Xtools.h"
*/
/***************************************************************************/
/*  FUNCTION NAME:   ok_callback()                                         */
/*       FUNCTION:   determine date/hour selected by user from Choose      */
/*                      Dates window                                       */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) Choose Dates OK button

Functions called:
   none

date_prev = previous date structure for use in case change of date is
            cancelled

datetime = date time string for use with Informix datetime format fields

******************************************** BEGIN ok_callback *************/

void ok_callback(Widget w, int* client_data, XmAnyCallbackStruct* call_data)
{
 //int dbg ;

// if (dbg)logMessage("in ok_callback\n");
// if (dbg)logMessage("iselect = %d\n",*client_data);

// date_prev = date_st3;
// date_st3 = dates[*client_data];
//logMessage("%d\n",*client_data);
 //date_st3 = dates[0];

 /* Send all queued X events to the server. */ 
// sprintf(datetime,"%04d-%02d-%02d %02d:00:00",date_st3.year,date_st3.month,
 //        date_st3.day, date_st3.hour);

// if (dbg)logMessage("date = %s\n",date_st3.cdate);


 /* Turn on the Mpe Legend if it is not already "on" . */
//turnOnMpeData ( ) ;
// _turn_legend_on ( ) ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
/********************************************* END ok_callback *************/
