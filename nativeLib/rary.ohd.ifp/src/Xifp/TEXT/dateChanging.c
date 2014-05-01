
/* ******************************************************************************

	date_changing.c

	Changes the date in the date widget.

	Coded by:               Tom Adams
	Affiliation:            NOAA/NWS/Office of Hydrology/Hydrologic Research Laboratory
	Date:                   06/01/91
	Last modified:          06/06/91

   ****************************************************************************** */




#include "set_dates.h"








/* **********************************************************************

	multiple_increment()

   ********************************************************************** */

void multiple_increment(w, date_widget, call_data)
	Widget                  w;
	the_widgets             *date_widget;
	XmAnyCallbackStruct     *call_data;
{
	XEvent          theEvent;
	Display         *display;
	float           time = 0.2;     /*      Time in seconds for delay...            */
	unsigned int    delay;          /*      Delay in microseconds...                */

	int             time_to_sleep = TRUE;

 display = XtDisplay(w);

 delay = (unsigned int) (time * 1000000);
 usleep(delay);

 while(time_to_sleep)
	{
	increment(w, date_widget, NULL);

	XNextEvent(display, &theEvent);
	if(theEvent.type == ButtonRelease ||
	   theEvent.type == KeyRelease) time_to_sleep = FALSE;

	XtDispatchEvent(&theEvent);
	XSync(display, 0);

	usleep(delay);
	}
}



/* **********************************************************************

	multiple_decrement()

   ********************************************************************** */

void multiple_decrement(w, date_widget, call_data)
	Widget                  w;
	the_widgets             *date_widget;
	XmAnyCallbackStruct     *call_data;
{

	XEvent          theEvent;
	Display         *display;
	float           time = 0.2;     /*      Time in seconds for delay...            */
	unsigned int    delay;          /*      Delay in microseconds...                */

	int             time_to_sleep = TRUE;

 display = XtDisplay(w);

 delay = (unsigned int) (time * 1000000);
 usleep(delay);

 while(time_to_sleep)
	{
	decrement(w, date_widget, NULL);

	XNextEvent(display, &theEvent);
	if(theEvent.type == ButtonRelease ||
	   theEvent.type == KeyRelease) time_to_sleep = FALSE;

	XtDispatchEvent(&theEvent);
	XSync(display, 0);

	usleep(delay);
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/dateChanging.c,v $";
 static char rcs_id2[] = "$Id: dateChanging.c,v 1.1 1995/09/08 15:00:07 page Exp $";}
/*  ===================================================  */

}

