/****************************************************************/
/*                                                              */
/*	FILE:		HandleDatesCB.c				*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/01/94                                */
/*                                                              */
/*      NOTE:		code modified from date_funcs.c		*/
/*                                                              */
/****************************************************************/


#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include "DateHandling.h"



extern void updateMonthDisplay(int, date *, dateWidgetStruct_p);
extern void updateDayDisplay(int, date *, dateWidgetStruct_p);
extern void updateYearDisplay(int, date *, dateWidgetStruct_p);
extern void updateHourDisplay(int, date *, dateWidgetStruct_p);

static int get_time_step(Widget);




void incrementMonth(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct *call_data)
{

	updateMonthDisplay(1, data->dateStruct, data->dateWidgets);

}

	
void incrementDay(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct *call_data)
{

	updateDayDisplay(1, data->dateStruct, data->dateWidgets);

}

	
void incrementYear(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct *call_data)
{

	updateYearDisplay(1, data->dateStruct, data->dateWidgets);

}

	
void incrementTime(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct *call_data)
{

	int	delta_t;
	
	
	delta_t = get_time_step(w);
	updateHourDisplay(delta_t, data->dateStruct, data->dateWidgets);

}



void decrementMonth(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct **call_data)
{

	updateMonthDisplay(-1, data->dateStruct, data->dateWidgets);

}

void decrementDay(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct **call_data)
{

	updateDayDisplay(-1, data->dateStruct, data->dateWidgets);

}

void decrementYear(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct **call_data)
{

	updateYearDisplay(-1, data->dateStruct, data->dateWidgets);

}

void decrementTime(Widget w, dateFieldStruct_p data, XmAnyCallbackStruct **call_data)
{

	int	delta_t;
	
	
	delta_t = get_time_step(w);
	updateHourDisplay(-delta_t, data->dateStruct, data->dateWidgets);

}


static int get_time_step(Widget w)
{

	/* THIS IS TEMPORARY, we still need to get the simulation Delta T...	*/
	 return(6);
	 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/HandleDatesCB.c,v $";
 static char rcs_id2[] = "$Id: HandleDatesCB.c,v 1.1 1995/11/14 12:19:00 page Exp $";}
/*  ===================================================  */

}
