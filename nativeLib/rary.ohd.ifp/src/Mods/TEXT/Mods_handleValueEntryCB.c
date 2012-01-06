/****************************************************************/
/*                                                              */
/*	FILE:		Mods_handleValueEntryCB.c		*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/16/94                                */
/*                                                              */
/*      Modified by:    D. Page - 31 Oct. 1995                  */
/*                      now checks the whole text field         */
/*                                                              */
/*      NOTE:	Taken from event_funcs.c			*/
/*								*/
/*                                                              */
/****************************************************************/



#include <stdlib.h>
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"


extern int is_valid_number(char *);
extern int checkValue(float, Mods_everythingStruct *);


/****************************************************************/
/*								*/
/*	handleValueEntryCB()					*/
/*								*/
/****************************************************************/

void handleValueEntryCB(Widget w,  Mods_everythingStruct *data, XmAnyCallbackStruct *call_data)
{

	char		*string;
	int		i;
	int             decimal_conversion = 1;
	int		displayValue;
	short           decimal_points;  /* number of decimal points */
	double		value;
        Display         *display;

	display = XtDisplay(data->widgetData->ifp_modsShell);
	
	string = XmTextFieldGetString(w);
	
	/* Check to see if the string will be a valid number */
	if(is_valid_number(string) == FALSE)
	{
	   XBell(display, 100);
	   return;
	}
	
	value  = atof(string);             
	XtFree(string);
	
	XtVaGetValues(data->widgetData->modsScale, XmNdecimalPoints, &decimal_points, NULL);
	for(i = 1; i <= decimal_points; i++) decimal_conversion *= 10;
	displayValue = value*decimal_conversion;
	
	/* Check to see if the value is in the accepted limits */
	if(checkValue(value, data) == FALSE)
	{
	   value = displayValue;
	   XBell(display, 100);
	   return;
	}
			
	XtVaSetValues(data->widgetData->modsScale,
			XmNvalue,	displayValue,
			NULL);
			

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_handleValueEntryCB.c,v $";
 static char rcs_id2[] = "$Id: Mods_handleValueEntryCB.c,v 1.1 1995/11/14 12:19:18 page Exp $";}
/*  ===================================================  */

}

