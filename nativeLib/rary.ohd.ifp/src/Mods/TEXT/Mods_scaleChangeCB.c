/****************************************************************/
/*                                                              */
/*	FILE:		Mods_scaleChangeCB.c			*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/15/94                                */
/*                                                              */
/*      NOTE:	Taken from mods_cbs.c				*/
/*								*/
/*                                                              */
/****************************************************************/



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"




extern int checkValue(float, Mods_everythingStruct *);


/****************************************************************/
/*								*/
/*	scaleChangeCB()						*/
/*								*/
/****************************************************************/

void scaleChangeCB(Widget w, Mods_everythingStruct *data, XmScaleCallbackStruct *call_data)
{

	Display         *display;
	float           displayValue;
	char		string[50];
	int             i;
	int             decimal_conversion = 1;
	short           decimal_points;  /* number of decimal points */



	display = XtDisplay(w);


	if(call_data->reason == XmCR_VALUE_CHANGED)
		{
		XtVaGetValues(w, XmNdecimalPoints, &decimal_points, NULL);

		for(i = 1; i <= decimal_points; i++) decimal_conversion *= 10;
		displayValue = (float) call_data->value/decimal_conversion;

		checkValue(displayValue, data);
		
	        if(data->modValueLimits->units != TIME_PERIODS)	
	           sprintf(string, "%f", displayValue);
	        else
	           sprintf(string, "%d", (int)displayValue);
		
		XmTextFieldSetString(data->widgetData->valueDisplayEntry, string);
		}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_scaleChangeCB.c,v $";
 static char rcs_id2[] = "$Id: Mods_scaleChangeCB.c,v 1.1 1995/11/14 12:19:30 page Exp $";}
/*  ===================================================  */

}

