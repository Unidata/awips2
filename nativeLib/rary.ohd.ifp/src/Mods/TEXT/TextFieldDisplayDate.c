/****************************************************************/
/*                                                              */
/*	FILE:		TextFieldDisplayDate.c			*/
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



#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"





/****************************************************************/
/*                                                              */
/*	void TextFieldDisplayDate()				*/
/*                                                              */
/*	RETURNS:	NONE					*/
/*                                                              */
/****************************************************************/

void TextFieldDisplayDate(date *dateStruct, char *monthString, 
			  Widget month, Widget day, Widget year, Widget time, Widget tzCode)
{

	char            string[5];


	XmTextFieldSetString(month, monthString);	/* MONTH...	*/

 	sprintf(string, "%d", dateStruct->day);		/* DAY...	*/
 	XmTextFieldSetString(day, string);


	sprintf(string, "%d", dateStruct->year);	/* YEAR...	*/
 	XmTextFieldSetString(year, string);

	sprintf(string, "%d", dateStruct->hour);	/* TIME...	*/
 	XmTextFieldSetString(time, string);
 	
							/* TIME ZONE...	*/
	XtVaSetValues(tzCode, XtVaTypedArg,
			XmNlabelString, XmRString, dateStruct->time_zone,
			strlen(dateStruct->time_zone)+1, NULL);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/TextFieldDisplayDate.c,v $";
 static char rcs_id2[] = "$Id: TextFieldDisplayDate.c,v 1.1 1995/11/14 12:19:38 page Exp $";}
/*  ===================================================  */

}

