/* ************************************************************************************

	getModStartDate()
		obtains the Mod start date from the current Mod display
		widget structure & fills a date structure.  

   ************************************************************************************ */
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

void getModStartDate(Mods_everythingStruct *data, date *modStartDate)
{

   int             intMonth;       /* Integer representation of each month */
   char            *month, *day, *year, *time, *tz_code;
   XmString        xmtz_code;

   /* Get the mod Start Date from the Mods Start Date widgets  
    * Put this date into a Date structure 
    */

   month = XmTextFieldGetString(data->widgetData->startMonthTextF);
   day   = XmTextFieldGetString(data->widgetData->startDayTextF);
   year  = XmTextFieldGetString(data->widgetData->startYearTextF);
   time  = XmTextFieldGetString(data->widgetData->startTimeTextF);
   
   XtVaGetValues(data->widgetData->startTZoneLabel,
                 XmNlabelString, &xmtz_code, NULL);

   XmStringGetLtoR(xmtz_code, XmSTRING_DEFAULT_CHARSET, &tz_code);
   
/*kwz.UHGCDATE.change all strcmp to strcasecmp.*/
   if(strcmp("Jan", month) == 0)      intMonth = 1;
   else if(strcasecmp("Feb", month) == 0) intMonth = 2;
   else if(strcasecmp("Mar", month) == 0) intMonth = 3;
   else if(strcasecmp("Apr", month) == 0) intMonth = 4;
   else if(strcasecmp("May", month) == 0) intMonth = 5;
   else if(strcasecmp("Jun", month) == 0) intMonth = 6;
   else if(strcasecmp("Jul", month) == 0) intMonth = 7;
   else if(strcasecmp("Aug", month) == 0) intMonth = 8;
   else if(strcasecmp("Sep", month) == 0) intMonth = 9;
   else if(strcasecmp("Oct", month) == 0) intMonth = 10;
   else if(strcasecmp("Nov", month) == 0) intMonth = 11;
   else if(strcasecmp("Dec", month) == 0) intMonth = 12;
   else intMonth = -1; /*kwz.UHGCDATE.default to -1 so it won't get a junk #.*/

   modStartDate->month = intMonth;
   modStartDate->day   = atoi(day);
   modStartDate->year  = atoi(year);
   modStartDate->hour  = atoi(time);
   strcpy(modStartDate->time_zone, tz_code);
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getModStartDate.c,v $";
 static char rcs_id2[] = "$Id: getModStartDate.c,v 1.3 2004/08/05 17:47:55 wkwock Exp $";}
/*  ===================================================  */

}
