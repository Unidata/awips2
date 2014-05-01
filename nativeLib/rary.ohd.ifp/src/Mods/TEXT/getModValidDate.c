/* ************************************************************************************

	getModValidDate()
		obtains the Mod valid date from the current Mod display
		widget structure & fills a date structure.  

   ************************************************************************************ */
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

void getModValidDate(Mods_everythingStruct *data, date *modValidDate)
{

   int             intMonth;       /* Integer representation of each month */
   char            *month, *day, *year, *time, *tz_code;
   XmString        xmtz_code;
      

   /* Get the mod Valid Date from the Mods Valid Date widgets  
    * Put this date into a Date structure 
    */

   month = XmTextFieldGetString(data->widgetData->validMonthTextF);
   day   = XmTextFieldGetString(data->widgetData->validDayTextF);
   year  = XmTextFieldGetString(data->widgetData->validYearTextF);
   time  = XmTextFieldGetString(data->widgetData->validTimeTextF);
   
   XtVaGetValues(data->widgetData->validTZoneLabel,
                 XmNlabelString, &xmtz_code, NULL);

   XmStringGetLtoR(xmtz_code, XmSTRING_DEFAULT_CHARSET, &tz_code);
   
   if(strcmp("Jan", month) == 0)      intMonth = 1;
   else if(strcmp("Feb", month) == 0) intMonth = 2;
   else if(strcmp("Mar", month) == 0) intMonth = 3;
   else if(strcmp("Apr", month) == 0) intMonth = 4;
   else if(strcmp("May", month) == 0) intMonth = 5;
   else if(strcmp("Jun", month) == 0) intMonth = 6;
   else if(strcmp("Jul", month) == 0) intMonth = 7;
   else if(strcmp("Aug", month) == 0) intMonth = 8;
   else if(strcmp("Sep", month) == 0) intMonth = 9;
   else if(strcmp("Oct", month) == 0) intMonth = 10;
   else if(strcmp("Nov", month) == 0) intMonth = 11;
   else if(strcmp("Dec", month) == 0) intMonth = 12;

   modValidDate->month = intMonth;
   modValidDate->day   = atoi(day);
   modValidDate->year  = atoi(year);
   modValidDate->hour  = atoi(time);
   strcpy(modValidDate->time_zone, tz_code);
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getModValidDate.c,v $";
 static char rcs_id2[] = "$Id: getModValidDate.c,v 1.1 1995/11/17 17:17:03 page Exp $";}
/*  ===================================================  */

}
