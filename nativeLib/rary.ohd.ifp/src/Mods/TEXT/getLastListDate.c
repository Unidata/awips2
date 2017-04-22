/* ************************************************************************************

	getLastListDate()
		obtains the last date in the current list of date in the setQMeanDatesList
		which holds the date & fills a date structure.  

       Modified: 21 Nov. 95 - D. Page - pass in a pointer to a date structure and
                 associated changes.

   ************************************************************************************ */
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

void getLastListDate(Mods_everythingStruct *data, int num_items, date *lastDate)
{

   int             intMonth;       /* Integer representation of each month */
   XmStringTable   xm_list_items;
   char            *last_date;
   char            *month, *day, *year, *time, *tz_code;

   XtVaGetValues(data->setQMeanWidgets->setQMeanDatesList,
                 XmNitems, &xm_list_items, NULL);
   XmStringGetLtoR(xm_list_items[num_items-1], XmFONTLIST_DEFAULT_TAG,
                   &last_date);
                                 
   month   = strtok(last_date, " ");
   day     = strtok(NULL, " ");
   year    = strtok(NULL, " ");
   time    = strtok(NULL, " ");
   tz_code = strtok(NULL, " ");
   
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

   lastDate->month = intMonth;
   lastDate->day   = atoi(day);
   lastDate->year  = atoi(year);
   lastDate->hour  = atoi(time);
   memset(lastDate->time_zone, '\0', 5);
   strcpy(lastDate->time_zone, tz_code);
   
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getLastListDate.c,v $";
 static char rcs_id2[] = "$Id: getLastListDate.c,v 1.2 1995/11/21 13:33:07 page Exp $";}
/*  ===================================================  */

}
