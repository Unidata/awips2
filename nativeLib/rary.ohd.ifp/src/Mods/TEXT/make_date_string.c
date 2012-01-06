/* *****************************************************************************

	make_date_string()
		creates a string containing the date from the date structure,
		which is passed to the function; a call is made to 'change_the_hour'
		to update the incremented time.

   ***************************************************************************** */

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <stdlib.h>
#include "DateHandling.h"

char *make_date_string(date * nextDate)

{

	char            *string;    /* month character pointer */
	char            *dateString;

string = (char *) malloc(21);
dateString = (char *) malloc(40);
memset(string, '\0', 21);
memset(dateString, '\0', 40);

/*
printf("The date is: %d %d %d %d\n", nextDate->month, nextDate->day, nextDate->year,
				     nextDate->hour);
*/
switch(nextDate->month)
		{
		case    1:
			 strcpy(string, "Jan");
			 break;

		case    2:
			 strcpy(string, "Feb");
			 break;

		case    3:
			 strcpy(string, "Mar");
			 break;

		case    4:
			 strcpy(string, "Apr");
			 break;

		case    5:
			 strcpy(string, "May");
			 break;

		case    6:
			 strcpy(string, "Jun");
			 break;

		case    7:
			 strcpy(string, "Jul");
			 break;

		case    8:
			 strcpy(string, "Aug");
			 break;

		case    9:
			 strcpy(string, "Sep");
			 break;

		case    10:
			 strcpy(string, "Oct");
			 break;

		case    11:
			 strcpy(string, "Nov");
			 break;

		case    12:
			 strcpy(string, "Dec");
			 break;

		default:
			 break;
		}

strcpy(dateString, string);
strcat(dateString, " ");

memset(string, '\0', 21);
sprintf(string, "%d", nextDate->day);
if(nextDate->day < 10) strcat(dateString, " ");
strcat(dateString, string);
strcat(dateString, " ");

memset(string, '\0', 21);
sprintf(string, "%d", nextDate->year);
strcat(dateString, string);
strcat(dateString, " ");

memset(string, '\0', 21);
sprintf(string, "%d", nextDate->hour);
if(nextDate->hour < 10) strcat(dateString, " ");
strcat(dateString, string);
strcat(dateString, " ");
strcat(dateString, nextDate->time_zone);

/* printf("Inside 'make_date_string(), dateString = %s\n", dateString);         */

return(dateString);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/make_date_string.c,v $";
 static char rcs_id2[] = "$Id: make_date_string.c,v 1.1 1995/11/14 12:19:50 page Exp $";}
/*  ===================================================  */

}

