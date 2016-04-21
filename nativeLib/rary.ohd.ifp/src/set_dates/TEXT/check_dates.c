
/* ****************************************************************************************************

	check_dates.c

	Coded by:       Tom Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/Hydrologic Research Lab.

	Initially coded:        06/04/91
	Last modified:          06/04/91


   **************************************************************************************************** */




#include "set_dates.h"
#include "c_call_f/fcitzc.h" /*-- added by AV -- */
#include "c_call_f/julda.h"








/* *******************************************************************************************

	date_compare()
		compares two date structures to see if they are the same or not;
		returns the difference between the two dates, in hours:

				-(diff) if date1 < date2,
				0       if date1 == date2, and
				+(diff) if date1 > date2.

		Note:   the dates must be in the same time zone...

   ******************************************************************************************* */


int date_compare(date1, date2)
	date    *date1;
	date    *date2;
{

	int     hour, day, month, year;
	int     first;  /* first day */
	int     second; /* second day */
	int     diff;   /* difference between the first and second days */
	int     zondum; /* Time zone number based on GMT */
	int     dlsdum; /* Daylight savings time flag, 1 for daylight savings time
			   0 for standard time. */
	int     julian_day, julian_hour;
	char    *time_zone_code = "INTL";



year = date1->year;
month = date1->month;
day = date1->day;
hour = date1->hour;

/* replace call to julda2 with fcitzc and julda for y2k */
FCITZC(&zondum, &dlsdum, time_zone_code);
JULDA(&julian_day, &julian_hour,
	&month,
	&day,
	&year,
	&hour,
	&zondum, &dlsdum, time_zone_code);

first = 24*(julian_day - 1) + julian_hour;

year = date2->year;
month = date2->month;
day = date2->day;
hour = date2->hour;

/* replace call to julda2 with fcitzc and julda for y2k */
FCITZC(&zondum, &dlsdum, time_zone_code);
JULDA(&julian_day, &julian_hour,
	&month,
	&day,
	&year,
	&hour,
	&zondum, &dlsdum, time_zone_code);

second = 24*(julian_day - 1) + julian_hour;

diff = first - second;

return(diff);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/set_dates/RCS/check_dates.c,v $";
 static char rcs_id2[] = "$Id: check_dates.c,v 1.3 2002/02/11 19:51:40 dws Exp $";}
/*  ===================================================  */

}



