/************************************************************************/
/*                                                              	*/
/*	FILE:		MakeDates.c					*/
/*							        	*/
/*	Coded by:	Tom Adams					*/
/*			NWS * Office of Hydrology * HRL			*/
/*	Date:		11/07/94					*/
/*									*/
/*      NOTE:		code modified from alter_date.c			*/
/*									*/
/************************************************************************/


#include <stdlib.h>
#include <Xm/Xm.h>
#include "ifp_inc/Date.h"
#include "c_call_f/fcitzc.h" /*-- added by AV-- */
#include "c_call_f/julda.h"



extern char *ConvertMonthToString(int);




/************************************************************************/
/*									*/
/*	makeDateString()						*/
/*		creates a string containing the date from the date	*/
/*		structure						*/
/*									*/
/************************************************************************/
char *makeDateString(date *nextDate)
{

	char	*string;
	char	*dateString;
	char	temp[6];


	dateString = (char *)XtMalloc(sizeof(char)*21);


	string = ConvertMonthToString(nextDate->month);

	strcpy(dateString, string);
	strcat(dateString, " ");

	memset(temp, '\0', 6);
	sprintf(temp, "%d", nextDate->day);
	if(nextDate->day < 10) strcat(dateString, " ");
	strcat(dateString, temp);
	strcat(dateString, " ");

	memset(temp, '\0', 6);
	sprintf(temp, "%d", nextDate->year);
	strcat(dateString, temp);
	strcat(dateString, " ");

	memset(temp, '\0', 6);
	sprintf(temp, "%d", nextDate->hour);
	if(nextDate->hour < 10) strcat(dateString, " ");
	strcat(dateString, temp);
	strcat(dateString, " ");
	strcat(dateString, nextDate->time_zone);


	/* printf("Inside 'makeDateString(), dateString = %s\n", dateString); */

	return(dateString);
}


/****************************************************************/
/*								*/
/*	int getNumTimeSteps()					*/
/*		counts & returns the number of time steps	*/
/*		between two dates, based on delta-T		*/
/*								*/
/****************************************************************/


int getNumTimeSteps(date *startDate, date *endDate, int deltaT, char *time_zone_code)
{

	int     start_hour, start_day, start_month, start_year;
	int     end_hour, end_day, end_month, end_year;
	int     start;		/* Start time					*/
	int     end;		/* End time					*/
	int     timeSteps;
	int     zondum;		/* Time zone value				*/
	int     dlsdum;		/* Daylight savings time flag, 1 for daylight	*/
				/* savings time, 0 for standard time		*/
	int     julian_day_start, julian_hour_start;
	int     julian_day_end, julian_hour_end;



	start_year	= startDate->year;
	end_year	= endDate->year;
	start_month	= startDate->month;
	end_month	= endDate->month;
	start_day	= startDate->day;
	end_day		= endDate->day;
	start_hour	= startDate->hour;
	end_hour	= endDate->hour;


        /* replace call to julda2 with fcitzc and julda for y2k */
        FCITZC(&zondum, &dlsdum, time_zone_code);
        JULDA(&julian_day_start, &julian_hour_start,
		&start_month,
		&start_day,
		&start_year,
		&start_hour,
		&zondum, &dlsdum, time_zone_code);

	start = 24*(julian_day_start - 1) + julian_hour_start;

        /* replace call to julda2 with fcitzc and julda for y2k */
        FCITZC(&zondum, &dlsdum, time_zone_code);
	JULDA(&julian_day_end, &julian_hour_end,
		&end_month,
		&end_day,
		&end_year,
		&end_hour,
		&zondum, &dlsdum, time_zone_code);

	end = 24*(julian_day_end - 1) + julian_hour_end;

	timeSteps = (end - start)/deltaT;

	return(timeSteps);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/MakeDates.c,v $";
 static char rcs_id2[] = "$Id: MakeDates.c,v 1.3 2002/02/11 19:24:50 dws Exp $";}
/*  ===================================================  */

}
