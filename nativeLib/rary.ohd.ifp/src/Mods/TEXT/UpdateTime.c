/****************************************************************/
/*                                                              */
/*	FILE:		UpdateTime.c				*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/02/94                                */
/*                                                              */
/*      NOTE:		code modified from sd_includes.c	*/
/*                                                              */
/****************************************************************/

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include "DateHandling.h"

void updateMonthDisplay(int, date *, dateWidgetStruct_p);
void updateDayDisplay(int, date *, dateWidgetStruct_p);
void updateYearDisplay(int, date *, dateWidgetStruct_p);
void updateHourDisplay(int, date *, dateWidgetStruct_p);

extern void updateTextFIntDisplay(Widget, int);
extern void updateTextFCharDisplay(Widget, char *);
extern char *ConvertMonthToString(int);


static int days_in_month(int, int);
static int check_date(date *);




void updateHourDisplay(int delta, date *the_date, dateWidgetStruct_p dateWidget)
{
	the_date->hour = the_date->hour + delta;

	if(the_date->hour < 1) {
		updateDayDisplay( -1, the_date, dateWidget);
		the_date->hour = the_date->hour + 24;
		}

	if(the_date->hour > 24) {
		updateDayDisplay( +1, the_date, dateWidget);
		the_date->hour = the_date->hour - 24;
		}

	updateTextFIntDisplay(dateWidget->time, the_date->hour);

}



void updateDayDisplay(int delta, date *the_date, dateWidgetStruct_p dateWidget)
{

	the_date->day = the_date->day + delta;

	if(the_date->day < 1) {
		updateMonthDisplay( -1, the_date, dateWidget);
		the_date->day = days_in_month(the_date->month, the_date->year);
		}

	if(the_date->day > days_in_month(the_date->month, the_date->year)) {
		the_date->day = the_date->day - days_in_month(the_date->month, the_date->year);
		updateMonthDisplay( +1, the_date, dateWidget);
		}
		
	updateTextFIntDisplay(dateWidget->day, the_date->day);

}



void updateMonthDisplay(int delta, date *the_date, dateWidgetStruct_p dateWidget)
{

	char	*string;
	

	the_date->month = the_date->month + delta;

	if(the_date->month < 1) {
		updateYearDisplay( -1, the_date, dateWidget);
		the_date->month = the_date->month + 12;
		}

	if(the_date->month > 12) {
		updateYearDisplay( +1, the_date, dateWidget);
		the_date->month = the_date->month - 12;
		}
		
	string = ConvertMonthToString(the_date->month);
	updateTextFCharDisplay(dateWidget->month, string);

}



void updateYearDisplay(int delta, date *the_date, dateWidgetStruct_p dateWidget)
{

	the_date->year = the_date->year + delta;

	updateTextFIntDisplay(dateWidget->year, the_date->year);
	
}

static int check_date(date *the_date)
{
	if(the_date->day <= days_in_month(the_date->month, the_date->year))
		return(1);

	else    return(0);
}



static int days_in_month(int month, int year)
{
	static int days[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
	int     leap;

	leap = 0;

	if(month == 2 && year%4 == 0 && (year%100 != 0  || year%400 == 0)) leap = 1;
	return (days[month-1] + leap);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/UpdateTime.c,v $";
 static char rcs_id2[] = "$Id: UpdateTime.c,v 1.2 2000/03/16 12:13:36 page Exp $";}
/*  ===================================================  */

}

