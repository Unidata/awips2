/* File: sd_include.c

	These four functions (change_hour, change_day, change_month, and
	change_year) are used in conjunction with the set_dates.c program
	to increment or decrement the dates displayed in the start, end_obs,
	and end widgets.  As hours are incremented and reach 25, the day will
	be changed, and if necessary the month and year.  These four functions
	are used together to cycle through changes in the dates.
*/

#include "libXifp.h"

/*******************   change_hour   ************************************/

void    change_hour(delta, which_date, the_date, date_widget)

int             delta;  /* value to increment or decrement the date */
int             which_date; /* value used to determine which date widget is is affected */
date            *the_date;  /* current date displayed */
the_widgets     *date_widget;
{
	the_date->hour = the_date->hour + delta;

	if(the_date->hour < 1)
		{ change_day( -1, which_date, the_date, date_widget);
		  the_date->hour = the_date->hour + 24;
		}

	if(the_date->hour > 24)
		{ change_day( +1, which_date, the_date, date_widget);
		  the_date->hour = the_date->hour - 24;
		}

	switch(which_date)
	   {
		case 1:
		case 4:
		       change_widget(
				date_widget->start->time,
				the_date->hour,
				the_date, which_date*10);
		       break;

		case 2:
		case 5:
		       change_widget(
				date_widget->end->time,
				the_date->hour,
				the_date, which_date*10);
		       break;

		case 3:
		case 6:
		       change_widget(
				date_widget->end_obs->time,
				the_date->hour,
				the_date, which_date*10);
		       break;

		default:
		       printf(">>>>> problem in change_hour - ");
		       printf("which_date = %d\n",which_date);
	   }
}

/*******************   days_in_month   **********************************/

int     days_in_month(month, year)

int     month, year;
{
	static int days[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
	int     leap;

	leap = 0;

	if(month == 2 && year%4 == 0 && (year%100 != 0  || year%400 == 0)) 
		leap = 1;

	return (days[month-1] + leap);
}

/*******************   change_day   *************************************/

void    change_day(delta, which_date, the_date, date_widget)

int             delta, which_date;
date            *the_date;
the_widgets     *date_widget;
{
	the_date->day = the_date->day + delta;

	if(the_date->day < 1)
		{ change_month( -1, which_date, the_date, date_widget);
		  the_date->day = days_in_month(the_date->month, the_date->year);
		}

	if(the_date->day > days_in_month(the_date->month, the_date->year))
		{ the_date->day = the_date->day - days_in_month(the_date->month,
								the_date->year);
		  change_month( +1, which_date, the_date, date_widget);
		}

	switch(which_date)
	   {
		case 1:
		case 4:
		       change_widget(
				date_widget->start->day,
				the_date->day,
				the_date, which_date*10+2);
		       break;

		case 2:
		case 5:
		       change_widget(
				date_widget->end->day,
				the_date->day,
				the_date, which_date*10+2);
		       break;

		case 3:
		case 6:
		       change_widget(
				date_widget->end_obs->day,
				the_date->day,
				the_date, which_date*10+2);
		       break;

		default:
		       printf(">>>>> problem in change_day - ");
		       printf("which_date = %d\n",which_date);
	   }
}

/*******************   change_month   ***********************************/

void    change_month(delta, which_date, the_date, date_widget)

int             delta, which_date;
date            *the_date;
the_widgets     *date_widget;
{
	the_date->month = the_date->month + delta;

	if(the_date->month < 1)
		{ change_year( -1, which_date, the_date, date_widget);
		  the_date->month = the_date->month + 12;
		}

	if(the_date->month > 12)
		{ change_year( +1, which_date, the_date, date_widget);
		  the_date->month = the_date->month - 12;
		}

	switch(which_date)
	   {
		case 1:
		case 4:
		       change_widget(
				date_widget->start->month,
				the_date->month,
				the_date, which_date*10+1);
		       break;

		case 2:
		case 5:
		       change_widget(
				date_widget->end->month,
				the_date->month,
				the_date, which_date*10+1);
		       break;

		case 3:
		case 6:
		       change_widget(
				date_widget->end_obs->month,
				the_date->month,
				the_date, which_date*10+1);
		       break;

		default:
		       printf(">>>>> problem in change_month - ");
		       printf("which_date = %d\n",which_date);
	   }
}

/*******************   change_year   ************************************/

void    change_year(delta, which_date, the_date, date_widget)

int             delta, which_date;
date            *the_date;
the_widgets     *date_widget;
{
	the_date->year = the_date->year + delta;

	switch(which_date)
	   {
		case 1:
		case 4:
		       change_widget(
				date_widget->start->year,
				the_date->year,
				the_date, which_date*10+3);
		       break;

		case 2:
		case 5:
		       change_widget(
				date_widget->end->year,
				the_date->year,
				the_date, which_date*10+3);
		       break;

		case 3:
		case 6:
		       change_widget(
				date_widget->end_obs->year,
				the_date->year,
				the_date, which_date*10+3);
		       break;

		default:
		       printf(">>>>> problem in change_year - ");
		       printf("which_date = %d\n",which_date);
	   }
}

/*******************   check_date   ************************************/

int     check_date(the_date)

date    *the_date;
{
	if(the_date->day <= days_in_month(the_date->month, the_date->year))
		return(1);

	else    return(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/sd_include.c,v $";
 static char rcs_id2[] = "$Id: sd_include.c,v 1.2 2000/03/16 11:54:46 page Exp $";}
/*  ===================================================  */

}
