/****************************************************************/
/*                                                              */
/*	FILE:		ChangeTime.c				*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/08/94                                */
/*                                                              */
/*      Modified by:    D. Page                                 */
/*                      28 Oct. 95  - Changed name of routines  */
/*                      to avoid duplicate symbols in libXifp.a */
/*                      21 Nov. 95 - Removed duplicate routine  */
/*                      days_in_month (in sd_include)           */
/*                                                              */
/*      NOTE:		code modified from alter_dates.c	*/
/*                                                              */
/****************************************************************/


#include "ifp_inc/Date.h"

void change_the_month(int, date *);
void change_the_day(int, date *);
void change_the_year(int, date *);
void change_the_hour(int, date *);


extern int days_in_month(int, int);
/*static int check_date(date *);*/

void change_the_hour(int delta, date *the_date)
{
	the_date->hour = the_date->hour + delta;

	if(the_date->hour < 1) {
		change_the_day( -1, the_date);
		the_date->hour = the_date->hour + 24;
		}

	if(the_date->hour > 24) {
		change_the_day( +1, the_date);
		the_date->hour = the_date->hour - 24;
		}

}



void change_the_day(int delta, date *the_date)
{
	the_date->day = the_date->day + delta;

	if(the_date->day < 1) {
		change_the_month( -1, the_date);
		the_date->day = days_in_month(the_date->month, the_date->year);
		}

	if(the_date->day > days_in_month(the_date->month, the_date->year)) {
		the_date->day = the_date->day - days_in_month(the_date->month, the_date->year);
		change_the_month( +1, the_date);
		}
}



void change_the_month(int delta, date *the_date)
{	

	the_date->month = the_date->month + delta;

	if(the_date->month < 1) {
		change_the_year( -1, the_date);
		the_date->month = the_date->month + 12;
		}

	if(the_date->month > 12) {
		change_the_year( +1, the_date);
		the_date->month = the_date->month - 12;
		}

}



void change_the_year(int delta, date *the_date)
{

	the_date->year = the_date->year + delta;
	
}


static int check_date(date *the_date)
{
	if(the_date->day <= days_in_month(the_date->month, the_date->year))
		return(1);

	else    return(0);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/ChangeTime.c,v $";
 static char rcs_id2[] = "$Id: ChangeTime.c,v 1.3 2001/10/09 15:02:36 dws Exp $";}
/*  ===================================================  */

}
