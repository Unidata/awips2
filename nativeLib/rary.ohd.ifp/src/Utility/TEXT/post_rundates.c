/*
 * Function to post the run dates to X-window properties.
 * Function written by George Smith, February 1991 -- HRL.
 *
 * Dates enter function as julian hours since 1900.
 * Must convert to proper format before posting.
 */
#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"


Widget  global_toplevel;

post_run_dates(idarun, ihrrun,
	       ldacpd, lhrcpd,
	       ldarun, lhrrun,
	       first_time)

int     *idarun, *ihrrun, *ldacpd, *lhrcpd, *ldarun, *lhrrun;
int     *first_time;
{
date    *the_date;
int     month, day, year, hour, dummy1, dummy2;

if((*first_time)++ < 1)crwdgt();

the_date = (date *) malloc(sizeof(date));

/*  start date  */

mdyh2_(idarun, ihrrun, &month, &day, &year, &hour, &dummy1, &dummy2, "Z   ");

the_date->month = month;
the_date->day   = day;
the_date->year  = year;
the_date->hour  = hour;
strcpy(the_date->time_zone, "Z   ");

XChangeProperty
	(
	XtDisplay(global_toplevel),
	DefaultRootWindow(XtDisplay(global_toplevel)),
	IFPA_run_start_date,
	IFPA_run_start_date_type,
	8,
	PropModeReplace,
	(unsigned char *)the_date,
	sizeof(date)
	);

/*  end_obs date  */

mdyh2_(ldacpd, lhrcpd, &month, &day, &year, &hour, &dummy1, &dummy2, "Z   ");

the_date->month = month;
the_date->day   = day;
the_date->year  = year;
the_date->hour  = hour;
strcpy(the_date->time_zone, "Z   ");

XChangeProperty
	(
	XtDisplay(global_toplevel),
	DefaultRootWindow(XtDisplay(global_toplevel)),
	IFPA_run_end_obs_date,
	IFPA_run_end_obs_date_type,
	8,
	PropModeReplace,
	(unsigned char *)the_date,
	sizeof(date)
	);

/*  end date  */

mdyh2_(ldarun, lhrrun, &month, &day, &year, &hour, &dummy1, &dummy2, "Z   ");

the_date->month = month;
the_date->day   = day;
the_date->year  = year;
the_date->hour  = hour;
strcpy(the_date->time_zone, "Z   ");

XChangeProperty
	(
	XtDisplay(global_toplevel),
	DefaultRootWindow(XtDisplay(global_toplevel)),
	IFPA_run_end_date,
	IFPA_run_end_date_type,
	8,
	PropModeReplace,
	(unsigned char *)the_date,
	sizeof(date)
	);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/post_rundates.c,v $";
 static char rcs_id2[] = "$Id: post_rundates.c,v 1.3 2006/04/19 20:54:14 aivo Exp $";}
/*  ===================================================  */

}
