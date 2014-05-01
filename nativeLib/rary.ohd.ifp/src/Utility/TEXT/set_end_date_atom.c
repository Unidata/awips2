#include "set_dates.h"
#include "ifp_atoms.h"
#include "c_call_f/julda.h"
#include "c_call_f/fcitzc.h"

Widget  global_toplevel;

set_end_date_atom(month, day, year, hour, tzc)

 int    *month, *day, *year, *hour;
 char   tzc[];
{
 int    type, format, nitems, left;
 long   offset = 0;
 date   *end_date, *obs_date;

 int    jobsday, jobshr, jendday, jendhr, jobshour, jendhour;
 int    dum1, dum2;

 obs_date = (date *) malloc(sizeof(date));
 end_date   = (date *) malloc(sizeof(date));

 end_date->month = *month;
 end_date->day = *day;
 end_date->year = *year;
 end_date->hour = *hour;
 strncpy(end_date->time_zone, tzc, 4);

 XChangeProperty
	 (
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_run_end_date,
	 IFPA_run_end_date_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)end_date,
	 sizeof(date)
	 );

 if(XGetWindowProperty(
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_run_end_obs_date,
	 offset,
	 (long) sizeof(date),
	 FALSE,
	 IFPA_run_end_obs_date_type,
	 (Atom *)&type,
	 (int *)&format,
	 (unsigned long *)&nitems,
	 (unsigned long *)&left,
	 (unsigned char **)&obs_date
	 ) == Success && type == IFPA_run_end_obs_date_type)
   {
    /* replace call to julda2 with FCITZC and JULDA for y2k */
    FCITZC(&dum1, &dum2, obs_date->time_zone);
    JULDA(&jobsday, &jobshr, &obs_date->month, &obs_date->day,
				 &obs_date->year, &obs_date->hour,
				 &dum1, &dum2, obs_date->time_zone);

    FCITZC(&dum1, &dum2, end_date->time_zone);
    JULDA(&jendday, &jendhr, &end_date->month, &end_date->day,
				 &end_date->year, &end_date->hour,
				 &dum1, &dum2, end_date->time_zone);

    jobshour = (jobsday-1)*24 + jobshr;
    jendhour = (jendday-1)*24 + jendhr;

    if(jobshour > jendhour)
	XChangeProperty
		(
		XtDisplay(global_toplevel),
		DefaultRootWindow(XtDisplay(global_toplevel)),
		IFPA_run_end_obs_date,
		IFPA_run_end_obs_date_type,
		8,
		PropModeReplace,
		(unsigned char *)end_date,
		sizeof(date)
		);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/set_end_date_atom.c,v $";
 static char rcs_id2[] = "$Id: set_end_date_atom.c,v 1.5 2006/04/19 20:54:18 aivo Exp $";}
/*  ===================================================  */

}
