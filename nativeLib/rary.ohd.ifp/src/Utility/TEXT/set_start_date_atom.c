#include "set_dates.h"
#include "ifp_atoms.h"
#include "c_call_f/mdyh2.h"

Widget  global_toplevel;

set_start_date_atom(julda, inthr)

 int    *julda, *inthr;
{
 int    type, format, nitems, left;
 long   offset = 0;
 date   *start_date;
 int    dum1, dum2;

 start_date   = (date *) malloc(sizeof(date));

 if(XGetWindowProperty(
	 XtDisplay(global_toplevel),
	 DefaultRootWindow(XtDisplay(global_toplevel)),
	 IFPA_run_start_date,
	 offset,
	 (long) sizeof(date),
	 FALSE,
	 IFPA_run_start_date_type,
	 (Atom *)&type,
	 (int *)&format,
	 (unsigned long *)&nitems,
	 (unsigned long *)&left,
	 (unsigned char **)&start_date
	 ) == Success && type == IFPA_run_start_date_type)
   {
    MDYH2(julda, inthr, &start_date->month, &start_date->day,
			    &start_date->year, &start_date->hour,
			    &dum1, &dum2, start_date->time_zone);

    XChangeProperty
	    (
	    XtDisplay(global_toplevel),
	    DefaultRootWindow(XtDisplay(global_toplevel)),
	    IFPA_run_start_date,
	    IFPA_run_start_date_type,
	    8,
	    PropModeReplace,
	    (unsigned char *)start_date,
	    sizeof(date)
	    );
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/set_start_date_atom.c,v $";
 static char rcs_id2[] = "$Id: set_start_date_atom.c,v 1.3 2006/04/19 20:54:03 aivo Exp $";}
/*  ===================================================  */

}
