
/* ************************************************************************************

	ifp_dates.h


		Coded by:       Tom Adams
		Affiliation:    NWS/Office of Hydrology/Hydrologic Research Laboratory
		Coded:          07/16/91


   ************************************************************************************ */

#ifndef ifp_dates_h
#define ifp_dates_h

#include <stdio.h>
#include <time.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>

#include "libXifp.h"
#include "ifp_atoms.h"



#define YES     1
#define NO      0



typedef struct
	{
	int     time_change;
	int     time;
	char    time_zone[5];
	}       date_data;


void            get_system_time();
void            change_the_day();
void            set_run_dates();
void            extract_date_changes();
int             date_compare();


date            *start_date;

int             julian_hours;

#endif
