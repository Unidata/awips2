/* Added underscore for all fcitzc and julda function calls fro fortran
 * compatibility---kwz
 *
 * File: getrdates.c
 *
 * Obtains the number of days in the month with the function
 * days_in_month(month, year).
 *
 * Gets start_run, end_obs_run, and end_run dates from X window properties.
 *
 * UPDATE:  routine name changed from getrundates to getrdates to resolve
 *          conflict with another routine with the same name - dp - June 98
 */


#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"
#include "c_call_f/julda.h"/* -- added by AV --*/
#include "c_call_f/fcitzc.h"

Widget           global_toplevel;

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

static int days_in_month(month, year)

int     month, year;
{
static  int days[12] = {31, 28, 31, 30, 31, 30,
			31, 31, 30, 31, 30, 31};
int     answer = 0;   /* number of days in the month specified */

if(month > 0 && month < 13)
  {
   answer = days[month - 1];

   if((month == 2) &&
      ((year %4 ==0 && year % 100 != 0) || year % 400 == 0)
     ) answer++;
  }
return(answer);
}

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

void getrdates(istrhr, ilcdhr, iendhr, isdahr, first_time)

int  *istrhr;     /* start date of run pointer */
int  *ilcdhr;     /* end date of observations pointer */
int  *iendhr;     /* end date of run pointer */
int  *isdahr;     /* hour of first data for any time series pointer */
int  *first_time; /* start time of run pointer */

{
struct  tm      *time_pointer;
time_t          tp;  /* time pointer address */

date            *start_date; /* start of run */
date            *end_date;   /* end of run */
date            *end_obs_date; /* end of observations */

Display         *display;
Window          root;

int             type;    /* type of data stored in the window property */
int             format;  /* format of the stored data */
int             nitems;  /* number of bytes retrieved */
int             left;    /* remaining bytes stored in the window */
int             julday, julhour, dummy1, dummy2;

long            offset = 0;

if((*first_time)++ < 1)crwdgt();

display = XtDisplay(global_toplevel);
root = DefaultRootWindow(display);
/*
 *  Get start_run, end_obs_run, and end_run dates from X window properties.
 *  If any of the date properties are not on window, use current date for
 *    end of obs, -3 days for start of run, and +5 days for end of run.
 */
if(
   (XGetWindowProperty
    (
    display,
    root,
    IFPA_run_start_date,
    offset,
    (long) sizeof(date),
    FALSE,
    (Atom)IFPA_run_start_date_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&start_date
    ) == Success && type == IFPA_run_start_date_type
   )      /*  end run_start_date check  */
  &&
   (XGetWindowProperty
    (
    display,
    root,
    IFPA_run_end_date,
    offset,
    (long) sizeof(date),
    FALSE,
    (Atom)IFPA_run_end_date_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&end_date
    ) == Success && type == IFPA_run_end_date_type
   )      /*  end run_end_date check  */
  &&
   (XGetWindowProperty
    (
    display,
    root,
    IFPA_run_end_obs_date,
    offset,
    (long) sizeof(date),
    FALSE,
    (Atom)IFPA_run_end_obs_date_type,
    (Atom *)&type,
    (int *)&format,
    (unsigned long *)&nitems,
    (unsigned long *)&left,
    (unsigned char **)&end_obs_date
    ) == Success && type == IFPA_run_end_obs_date_type
   )      /*  end run_end_obs_date check  */
  )       /*  end logical expression of if statement  */
    {
     /*
      * If get here, all three of start, end, and end_obs dates
      *  have been defined as window properties.
      * Use dates from window properties to set dates for the run.
      * Pass the month, day, year, hour, and time zone to julda2,
      *  and get the Julian day and hour back.
      * From the Julian day and hour compute the Julian hour
      *  of the start, end, and end_obs date.
      * Set the start of data date to the start_run date.
      */
    }  /*  end of if (true) processing  */
else
    {
     /*
      * Get here if at least one of the run date window properties is
      *  missing.  Now must get current date from system clock and
      *  set start, end, and end_obs dates accordingly.
      */
     time(&tp);
     time_pointer = localtime(&tp);

     end_obs_date = (date *) malloc(sizeof(date));

     end_obs_date->month = time_pointer->tm_mon + 1;
     end_obs_date->day = time_pointer->tm_mday;
     end_obs_date->year = 1900 + time_pointer->tm_year;
     end_obs_date->hour = time_pointer->tm_hour;
     strcpy(end_obs_date->time_zone, "Z   ");  /*  assume GMT  */
					  /*  this isn't right, but its the
					   *  best we can do for now.
					   */

     start_date = (date *) malloc(sizeof(date));

     start_date->month = end_obs_date->month;
     start_date->day = end_obs_date->day - 3;
     start_date->year = end_obs_date->year;
     start_date->hour = end_obs_date->hour;
     strcpy(start_date->time_zone, end_obs_date->time_zone);
     /*
      *  Now check to be sure we haven't wrapped around the beginning
      *   of a month.
      */
     if(start_date->day < 1)
       {                                        /* have wrapped month */
	if(start_date->month-- < 1)
	  {                                     /* have wrapped year  */
	   start_date->year--;
	   start_date->month = 12;
	  }
	start_date->day = days_in_month(start_date->month, start_date->year) +
			  start_date->day;
       }

     end_date = (date *) malloc(sizeof(date));

     end_date->month = end_obs_date->month;
     end_date->day = end_obs_date->day + 5;
     end_date->year = end_obs_date->year;
     end_date->hour = end_obs_date->hour;
     strcpy(end_date->time_zone, end_obs_date->time_zone);
     /*
      *  Now check to be sure we haven't wrapped around the end
      *   of a month.
      */
     if(end_date->day > days_in_month(end_date->month, end_date->year))
       {                                        /* have wrapped month */
	end_date->day = end_date->day -
			days_in_month(end_date->month, end_date->year);
	if(end_date->month++ > 12)
	  {                                     /* have wrapped year  */
	   end_date->year++;
	   end_date->month = 1;
	  }
       }
     /*
      *  Now that we have gotten the dates from the system clock,
      *   post window properties for each date.
      */
     XChangeProperty(
		     display,
		     root,
		     IFPA_run_start_date,
		     IFPA_run_start_date_type,
		     8,
		     PropModeReplace,
		     (unsigned char *)start_date,
		     sizeof(date)
		    );

     XChangeProperty(
		     display,
		     root,
		     IFPA_run_end_date,
		     IFPA_run_end_date_type,
		     8,
		     PropModeReplace,
		     (unsigned char *)end_date,
		     sizeof(date)
		    );

     XChangeProperty(
		     display,
		     root,
		     IFPA_run_end_obs_date,
		     IFPA_run_end_obs_date_type,
		     8,
		     PropModeReplace,
		     (unsigned char *)end_obs_date,
		     sizeof(date)
		    );

    }           /*  end else  */
/*
 *  No matter what, dates are now posted as window properties
 *   and are set in the start_date, end_date, and end_obs_date
 *   structures.
 *  Use these dates to compute the Julian hours to return.
 */

/* replace calls to julda2 with fcitzc and julda for y2k */
FCITZC(&dummy1, &dummy2, start_date->time_zone);
JULDA(&julday, &julhour, &start_date->month,
			   &start_date->day,
			   &start_date->year,
			   &start_date->hour,
			   &dummy1,
			   &dummy2,
			   start_date->time_zone);

*istrhr = (julday - 1)*24 + julhour;
*isdahr = *istrhr;

FCITZC(&dummy1, &dummy2, end_date->time_zone);
JULDA(&julday, &julhour, &end_date->month,
			   &end_date->day,
			   &end_date->year,
			   &end_date->hour,
			   &dummy1,
			   &dummy2,
			   end_date->time_zone);

*iendhr = (julday - 1)*24 + julhour;

FCITZC(&dummy1, &dummy2, end_obs_date->time_zone);
JULDA(&julday, &julhour, &end_obs_date->month,
			   &end_obs_date->day,
			   &end_obs_date->year,
			   &end_obs_date->hour,
			   &dummy1,
			   &dummy2,
			   end_obs_date->time_zone);

*ilcdhr = (julday - 1)*24 + julhour;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/getrdates.c,v $";
 static char rcs_id2[] = "$Id: getrdates.c,v 1.4 2006/04/07 14:10:42 aivo Exp $";}
/*  ===================================================  */

}
