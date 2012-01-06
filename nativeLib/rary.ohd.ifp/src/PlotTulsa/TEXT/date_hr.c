#include <stdlib.h>
#include "c_call_f/mdyh2.h" /*--Added by AV --*/
#include "c_call_f/set_fctime_cb.h"

/* File: date_hr.c
 *
 * Saves starting month, day, hour, and time zone and creates
 * the axis label.
 *
 */

void date_hr(myt, dt, day_hr, data_len, for_plot,
	     end_dis, locp, p_float, start_run, 
	     start_month, start_day, start_hour, time_zone_code)

	int     dt;                  /* time interval          */
	int     *data_len;           /* length of data         */
	int     *end_dis;
	int     *locp;               /* pointer to the location of the beginning of
					parameter array. */
	int     *start_run;
	char    *myt;                /* month, year, time zone pointer */
	char    *day_hr[];
	char    *time_zone_code;
	float   p_float[];           /* parameter floating point data */
	int     *start_month;
	int     *start_day;
	int     *start_hour;
	int     for_plot;            /* flag for if labels are for the plot or table */
	   {
	   int          julda;       /* Julian day    */
	   int          julhr;       /* Julian hour   */
	   int          month;       /* Month         */
	   int          day;         /* Day           */
	   int          year;        /* year          */
	   int          hour;        /* hour          */
	   int          zondum;      /* time zone value */
	   int          dlsdum;      /* Daylight savings time flag, 1 for
					daylight savings time, 0 for standard time */
	   int          index;
	   int          i;             /* counter    */
	   char         yr_text[5];    /* year string representation */
	   char         hour_text[4];  /* hour string representation */

	   julda = *start_run/24 + 1;   /* julian days since 1/1/1990     */
	   julhr = *start_run%24;       /* hours in hydrologic day (0-23) */

	   SET_FCTIME_CB();

	   MDYH2(&julda, &julhr, &month, &day, &year, &hour,
			    &zondum, &dlsdum, time_zone_code);

	   sprintf(myt, "%d", month);
	   strcat(myt, "/");
	   sprintf(yr_text, "%d", year);
	   strcat(myt, yr_text);
	   strcat(myt, " ");
	   strcat(myt, time_zone_code);

	   /* Save starting month, day, hour, and tz */
	   *start_month = month;
	   *start_day   = day;
	   *start_hour  = hour;

	   /* Create axis labels. */
	   /*index = *start_dis;*/
	   index = *start_run;
	   for(i = 0; i < *data_len; i++)
	     {
	     /*printf("%d  julda = %d   julhr = %d\n", i, julda, julhr);*/
	     MDYH2(&julda, &julhr, &month, &day, &year, &hour, &zondum,
		   &dlsdum, time_zone_code);
	     /*printf("     day = %d\n", day);*/
	     day_hr[i] = (char*)malloc(8);
	     sprintf(day_hr[i], "%d", day);
	     if(for_plot)
	        strcat(day_hr[i], ".");
	     else
	        strcat(day_hr[i], "  ");
	     sprintf(hour_text, "%02d", hour);
	     strcat(day_hr[i], hour_text);

	     index = index + dt;
	     julda = index/24 + 1.01;
	     julhr = index%24 + 0.01;
	     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/date_hr.c,v $";
 static char rcs_id2[] = "$Id: date_hr.c,v 1.3 2002/02/11 19:26:32 dws Exp $";}
/*  ===================================================  */

     }
/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */
