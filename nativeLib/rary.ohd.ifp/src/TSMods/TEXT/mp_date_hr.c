/*Added underscore to MDYH2 and SET_FCTIME_CB ---kwz*/
/* File: mp_date_hr.c
 *
 * Saves starting month, day, hour, and time zone and creates
 * the axis label.
 *
 */

#include "mods_plot.h"
#include "c_call_f/mdyh2.h"
#include "c_call_f/set_fctime_cb.h"

void mp_date_hr(myt, dt, day_hr, num_pts, jul_day_st, jul_hr_st,
		time_zone_code, mod_type_sw)
   int     dt;                  /* time interval        */
   int     num_pts;             /* Number of time periods plotted       */
   int     jul_day_st;          /* Julian start day     */
   int     jul_hr_st;           /* Julian start hour    */
   int     mod_type_sw;         /* Switch giving the type of mod being plotted */
   char    *myt;                /* month, year, time zone pointer       */
   char    **day_hr;
   char    *time_zone_code;


{
   int  julda;          /* Julian day    */
   int  julhr;          /* Julian hour   */
   int  month;          /* Month         */
   int  day;            /* Day           */
   int  year;           /* Year          */
   int  hour;           /* Hour          */
   int  zondum;         /* time zone value */
   int  dlsdum;         /* Daylight savings time flag,
			   1 for  daylight savings time,
			   0 for standard time           */
   int  index;
   int  i;              /* counter    */
   char yr_text[5];     /* year string representation */
   char hour_text[4];   /* hour string representation */

   julda = jul_day_st;
   julhr = jul_hr_st;
   index = jul_day_st * 24;

   SET_FCTIME_CB();

   MDYH2(&julda, &julhr, &month, &day, &year, &hour,
	 &zondum, &dlsdum, time_zone_code);

   memset(myt, '\0', 13);
   if(mod_type_sw == UH || mod_type_sw == UHD) strncpy(myt, " ", 1);

   if(mod_type_sw != UH && mod_type_sw != UHD)
   {
      sprintf(myt, "%d", month);
      strcat(myt, "/");
      sprintf(yr_text, "%d", year);
      strcat(myt, yr_text);
      strcat(myt, " ");
      strcat(myt, time_zone_code);
   }

   /* Create axis labels. */
   for(i=0; i<num_pts; i++)
   {
      if(mod_type_sw != UH && mod_type_sw != UHD)
      {
	 /*printf("%d  julda = %d   julhr = %d\n", i, julda, julhr);*/

	 MDYH2(&julda, &julhr, &month, &day, &year, &hour, &zondum,
	       &dlsdum, time_zone_code);

	 /*printf("     day = %d\n", day);*/

	 sprintf(day_hr[i], "%d", day);
	 strcat(day_hr[i], ".");
	 sprintf(hour_text, "%02d", hour);
	 strcat(day_hr[i], hour_text);

	 /*printf("day_hr[%d] = %s\n", i, day_hr[i]);*/

	 index = index + dt;
	 julda = index/24 + 1.01;
	 julhr = index%24 + 0.01;
      }
      else /* mod_type_sw == UH */
	 sprintf(day_hr[i], "%d", dt*(i));
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_date_hr.c,v $";
 static char rcs_id2[] = "$Id: mp_date_hr.c,v 1.3 2004/08/05 17:53:02 wkwock Exp $";}
/*  ===================================================  */

}
/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */
