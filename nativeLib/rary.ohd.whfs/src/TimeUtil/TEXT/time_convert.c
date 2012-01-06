#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "GeneralUtil.h"
#include "time_convert.h"

int numDaysInMonth[] = {31,28,31,30,31,30,31,31,30,31,30,31};

  

/*static codetimer timer;*/

/*********************************************************************/

/*codetimer * get_timer()
{
    static int first = 0;
    
    if (first == 0)
    {
        init_timer(&timer);
	first = 1;
    }

    return & timer;
}*/

/*********************************************************************/

static bool
_check(unsigned char *ptr, int length)
{

    /*
        This function was copied from PostgreSQL source code.
        It checks if the series of bytes are all equal to ff, with length
        indicating the number of bytes to check.
    */

	for (; ptr[--length] == 0xff && length >= 0; length--);
	if (length < 0)
		return true;
	return false;
}


static bool IsNullDateTime ( dtime_t dt )
{
     return _check((unsigned char *)&dt, sizeof(dtime_t));
}

/*********************************************************************/


int isTimeError(int value)
{
	return (value < 0 );     
}


/*********************************************************************/


int old_timet_to_yearsec_dt(time_t timet, dtime_t *dt)
{
   
   /*

   Convert from the C language time_t representation to the
   Informix dtime_t representation.
   
   The ANSI time string standard is YYYY-MM-DD HH:MM:SS (19 chars)
   
   */
   
   
   char 	ansi[ANSI_TIME_LEN];
   int		rv = 0,
      		timet_rv,
		ansi_rv;
 
   
   timet_rv = timet_to_yearsec_ansi(timet, ansi);
   if (timet_rv < 0)
   {
        rv = timet_rv;
   }
   
   else
   {
   	ansi_rv = yearsec_ansi_to_dt(ansi, dt);
	if (ansi_rv < 0)
	{
	   rv = ansi_rv;
	}
   }
   
   return(rv);
}

/*********************************************************************/


int timet_to_yearsec_dt(time_t timet, dtime_t *dt)
{

   /*
    	 the epoch for PGSQL is 2000-01-01 00:00:00
	 the epoch for time_t is 1970-01-01 00:00:00
   */

   
   *dt = timet - PGSQL_EPOCH_IN_TIMET;
 
    return 0;

}
   
   
/*********************************************************************/

 
int yearsec_ansi_to_dt(char *ansi_time, dtime_t *dt)
{
     int rv;
     
     /* dt->dt_qual = TU_DTENCODE(TU_YEAR, TU_SECOND); */
     
     rv = dtcvasc(ansi_time, dt);
   
     return rv;
}

/*********************************************************************/

int yearhour_ansi_to_dt(char *ansi_time, dtime_t *dt)
{
     int rv;
     
    /* dt->dt_qual = TU_DTENCODE(TU_YEAR, TU_HOUR); */
     
     rv = dtcvasc(ansi_time, dt);
    
     return rv;
}  

/*********************************************************************/

 
int yearday_ansi_to_dt(char *ansi_time, dtime_t *dt)
{
     int rv;
     
     /* dt->dt_qual = TU_DTENCODE(TU_YEAR, TU_DAY); */
     
     rv = dtcvasc(ansi_time, dt);
   
     return rv;
}


/*********************************************************************/

int monthday_ansi_to_dt(char *ansi_time, dtime_t *dt)
{
     int rv;
     
     /* dt->dt_qual = TU_DTENCODE(TU_MONTH, TU_DAY); */
     
     rv = dtcvasc(ansi_time, dt);
    
     return rv;
}   

/*********************************************************************/

int sheftime_to_yearsec_dt(const long shefDate,
			   const long shefHour,
			   dtime_t *dt)
{
     
     /*
     
     Convert from the integer representation to the
     Informix datetime representation.
    
     */
     int	rv;
     time_t	timet;
      
      
     timet = sheftime_to_timet(shefDate, shefHour);
     rv = timet_to_yearsec_dt(timet, dt);
     
     
     return(rv);
}

/****************************************************************/

int timet_to_yearsec_ansi(time_t timet, char *ansi)
{
   /*

   Convert from the C language time_t representation to 
   an ANSI time string representation.

   */
 
   struct tm 	*gm_struct;
   int rv  = 0;
   
   
   /* convert the time_t variable into a time structure for gm time */
   
   gm_struct = gmtime(&timet);
   
   
   /* convert the time_t variable to a time string */
   
   strcpy(ansi,"");
   strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S", gm_struct); 
   
   return(rv);
}

/****************************************************************/

int timet_to_yearmin_ansi(time_t timet, char *ansi)
{
   /*

   Convert from the C language time_t representation to 
   an ANSI time string representation.

   */
 
   struct tm 	*gm_struct;
   int rv  = 0;
   
   
   /* convert the time_t variable into a time structure for gm time */
   
   gm_struct = gmtime(&timet);
   
   
   /* convert the time_t variable to a time string */
   
   strcpy(ansi,"");
   strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M", gm_struct); 
   
   return(rv);
}


/****************************************************************/

int timet_to_yearhour_ansi(time_t timet, char *ansi)
{
   /*

   Convert from the C language time_t representation to 
   an ANSI time string representation.

   */
 
   struct tm 	*gm_struct;
   int rv  = 0;
   
   
   /* convert the time_t variable into a time structure for gm time */
   
   gm_struct = gmtime(&timet);
   
   
   /* convert the time_t variable to a time string */
   
   strcpy(ansi,"");
   strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d %H", gm_struct); 
   
   return(rv);
}

/****************************************************************/

int timet_to_yearday_ansi(time_t timet, char *ansi)
{
   /*

   Convert from the C language time_t representation to 
   an ANSI time string representation.

   */
 
   struct tm 	* gm_struct = NULL ;
   int rv  = 0;
   
   
   /* convert the time_t variable into a time structure for gm time */
   
   gm_struct = gmtime(&timet);
   
   /* convert the time_t variable to a time string */
   
   strcpy(ansi,"");
   strftime(ansi, ANSI_TIME_LEN, "%Y-%m-%d", gm_struct); 
   
   return(rv);
}

/*********************************************************************/
char * dtimet_to_ansi(dtime_t dt, char buffer[] )
{

    int rv;
    
    strcpy(buffer,"");
    rv = dttoasc(&dt, buffer);

    return buffer;   
}  
  
/*********************************************************************/
   
int yearsec_dt_to_ansi(dtime_t dt, char * ansi)
{
   int	rv;
  
   /*
      convert yearsec_dt_to_year_ansi
      Note: is implemented in the same way as dt_to_ansi
   */
   
   strcpy(ansi,"");
   rv = dttoasc(&dt, ansi);
   
   return(rv);
}

/*********************************************************************/
     
int yearday_dt_to_ansi(dtime_t dt, char * ansi)
{
   int	rv;
  
   strcpy(ansi,"");
   rv = dttoasc(&dt, ansi);
   
   return(rv);
}

/*********************************************************************/
     

int yearhour_dt_to_ansi(dtime_t dt, char * ansi)
{
   int	rv;
  
   /*
      convert yearhour_dt_to_yearhour_ansi
      Note: is implemented in the same way as dt_to_ansi
   */
   
   strcpy(ansi,"");
   rv = dttoasc(&dt, ansi);
   
   return(rv);
}

/*********************************************************************/
     

int monthday_dt_to_ansi(dtime_t dt, char * ansi)
{
   int	rv;
  
   /*
      convert yearhour_dt_to_yearhour_ansi
      Note: is implemented in the same way as dt_to_ansi
   */
   
   strcpy(ansi,"");
   rv = dttoasc(&dt, ansi);
   
   return(rv);
}

/********************************************************************/

int  yearsec_ansi_to_timet(char *ansi, time_t *timet)
{
   
   struct tm	gm_struct;
   int		rv = 0,
                scan_rv = 0;
   
  
   memset(&gm_struct,0,sizeof(struct tm));
   scan_rv = sscanf(ansi, "%d-%d-%d %d:%d:%d",
		      &(gm_struct.tm_year),
		      &(gm_struct.tm_mon),
		      &(gm_struct.tm_mday),
		      &(gm_struct.tm_hour),
		      &(gm_struct.tm_min),
		      &(gm_struct.tm_sec));
   
   gm_struct.tm_year = gm_struct.tm_year - 1900;
   gm_struct.tm_mon  = gm_struct.tm_mon - 1;
   
   if (scan_rv < 6)
   {
    	rv = -1;
   }
      
   else
   {
      /* convert the tm structure to a time_t value using
	 the inhouse function gm_mktime; this is needed because
	 there is no system library function to convert to time_t
	 in gm time; one only exists for local time */
      
      *timet = gm_mktime(&gm_struct);
   }
   
   return rv;   
}

/********************************************************************/

int  yearday_ansi_to_timet(char *ansi, time_t *timet)
{
   
   struct tm	gm_struct;
   int		rv = 0,
                scan_rv = 0;
   
  
   scan_rv = sscanf(ansi, "%d-%d-%d",
		      &(gm_struct.tm_year),
		      &(gm_struct.tm_mon),
		      &(gm_struct.tm_mday));
		      
   
   gm_struct.tm_year = gm_struct.tm_year - 1900;
   gm_struct.tm_mon  = gm_struct.tm_mon - 1;
   gm_struct.tm_hour = 0 ;
   gm_struct.tm_min = 0 ;
   gm_struct.tm_sec = 0 ;
   
   if (scan_rv < 3)
   {
    	rv = -1;
   }
      
   else
   {
      /* convert the tm structure to a time_t value using
	 the inhouse function gm_mktime; this is needed because
	 there is no system library function to convert to time_t
	 in gm time; one only exists for local time */
      
      *timet = gm_mktime(&gm_struct);
   }
   
   return rv;   
}

/*********************************************************************/

int  yearhour_ansi_to_timet(char *ansi, time_t *timet)
{
   
   struct tm	gm_struct;
   int		rv = 0,
      		scan_rv = 0;
   
  
   scan_rv = sscanf(ansi, "%d-%d-%d %d", 
		       &(gm_struct.tm_year),
		       &(gm_struct.tm_mon),
		       &(gm_struct.tm_mday),
		       &(gm_struct.tm_hour));
		       
		       
   gm_struct.tm_min  = 0;
   gm_struct.tm_sec  = 0;
   
   
   gm_struct.tm_year = gm_struct.tm_year - 1900;
   gm_struct.tm_mon  = gm_struct.tm_mon - 1;
   
   if (scan_rv < 4)
   {
    	rv = -1;
   }
      
   
   /* convert the tm structure to a time_t value using
      the inhouse function gm_mktime; this is needed because
      there is no system library function to convert to time_t
      in gm time; one only exists for local time */
   
   *timet = gm_mktime(&gm_struct);
   
   return rv;   
}

/*********************************************************************/
int yearsec_dt_to_timet(dtime_t dt, time_t *timet)
{

    /*
    	 the epoch for PGSQL is 2000-01-01 00:00:00
	 the epoch for time_t is 1970-01-01 00:00:00
    */
    /*codetimer * timer = get_timer();*/
    /*restart_timer(timer);*/   
    *timet = (long)dt + PGSQL_EPOCH_IN_TIMET;
  
    
    /*stop_timer(timer);*/
    
    return 0;

}

/*********************************************************************/

int yearsec_dt_to_timet_old(dtime_t dt, time_t *timet)
{
    
     /*codetimer * timer = get_timer();*/
     
     /*restart_timer(timer);*/
     /*
     
     Convert from the informix datetime year to second,
     to the C language time_t representation.
     */
     
     
     char		ansi[ANSI_TIME_LEN];
     int		rv = 0;
     int                date_rv = 0;
     int                ansi_rv = 0;
     
     *timet = 0;

     bool isNull = IsNullDateTime ( dt ); 

     if ( ! isNull )  
     { 
     
        /* convert the Informix datetime variable to an ANSI string */
     
        date_rv = yearsec_dt_to_ansi(dt, ansi);
        if (date_rv < 0)
        {
   	     rv = date_rv;  
        }
     
        else
        {
   	     ansi_rv = yearsec_ansi_to_timet(ansi, timet);
	     if (ansi_rv < 0)
	     {
	          rv = ansi_rv;
	     }
	  
        }
     }
     else /* is null */
     {
        rv = -1;
	/*  timet is already set to 0, which we want */
     }
     
     /*stop_timer(timer);*/
    // print_elapsed_time( &timer, "total time in yearsec_dt_to_timet so far = " , stdout );
  
     return( rv );
}


/******************************************************************/

int yearhour_dt_to_timet(dtime_t dt, time_t *timet)
{
   
   /*

   Convert from the informix datetime to the C language time_t
   representation.
 
   */
   
   
   char		ansi[ANSI_TIME_LEN];
   int		rv = 0,
      		date_rv = 0,
		ansi_rv = 0;
   
   /*
   	init timet
   */
   *timet = 0;
   
   
   /* convert the Informix datetime variable to an ANSI string */
   
   date_rv = yearhour_dt_to_ansi(dt, ansi);
   if (date_rv < 0)
   {
    	rv = date_rv;  
   }
   
   else
   {
	ansi_rv = yearhour_ansi_to_timet(ansi, timet);
	if (ansi_rv < 0)
	{
	     rv = ansi_rv;  
	}
   }
   return( rv );
}

/*********************************************************************/

int yearday_dt_to_timet(dtime_t dt, time_t *timet)
{
     
     /*
     Convert from the Informix datetime year to day,
     to the C language time_t representation.
     */
     
     char		ansi[ANSI_TIME_LEN];
     int		rv = 0,

     date_rv = 0,
     ansi_rv = 0;
     
     *timet = 0;
     
     /* convert the Informix datetime variable to an ANSI string */
     date_rv = yearday_dt_to_ansi(dt, ansi);
     
     if (date_rv < 0)
     {
	  rv = date_rv;  
     }
     
     else
     {
	  ansi_rv = yearday_ansi_to_timet(ansi, timet);
	  if (ansi_rv < 0)
	  {
	       rv = ansi_rv;
	  }
	  
     }
     
     return( rv );
}


/********************************************************************/


time_t sheftime4_to_timet(const long shefDate, const long shefHour)
{
   
    /*
	
    This routine converts the input date and time to 
    time of type time_t.  The input date and time are
    assumed to be in Z time.  The C time library routines
    assume times are local, so a conversion to Z time
    was necessary. 
    
    
    shefDate is in form: yyyymmdd
    shefHour is in form: hhmmss 
    
    */
     
     
   struct tm t;
   time_t newtime;
   
   
   /* define the elements of the tm structure;
      note that a base year of 1900 is used to be 
      consistent with the Unix time functions */
   
   t.tm_year = (shefDate - 19000000) / 10000;   
   t.tm_mon =  ((shefDate/100) % 100) - 1;
   t.tm_mday = shefDate%100;
   
   t.tm_hour = shefHour/10000;
   t.tm_min =  ((shefHour/100) % 100);
   t.tm_sec =  shefHour%100; 
   
   
   /* gm_mktime() works like mktime() except gm_mktime() assumes
      the input t is in GMT  */
   
   newtime = gm_mktime(&t); 
   
   
   if (newtime == -1 )
      return (time_t)-1;
   
   else   
      return newtime;
   
} 


/********************************************************************/ 

time_t sheftime_to_timet(const long shefDate, const long shefHour)
{
	
	 /* 
	
	    This routine converts the input date and time to 
	    time of type time_t.  The input date and time are assumed
	    to be in Z time.  The C time library routines
	    assume times are local, so a conversion to Z time
	    was necessary.  
	    
	    shefDate is in form: yyyymmdd
    	    shefHour is in form: hhmmss 
	 */
	
	
	/* shefDate is in form [yymmdd] */
	/* shefHour is in form [hhmmss] */
	
	
	struct tm t;
	time_t newtime;
	
     	t.tm_year = shefDate/10000;
	if (t.tm_year < 70)	/* will work until 2069 */
	   t.tm_year+=100;
	   
	t.tm_mon =  ((shefDate/100) % 100) - 1;  /* month is 0 - 11 */
	t.tm_mday = shefDate%100;

	t.tm_hour = shefHour/10000;
	t.tm_min =  ((shefHour/100) % 100);
	t.tm_sec =  shefHour%100; 
	
	/*  This field has no effect since this is in GMT or UTC
 		t.tm_isdst = 0;
	*/ 
	
	
	/* 
		gm_mktime() works like mktime() except gm_mktime() assumes
	  	the input t is in GMT
	*/
	
	newtime = gm_mktime(&t);  /* works like mktime except assumes
					the input is GMT
				   */

	if (newtime == -1 )
		return (time_t)-1;

	return newtime;
	
}   /* sheftime_to_timet */

/********************************************************************/


time_t sheftime_to_time_t(const long shefDate, const long shefHour)
{
	return sheftime_to_timet(shefDate, shefHour);

}

/********************************************************************/

void  timet_to_sheftime(const time_t timevar,
			 long *shefDate, long *shefHour)
{
	/*
		This function converts from the type time_t to the
		format of time required by shef.
	*/
	
   	struct	tm	*tmptr;
   	struct tm  perm;
	
	tmptr = gmtime(&timevar);
	
	perm = *tmptr;
	
	*shefDate = perm.tm_year*10000;
	*shefDate += (perm.tm_mon + 1) * 100;
	*shefDate += perm.tm_mday;
	
	*shefHour = perm.tm_hour*10000;
	*shefHour += perm.tm_min*100;
	*shefHour += perm.tm_sec;
	
	return;
}   

/*********************************************************************/
void  time_t_to_sheftime(const time_t timevar,
			 long *shefDate, long *shefHour)
{
	timet_to_sheftime(timevar, shefDate, shefHour);			 
	
	return;		 
}
/*********************************************************************/
      
 
time_t gm_mktime(struct tm *t)
{
	/* 
	    This function works like mktime(), but assumes the
 	    input t is in GMT (UTC) instead of local time.
 	    The function could be more efficient, but for readability
 	    it has been left this way.  
 	    
 	*/	
	
	
	int 	months[] = { 31,28,31,30,31,30,31,31,30,31,30,31 };

	time_t 		total_seconds,
			total_days = 0;
			
			
	long 		base_year = 70,
			century_base = 1900,
			i,
			full_year;
			
				
	/*
	
	 This function is just like mktime() except that t is
	 assumed to be GMT time, not local time.
	
	*/

	
	base_year += century_base;
	full_year = t->tm_year + century_base;

	
	/* years */
        if ( base_year <= full_year )
        {
	   for (i = base_year; i  <  full_year; i++)
	   {
	   	if (leap_year(i))
			total_days += 366;
		else
			total_days += 365;
		
	   }
        }
        else
        {
           for (i = base_year; i > full_year; i --)
           {
                       if (leap_year(i))
                               total_days -= 366;
                       else
                               total_days -= 365;
           }
        }
	
	
	/* current year is leap_year */
	
	/* tm_mon == month - 1, > 1 means later than Feb */
	if  ( (t->tm_mon > 1) &&  leap_year(full_year) )
	{
		total_days ++;
	}
	

	for ( i = 0 ;  i < t->tm_mon; i++)
	{
		total_days += months[i];
	}
	

	total_days += t->tm_mday - 1;
	
	/* don't care about leap seconds */
	total_seconds = (total_days*24*3600); 
	
	total_seconds += t->tm_hour * 3600;
	total_seconds += t->tm_min * 60;
	total_seconds += t->tm_sec;

	return total_seconds;		

} 


/*********************************************************************/

int leap_year(const long year)
{
     /*
       leap_year() takes a long integer for the year and
   	returns 1 if it is a leap year and 0 if it is not
 
     */
          
   int rv;
   

   /* 1900, 2100 not leap years, 2000 is */
   
   if ( (year % 100) == 0) 
   {  
      if ((year % 400) == 0)
	 rv = 1;
      else
	 rv = 0;
   }
   
   else if ( (year % 4) == 0)
      rv = 1;	
   
   else
      rv = 0;
   
   return rv;	
   
}


/***********************************************************************
   
   converts the encoded duration into a count of seconds
   
   *********************************************************************/

int  shefdur_to_timet(   int 	dur,
		         time_t	obstime,
		         time_t	*datadur)
{
   int          local_7am_window;
   int		status;
   time_t       diff;
   time_t	local7time;
   time_t 	retval;
   struct tm	* obstm = NULL;
   
   /* initialize */
   status = 0;

   
   /* 000x values are in minutes */
   if (dur < 1000L )
      retval = dur * SECONDS_PER_MINUTE;


   /* 100x values are in hours */
   else if (dur >= 1000L && dur < 2000L)
      retval = (time_t) ((dur%1000) * SECONDS_PER_HOUR);

   
   /* 200x values are in days */
   else if (dur >= 2000L && dur < 3000L)
      retval = (time_t) ((dur%2000) * SECONDS_PER_DAY);

   
   /* 300x values are in months */
   else if (dur >= 3000L && dur < 4000L)
      retval = (time_t) ((dur%2000) * SECONDS_PER_DAY);
   
   
   /* 400x values are in years.
      doesn't handle this, need to know the number of days in
      the year to do this, and therefore need to know the year */

   else if (dur >= 4000L && dur < 5000L)
   {
      status = -1;
      retval = 0;
   }
   
   
   /* 500x values are specially defined.
      only consider 5004; its duration is defined as the
      period ending at the observation time and beginning at
      the most recent 7AM local */
   
   else if (dur == 5004)
   {
      /* find the time_t for 7AM local.  use the data time.
	 subtract a day from it so the modulo approach
	 can be used to determine the duration. this is needed
	 to easily handle the cases where the ending time is
	 between midnight and 7 AM. */
      
      /* Get the search window around local 7 AM. */
      local_7am_window = get_local_7am_search_window ( );

      obstm = localtime(&obstime);     
      obstm->tm_hour = 07;
      obstm->tm_min  = 0;
      obstm->tm_sec  = 0;
      local7time = mktime(obstm);

      diff = labs ( obstime - local7time );

      if ( diff < ( SECONDS_PER_HOUR * local_7am_window ) )
      {
         /* The difference between the 5004 report obstime and local 7am is
            within the window within which the report is considered to have
            a duration of 24 hours ending at local 7am. */
         retval = SECONDS_PER_DAY;
      }
      else if ( obstime < local7time )
      {
         /* The 5004 report obstime is smaller than local 7am and outside
            of the 7am window.  The duration of this report is the difference
            between its time and 7am local of the previous day. */
         retval = SECONDS_PER_DAY - diff;
      }
      else
      {
         /* The 5004 report obstime is greater than local 7am and outside of
            the 7am window.  The duration of this report is the difference
            between its time and 7am local. */
         retval = diff;
      }
   }
    
   /* then there are some undefined values */
     
   else
   {
      status = -1;
      retval = 0;
   }
	 
   *datadur = retval;
   return status;
}


/***********************************************************************
   
   converts the duration in seconds into the shef code duration;
   this algorithm assumes standard time durations; roundoff error
   can occur if unusual durations used.

   output duration coded as:
   
   000x values are in minutes
   100x values are in hours
   200x values are in days
   300x values are in months (not used)
   400x values are in years (not used)
   
   *********************************************************************/


int timet_to_shefdur(time_t	duration,
		     int	*shefdur)
{

   time_t 	retval;
   int		status;
   int 		MAX_DAYS = 90;
     
    
   /* initialize */
   
   status = 0;

   
   /* instantaneous value */

   if (duration <= 0)
      retval = 0000;
   
   
   /* check for case of less than a minute duration */
   
   if (duration < SECONDS_PER_MINUTE)
      retval = 0001;
   
   
   /* if less than an hour duration, represent in minutes */
   
   else if (duration < SECONDS_PER_HOUR)
      retval = 0000 + (duration / SECONDS_PER_MINUTE);
   
   
   /* if less than a days duration, represent in hours */
   
   else if (duration < SECONDS_PER_DAY)
      retval = 1000 + (duration / SECONDS_PER_HOUR);
      
      
   /* unless it is even number of days, if the duration
      is an even multiple of hours up to 999 hours,
      format in the hours form. */
      
   else if ((duration % SECONDS_PER_DAY)  != 0 && 
            (duration % SECONDS_PER_HOUR) == 0 &&
	    duration <= (999 * SECONDS_PER_HOUR))
      retval = 1000 + (duration / SECONDS_PER_HOUR);
   
   
   /* if less= than MAX_DAYS days duration, represent in days. */
   
   else if (duration <= (SECONDS_PER_DAY * MAX_DAYS))
      retval = 2000 + (duration / (double )(SECONDS_PER_DAY));
  
   
   /* don't deal with larger durations */
   
   else
   {
      status = -1;
      retval = 0;
   }
   
	   
   *shefdur = retval;
   
   
   return(status);

}

/********************************************************************
   
  check that date string ends with 4 digit year (y2k compliant stuff)
   
*********************************************************************/
int four_digit_year(char * datestr)
{
   int length, pos;

   /* calculate length of "date" string. */
   length = strlen(datestr);

   /* if length is zero then string is blank which is OK */
   if (length == 0)
     return(0);

   /* check that minimum length (8 characters) is entered (mmddyyyy) */
   if (length < 8)
     return(1);

   /* check that the last 4 charaters of date are integers (4 digit year) */
   for (pos=length-1; pos > (length - 5); pos--)
   {
     if (isdigit(datestr[pos]) == 0)
       return(2);
   }

   return(0);
}

int get_local_7am_search_window ( )
{
   char reply [ 50 ] = {'\0'};
   static const char * token_name = "ppp_ppd_local_7am_window";
   static int first = 1;
   static int local_7am_window = LOCAL_5004_7AM_WINDOW;
   int request_len;
   int reply_len;
   int status;

   if ( first == 1 )
   {
      first = 0;

      request_len = strlen ( token_name );

      /* Check token for value. */
      status = get_apps_defaults ( (char *) token_name,
                                    & request_len,
                                    reply,
                                    & reply_len );

      if ( ( status == 0 ) && ( reply_len > 0 ) )
      {
         local_7am_window = atoi ( reply );
      }
   }

   return local_7am_window ;
}


/****************************************************************
   Convert from the C language time_t representation
           to mm/dd/yyyy string representation.
****************************************************************/
int timet_to_yearday_USA(time_t timet, char *usadate)
{
 
   struct tm 	* gm_struct = NULL ;
   int rv  = 0;
   
   
   /* convert the time_t variable into a time structure for gm time */
   gm_struct = gmtime(&timet);
   
   /* convert the time_t variable to a time string */
   strcpy(usadate,"");
   strftime(usadate, ANSI_TIME_LEN, "%m/%d/%Y", gm_struct); 
   
   return(rv);
}

/****************************************************************
   Convert from the C language time_t representation
           to an ANSI time string representation.
****************************************************************/
char * timet_to_ansi ( time_t timet, char *ansi )
{
   struct tm *gm_struct;
   
   /* convert the time_t variable into a time structure for gm time */
   gm_struct = gmtime ( &timet );
   
   /* convert the gm time variable to a time string */
   strcpy ( ansi, "" );
   strftime ( ansi, ANSI_TIME_LEN, "%Y-%m-%d %H:%M:%S", gm_struct ); 
   
   return ( ansi );
}

/****************************************************************
   Convert from the PostgreSQL timestamp,
           to the C language time_t representation.
****************************************************************/
time_t timestamp_to_timet ( timestamp tstamp )
{

     char ansi[ANSI_TIME_LEN];
     time_t tmp_timet = 0;

     /* convert the PostgreSQL timestamp variable to an ANSI string */
     strcpy ( ansi, PGTYPEStimestamp_to_asc ( tstamp ) );

     /* convert the ANSI string to a time_t */
     yearsec_ansi_to_timet ( ansi, &tmp_timet );

     return tmp_timet;
}

/****************************************************************
   Convert from the C language time_t,
           to the PostgreSQL timestamp representation.
****************************************************************/
timestamp timet_to_timestamp ( time_t timet )
{

     char ansi[ANSI_TIME_LEN];
     timestamp tmp_timestamp = 0;

     /* convert the C language time_t variable to an ANSI string */
     timet_to_yearsec_ansi ( timet, ansi );

     /* convert the ANSI string to a PostgreSQL timestamp */
     tmp_timestamp = PGTYPEStimestamp_from_asc ( ansi, NULL );

     return tmp_timestamp;
}

/****************************************************************
   Convert from the HSEB date_t representation
           to an ANSI date string representation.
****************************************************************/
char * date_t_to_ansi_date ( date_t datet, char *ansi )
{
   char tempString[10];

   strcpy  ( ansi, "" );

   if ( valid_date_t(datet) )
   {
      /* convert the date_t variable (yyyymmdd stored as a long) into
         a date string (yyyy-mm-dd) */
   
      strcpy  ( ansi, "" );
      sprintf ( tempString, "%8ld", datet );
      strncat ( ansi, tempString, 4 );
      strcat  ( ansi, "-");
      strncat ( ansi, &tempString[4], 2 );
      strcat  ( ansi, "-");
      strncat ( ansi, &tempString[6], 2 );
      strcat  ( ansi, "\0");
   }
   
   return  ( ansi );
}

/****************************************************************
   Convert from the HSEB date_t representation
           to a date string representation.
****************************************************************/
char * date_t_to_USA_date ( date_t datet, char *usadate )
{
   char tempString[10];

   strcpy  ( usadate, "" );

   if ( valid_date_t(datet) )
   {
      /* convert the date_t variable (yyyymmdd stored as a long) into
         a date string (mm/dd/yyyy) */
   
      strcpy  ( usadate, "" );
      sprintf ( tempString, "%8ld", datet );
      strncat ( usadate, &tempString[4], 2 );
      strcat  ( usadate, "/");
      strncat ( usadate, &tempString[6], 2 );
      strcat  ( usadate, "/");
      strncat ( usadate, tempString, 4 );
      strcat  ( usadate, "\0");
   }
   
   return  ( usadate );
}

/***************************************************************
   Convert from an ANSI date string representation (yyyy-mm-dd)
           to the HSEB date_t representation.
****************************************************************/
int  ansi_date_to_date_t ( char *ansi, date_t *datet )
{
   
   int rv = 0;
   int scan_rv = 0;
   int yyyy = 0;
   int mm = 0;
   int dd = 0;
   date_t tmp_datet = 0;


   scan_rv = sscanf ( ansi, "%d-%d-%d", &yyyy, &mm, &dd );


   if (scan_rv < 3)
   {
    	rv = -1;
   }
   else
   {
      /* take the year, month and day parsed from the ansi string
         and create a yyymmdd stored as a long integer */
      
      tmp_datet = (yyyy * 10000) + (mm * 100) + dd;

      /* check if a valid date_t was created
         if not, then set return code as -2 and set datet to ZERO */
      
      if ( valid_date_t (tmp_datet) )
      {
         *datet = tmp_datet;
      }
      else
      {
         rv = -2;
         *datet = 0;
      }

   }
   
   return rv;   
}

/***************************************************************
   Convert from a USA date string representation (mm/dd/yyyy)
           to the HSEB date_t representation.
****************************************************************/
int  USA_date_to_date_t ( char *usadate, date_t *datet )
{
   
   int rv = 0;
   int scan_rv = 0;
   int yyyy = 0;
   int mm = 0;
   int dd = 0;
   date_t tmp_datet = 0;


   scan_rv = sscanf ( usadate, "%d/%d/%d", &mm, &dd, &yyyy );


   if (scan_rv < 3)
   {
    	rv = -1;
   }
   else
   {
      /* take the month, day and year parsed from the ansi string
         and create a yyymmdd stored as a long integer */
      
      tmp_datet = (yyyy * 10000) + (mm * 100) + dd;

      /* check if a valid date_t was created
         if not, then set return code as -2 and set datet to ZERO */
      
      if ( valid_date_t (tmp_datet) )
      {
         *datet = tmp_datet;
      }
      else
      {
         rv = -2;
         *datet = 0;
      }
   }
   
   return rv;   
}

/****************************************************************
   Convert from the PostgreSQL date,
           to the HSEB date_t representation.
****************************************************************/
date_t pg_date_to_date_t ( date pgdate )
{
     char ansi[ANSI_TIME_LEN + 1] = { '\0' };
     char * pDate = NULL;
     date_t tmp_datet = 0;

     /* convert the PostgreSQL date variable to an ANSI string */
     pDate = PGTYPESdate_to_asc ( pgdate );

     if ( pDate != NULL )
     {
        strncpy ( ansi, pDate, ANSI_TIME_LEN );
        free ( pDate );
        pDate = NULL;

        /* convert the ANSI string to a date_t */
        ansi_date_to_date_t ( ansi, &tmp_datet );
     }

     return tmp_datet;
}

/****************************************************************
   Convert from the HSEB date_t,
           to the PostgreSQL date type.
****************************************************************/
date date_t_to_pg_date ( date_t datet )
{

     char ansi[ANSI_TIME_LEN];
     date tmp_pgdate = 0;

     /* convert the HSEB date_t variable to an ANSI string (yyyy-mm-dd) */
     strcpy ( ansi,  date_t_to_ansi_date ( datet, ansi ) );

     /* convert the ANSI string (yyyy-mm-dd) to a PostgreSQL date */
     tmp_pgdate = PGTYPESdate_from_asc ( ansi, NULL );

     return tmp_pgdate;
}

/***************************************************************
   Convert from an ANSI month & day string representation (mm-dd)
           to the HSEB monthday_t representation.
****************************************************************/
int  ansi_monthday_to_monthday_t ( char *ansimonthday, monthday_t *monthdayt )
{
   
   int rv = 0;
   int scan_rv = 0;
   int mm = 0;
   int dd = 0;
   int days_per_month = 0;


   scan_rv = sscanf ( ansimonthday, "%d-%d", &mm, &dd );

   if (scan_rv < 2)
   {
    	rv = -1;
   }
   else /* at least month and day have been scaned in */
   {
      if ( mm < 1 || mm > 12 )
      {
    	   rv = -2; 
      }
      else /* valid month so array bounds won't be hosed */
      {
         days_per_month = numDaysInMonth[mm-1];
      
         /* if February, allow for leap day */
         if ( mm == 2 )
            days_per_month += 1;

         if ( dd < 1 || dd > days_per_month )
         {
    	      rv = -3;
         }
         else
         {
            /* take the month and day parsed from the ansi string
               and create a mmdd stored as an int */
      
            *monthdayt = (mm * 100) + dd;
         }
         
      } /* Valid Month */
      
   } /* at least 2 arguments scan in */
   
   return rv;   
}

/***************************************************************
   Convert from a USA month & day string representation (mm/dd)
           to the HSEB monthday_t representation.
****************************************************************/
int  USA_monthday_to_monthday_t ( char *USAmonthday, monthday_t *monthdayt )
{
   
   int rv = 0;
   int scan_rv = 0;
   int mm = 0;
   int dd = 0;
   char tempString[6];


   scan_rv = sscanf ( USAmonthday, "%d/%d", &mm, &dd );

   if (scan_rv < 2)
   {
    	rv = -1;
   }
   else
   {
      /* take the month and day parsed from the USA string
         and create an ansi string and call ansi_monthday_to_monthday_t */

      sprintf ( tempString, "%2d-%2d", mm, dd );
      
      rv = ansi_monthday_to_monthday_t ( tempString, monthdayt );
   }
   
   return rv;   
}

/****************************************************************
   Convert from the HSEB monthday_t representation
           to an ANSI month & day string representation.
****************************************************************/
char * monthday_t_to_ansi_monthday ( monthday_t monthdayt, char *ansimonthday )
{
   int mm = 0;
   int dd = 0;

   /* convert the monthday_t variable (mmdd stored as an int) into
      a month day string (mm-dd) */
   
   mm = monthdayt / 100;
   dd = monthdayt - (mm * 100);
   
   sprintf ( ansimonthday, "%2.2d-%2.2d", mm, dd );
   strcat  ( ansimonthday, "\0");
   
   return  ( ansimonthday );
}

/****************************************************************
   Convert from the HSEB monthday_t int representation
           to a USA month & day string representation.
****************************************************************/
char * monthday_t_to_USA_monthday ( monthday_t monthdayt, char *usamonthday )
{
   int mm = 0;
   int dd = 0;

   /* convert the monthday_t variable (mmdd stored as an int) into
      a month day string (mm/dd) */
   
   mm = monthdayt / 100;
   dd = monthdayt - (mm * 100);
   
   sprintf ( usamonthday, "%2.2d/%2.2d", mm, dd );
   strcat  ( usamonthday, "\0");
   
   return  ( usamonthday );
}

/****************************************************************
   Convert from the C language time_t representation,
           to the HSEB monthday_t int representation.
****************************************************************/
monthday_t timet_to_monthday_t ( time_t timet )
{

   struct tm  *gm_struct = NULL;
   int mm = 0;
   int dd = 0;
   monthday_t tmp_monthdayt = 0;
   
   /* convert the time_t variable into a time structure for gm time */
   gm_struct = gmtime(&timet);

   mm = gm_struct->tm_mon + 1;
   dd = gm_struct->tm_mday;
   tmp_monthdayt = (100 * mm) + dd;

   return tmp_monthdayt;
}

/****************************************************************
   Convert from the HSEB date_t,
           to the C language time_t representation.
****************************************************************/
time_t date_t_to_timet ( date_t datet )
{

     char ansi[ANSI_TIME_LEN];
     time_t tmp_timet = 0;

     /* convert the HSEB date_t variable to an ANSI date string (yyyy-mm-dd) */
     strcpy ( ansi, date_t_to_ansi_date ( datet, ansi ) );

     /* convert the ANSI date string (yyyy-mm-dd) to a time_t */
     yearday_ansi_to_timet ( ansi, &tmp_timet );

     return tmp_timet;
}

/****************************************************************
   Convert from the C language time_t representation,
           to the HSEB date_t representation.
****************************************************************/
date_t timet_to_date_t ( time_t timet )
{

     char ansi[ANSI_TIME_LEN];
     date_t tmp_datet = 0;

     /* convert the C language time_t variable to an ANSI string */
     timet_to_yearday_ansi ( timet, ansi );

     /* convert the ANSI string to a date_t */
     ansi_date_to_date_t ( ansi, &tmp_datet );

     return tmp_datet;
}

/****************************************************************
   Check if the HSEB date_t type passed in is a valid date
   returns 1 if it is a valid date_t and 0 if it is not
****************************************************************/
int valid_date_t ( date_t datet )
{
   int rv = 0;
   int yyyy = 0;
   int mm = 0;
   int dd = 0;
   int days_per_month = 0;
   
   /* a valid date_t falls between 01/01/1000 and 12/31/3000 */
   if ( (datet < 10000101) || (datet > 30001231) )
   {
      rv = 0;
   }
   else
   {
      yyyy = datet / 10000;
      mm = ( datet - (yyyy * 10000) ) / 100;
      dd = datet - (yyyy * 10000) - (mm * 100);

      if ( mm < 1 || mm > 12 )
      {
         rv = 0;
      }
      else /* valid month so array bounds won't be hosed */
      {
         days_per_month = numDaysInMonth[mm-1];
      
         /* if February and a leap year, account for leap day */
         if ( (mm == 2) && (leap_year(yyyy) == 1) )
            days_per_month += 1;
      
         if ( dd < 1 || dd > days_per_month )
            rv = 0;
         else
            rv = 1;
      }
   }

   return rv;	
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
