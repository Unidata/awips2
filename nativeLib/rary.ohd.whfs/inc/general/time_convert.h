#ifndef TIME_CONVERT_H
#define TIME_CONVERT_H

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <datetime.h>

#include <sql3types.h>
#include <sqltypes.h>

#define ANSI_TIME_LEN 26
#define ANSI_YEARSEC_TIME_LEN 19
#define YYYYMMDD_DATE_LEN 8
#define MMDDYYYY_DATE_LEN 8
#define YYYYMMDDHH_LEN 10

#include "time_defs.h"
//#include "CodeTimer.h"


//time_t value for 2000-01-01 00:00:00 UTC
#define PGSQL_EPOCH_IN_TIMET 946684800


typedef long date_t;
typedef int  monthday_t;

#ifdef __cplusplus
extern "C"
{
#endif

//codetimer * get_timer();

int  isTimeError(int rv);

/*
		Conversions to various dtime_t formats
*/

int  yearsec_ansi_to_dt(char * ansi, dtime_t *dt);
int  yearhour_ansi_to_dt(char *ansi_time, dtime_t *dt);
int  yearday_ansi_to_dt(char *ansi_time, dtime_t *dt);
int  monthday_ansi_to_dt(char * ansi, dtime_t *dt);
int  timet_to_yearsec_dt(time_t timet, dtime_t *dt);
int  sheftime_to_yearsec_dt(const long shefDate,
			    const long shefHour,
			    dtime_t *dt);

/*
		Conversions to various ansi time, date and month/day formats
*/
int  timet_to_yearsec_ansi(time_t timet, char *ansi);
int  timet_to_yearmin_ansi(time_t timet, char *ansi);
int  timet_to_yearhour_ansi(time_t timet, char *ansi);
int  timet_to_yearday_ansi(time_t timet, char *ansi);
int  yearsec_dt_to_ansi(dtime_t dt, char *ansi);
int  yearhour_dt_to_ansi(dtime_t dt, char *ansi);
int  yearday_dt_to_ansi(dtime_t dt,char * ansi);
int monthday_dt_to_ansi(dtime_t dt, char * ansi);

//used during conversion, but not after
char * dtimet_to_ansi(dtime_t dt, char buffer[] );

// used during and after conversion
int  timet_to_yearday_USA(time_t timet, char *ansi);
char * timet_to_ansi       ( time_t timet, char *ansi );
char * date_t_to_ansi_date ( date_t datet, char *ansi );
char * date_t_to_USA_date  ( date_t datet, char *usadate );
char * monthday_t_to_ansi_monthday ( monthday_t monthdayt, char *ansimonthday );
char * monthday_t_to_USA_monthday  ( monthday_t monthdayt, char *usamonthday );

/*
		Conversions to time_t
*/
int  yearsec_ansi_to_timet(char *ansi, time_t *timet);
int  yearhour_ansi_to_timet(char *ansi, time_t *timet);
int  yearday_ansi_to_timet(char *ansi, time_t * timet);


int  yearsec_dt_to_timet(dtime_t dt, time_t *timet);
int  yearhour_dt_to_timet(dtime_t dt,  time_t *timet);
int  yearday_dt_to_timet(dtime_t dt, time_t * timet);

time_t	sheftime4_to_timet(const long shefDate, const long shefHour);
time_t	sheftime_to_timet(const long shefDate, const long shefHour);
time_t	sheftime_to_time_t(const long shefDate, const long shefHour);

time_t timestamp_to_timet ( timestamp tstamp );
time_t date_t_to_timet    ( date_t datet );

/*
		Conversions to PostgreSQL timestamp and date
*/
timestamp timet_to_timestamp ( time_t timet );
date      date_t_to_pg_date  ( date_t datet );

/*
		Conversions to HSEB date_t
*/
int  ansi_date_to_date_t ( char *ansi, date_t * datet );
int  USA_date_to_date_t  ( char *usadate, date_t * datet );
date_t pg_date_to_date_t ( date pgdate );
date_t time_t_to_date_t  ( time_t timet );

/*
		Conversions to HSEB monthday_t
*/
int  ansi_monthday_to_monthday_t ( char *ansimonthday, monthday_t * monthdayt );
int  USA_monthday_to_monthday_t  ( char *usamonthday,  monthday_t * monthdayt );
monthday_t timet_to_monthday_t ( time_t timet );

/*
		Conversions to sheftime
*/
void	timet_to_sheftime(const time_t timevar, long *shefDate, long *shefHour);
void	time_t_to_sheftime(const time_t timevar, long *shefDate, long *shefHour);

/*
		Conversions from shef code time to time_t duration
*/
int  shefdur_to_timet(int 	dur,
		      time_t	obstime,
		      time_t	*datadur);
int timet_to_shefdur(time_t	duration,
		     int	*shefdur);
int get_local_7am_search_window ( );

/*
	Functions of general use
*/
time_t	gm_mktime(struct tm *t);
int	leap_year(const long year);
int     four_digit_year(char * datestr);
void    datimls(int time_out[7]);
int valid_date_t ( date_t datet );

/* Function to get the current gmtime in char format for ex:Wed Apr 13 15:05:27 2005 */

#ifdef __cplusplus
}
#endif


#endif
