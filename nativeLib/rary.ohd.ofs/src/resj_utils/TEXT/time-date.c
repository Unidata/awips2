/* These are some time / date functions yanked out of HM libraries 
   for use in ESPADP  */


#include "ResJ.h"
#include <stdio.h>
#include <stdlib.h>

/* ----------------------------------------------------------------------------
** GetJulianDay1900FromDate - calculate the Julian date relative to
**                              Jan 1, 1900
** ----------------------------------------------------------------------------
** Notes:       (1)     The Gregorian calendar uses the following conventions:
**                      -       years which are evenly divisible by 4 are leap
**                              years and have 29 days in February
**                      -       years which end in 00 (e.g. 2000) are only leap
**                              years if they are divisible by 400 (i.e. 3 days
**                              are lost every 400 years)
**              (2)     Julian day 1 is on 1/1/1900.  There is no Julian day 0.
**                      Therefore, we have to subtract one day to the value.
**                      The day is added if converting using
**                      GetDateFromJulianHour1900.
**     (Correction)     Indeed there is no Julian date 0.  Therefore, if we are
**                      working with discrete (integer) julian days we should
**                      return 1 for 01/01/1900.  Any use of this elsewhere,
**                      say for for determining julian hour, you would need to
**                      subtract one from the julian day before you multiply it
**                      by 24 (see newly updated GetJulianHour1900FromDate
**                      below).  JRV, RTi--03/10/2004.
** ----------------------------------------------------------------------------
** History:
**
** 07-13-95     Steven A. Malers, RTi   Renamed this routine - used to be
**                                      HMJulianDay1900.
** 04 Feb 96    SAM, RTi                Add leap year flag to properly adjust
**                                      by one day during leap years.
** 10 Mar 04	James R. VanShaar, RTi	Revised calculation to provide a value
**                                      of one (1) for 01/01/1900, instead of
**					zero (0).
**        WARNING: THERE ARE OTHER FUNCTIONS IN NWSRFS THAT ARE VIRTUALLY
**                 COPIES OF THIS, PRIOR TO THE CHANGE.  
**                 (see HMGetJulianHour1900FromDate.c in the verify tree, 
**                 time-date.c (GetJulianHour1900FromDate) in the ens tree.)
**
** ----------------------------------------------------------------------------
** Variable     I/O     Description
**
** day          I       Day of month (1-31).
** jday         O       Number of days since Jan 1, 1900.
** leap         L       Indicates if the year is a leap year.
** month        I       Month of year (1-12).
** year         I       Year (4-digit).
** ----------------------------------------------------------------------------
*/
int GetJulianDay1900FromDate ( int month, int day, int year, int *jday )
{       int     leap;

        if ( !IsValidDay(day, month, year) )
                return STATUS_FAILURE;
        if ( !IsValidMonth(month) )
                return STATUS_FAILURE;
        if ( !IsValidYear(year) )
                return STATUS_FAILURE;
        leap = IsLeapYear ( year );
        *jday = day +                                   /* day of month */
                NumDaysInMonthS (1, year, (month-1))+ /* days in prev months*/
                365*year +                              /* days in prev years */
                year/4 -                                /* 1 if leap year */
                year/100 +                              /* -3 every 400 years */
                year/400 -                              /* 1 every 400 years */
                JULIAN1900DAYS -                      /* Dec 31, 1899 */
                leap;                                   /* cancel /4 term -
                                                           will be added by
                                                           HMNumDaysInMonths
                                                           term */

        /*
        ** Add 1 to make sure that Julian day 1 is on 1/1/1900 (there is no
        ** Julian day 0. ??? Why subtract then ???
        */
	// I guess this was instruction for whatever function called this one.
	// I find this rediculous.  I should get 1 out of this function and
	// I should subtract 1 elsewhere if I am trying to get a 0 for 
	// calculation of JulianHour.

        //*jday -= 1;

        return STATUS_SUCCESS;
}
/* ----------------------------------------------------------------------------
** HMGetJulianHour1900FromDate - get Julian hour (relative to 1900) from date
** ----------------------------------------------------------------------------
** Copyright:   See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	Julian hour 0/0/1900 corresponds to Julian hour 0.
** ----------------------------------------------------------------------------
** History:
**
** 07-13-95	Steven A. Malers, RTi	Created routine.
** 06 Sep 1996  Steven A. Malers, RTi   Split out of the HMTD.c file.
** 21 Jan 1996	Catherine E. Nutting, RTi	Incorporated from HMData to ESPUtil
** 10 Mar 2004	James R. VanShaar, RTi	Revised calculation due to revision in
**					GetJulianDay1900FromDate (see that
**					function above).
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** day		I	Day corresponding to "jhour".
** hour		I	Hour corresponding to "jhour".
** jday		L	Julian day corresponding to date.
** jhour	O	Julian hour.
** message	L	String for messages.
** month	I	Month corresponding to "jhour".
** year		I	Year corresponding to "jhour".
** routine	L	Name of this routine.
** ----------------------------------------------------------------------------
*/

int GetJulianHour1900FromDate (	int month, int day, int year, int hour,
					int *jhour )
{	int	jday;
	char	message[256], routine[] = "GetJulianHour1900FromDate";

	if (	GetJulianDay1900FromDate( month, day, year, &jday) ) {
		PrintWarning ( 2, routine,
			"Unable to convert date (%d/%d/%d:%d) to Julian hour",
			month, day, year, hour );
		return STATUS_FAILURE;
	}

	// We NOW expect a jday of 1 for Jan 01, 1900.  
	// Shift for hour calculation.
	//*jhour = jday*24 + hour;  <--- Old zero based julian day.
	*jhour = (jday-1)*24 + hour;

	return STATUS_SUCCESS;
}

/* ----------------------------------------------------------------------------
** NumDaysInMonth - get the number of days in a month
** ----------------------------------------------------------------------------
** History:
**
** 24 Oct 1995  Steven A. Malers, RTi           Allow months to be > 12.  If
**                                              so, project the date into the
**                                              future.  This allows iterators
**                                              to be simpler.
** ----------------------------------------------------------------------------
** Variable     I/O     Description
**
** month        I       Integer (1-12) representing month.
** ndays        O       Number of days in month.
** year         I       Year of interest (4 digit).
** ----------------------------------------------------------------------------
*/
int NumDaysInMonth ( int month, int year )
{       int     ndays;
 
        if ( month < 0 ) {
                /*
                ** Assume that something is messed up...
                */
                ndays = 0;
        }
        else if ( month > 12 ) {
                /*
                ** Project out into the future...
                */
                return NumDaysInMonth ( (month%12), (year + month/12) );
        }
        else {  /*
                ** Usual case...
                */
                ndays = MonthDays[month - 1];
                if ( (month == 2) && IsLeapYear(year) )       ++ndays;
        }
        return ndays;
}
/* ----------------------------------------------------------------------------
** IsLeapYear - is the year a leap year?
** ----------------------------------------------------------------------------
** Notes:       (1)     Leap years occur on years evenly divisible by four.
**                      However, years evenly divisible by 100 are not leap
**                      years unless they are also evenly divisible by 400.
** ----------------------------------------------------------------------------
** Variable     I/O     Description
**
** year         I       year of interest
** ----------------------------------------------------------------------------
*/
int IsLeapYear ( int year )
{       if (    (((year%4) == 0) && ((year%100) != 0)) ||
                (((year%100) == 0) && ((year%400) == 0)) )
                return 1;
        else    return 0;
}

/*
** IsValidDay - is the day a valid value?
**
** Variable     I/O     Description
**
** day          I       day of interest
** daysinmonth  L       days in the month of interest
** month        I       month of interest
** year         I       year of interest (4 digit)
*/
int IsValidDay ( int day, int month, int year )
{       int     daysinmonth;

        /*
        ** First check month and year...
        */
        if ( !IsValidMonth(month) )
                return 0;
        if ( !IsValidYear(year) )
                return 0;
        /*
        ** Now check day, accounting for leap years...
        */
        daysinmonth = MonthDays[month - 1];
        if ( (month == 2) && IsLeapYear(year) )
                ++daysinmonth;
        if ( (day > 0) && (day <= daysinmonth) )
                return 1;
        else    return 0;
}

/*
** IsValidHour - is the hour a valid value?
**
** Variable     I/O     Description
**
** hour         I       hour of interest
*/
int IsValidHour ( int hour )
{       if ( (hour >= 0) && (hour <= 24) )
                return 1;
        else    return 0;
}

/*
** IsValidMonth - is the month a valid value?
**
** Variable     I/O     Description
**
** month        I       month of interest
*/
int IsValidMonth ( int month )
{       if ( (month > 0) && (month < 13) )
                return 1;
        else    return 0;
}

/*
** IsValidYear - is the year a valid value?
**
** Variable     I/O     Description
**
** year         I       year of interest
*/
int IsValidYear ( int year )
{       if ( year < 100 )
                return 0;
        else    return 1;
}
/*========================================================*/
/*
** NumDaysInMonthS - get the number of days in the next N months
**
** month0       .... starting month
** year0        .... starting year
*/
int NumDaysInMonthS ( int month0, int year0, int n )
{       int     i, month, ndays = 0, year;

        month   = month0;
        year    = year0;
        for ( i = 0; i < n; i++ ) {
                ndays += NumDaysInMonth ( month, year );
                ++month;
                if ( month == 13 ) {
                        month = 1;
                        ++year;
                }
        }
        return ndays;
}
/* ----------------------------------------------------------------------------
** GetDateFromJulianHour1900 - get the date from Julian hour information
** ----------------------------------------------------------------------------
** notes:       (1)     This routine gets the date from the Julian hour (which
**                      is relative to Jan 1, 1900).  The Julian hour starts at
**                      zero on this date.  However, the corresponding Julian
**                      day is one (there is no day zero).
**              (2)     This routine makes ABSOLUTELY no assumptions about
**                      time zones, etc.  It is assumed that corrections for
**                      time zones and internal clock offsets are made
**                      elsewhere.  Consequently, this function can be called
**                      from other more specific functions.
** ----------------------------------------------------------------------------
** History:
**
** ?                                            The original version of this
**                                              was based closely on the NWS
**                                              MDYH routines.
** 07-12-95     Steven A. Malers, RTi           Add the +1 offset for the
**                                              Julian day.
** ----------------------------------------------------------------------------
** Variable     I/O     Description
**
** day          L       Day of interest (1-31) after checking value.
** dl           L       Debug level.
** dsflag       L       Flag indicating daylight savings time zone (1)
**                      or not (0).
** hour         L       Hour of interest (1-24) after check value.
** jd           L       Julian day.
** jhour        L       Julian hour since Jan 1, 1900.
** month        L       Month of interest (1-12) after checking value.
** year         L       Year of interest (4 digit) after checking value.
** ----------------------------------------------------------------------------
*/
int GetDateFromJulianHour1900 (       int jhour, int *month,
                                        int *day, int *year, int *hour )
{       int     bflag, dl = 80, i, id1, jd, leap, ndoff;
        char    routine[] = "GetDateFromJulianHour1900";
	char message[256];
 
        jd      = jhour/24 + 1;         /* The + 1 make sure that the day is */
        *hour   = jhour%24;             /* always 0 - 23 */
 
        sprintf ( message,
        "For Julian hour 1900:   %d, Julian day = %d, hour = %d",
        jhour, jd, *hour );
        PrintDebug ( dl, routine, message );
 
        /*
        ** Compute year and check for leap year.  Note that there are 146097
        ** days in 400 years - exactly.  This accounts for leap year, plus every
        ** 100th year not being a leap year unless it is a 400th year as well.
        */
 
        *year = (jd*400)/146097 + 1900;
        sprintf ( message, "First guess at year is %d", *year );
        PrintDebug ( dl, routine, message );
        while ( 1 ) {
               leap = IsLeapYear ( *year );
                /*
                ** "id1" is the Julian day for the guessed year, including
                ** days in previous years but not the current year.
                **
                ** "id1" for jhour 0 = 0, = 1/1/1900:0
                **
                **                                         diff from prev
                ** Consider 1900, which gives id1 = 0           0
                **      and 1901, which gives id1 = 365         365
                **      and 1902, which gives id1 = 730         365
                **      and 1903, which gives id1 = 1095        365
                **      and 1904, which gives id1 = 1460        365
                **
                **      and 1995, which gives id1 = 34698
                **      and 1996, which gives id1 = 35063       365
                **
                */
                id1 =   365*(*year)     + (*year/4)
                                        - (*year/100)
                                        + (*year/400)
                                        - JULIAN1900DAYS
                                        - leap;         /* toss out current
                                                           leap year because it
                                                           is considered in the
                                                           /4 term */
                sprintf ( message, "jd=%d id1=%d leap=%d", jd, id1, leap );
                PrintDebug ( dl, routine, message );
                if ( id1 < jd ) {
                        /*
                        ** For the current year guessed, the total number of
                        ** Julian days is less than that of the Julian year for
                        ** the requested Julian hour.  So, we are either in the
                        ** correct year or one less than the correct year (with
                        ** too many days)...
                        */
                        break;
                }
                else {  /*
                        ** We need to decrement the year.  "id1" will be
                        ** recalculated...
                        */
                        --(*year);
                        sprintf ( message,
                        "Decremented year, now year=%d", *year );
                        PrintDebug ( dl, routine, message );
                }
        }
        sprintf ( message, "After first loop, year=%d leap=%d id1=%d",
        *year, leap, id1 );
        PrintDebug ( dl, routine, message );
 
        /*
        ** Guess at the correct total day for the year being the difference
        ** between the initial Julian day calculated and the one calculated for
        ** our guessed year...
        */
 
        *day    = jd - id1;
        sprintf ( message, "First guess at day is %d", *day );
        PrintDebug ( dl, routine, message );
        while ( 1 ) {
                /*leap = IsLeapYear ( *year );*/
                if ( *day <= (365 + leap) ) {
                        /*
                        ** We are in the middle of the year and so our day is
                        ** OK.
                        */
                        break;
                }
                else {  /*
                        ** We need to increment the year...
                        */
                        ++(*year);
                        /*
                        ** Since we are incrementing the year, the number of
                        ** days left decreases by the number of days in the
                        ** year that we had last...
                        */
                        *day = *day - 365 - leap;
                        sprintf ( message,
                        "Incrementing year, now year=%d leap=%d day=%d",
                        *year, leap, *day );
                        PrintDebug ( dl, routine, message );
                }
        }
        sprintf ( message, "After 2nd loop, year=%d leap=%d day=%d",
        *year, leap, *day );
        PrintDebug ( dl, routine, message );
 
        /*
        ** Find month...
        */
 
        *month = 0;
        if ( *day <= 31 )
                *month = 1;
        if ( *month <= 0 ) {
                bflag = 0;
                for ( i = 3; i <= 12; i++ ) {
                        *month = i - 1;
                        if ( *day <= (MonthYearDays[i - 1] + leap) ) {
                                bflag = 1;
                                break;
                        }
                }
                if ( bflag == 0 )
                        *month = 12;
        }
 
        /*
        **  MONTH KNOWN, COMPUTE DAY OFFSET FROM MONTHa
       */
 
        *day -= MonthYearDays[*month - 1];
        sprintf ( message, "After getting month, month=%d day=%d",
        *month, *day );
        PrintDebug ( dl, routine, message );
        if ( *month >= 3 ) {
                *day -= leap;
        }
        sprintf ( message, "Final day=%d", *day );
        PrintDebug ( dl, routine, message );
 
        return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/time-date.c,v $";
 static char rcs_id2[] = "$Id: time-date.c,v 1.2 2004/09/08 17:14:44 hank Exp $";}
/*  ===================================================  */

}
