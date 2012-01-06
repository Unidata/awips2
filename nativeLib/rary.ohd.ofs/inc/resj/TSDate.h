// ----------------------------------------------------------------------------
// TSDate - time series date class
// ----------------------------------------------------------------------------
// Notes:	(1)	Although a date class could be made generic, for now
//			associate it with the TS++ library.
// ----------------------------------------------------------------------------
// History:
//
// May 96	Steven A. Malers, RTi	Start developing the class based on the
//					HMData HMTimeData structure.
// 24 Jan 97	Matthew J. Rutherford, 	Added TSDATE_STRICT, and TSDATE_FAST
//		RTi.			to help with speed issues brought
//					about by the reset function.
// 11 Mar 97	MJR, RTi		Put in cast to char* for use in print
//					statements. Couldn't figure out how
//					the syntax for defining the operator
//					so I had to inline it.
// 21 May 1997	MJR, RTi		Put in operator= (char*) to handle
//					string to date conversions.
// 16 Jun 1997	MJR, RTi		Added string as private member to 
//					be used on (char*) cast.
// 23 Sep 1997	SAM, RTi		Move some stand-alone functions into
//					the class.  Also move the defines into
//					the class as static data members.
//					Add a constructor that converts the
//					time zone.  Add a void contructor and
//					don't use the C++ default argument
//					feature.
// 20 Feb 1998	MJR, RTi		Added a ! operator so that we can 
//					test a date versus a null date.
// 10 Mar 2004	James R. VanShaar, RTi	Added a getWeekday function.
// 14 Feb 2006	James R. VanShaar, RTi	Added a toNoYearJulianDouble function.
// ----------------------------------------------------------------------------

#ifndef TSDate_INCLUDED
#define TSDate_INCLUDED

#include <string.h>
#include <stdio.h>
#include "ResJ.h"

class TSDate
{

// Static data members...

public:

	static const unsigned int DATE_STRICT;	// Controls how the
	static const unsigned int DATE_FAST;	// date operations
						// (mainly for iteration)
	static const unsigned int DATE_ZERO;	// Initialize the date to zero
						// (default is current date).

	// Flags used in equalsTo...

	static const unsigned int YEAR;
	static const unsigned int MONTH;
	static const unsigned int DAY;
	static const unsigned int HOUR;
	static const unsigned int MINUTE;
	static const unsigned int SECOND;
	static const unsigned int HSECOND;
	static const unsigned int TIME_ZONE;

// Static functions...

public:
	static TSDate getDateFromIndex ( TSDate, int, int, int );
							// Get a date relative
							// to another given an
							// index offset.
	static int getNumIntervals ( TSDate, TSDate, int, int );
							// Get the number of
							// intervals between
							// two dates.

public:
	// The basic member functions...
	TSDate ( void );			// Default constructor.
	TSDate ( unsigned int );		// Constructor using flag.
	TSDate ( const TSDate& );		// Copy constructor.
	TSDate ( const TSDate&, unsigned int );	// Copy constructor using flag.
	TSDate ( const TSDate&, char * );	// Constructor to take a time
						// zone abbreviation..
	~TSDate ( );				// Destructor.
	TSDate& operator= ( const TSDate& );	// Overload =.
	int operator= ( char* );		// Overload =.
	int operator! ( void );			// Overload !.

	int compareTo ( const TSDate& );	// Same as ==
	int equals ( const TSDate& );		// Same as ==.
	int equals ( const TSDate&, unsigned int );
						// Same as ==.  The second
						// argument allows a check only
						// to part of the date.
	int operator== ( const TSDate& );	// Overload ==.
	int notEquals ( const TSDate& );	// Same as !=
	int operator!= ( const TSDate& );	// Overload !=.
	int lessThanOrEqualTo ( const TSDate& );// Same as <=
	int operator<= (const TSDate&);		// overload <=.
	int greaterThanOrEqualTo ( const TSDate& );// Same as >=
	int operator>= (const TSDate&);		// overload >=.
	int lessThan ( const TSDate& );		// Same as <
	int operator < (const TSDate&);		// overload <.
	int greaterThan ( const TSDate& );	// Same as >
	int operator > (const TSDate&);		// overload >.
	char *toString ();			// Same as (char *) cast.
	operator char *();			// a (char*) cast.

	// Member functions more specific to this class...

	int addInterval ( int, int );	// Add interval (interval, mult)
	int addHSecond ( int );		// Add 1/100 second to date.
	int addSecond ( int );		// Add second to date.
	int addMinute ( int );		// Add minute to date.
	int addDay ( int );		// Add day to date.
	int addHour ( int );		// Add hour to date.
	int addMonth ( int );		// Add month to date.
	int addYear ( int );		// Add year to date.

	char *formatTimeString (void);	// Format the date as a string
	char *formatTimeString ( char * );

	int setHSecond ( int );		// Set 1/100 second.
	int setSecond ( int );		// Set second.
	int setMinute ( int );		// Set the minute.
	int setHour ( int );		// Set the hour.
	int setDay ( int );		// Set the day.
	int setMonth ( int );		// Set the month.
	int setYear ( int );		// Set the year.
	int setTimeZone ( int, int );	// Set the time zone by number.
	int setTimeZone ( char * );	// Set the time zone by string.
	int setWeekday( int );		// Set the weekday.
	int setWeekday( );		// Set the weekday based on day, month, year.

	int getAbsMonth( );		// Get the absolute month.
	int getHSecond ( );		// Get 1/100 second.
	int getSecond ( );		// Get second.
	int getMinute ( );		// Get the minute.
	int getHour ( );		// Get the hour.
	int getDay ( );			// Get the day.
	int getMonth ( );		// Get the month.
	int getYear ( );		// Get the year.
	int getTimeZone ( );		// Get the time zone by number.
	char *getTimeZoneAbbr ( );	// Get the time zone by string.
	int getWeekday( int );		// Get the weekday.
	int getYearDay();		// Get the Year Day.

	int shiftTimeZone ( char * );	// Shift the date to a new time zone.
	double toDouble ( void );	// Return a floating-point
					// representation of the date.
	double toNoYearJulianDouble ( void );	// Return a floating-point
					// representation of the Julian day
                                        // without year consideration.

private:
	int init ();			// Initialize an instance to the current
					// date - use this in the constructors.
	int init0 ();			// Initialize to zero date.
	int setAbsMonth();		// Sets the _abs_month data member.
	int setYearDay();		// Sets the _yearday data member.
	int reset ( );			// Reset the secondary data such as
					// leap year based on the primary data.

	int		_hsecond,	// Hundredths of a second (0-99).
			_second,	// Seconds (0-59).
			_minute,	// Minutes past hour (0-59).
			_hour,		// Hours past midnight (0-23).
			_day,		// Day of month (1-31).
			_month,		// Month (1-12).
			_year,		// Year (4 digit)
			_isleap,	// Is the year a leap year (1) or not
					// (0)?
			_weekday,	// Day of week (0=Sunday).
			_yearday;	// Day of year (0-365).
	long int	_abs_month;	// Absolute month (year*12 + month).
	int		_itz,		// Time zone number for standard
					// time (0=GMT).
			_dsflag;	// Daylight savings flag (1 if DS).
	char		_date_string[256],
					// String version of date.
			*_tz;		// Time zone.

	unsigned int	_behavior_flag;	// Flag for special behavior of dates.
};

#endif
