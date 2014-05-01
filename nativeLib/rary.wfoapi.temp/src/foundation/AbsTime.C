// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// AbsTime.C
// Absolute Time Class
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const AbsTime_C_Id =
"$Id: .AbsTime.C__temp7312,v 1.3 2005/07/29 20:57:58 fluke Exp $";
#endif

// -- module -----------------------------------------------------------------
// The absolute time class provides the notion of date (month/day/year) and
// time-of-day (hour, minute, second) into a single object with a resolution
// of one second. It supports only UTC time zone.
//
// Arithmetic operations permit addition and subtraction of a duration from
// an AbsTime. Two AbsTimes may be subtracted to yield a duration. A full
// set of relational operators allow for comparison of two AbsTimes.
//
// Conversion routines provide for converting the UNIX time_t type into an
// AbsTime object and visa versa. An AbsTime object may also be converted
// to a TextString for output.
//
// AbsTime is Year 2000 Compliant.  The constructor has been changed
// to only allow a 4-digit year.  In the event of bad constructor values,
// this AbsTime will be set to the default AbsTime.
//
// -- implementation ----------------------------------------------------------
// The internal representation of time for this class is time_t which is
// the same as time_t which is the
// that used by the UNIX file system, thus allowing easy conversion from
// UNIX time to the internal representation and vice versa. Arithmetic and
// relational operators are efficient due to the single variable representing
// time.
//
// The individual time components (e.g., hour) that serve as input to the time
// constructors are not automatically normalized to represent proper time.
//
// The time class makes heavy use of the C library routines described in
// ctime(3) to format the Strings and to convert the internal representation of
// time to the struct tm *tm structure containing individual components of
// time.
//-----------------------------------------------------------------------------
#define _XOPEN_SOURCE // needed for strptime with glibc
#include <time.h>
#include <limits.h>
#include <string.h>
#include "AbsTime.H"
#include "LogStream.H"
#include <stdlib.h>

// -- public ------------------------------------------------------------------
// AbsTime::AbsTime()
// Constructor to create AbsTime with specified time components.  Year
// specified is the four-digit year.  Month is from 1-12.  Day is from 1-31.
// Hour is from 0-23.  Minute and second is from 0-59.
// Proper input values are verified and program logs a problem if they
// are incorrect. If a problem is detected, then the AbsTime will have a
// value of (time_t)0, or Jan 1 1970 at 00:00:00 GMT.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
AbsTime::AbsTime(int year, int month, int day, int hour, int minute,
  int second)
    {
    bool badData = false;

#ifdef ALLOW_2_DIGIT_YEAR
    // normalize the year to the 4-digit if the user only specified 2 digits
    // or if the year is specified to be 100.

    if (year <= 100)
        {
        if (year >= 70)   // 1970
            year += 1900;
        else if (year <= 38)  // 2038
            year += 2000;

        // logBug
        //  << "AbsTime constructed with 2-digit year - NOT YEAR 2000 Compliant"
        //  << std::endl;

        }
#endif

     // verify proper input values
    if (second < 0 || minute < 0 || hour < 0 || day < 1 || month < 1
      || year < 1970)
         badData = true;
    else if (second > 59 || minute > 59 || hour > 23
      || month > 12 || year > 2038
      || day > daysInMonth(month, year))
          badData = true;

    if(badData)
        {
        logBug << "Improper time values for AbsTime::AbsTime()" << std::endl;
        _time = 0;
        }

    // fill up time structure with specified data
    else
        _time = timeFromComponents(year,month,day,hour,minute,second);
    }

// -- public ------------------------------------------------------------------
// AbsTime::AbsTime()
// strptime() based ctor.
//-----------------------------------------------------------------------------
AbsTime::AbsTime(const TextString &str, const TextString &format)
    {
    struct tm tstruct;
    memset(&tstruct, '\0', sizeof(tstruct));
    if (strptime(str.stringPtr(), format.stringPtr(), &tstruct) == NULL)
        {
        logProblem << "AbsTime::AbsTime(): strptime failed. str:"
          << str << ", fmt:" << format << std::endl;
        _time = 0;
        }
    else
        {
        // mktime resets the timezone global variable incorrectly, so
        // reset it by calling tzset().
        _time = mktime(&tstruct);
        tzset();
        _time -= timezone;
        }
    }

// -- public ------------------------------------------------------------------
// AbsTime::year()
// Component extractor for year.  Returns four-digit year.
//-----------------------------------------------------------------------------
int AbsTime::year() const
    {
    struct tm *components;
    components = componentsFromTime();
    return 1900 + components->tm_year;
    }

// -- public ------------------------------------------------------------------
// AbsTime::month()
// Component extractor for month.  Returns month (1-12).
//-----------------------------------------------------------------------------
int AbsTime::month() const
    {
    struct tm *components;
    components = componentsFromTime();
    return 1 + components->tm_mon;
    }

// -- public ------------------------------------------------------------------
// AbsTime::day()
// Component extractor for day.  Returns day of month (1-31).
//-----------------------------------------------------------------------------
int AbsTime::day() const
    {
    struct tm *components;
    components = componentsFromTime();
    return components->tm_mday;
    }

// -- public ------------------------------------------------------------------
// AbsTime::hour()
// Component extractor for hour.  Returns hour of day (0-23).
//-----------------------------------------------------------------------------
int AbsTime::hour() const
    {
    struct tm *components;
    components = componentsFromTime();
    return components->tm_hour;
    }

// -- public ------------------------------------------------------------------
// AbsTime::minute()
// Component extractor for minute of hour.  Returns minute (0-59).
//-----------------------------------------------------------------------------
int AbsTime::minute() const
    {
    struct tm *components;
    components = componentsFromTime();
    return components->tm_min;
    }

// -- public ------------------------------------------------------------------
// AbsTime::second()
// Component extractor for second of minute.  Returns second (0-59).
//-----------------------------------------------------------------------------
int AbsTime::second() const
    {
    struct tm *components;
    components = componentsFromTime();
    return components->tm_sec;
    }

// -- public ------------------------------------------------------------------
// AbsTime::weekday()
// Component extractor for day of week.  Returns day of week (0-6).
// Sunday is indicated by a 0.
//-----------------------------------------------------------------------------
int AbsTime::weekday() const
    {
    struct tm *components;
    components = componentsFromTime();
    return components->tm_wday;
    }

// -- public ------------------------------------------------------------------
// --for cldhgt--
// AbsTime::julianDay() const
// Component extractor for julian day of year.  Returns day of year (1-365).
// (1-366) in leap year.
//-----------------------------------------------------------------------------
int AbsTime::julianDay() const
    {
    struct tm *components;
    components = componentsFromTime();
    return components->tm_yday+1;
    }


// -- public ------------------------------------------------------------------
// AbsTime::string()
// Formats an AbsTime into a TextString based on the specified format.
// The format is defined in the ctime man page for the
// strftime() call.  Errors in the formatting produce a problem message and
// returns an empty string.
//-----------------------------------------------------------------------------
TextString AbsTime::string(const TextString& format) const
    {
    const int size = 200;
    char data[size];
    struct tm *timeStructure;
    timeStructure = componentsFromTime();
    if (strftime(data, size, format.stringPtr(), timeStructure) <= 0)
        {
        // log warning
        logBug << "AbsTime string formatting problem, empty string returned"
          << std::endl;
        return TextString(); // warning
        }
    else
        return TextString(data);
    }

// -- public ------------------------------------------------------------------
// AbsTime::maxFutureTime()
// Return an AbsTime with the largest possible value of time_t.
//-----------------------------------------------------------------------------
AbsTime AbsTime::maxFutureTime()
    {
    // We are assuming that time_t contains signed values.
    return AbsTime(time_t(~(1L << (CHAR_BIT * sizeof (time_t) - 1))));
    }

// -- protected ---------------------------------------------------------------
// AbsTime::componentsFromTime()
// Obtains components (e.g., month, day, hour) from this AbsTime.
// Returns a pointer to structure tm containing the
// component time.
// -- implementation ----------------------------------------------------------
// Returned pointer points to static data which is overwritten by the
// next call.
//-----------------------------------------------------------------------------
struct tm* AbsTime::componentsFromTime() const
    {
    return gmtime(&_time);
    }

// -- protected ---------------------------------------------------------------
// AbsTime::timeFromComponents()
// Obtains time from individual components. Returns a UNIX time.
// -- implementation ----------------------------------------------------------
// Function determines the UNIX time using brute force since there is no
// UNIX call to perform this conversion. CHECK THIS ON THE HP?
//-----------------------------------------------------------------------------
time_t AbsTime::timeFromComponents(int year, int month, int day,
  int hour, int minute, int second) const
    {
    int epochDays = 0;
    time_t epochTime;

    int pyear = 1970, pmonth = 1;  //epoch time start

    while(pyear != year)
        {
        epochDays += 365;   // days in year
        if (leapYear(pyear))
           epochDays++;    // account for leap year
        pyear++;
        }

    while(pmonth != month)
        {
        epochDays += daysInMonth(pmonth, year);
        pmonth++;
        }

    epochDays += day - 1;   // but not this day

    epochTime = epochSeconds(epochDays, (int)hmsToSecs(hour,minute,second));

    return epochTime;
    }

// -- protected ---------------------------------------------------------------
// AbsTime::daysInMonth()
// Returns the number of days in the specified month and year.  The year is a
// four-digit number and month ranges from one to twelve.
// -- implementation ----------------------------------------------------------
// Contains a table of the number of days in each month of a conventional
// year.  For Feburary, it uses the leapYear() function to determine whether
// the specified year is a leap year.
//-----------------------------------------------------------------------------
int AbsTime::daysInMonth(int month, int year)
    {
    static int days[12] =
        { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

    if (month < 1 || month > 12)
        {
        logBug << "daysInMonth supplied with bad month number" << std::endl;
        return 0;
        }

    if (month != 2)
        return days[month-1];

    // special February handling for leap years
    if (leapYear(year))
        return days[1]+1;
    else
        return days[1];
    }
