// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// AbsTime.H
// Absolute Time Class
//-----------------------------------------------------------------------------
#ifndef _AbsTime_H
#define _AbsTime_H

#ifdef IDENT_H
static const char* const AbsTime_H_Id =
"$Id: .AbsTime.H__temp27950,v 1.2 2003/05/06 23:11:32 fluke Exp $";
#endif

#include <time.h>
#include "commonDefs.h"
#include <sys/types.h>
#include <sys/time.h>
#include "TextString.H"
#include <iostream>

class AbsTime
    {
    public:

        // constructors and destructors
        AbsTime();
        AbsTime(time_t unixTime);
        AbsTime(int year, int month, int day,
          int hour = 0, int minute = 0, int second = 0);
        // Julian date ctor.  ndays = days into year.
        AbsTime(int year, int ndays);
        // strptime() based
        AbsTime(const TextString &str, const TextString &format);

        // Compiler generated copy ctor and assign op.

        // component extractors
        int year() const;
        int month() const;
        int day() const;
        int hour() const;
        int minute() const;
        int second() const;
        int weekday() const;

        // --for cldhgt--
        int julianDay() const;

        // relation operators
        bool operator==(const AbsTime &right) const;
        bool operator!=(const AbsTime &right) const;
        bool operator<=(const AbsTime &right) const;
        bool operator>(const AbsTime &right) const;
        bool operator>=(const AbsTime &right) const;
        bool operator<(const AbsTime &right) const;

        // arithmetic operators
        AbsTime operator+(long duration) const;
        AbsTime operator-(long duration) const;
        long operator-(const AbsTime &right) const;
        long operator%(unsigned long divisor) const;

        AbsTime& operator+=(long duration);
        AbsTime& operator-=(long duration);

        // Unsigned operator - its return value is ONLY suitable for use as
        // a hash value. It is not a time.
//        operator unsigned int() const;

        // string format
        TextString string() const;
        TextString string(const TextString& format) const;

        // conveience routines to obtain current time and the greatest
        // possible future time.
        static AbsTime current();
        static AbsTime maxFutureTime();

        // conversion operator
        time_t unixTime() const;  // returns unix time equivalent

    private:
        time_t _time;

        struct tm *componentsFromTime() const;
        time_t timeFromComponents(int year, int month, int day, int hour,
          int minute, int second) const; // create time_t

        static time_t hmsToSecs(int hours, int mins, int secs);
        static time_t epochSeconds(int epochDays, int secondOfDay);
        static bool leapYear(int year);  // is year a leap year?
        static int daysInMonth(int month, int year);

        // following are not implemented to prevent promotion from
        // int to AbsTime
        bool operator==(long);
        bool operator!=(long);
        bool operator<=(long);
        bool operator>(long);
        bool operator>=(long);
        bool operator<(long);
        AbsTime& operator=(long);
    };

//: Arithmetic operator.
AbsTime operator+(long duration, AbsTime time);
//: Arithmetic operator.
std::ostream& operator<<(std::ostream& o, const AbsTime &time);

//: Not Implemented.
bool operator==(int, const AbsTime &);
//: Not Implemented.
bool operator!=(int, const AbsTime &);
//: Not Implemented.
bool operator<=(int, const AbsTime &);
//: Not Implemented.
bool operator>(int, const AbsTime &);
//: Not Implemented.
bool operator>=(int, const AbsTime &);
//: Not Implemented.
bool operator<(int, const AbsTime &);
//: Not Implemented.
int operator-(int, const AbsTime &);

// -- public ------------------------------------------------------------------
// AbsTime::unixTime()
// Returns UNIX time equivilant of this object.
//-----------------------------------------------------------------------------
inline time_t AbsTime::unixTime() const
    {
    return _time;
    }

// -- public ------------------------------------------------------------------
// AbsTime::string()
// Returns the default TextString representation of this AbsTime.
//-----------------------------------------------------------------------------
inline TextString AbsTime::string() const
    {
    return string("%h %d %y %T GMT");
    }

// -- global ------------------------------------------------------------------
// operator<<()
// Outputs in ASCII the value of an AbsTime object.
//-----------------------------------------------------------------------------
inline std::ostream& operator<<(std::ostream& o, const AbsTime &time)
    {
    return o << time.string();
    }

// -- public ------------------------------------------------------------------
// AbsTime::AbsTime()
// Constructor to create AbsTime with UNIX epoch time of zero.
//-----------------------------------------------------------------------------
inline AbsTime::AbsTime()
    {
    _time = 0;
    }

// -- public ------------------------------------------------------------------
// AbsTime::AbsTime()
// Constructor to create a new AbsTime object with the specified unixTime.
//-----------------------------------------------------------------------------
inline AbsTime::AbsTime(time_t unixTime)
    {
    _time = unixTime;
    }

// -- public ------------------------------------------------------------------
// AbsTime::AbsTime()
// Constructor to create a new AbsTime object with the specified
// Julian year and day.
//-----------------------------------------------------------------------------
inline AbsTime::AbsTime(int year, int ndays)
    {
    time_t y = timeFromComponents(year, 1, 1, 0, 0, 0);
    _time = y + (ndays - 1) * 3600 * 24;
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator==()
// Compares two AbsTimes for equality.  Returns true if equal.
//-----------------------------------------------------------------------------
inline bool AbsTime::operator==(const AbsTime &right) const
    {
    return (_time == right._time);
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator!=()
// Compares two AbsTimes for inequality.  Returns true if not equal.
//-----------------------------------------------------------------------------
inline bool AbsTime::operator!=(const AbsTime &right) const
    {
    return (_time != right._time);
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator<=()
// Compares one AbsTime against another. Returns true if this AbsTime is
// less than or equal to the second.
//-----------------------------------------------------------------------------
inline bool AbsTime::operator<=(const AbsTime &right) const
    {
    return (_time <= right._time);
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator>()
// Compares one AbsTime against another.  Returns true if this AbsTime is
// later than the second.
//-----------------------------------------------------------------------------
inline bool AbsTime::operator>(const AbsTime &right) const
    {
    return (_time > right._time);
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator>=()
// Compares one AbsTime against another.  Returns true if this AbsTime is
// later than or equal to the second.
//-----------------------------------------------------------------------------
inline bool AbsTime::operator>=(const AbsTime &right) const
    {
    return (_time >= right._time);
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator<()
// Compares one AbsTime against another.  Returns true if this AbsTime is
// earlier than the second.
//-----------------------------------------------------------------------------
inline bool AbsTime::operator<(const AbsTime &right) const
    {
    return (_time < right._time);
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator%()
// Returns the modulus of this AbsTime given a divisor duration.
//-----------------------------------------------------------------------------
inline long AbsTime::operator%(unsigned long divisor) const
    {
    return (_time % divisor);
    }

// -- public ------------------------------------------------------------------
// Abstime::operator+()
// Adds a duration to this AbsTime and returns a new AbsTime object.
//-----------------------------------------------------------------------------
inline AbsTime AbsTime::operator+(long duration) const
    {
    return AbsTime((time_t)(_time + duration));
    }

// -- global ------------------------------------------------------------------
// operator+()
// Adds a time to a duration and returns a new AbsTime object.
//-----------------------------------------------------------------------------
inline AbsTime operator+(int duration, AbsTime time)
    {
    return AbsTime((time_t)(duration + time.unixTime()));
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator-()
// Subtracts a duration from this AbsTime and returns a new AbsTime object.
//-----------------------------------------------------------------------------
inline AbsTime AbsTime::operator-(long duration) const
    {
    return AbsTime((time_t)(_time - duration));
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator-()
// Subtracts an AbsTime from another AbsTime and returns the interval of
// time between them.  The interval of time is known as the duration.
//-----------------------------------------------------------------------------
inline long AbsTime::operator-(const AbsTime &right) const
    {
    return (_time - right._time);
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator+=()
// Adds the specified duration to an existing AbsTime object.
//-----------------------------------------------------------------------------
inline AbsTime& AbsTime::operator+=(long duration)
    {
    _time += (time_t)duration;
    return *this;
    }

// -- public ------------------------------------------------------------------
// AbsTime::operator-=()
// Subtracts the specified duration from an existing AbsTime object.
//-----------------------------------------------------------------------------
inline AbsTime& AbsTime::operator-=(long duration)
    {
    _time -= (time_t)duration;
    return *this;
    }

// -- public ------------------------------------------------------------------
// AbsTime::current()
// Creates a new AbsTime object with the present time.
//-----------------------------------------------------------------------------
inline AbsTime AbsTime::current()
    {
    return AbsTime(time(0));
    }

// -- protected ---------------------------------------------------------------
// AbsTime::hmsToSecs()
// Converts hours, minutes, and seconds into seconds.
//-----------------------------------------------------------------------------
inline time_t AbsTime::hmsToSecs(int hours, int minutes,
  int secs)
    {
    return (hours*3600) + (minutes*60) + secs;
    }

// -- protected ---------------------------------------------------------------
// AbsTime::epochSeconds()
// Converts epoch days and second of day into epoch seconds.  Epoch is
// defined as January 1, 1970 at 0000 UTC.
//-----------------------------------------------------------------------------
inline time_t AbsTime::epochSeconds(int epochDays,
  int secondOfDay)
    {
    return (epochDays * 86400) + secondOfDay;
    }

// -- protected ---------------------------------------------------------------
// AbsTime::leapYear()
// Returns true if the specified year is a leap year.
//-----------------------------------------------------------------------------
inline bool AbsTime::leapYear(int year)
    {
    return (year % 4 == 0 && (year+100) % 400 != 0);
    }

// -- public ------------------------------------------------------------------
// AbsTime::unsigned int()
// Returns true if the specified year is a leap year.
//-----------------------------------------------------------------------------
//inline AbsTime::operator unsigned int() const
    //{
    //return (unsigned int)_time;
    //}

#endif
