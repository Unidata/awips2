// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// TimeRange.H
// Concept of a Period of Time based on a Start/End Absolute Time
//-----------------------------------------------------------------------------
#ifndef _TimeRange_H
#define _TimeRange_H

#ifdef IDENT_H
static const char* const TimeRange_H_Id =
"$Id: .TimeRange.H__temp10480,v 1.4 2004/02/04 23:14:33 fluke Exp $";
#endif

#include "commonDefs.h"
#include "AbsTime.H"

class TimeRange
    {
    public:

        // constructors and destructors
        TimeRange();
        TimeRange(const AbsTime &baseTime, long duration);
        TimeRange(const AbsTime &time1, const AbsTime &time2);

        // component extractors
        const AbsTime &startTime() const;
        const AbsTime &endTime() const;
        AbsTime centerTime() const;
        long duration() const;
        TextString string() const;
        TextString durationAsPrettyString() const;

        static TimeRange allTimes();

        // equality operations
        bool operator==(const TimeRange& timeRange) const;
        bool operator!=(const TimeRange& timeRange) const;

        // comparision operations
        bool contains(const AbsTime &time) const;
        bool contains(const TimeRange& timeRange ) const;
        bool overlaps(const TimeRange& timeRange) const;
        bool isAdjacentTo(const TimeRange& timeRange) const;

        // validity operation
        bool isValid() const;  // checks for a valid time range object

        // other operations
        TimeRange join(const TimeRange& timeRange) const;
        TimeRange intersection(const TimeRange& timeRange) const;
        TimeRange gap(const TimeRange& timeRange) const;
        TimeRange span(const TimeRange& timeRange) const;
        TimeRange combineWith(const TimeRange& timeRange) const;

    private:
        AbsTime _start;   // time range starting time
        AbsTime _end;    // time range ending time
        bool _valid;    // is this time range valid?
    };

// output operator
std::ostream& operator<<(std::ostream& o, const TimeRange& timeRange);


// -- public ------------------------------------------------------------------
// TimeRange::startTime()
// Returns the starting time for the time range.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline const AbsTime &TimeRange::startTime() const
    {
    return _start;
    }

// -- public ------------------------------------------------------------------
// TimeRange::centerTime()
// Returns the center time for the time range.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline AbsTime TimeRange::centerTime() const
    {
    time_t diff = _end.unixTime() - _start.unixTime(); 
    return AbsTime(_start.unixTime() + (diff/2));
    }

// -- public ------------------------------------------------------------------
// TimeRange::endTime()
// Returns the ending time for the time range.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline const AbsTime &TimeRange::endTime() const
    {
    return _end;
    }

// -- public ------------------------------------------------------------------
// TimeRange::duration()
// Returns the duration for the time range. Duration is the starting time
// subtracted from the ending time.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline long TimeRange::duration() const
    {
    return (_end - _start);
    }

// -- public ------------------------------------------------------------------
// TimeRange::operator==()
// Tests two time ranges for equality and returns true if equal.
// If either timeRange is invalid, then false is returned.  If both
// timeRanges are invalid, then true is returned.
//-----------------------------------------------------------------------------
inline bool TimeRange::operator==(const TimeRange& timeRange) const
    {
    if (_valid && timeRange._valid)
        return (_start == timeRange._start && _end == timeRange._end);
    else if (!_valid && !timeRange._valid)
        return true;
    else
        return false;
    }

// -- public ------------------------------------------------------------------
// TimeRange::operator!=()
// Tests two time ranges for non-equality and returns true if not equal.
// If either timeRange is invalid, then true is returned since the two
// time ranges cannot possibly be equal.
// -- implementation ----------------------------------------------------------
// Uses operator==().
//-----------------------------------------------------------------------------
inline bool TimeRange::operator!=(const TimeRange& timeRange) const
    {
    return !operator==(timeRange);
    }

// -- public ------------------------------------------------------------------
// TimeRange::contains()
// Returns true if an absolute time is contained within this time range.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
// Must check for time ranges with zero duration.
//-----------------------------------------------------------------------------
inline bool TimeRange::contains(const AbsTime &time) const
    {
    // the end time is not part of the time range (hence the < operator)
    if (duration() != 0)
        return (time >= _start && time < _end);
    else
        return (time == _start);  // special case for zero duration timerange
    }

// -- public ------------------------------------------------------------------
// TimeRange::contains()
// Returns true if an time range is contained within this time range.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
// Must check for time ranges with zero duration.  These are treated
// like an AbsTime.
//-----------------------------------------------------------------------------
inline bool TimeRange::contains(const TimeRange& timeRange) const
    {
    if (duration() == 0)
        return (*this == timeRange);
    else if (timeRange.duration() == 0)
        return (contains(timeRange.startTime()));
    else
        {
        // the end time is not part of the time range (hence the < operator)
        // but since we are comparing two end times from the time ranges, we
        // can use the <= operator.
        return (timeRange._start >= _start && timeRange._end <= _end);
        }
    }

// -- public ------------------------------------------------------------------
// TimeRange::isAdjacentTo()
// Returns true if another time range is adjacent to this time range.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
// Returns true if the ending time of this time range is equal
// to the starting time of the specified time range or the starting
// of this time range is equal to the ending time of the specified
// time range.
//-----------------------------------------------------------------------------
inline bool TimeRange::isAdjacentTo(const TimeRange& timeRange) const
    {
    return (_start == timeRange._end || _end == timeRange._start);
    }

// -- public ------------------------------------------------------------------
// TimeRange::isValid()
// Returns true if this time range represents a valid time range.
//-----------------------------------------------------------------------------
inline bool TimeRange::isValid() const
    {
    return _valid;
    }

//-----------------------------------------------------------------------------
// TimeRange::overlaps()
// Checks if this time range overlaps in any way another time range.
// Returns true if any part of this time range is part of the specified
// time range.
// Undefined results are returned for invalid time ranges. The user should
// use isValid() to ensure that this is a valid time range.
// -- implementation ----------------------------------------------------------
// Special code checks for zero duration time ranges which are treated as
// if they were AbsTimes.
//-----------------------------------------------------------------------------
inline bool TimeRange::overlaps(const TimeRange& timeRange) const
    {
    if (timeRange.contains(_start) || contains(timeRange.startTime()))
        return true;
    else
        return false;
    }

// -- public ------------------------------------------------------------------
// TimeRange::intersection()
// Returns the intersection of two time ranges.  If there is no intersection,
// then an invalid TimeRange is returned which can be checked with the
// isValid() routine.
// An invalid TimeRange is returned if an invalid time range is accessed.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline TimeRange TimeRange::intersection(const TimeRange& timeRange) const
    {
    if(_valid && timeRange._valid && overlaps(timeRange))
        return TimeRange(max(_start, timeRange.startTime()),
          min(_end, timeRange.endTime()));
    else
        return TimeRange();  // invalid time range
    }

// -- public ------------------------------------------------------------------
// TimeRange::span()
// Returns the span for two time ranges.
// An invalid TimeRange is returned if an invalid time range is accessed.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline TimeRange TimeRange::span(const TimeRange& timeRange) const
    {
    if (_valid && timeRange._valid)
        return TimeRange(min(_start,timeRange.startTime()),
          max(_end, timeRange.endTime()));
    else
        return TimeRange(); // an invalid time range
    }

// -- public ------------------------------------------------------------------
// TimeRange::combineWith()
// Returns the combined span for two time range.
// If either of the time ranges are invalid, then this function returns
// simply the other's valid time.
// -- implementation ----------------------------------------------------------
// This routine is similar to span() except that invalid input time ranges
// do not necessarily result in an invalid output time range.
//-----------------------------------------------------------------------------
inline TimeRange TimeRange::combineWith(const TimeRange& timeRange) const
    {
    if (_valid && timeRange._valid)
        return TimeRange(min(_start,timeRange.startTime()),
          max(_end, timeRange.endTime()));
    else if (_valid)
        return *this;
    else
        return timeRange;
    }

// -- public ------------------------------------------------------------------
// TimeRange::string()
// Returns a TextString representation of this time range.
//-----------------------------------------------------------------------------
inline TextString TimeRange::string() const
    {
    if (isValid())
        return TextString("(") + startTime().string()
          + ", " + endTime().string() + ")";
    else
        return "(Invalid)";
    }

// -- global ------------------------------------------------------------------
// operator<<()
// Output operator for TimeRange object.
// -- implementation ----------------------------------------------------------
// Has special output format for an invalid TimeRange object.  Uses AbsTime
// output operator for output of starting and ending times.
//-----------------------------------------------------------------------------
inline std::ostream& operator<<(std::ostream& o, const TimeRange& timeRange)
    {
    return o << timeRange.string();
    }

#endif
