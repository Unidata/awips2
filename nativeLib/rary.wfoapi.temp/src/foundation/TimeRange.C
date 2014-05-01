// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// TimeRange.C
// Concept of a Period of Time based on a Start/End Absolute Time
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const TimeRange_C_Id =
"$Id: .TimeRange.C__temp27950,v 1.3 2003/05/06 23:11:53 fluke Exp $";
#endif

#include "TimeRange.H"
#include <sstream>

// -- module ------------------------------------------------------------------
// The TimeRange class provides the notion of a span of time defined by a
// starting and ending time. The constructors allow the specification of
// starting/ending time, or a time and a duration. If the duration is
// positive (or zero), then the specified time is the start time. If the
// duration is negative, then the specified time is the end time as shown in
// the design document*.
//
// TimeRanges are generally used to define a valid time range for data files.
// TimeRange components (start time, end time, and duration) may be
// retrieved. TimeRanges may not be modified once created. A new TimeRange
// object must be created from an previous one to emulate modification.
// Comparison routines allow the user to determine the relationships between
// two TimeRanges.
//
// A time range is exclusive of the ending time. For example, a start time of
// February 9, 1993 01:00:00 UTC and a duration of 1 hour indicates that
// the time range is from February 9, 1993 01:00:00 UTC up to but not
// including February 9, 1993 01:00:00 UTC.
//
// Routines are provided to calculate intersections, unions, spans, and gaps
// between two time ranges. An invalid time range object is used to indicate a
// that no intersection or gap is present. A routine is provided to check
// whether a time range is valid or not.
//
// TimeRanges with a zero duration are defined differently than a TimeRange
// with a duration. A TimeRange with a zero duration is defined as the
// starting time including the ending time which is analogous to a AbsTime.
// A zero duration time range only can contain another time range that is
// a zero duration time range with the same starting time. Zero duration
// timeranges only contain AbsTimes that are equal to the starting time of
// the zero duration time range.
// -- implementation ----------------------------------------------------------
// The TimeRange object is internally represented by two AbsTime objects.
// The ending time is not included in the TimeRange due to ambiguties.
//-----------------------------------------------------------------------------

// these two functions are here since the templates for max/min don't
// seem to be working for the AbsTime case.  Refer to commonDefs.h.
static inline AbsTime max(AbsTime a, AbsTime b)
    {
    return (a > b) ? a : b;
    }
static inline AbsTime min(AbsTime a, AbsTime b)
    {
    return (a < b) ? a : b;
    }

// -- public ------------------------------------------------------------------
// TimeRange::TimeRange()
// Default time range constructor.
// -- implementation ----------------------------------------------------------
// Sets _valid to false to indicate that the time range is undefined.
//-----------------------------------------------------------------------------
TimeRange::TimeRange()
    {
    _valid = false;
    }

// -- public ------------------------------------------------------------------
// TimeRange::TimeRange()
// Constructor for creating a time range based on a base time and duration.
// -- implementation ----------------------------------------------------------
// The other time for the time range is calculated by adding the duration
// to the end time. The start time is determined from the minimum of
// the two times and the ending time from the maximum of the two times.
//-----------------------------------------------------------------------------
TimeRange::TimeRange(const AbsTime &baseTime, long duration)
    {
    _start = min(baseTime, baseTime + duration);
    _end = max(baseTime, baseTime + duration);
    _valid = (_start != _end) ? true : false;
    }

// -- public ------------------------------------------------------------------
// TimeRange::TimeRange()
// Constructor for creating a time range based on two AbsTimes.
// -- implementation ----------------------------------------------------------
// The start time is determined by the earlier of the specified times and
// the ending time by the later of the specified times.
//-----------------------------------------------------------------------------
TimeRange::TimeRange(const AbsTime &time1, const AbsTime &time2)
    {
    _start = min(time1, time2);
    _end = max(time1, time2);
    _valid = (_start != _end) ? true : false;
    }

// -- public ------------------------------------------------------------------
// TimeRange::join()
// Joins two time ranges and returns a new TimeRange.  Returns an invalid
// TimeRange if there is gap between the specified time ranges and this
// TimeRange which can be checked with the isValid() routine.
// An invalid TimeRange is returned if an invalid time range is accessed.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
TimeRange TimeRange::join(const TimeRange& timeRange) const
    {
    if (_valid && timeRange._valid
      && (overlaps(timeRange) || isAdjacentTo(timeRange)))
        {
        return TimeRange(min(_start, timeRange.startTime()),
          max(_end, timeRange.endTime()));
        }
    else
        return TimeRange();  // an invalid time range
    }

// -- public ------------------------------------------------------------------
// TimeRange::gap()
// Returns the gap between two time ranges.  If there is no gap, then
// an invalid time range is returned which can be checked with the isValid()
// routine.
// An invalid TimeRange is returned if an invalid time range is accessed.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
TimeRange TimeRange::gap(const TimeRange& timeRange) const
    {
    if (!_valid || !timeRange._valid || overlaps(timeRange)
      || isAdjacentTo(timeRange))
        return TimeRange(); // an invalid time range
    else
        {
        if(_start > timeRange.startTime())
           return TimeRange(timeRange.endTime(), _start);
        else
           return TimeRange(_end, timeRange.startTime());
        }
    }

// -- public ------------------------------------------------------------------
// TimeRange::durationAsPrettyString()
//
// Returns the duration of this TimeRange as a TextString.
//
//-----------------------------------------------------------------------------
TextString TimeRange::durationAsPrettyString() const
    {
    static const long sec_per_day = 3600 * 24;

    long dur = duration();
    long days = dur / sec_per_day;
    long hours = (dur - days * sec_per_day) / 3600;
    long min = (dur - days * sec_per_day - hours * 3600) / 60;
    long sec = dur - days * sec_per_day - hours * 3600 - min * 60;

    std::ostringstream os;

    bool spaceNeeded = false;

    if (days)
        {
        os << days << 'd';
        spaceNeeded = true;
        }
    if (hours || min || sec)
        {
        if (spaceNeeded)
            os << ' ';
        os << hours << 'h';
        spaceNeeded = true;
        }
    if (min || sec)
        {
        if (spaceNeeded)
            os << ' ';
        os << min << 'm';
        spaceNeeded = true;
        }
    if (sec)
        {
        if (spaceNeeded)
            os << ' ';
        os << sec << 's';
        }

    os << std::ends;

    return TextString(os.str().c_str());
    }


// -- public ------------------------------------------------------------------
// TimeRange::allTimes()
// Returns a time range that spans all possible times.
// -- implementation ----------------------------------------------------------
// Uses AbsTime::maxFutureTime().
//-----------------------------------------------------------------------------
TimeRange TimeRange::allTimes()
    {
    return TimeRange(AbsTime(0), AbsTime::maxFutureTime());
    }
