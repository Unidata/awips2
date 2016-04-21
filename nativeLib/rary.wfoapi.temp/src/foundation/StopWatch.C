//-------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// StopWatch.C
// A code timing class.
//
// Author:   Gerry Murray
//-------------------------------------------------------------------

#ifdef IDENT_C
static const char* const StopWatch_C_Id =
  "$Id: .StopWatch.C__temp27950,v 1.2 2003/05/06 23:11:50 fluke Exp $";
#endif


// -- module ----------------------------------------------------------------- 
// To use, do the following:
// Declare an object from the StopWatch class.  Be careful of
// scope. If the start and stop timing locations are in different
// modules or classes, then StopWatch has to be an external global.
// If you are interested in repeated timings of the same code and
// receiving an average, then declare the stop watch object outside of
// the body of the loop, or declare the object static.
//
// Using the declared StopWatch object, call the start method directly
// before the line of code that starts the path of execution that you
// want timed.  Call the stop method directly after the last line of
// code of the timed path of execution.  This method will return a
// struct containing the following data:
//   a: world time between the two methods.
//   b: cpu time between the two methods spend executing your code.
//   c: cpu time between the two methods spend executing the system code.
//   d: the average world time of the current and all previous timings.
//
// -- implementation ---------------------------------------------------------
// There is a wide variety of BSD and SYSV time utility calls to
// choose from, but we chose "times" since it has a granularity of
// micro-seconds and delivers CPU time in addition to wall clock time.
//
// The following warning is extracted from the man pages for the UNIX
// call, "times". "Not all CPU time expended by system processes on
// behalf of a user process is counted in the system CPU time for that
// process."
//-------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include "StopWatch.H"
#include "LogStream.H"


// -- public ------------------------------------------------------------------ 
// void StopWatch::start (void)
//
// Starts the timing of the code nested between the call of this
// method and the stop method.
//-----------------------------------------------------------------------------
void StopWatch::start (void)
    {
    times (&_startCPUTime);
    timeval startTime;
    gettimeofday (&startTime, 0);
    _startTime =  startTime.tv_sec +
        (double) (startTime.tv_usec / 1000000.0);
    _running = true;
    }

// -- public ------------------------------------------------------------------ 
// double StopWatch::getSplit (void)
//
// Returns time in seconds since start() was called.  If watch
// is stopped, returns the same as wallClockTime()
//
//-----------------------------------------------------------------------------
double StopWatch::getSplit (void)
    {
    if (_running)
        {
        timeval endTime;
        gettimeofday (&endTime, 0);
        return (endTime.tv_sec + (double) (endTime.tv_usec / 1000000.0)) -
        _startTime;
        }
    else
        return _wallClockTime;
    }


// -- public ------------------------------------------------------------------ 
// void StopWatch::stop (void)
//
// Ends the timing of the code nested between the call of the start
// method and this method.  This method computes or updates four different
// kinds of times, which the client can inquire using the four access methods:
// "wallClockTime", "userCPU_Time" "systemCPU_time", "avgWallClockTime".
// 
// -- implementation ----------------------------------------------------------
// The system call "times" is used to get the current time.  "Times"
// returns its time in the UNIX time format, which is time in clock
// tick units rather than seconds.  Since the user is not interested
// in clock ticks, it needs to be converted.
//-----------------------------------------------------------------------------
void StopWatch::stop (void)
    {
    //if the start method was never called, then make sure all the
    //times are set to null values.  Also, log an error.
    if (!_running)
        {
        _wallClockTime = _userCPU_Time = _systemCPU_Time = 0;
        logBug << "The timing must be started or restarted "
               << std::endl
               << "by the start method before the timing can be stopped."
               << std::endl;
        return;
        }

    _running = false;
    
    // get the current wall clock time and cpu time.
    tms endCPUTime;
    times (&endCPUTime);
    timeval endTime;
    gettimeofday (&endTime, 0);
    double endTimeSecs = endTime.tv_sec +
        (double) (endTime.tv_usec / 1000000.0);

    //the "times" routine returns values measured in clock ticks.  This
    //var is used to convert values from clock ticks to SECS
    static const double TICKS_TO_SECS = sysconf (_SC_CLK_TCK);

    //compute delta times by subtracting the start times from the
    //current time.  These times will be in floating point seconds.
    _wallClockTime = (endTimeSecs - _startTime);
    _userCPU_Time = (endCPUTime.tms_utime - _startCPUTime.tms_utime) 
                    / TICKS_TO_SECS;
    _systemCPU_Time =(endCPUTime.tms_stime - _startCPUTime.tms_stime)
                     / TICKS_TO_SECS;
    
    //update the cumulative data used for computing an average, and
    //then compute the average, which is part of the return data.
    _totalTime += _wallClockTime;
    _numTimings++;
    
    //nullify the start time so we can catch the user calling the stop
    //method without calling the start method first.
    _startTime = 0;
    }





