//-------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// StopWatch.H
// A code timing class.
//
// Author:  Gerry Murray
//-------------------------------------------------------------------
#ifndef StopWatch_H
#define StopWatch_H

#include <sys/times.h>
#include "TextString.H"

#ifdef IDENT_H
static const char* const StopWatch_H_Id =
  "$Id$";
#endif


class StopWatch
    {
    public:
        StopWatch (void);

        // Uses default copy c'tor, destructor, & op=

        // The following are needed so that this can be a Dict value
        bool operator== (const StopWatch &rhs) const;
        bool operator!= (const StopWatch &rhs) const;

        // Main operating methods.
        void start (void);
        void stop (void);
        void reset (void) {start(); _totalTime = 0; _numTimings = 0;}

        // Returns time in seconds since start() was called.  If watch
        // is stopped, returns the same as wallClockTime()
        double getSplit (void);

        // These can only be called when the watch has been stopped;
        // i.e. stop() has been called.
        double wallClockTime (void);
        double userCPU_Time (void);
        double systemCPU_Time (void);
        double avgWallClockTime (void);
    
    private:
        //the time, in clock ticks, that the code timing began.
        double       _startTime;
        //same function as _startTime, but holds the cpu seconds in a UNIX
        //struct defined in sys/times.h
        tms           _startCPUTime;
        //the total time of all the code timings in seconds.  Useful for
        //computing averages.
        double        _totalTime;
        //for computing and printing the average.     
        int           _numTimings;
        //the actual times that are filled or updated after every stop
        //method.  All these times are in floating point seconds.
        double _wallClockTime, _userCPU_Time, _systemCPU_Time;

        bool _running;
    };

// -- public -----------------------------------------------------------------
// void StopWatch::StopWatch (void)
//   
// Constructor that initializes all the data members to NULL values.
//-----------------------------------------------------------------------------
inline StopWatch::StopWatch (void)
    : _startTime(0), _totalTime(0), _numTimings(0),
      _wallClockTime(0), _userCPU_Time(0), _systemCPU_Time(0), _running(false)
    {}

// -- public -----------------------------------------------------------------
// double StopWatch::wallClockTime (void)
//
// Returns the actual elasped time spent executing the code between
// the start and stop method.
//-----------------------------------------------------------------------------
inline double StopWatch::wallClockTime (void)
    {
    return (_wallClockTime);
    }

// -- public -----------------------------------------------------------------
// double StopWatch::userCPU_Time (void)
//
// Returns the time the CPU spent executing the client code between
// the start and stop method.
//-----------------------------------------------------------------------------
inline double StopWatch::userCPU_Time (void)
    {
    return (_userCPU_Time);
    }

// -- public -----------------------------------------------------------------
// double StopWatch::systemCPU_Time (void)
//
// Returns the time the CPU spent executing the system code between
// the start and stop method.
//-----------------------------------------------------------------------------
inline double StopWatch::systemCPU_Time (void)
    {
    return (_systemCPU_Time);
    }


// -- public -----------------------------------------------------------------
// double StopWatch::systemCPU_Time (void)
//
// Returns the average wall-clock time for all the timings thus far.
//-----------------------------------------------------------------------------
inline double StopWatch::avgWallClockTime (void)
    {
    return (_totalTime / _numTimings);
    }

// -- public -----------------------------------------------------------------
// bool StopWatch::operator==(const StopWatch &rhs) const
//
// Returns true if both dataTimes and the address to the seq object are equal.
// Otherwise returns false.
//
// ---------------------------------------------------------------------------
inline bool StopWatch::operator==(const StopWatch &rhs) const
    {
    return (_startTime == rhs._startTime &&
            _startCPUTime.tms_utime == rhs._startCPUTime.tms_utime &&
            _startCPUTime.tms_stime == rhs._startCPUTime.tms_stime &&
            _totalTime == rhs._totalTime &&
            _numTimings == rhs._numTimings);
    }

// -- public -----------------------------------------------------------------
// bool StopWatch::operator!= (const StopWatch &rhs) const
//
// Returns true if both depict keys or both dataTimes are not equal.
// Otherwise returns false.
//
// ---------------------------------------------------------------------------
inline bool StopWatch::operator!= (const StopWatch &rhs) const 
    {
    return !operator==(rhs);
    }

#endif
