//------------------------------------------------------------------------------
// TSUtil - class for static time series functions
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This class contains static functions used to manipulate
//			time series.  Putting functions here rather than in
//			TS, for example, makes it easier to do development
//			because changes to TSUtil do not require a recompile of
//			the derived classes (the way that a recompile of TS
//			would).
//------------------------------------------------------------------------------
// History:
// 
// 05 Jan 1998	Steven A. Malers,	Created initial version of class.
//		Riverside Technology,	Move "static" functions out of TS to
//		inc			here to simplify development.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef TSUtil_INCLUDED
#define TSUtil_INCLUDED

#include <string.h>
#include <stdio.h>
#include "ResJ.h"
#include "resj/TS.h"
#include "resj/TSDate.h"
#include "HourTS.h"
//#include "MinuteTS.h"
//#include "MonthTS.h"

class TSUtil
{
public:
	static const int MAX_POR;	// Get the maximum period.
	static const int MIN_POR;	// Get the minimum period.

	static TS changeInterval ( TS *, int, int, unsigned int );
					// General routine to change the
					// interval of a time series - calls
					// other change* routines.
	static HourTS changeIntervalToHour ( TS *, unsigned int );
					// Change to an hourly time series.
	//static MinuteTS changeIntervalToMinute ( TS *, unsigned int );
					// Change to an minute time series.
	//static MonthTS changeIntervalToMonth ( TS *, unsigned int );
					// Change to an monthly time series.
	//static YearTS changeIntervalToYear ( TS *, unsigned int );
					// Change to a yearly time series (no
					// yearly time series class has been
					// defined yet).
	static char * findTSFile ( char *, char * );
					// Find a time series file on the system
					// using a path list.  May need to
					// overload this.
	static TSLimits getDataLimits ( TS *, TSDate&, TSDate& );
					// Calculate the max and min
					// values between two dates.
	static TSLimits getPeriodFromTS ( int, TS *[], int );
					// Get the period of record from a
					// list of time series.
	static TSLimits getValidPeriod ( TS *, TSDate, TSDate );
					// Get a valid period to use for a
					// time series.
	static TSDate getTSDateFromString( char* );
					// Converts string to TSDate
};

#endif // TSUtil_INCLUDED
