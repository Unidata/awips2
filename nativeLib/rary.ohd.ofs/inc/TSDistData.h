//------------------------------------------------------------------------------
// TSDistData - class for storing one data point and date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This class holds one data point for a time series.  It
//			is used for data storage by the IrregularTS class and is
//			used by other TS classes to return a data point.
//------------------------------------------------------------------------------
// History:
// 
// 05 Mar 1998	Daniel Weiler,	Created initial version of class.
//		Riverside Technology, inc.
// 05 May 1998	Matthew J. Rutherford, RTi	Added two access routines
//						to help with the copy 
//						constructors.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// _data	C	Data value.
// _data_flag	C	Data flag (often used for quality, etc.).
// _date	C	Date associated with data value.
// _next	C	Pointer to next TSDistData in list (an internally maintained
//			linked list).
// _previous	C	Pointer to previous TSDistData in list (an internally
//			maintained linked list).
// _units	C	Units of data.
//------------------------------------------------------------------------------

#ifndef TSDistData_INCLUDED
#define TSDistData_INCLUDED

#include <string.h>
#include <stdio.h>
#include "TSData.h"

class TSDistData : public TSData
{
public:
	// The basic member functions...
	TSDistData();	// Standard constructor.
	~TSDistData();	// Destructor.
	TSDistData( const TSDistData& );
	TSDistData& operator = ( const TSDistData& );

	int	setDistribution( float*, int );
					// Sets distribution values.
	float*	getDistArray();		// Returns the array of values.

	int 	getNDist();		// Returns the distribution size.

	float	getDistribution( int );
					// Gets distribution values.

private:
	int initialize( );		// Initialize an instance - use this
					// in the constructors.  

protected:
	float*		_distrib;	// Data value associated with date.

	int		_n_dist;	// The number of distribution values
					// for a given TSDistData object. There
					// must be 1 distribution value for
					// each time step.

};

#endif // TSDistData class definition
