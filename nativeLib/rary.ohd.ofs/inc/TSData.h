//------------------------------------------------------------------------------
// TSData - class for storing one data point and date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This class holds one data point for a time series.  It
//			is used for data storage by the IrregularTS class and is
//			used by other TS classes to return a data point.
//------------------------------------------------------------------------------
// History:
// 
// 22 Sep 1997	Matthew J. Rutherford,	Created initial version of class.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	Steven A. Malers, RTi	Update to be more generic for use with
//					all time series classes.  This will
//					allow, for example, a way to graph time
//					series and then inquire about the data
//					flag.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// _data	C	Data value.
// _data_flag	C	Data flag (often used for quality, etc.).
// _date	C	Date associated with data value.
// _next	C	Pointer to next TSData in list (an internally maintained
//			linked list).
// _previous	C	Pointer to previous TSData in list (an internally
//			maintained linked list).
// _units	C	Units of data.
//------------------------------------------------------------------------------

#ifndef TSData_INCLUDED
#define TSData_INCLUDED

#include <string.h>
#include <stdio.h>
#include "ResJ.h"
#include "resj/TSDate.h"

class TSData
{
public:
	// The basic member functions...
	TSData();	// Standard constructor.
	virtual ~TSData();	// Destructor.
	TSData( const TSData& );	// Copy constructor.
	TSData& operator= ( const TSData& );// Overload =.

	operator char *();	// a (char*) cast for printing.

	double	getData();		// Returns the data value.
	TSDate	getDate();		// Returns the TSDate object.
	TSData*	getNext();		// Returns the next pointer.
	TSData* getPrevious();		// Returns the previous pointer.
	char*	getDataFlag();		// Returns the quality flag.
	char*	getUnits();		// Returns the data units.
	int	setData( double );	// Sets the data value.
	int	setDate( TSDate& );	// Sets the TSDate object.
	int	setNext( TSData* );	// Sets the next pointer.
	int	setPrevious( TSData* );	// Sets the previous pointer.
	int	setDataFlag( char* );// Sets the quality flag.
	int	setUnits( char* );	// Sets the data units.
	int	setValues( TSDate&, double, char*, char* );
					// Sets both the date and data values.
	char *	toString ( void );	// Convert to a string representation.

private:
	int initialize( );		// Initialize an instance - use this
					// in the constructors.  

protected:
	char		*_data_flag,	// Data flag.
			*_units;	// Units of the data.

	TSDate		_date;		// Date of recording. 

	double		_data;		// Data value associated with date.

	TSData		*_next,		// Next/Previous pointers for use 
			*_previous;	// With a linked list.  This is used
					// by IrregularTS to store data.
};

#endif // TSData class definition
