// ----------------------------------------------------------------------------
// TSInterval - time series interval class
// ----------------------------------------------------------------------------
// Notes:	(1)	This class is currently just used by a select few
//			TSIdent member functions to pass interval information.
//			It may or may not be expanded further to include a full
//			class interface.
// ----------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	Steven A. Malers, RTi	Initial version in Java.
// 08 Jan 1998	SAM, RTi		Port to C++.
// 04 Feb 1998	SAM, RTi		Add getIntervalName().
// ----------------------------------------------------------------------------

#ifndef TSInterval_INCLUDED
#define TSInterval_INCLUDED

#include "ResJ.h"
#include <stdio.h>

class TSInterval
{
public:
	// The basic member functions...
	TSInterval();				// Standard constructor.
	~TSInterval();				// Destructor.
	TSInterval( const TSInterval& );	// Copy constructor.
	TSInterval& operator= ( const TSInterval& );// Overload =.

	operator char *();			// a (char*) cast for printing.

	int getBase ();				// Return the base.
	char *getBaseString ();			// Return the base as string.
	static char *getName ( int );		// Return the interval name as
						// a string.
	int getMultiplier ();			// Return the multiplier.

	static TSInterval parseInterval ( char * );
						// Parse a string and return
						// a TSInterval.

	int setBase ( int );			// Return the base.
	int setBaseString ( char * );		// Return the base as string.
	int setMultiplier ( int );		// Return the multiplier.

	char *	toString ( void );		// Convert to a string
						// representation.

private:
	int init( );			// Initialize an instance - use this
					// in the constructors.  

	char	*_interval_base_string;	// Interval base as a string.
	int	_interval_base;		// Interval base;
	int	_interval_mult;		// Interval multiplier;
};

#endif // TSInterval class definition
