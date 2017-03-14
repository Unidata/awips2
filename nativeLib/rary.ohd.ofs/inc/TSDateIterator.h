// ----------------------------------------------------------------------------
// TSDateIterator - time series date iterator
// ----------------------------------------------------------------------------
// Notes:	(1)	This class has some intelligence for iterating through
//			the dates for different time series types.
// ----------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	Initial version.  Get this working with
//		Riverside Technology,	MonthTS, HourTS, and IrregularTS.
//		inc.
// 05 Jan 1998	SAM, RTi		Get code to be compatible with Java
//					enhancements.
// ----------------------------------------------------------------------------

#ifndef TSDateIterator_INCLUDED
#define TSDateIterator_INCLUDED

#include "resj/TSDate.h"
#include "resj/TS.h"

#include <string.h>
#include <stdio.h>
#include "ResJ.h"

class TSDateIterator
{

public:
	// The basic member functions...
	TSDateIterator ( TS * );		// Standard constructor.
	TSDateIterator ( TS *, TSDate, TSDate );// Constructor that allows date
						// to be set.
	~TSDateIterator ( );			// Destructor.
	TSDateIterator ( const TSDateIterator& );
						// Copy constructor.
	TSDateIterator& operator= ( const TSDate& );
						// Overload =.

/* Don't need any of these right now
	int operator== ( const TSDate& );// Overload ==.
	int operator!= ( const TSDate& );// Overload !=.
	int operator<= (const TSDate&);	// overload <=.
	int operator>= (const TSDate&);	// overload >=.
	int operator < (const TSDate&);	// overload <.
	int operator > (const TSDate&);	// overload >.
	operator char *();	// a (char*) cast.
*/

	// Member functions more specific to this class...

	int advanceDate (void);			// Advance the date to the
						// next available data.
	TSDate getCurrentDate ( void );		// Get the current date in the
						// loop.
	int isIterationComplete ( void );	// Determine if the current
						// iterator date is within the
						// limits of the loop.
	int setBeginDate ( TSDate );		// Set the begin date.
	int setEndDate ( TSDate );		// Set the end date.

private:
	int init ( void );		// Initialize data.

	TS	*_ts;			// Pointer to the time series to use.
	TSDate	_current_date;		// Current date in iteration.
	TSDate	_date1;			// Date to start on.
	TSDate	_date2;			// Date to end on.
	int	_last_date_encountered;	// Indicates if we have encountered the
					// last date.  If this is true, then we
					// are done with the iteration.
};

#endif	// TSDateIterator_INCLUDED
