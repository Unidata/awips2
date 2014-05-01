// ----------------------------------------------------------------------------
// TSLimits - simple class for returning data limits in data
// ----------------------------------------------------------------------------
// Notes:	(1)	This class stores the data limits for data space.
//			It stores the maximum and minimum values and the dates
//			associated with the values.  IT DOES NOT AT THIS TIME
//			COMPUTE THE LIMITS (that code is still in the *TS
//			classes in the calcMaxMinValues() functions).
// ----------------------------------------------------------------------------
// History:
//
// 24 Sep 1997	Steven A. Malers, RTi	Initial version in Java.
// 06 Jan 1998	SAM, RTi		Port to C++.  Update to have _date1 and
//					_date2 so that TSLimits can be returned
//					by code that processes TS intervals.
// 23 Jan 1998	SAM, RTi		Add _non_missing_data_count and
//					accessor functions.  Add checks to make
//					sure that the dates get set.  Don't
//					use pointers for dates (that allow
//					nulls).  Just use some private ints
//					so show if dates have been set.  On the
//					Java side we do use nulls.
// ----------------------------------------------------------------------------

#ifndef TSLimits_INCLUDED
#define TSLimits_INCLUDED

#include "resj/TSDate.h"

class TSLimits
{
public:
	// The basic member functions...
	TSLimits();				// Standard constructor.
	~TSLimits();				// Destructor.
	TSLimits( const TSLimits& );		// Copy constructor.
	TSLimits& operator= ( const TSLimits& );// Overload =.

	operator char *();	// a (char*) cast for printing.

	int	areLimitsFound ( void );// Have limits been found?, meaning that
					// the dates have been set.
	TSDate	getDate1();		// Returns first date.
	TSDate	getDate2();		// Returns last date.
	double	getMaxValue();		// Returns the maximum value.
	TSDate	getMaxValueDate();	// Returns date for the maximum value.
	double	getMinValue();		// Returns the minimum value.
	TSDate	getMinValueDate();	// Returns date for the minimum value.
	int	getNonMissingDataCount();
					// Return the number of non-missing
					// data values in the period.
	TSDate	getNonMissingDataDate1();
					// Gets date for the first non-missing
					// data value.
	TSDate	getNonMissingDataDate2();
					// Gets date for the last non-missing
					// data value.

	int	hasNonMissingData();	// Return 1 if some non-missing data
					// are available.

	int	setDate1(TSDate);	// Sets first date.
	int	setDate2(TSDate);	// Sets last date.
	int	setMaxValue(double);	// Sets the maximum value.
	int	setMaxValue(double,TSDate);
					// Sets the maximum value and date.
	int	setMaxValueDate(TSDate);// Sets date for the maximum value.
	int	setMinValue(double);	// Sets the minimum value.
	int	setMinValue(double,TSDate);
					// Sets the minimum value and date.
	int	setMinValueDate(TSDate);// Sets date for the minimum value.
	int	setNonMissingDataCount(int);
					// Set the number of non-missing
					// data values in the period.
	int	setNonMissingDataDate1(TSDate);
					// Sets date for the first non-missing
					// data value.
	int	setNonMissingDataDate2(TSDate);
					// Sets date for the last non-missing
					// data value.
	char *	toString ( void );	// Convert to a string representation.

private:
	int initialize( );		// Initialize an instance - use this
					// in the constructors.  
	int	checkDates();		// If the dates have been set, then
					// _found is true.

	TSDate	_date1;			// First date.
	int	_date1_set;		// The _*set data members indicate if
					// the dates have been set.  They are
					// used by _checkDates.  Another option
					// would have been to use pointers for
					// the dates and allowed NULLs but this
					// just adds more potential for
					// errors on the C++ side.
	TSDate	_date2;			// Last date.
	int	_date2_set;
	double	_max_value;		// Maximum value.
	TSDate	_max_value_date;	// Maximum value date.
	int	_max_value_date_set;
	double	_min_value;		// Minimum value.
	TSDate	_min_value_date;	// Minimum value date.
	int	_min_value_date_set;
	int	_found;			// Indicates whether limits are found.
					// Slated for deprication?
	int	_non_missing_data_count;// The number of non-missing data
					// values in the period.
	TSDate	_non_missing_data_date1;// Date for first non-missing.
	int	_non_missing_data_date1_set;
	TSDate	_non_missing_data_date2;// Date for last non-missing.
	int	_non_missing_data_date2_set;
};

#endif // TSLimits class definition
