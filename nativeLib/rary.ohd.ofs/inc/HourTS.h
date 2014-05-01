// ----------------------------------------------------------------------------
// HourTS - base class from which all hourly time series are derived
// ----------------------------------------------------------------------------
// Notes:	(1)	This base class is provided so that specific hourly
//			time series can derive from this class.
//		(2)	Data for this time series interval is stored as follows:
//
//			data interval within month  ->
//
//			------------------------
//              	|  |  |.......|  |  |  |     first month in period
//			------------------------
//			|  |  |.......|  |  |
//			------------------------
//			|  |  |.......|  |  |  |
//			------------------------
//			|  |  |.......|  |  |
//			---------------------
//			.
//			.
//			.
//			------------------------
//			|  |  |.......|  |  |  |
//			------------------------
//			|  |  |.......|  |  |        last month in period
//			---------------------
//
//			So, the base block of storage is the month.  This lends
//			itself to very fast data retrieval but may waste some
//			memory for short time series in which full months are
//			not stored.  This is considered a reasonable tradeoff.
// ----------------------------------------------------------------------------
// History:
//
// Apr 1996	Steven A. Malers, RTi	Begin to develop this with the
//					anticipation of supporting DATACARD,
//					HMData, and other formats
// 30 Jan 1997	SAM, RTi		Change name of class from HourlyTS to
//					HourTS because it is easier to use and
//					document.
// 05 Feb 1997	SAM, RTi		Add getDataPosition routine.
// 06 Jun 1997	SAM, RTi		Add third positional argument to the
//					getDataPosition routine.  It is not used
//					here.
// 16 Jun 1997	MJR, RTi		Added overload of calcMaxMinValues.
// 30 Sep 1997	SAM, RTi		Add formatOutput functions.
// 06 Jan 1998	SAM, RTi		Change calcMaxMinValues to refresh and
//					put getDataLimits in TSUtil.
// 17 Nov 1998 	DKW, RTi		Added interpolation ability to 
//					getDataValue.
// ----------------------------------------------------------------------------

#ifndef HourTS_INCLUDED
#define HourTS_INCLUDED

#include <iostream.h>

#include "resj/TS.h"
#include "TSLimits.h"
#include "TSData.h"

#define	NORMAL 0
#define INTERPOLATE 1

class HourTS : public TS
{
public:
	// The basic member functions...
	HourTS ( void );		// Standard constructor.
	HourTS ( char * );		// Need to use a "virtual" constructor
					// inside of this to instantiate a time
					// series from a persistent source.
					// In other words, we want the more
					// specific derived classes to call
					// their version of readPersistent()
					// when a time series is constructed
					// from a char * file name.
	virtual ~HourTS ( void );	// Destructor.
        HourTS& operator= ( const HourTS& );
					// Copy constructor.
	HourTS ( const HourTS& );	// Overload =.

	// Member functions defined in this class but virtual in the base
	// class...

	int allocateDataSpace( void );	// Set up _data array
	char **formatOutput ( int, unsigned long );
	int formatOutput ( ostream &, int, unsigned long int );
	int formatOutput ( char *, int, unsigned long int );
					// Perform simple output formatting on
					// hourly time series.
	int freeDataSpace( void );	// delete the _data array
	double *getDataPointer ( TSDate& );
					// Get pointer to a data value
					// corresponding to a date.
	TSData getDataPoint ( TSDate& );// Get a data point.
	int getDataPosition ( TSDate&, int *, int *, int * );
					// Get the data position in the data
					// array.  First value is the month,
					// second is the interval in the month.
					// The third is unused.
	double getDataValue ( TSDate& );
					// Get a data value corresponding to
					// a date.
	int setDataValue( TSDate&, double );
					// Set a value into memory
	int refresh ( void );		// Reset the maximum and minimum values
					// in the time series.

	// Member functions that will be defined in the derived class...

	virtual int printSample ( ostream& = cout ); 
					// Print a sample of time series format
	virtual int readPersistent ( istream& = cin ); 
	virtual int readPersistent ( char* ); 
					// Read a time series
	virtual int readPersistentHeader( istream& = cin );
	virtual int readPersistentHeader( char* );
					// Read a time series header
	virtual int writePersistent ( ostream& = cout ); 
	virtual int writePersistent ( char* ); 
					// Write a time series
private:
	int	init();			// This initialized data members.

	double	**_data;		// This is the data space for hourly
					// time series.
};

#endif
