// ----------------------------------------------------------------------------
// IrregularTS - base class from which all sparse time series are derived
// ----------------------------------------------------------------------------
// Notes:	(1)	This base class is provided so that specific sparse
//			time series can derive from this class.
//		(2)	Data for this time series interval is stored either as
//			a linked list of TSData objects, or as an array 
//			depending on how the data are read in.
// ----------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford,	Created initial version of class def.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	Steven A. Malers, RTi	Update to be consistent with Java.
//					Use TSLimits for limits.  Change
//					calcMaxMinValues to refresh and
//					getDataLimits.  Add enforceDataGaps.
// ----------------------------------------------------------------------------

#ifndef IrregularTS_INCLUDED
#define IrregularTS_INCLUDED

#include <iostream.h>

#include "resj/TS.h"
#include "TSData.h"

class IrregularTS : public TS
{
public:
	// The basic member functions...
	IrregularTS ( void );		// Standard constructor.
	IrregularTS ( char * );		// Need to use a "virtual" constructor
					// inside of this to instantiate a time
					// series from a persistent source.
					// In other words, we want the more
					// specific derived classes to call
					// their version of readPersistent()
					// when a time series is constructed
					// from a char * file name.
	virtual ~IrregularTS ( void );	// Destructor.
        IrregularTS& operator= ( const IrregularTS& );
					// Copy constructor.
	IrregularTS ( const IrregularTS& );	// Overload =.

	// Member functions defined in this class but virtual in the base
	// class...

	int allocateDataSpace( void );	// Set up _data array
	int enforceDataGaps ( int, int, TSDate, TSDate );
        char **formatOutput ( int, unsigned long );
        int formatOutput ( ostream &, int, unsigned long int );
        int formatOutput ( char *, int, unsigned long int );
                                        // Format irregular time series output
                                        // for simple formats.  Use derived
                                        // classes for specific irregular time
                                        // series formats.
	int freeDataSpace( void );	// delete the _data array
	TSData *getData ( void );	// Return the data space.  This is used
					// only by some TSUtil routines in
					// Java but put a placeholder function
					// here.
	TSDate getDataDate ( TSDate&, int );
					// Get the date relative to a given
					// date.
	TSLimits getDataLimits ( TSDate&, TSDate& );
					// Get the data limits for a range of
					// dates.
	TSData getDataPoint ( TSDate& );// Return a data point.
	double *getDataPointer ( TSDate& );
					// Get pointer to a data value
					// corresponding to a date.
	int getDataPosition ( TSDate&, int *, int *, int * );
					// Get the data position in the data
					// array.  First value is the month,
					// second is the interval in the month.
					// The third is unused.
	double getDataValue ( TSDate& );
					// Get a data value corresponding to
					// a date.
	double getDistDataValue( TSDate& );
					// Get data value assuming the TS
					// has a distribution associated with
					// it.
	int refresh ( void );		// Reset the maximum and minimum values
					// in the time series.
	int setDataValue( TSDate&, double );
					// Set a value into memory

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

protected:
	int	_ndata;			// Number of data points. This should
					// not be used as in indexer in 
					// any array.
	TSData	*_ts_data_head,		// The head of the linked list
					// of TSData objects.
		*_ts_data_tail;		// The tail of the linked list
					// used in setData() function.
	int	_data_index;		// Index used when traversing the data
					// array.  This is the element in
					// _ts_data_head (zero-reference) that
					// was last accessed.
};

#endif
