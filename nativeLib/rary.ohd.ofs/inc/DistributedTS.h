// ----------------------------------------------------------------------------
// DistributedTS - base class from which all sparse time series are derived
// ----------------------------------------------------------------------------
// Notes:	(1)	This base class is provided so that specific sparse
//			time series can derive from this class.
//		(2)	Data for this time series interval is stored either as
//			a linked list of TSData objects, or as an array 
//			depending on how the data are read in.
// ----------------------------------------------------------------------------
// History:
//
// 05 Mar 1998	Daniel Weiler,	Created initial version of class def.
//		Riverside Technology, inc.
// 05 May 1998	Matthew J. Rutherford, RTi	Added operator=.
// ----------------------------------------------------------------------------

#ifndef DistributedTS_INCLUDED
#define DistributedTS_INCLUDED

#include <iostream.h>

#include "IrregularTS.h"
#include "TSDistData.h"

#define NORMAL 0 
#define INTERPOLATE 1 

class DistributedTS : public IrregularTS
{
public:
	// The basic member functions...
	DistributedTS ( void );		// Standard constructor.

	void operator= ( const DistributedTS& );
					// Assignment operator.

	virtual ~DistributedTS ( void );// Destructor.

	// Member functions defined in this class but virtual in the base
	// class...

	double getDataValue( TSDate&, int );
					// Get the distributed TS data value
					// according to the int flag which
					// specifies an interpolation or a
					// block distribution.

	TSDate	getDataDate( TSDate&, int );

	int getPrevNextDates( TSDate&, TSDate*, TSDate* );
					// Gets the previous and future data 
					// values dates 

	int getPrevNextValues( TSDate&, double*, double* );
					// Gets the previous and future data 
					// values relative to the date passed 
					// in.
	int isDateInTS( TSDate& );

	int setDataValue( TSDate&, double, float*, int );

	void setRelativeFlag( int );

private:
	int init();

	int _n_dist;

	int _is_relative;

};

#endif
