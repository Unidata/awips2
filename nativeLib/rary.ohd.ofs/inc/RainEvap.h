//------------------------------------------------------------------------------
// RainEvap - Object containing all RainEvap scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 04 Mar 1998	Matthew J. Rutherford, RTi
//					Changed constructTS to construct to
//					make sense for all the methods.
// 01 Apr 1998	DKW		Added private TS* for precip and evap. Differ-
//				entiated control file and input TS.
// 24 Sep 1998	DKW		Moved to Method-centric input TS sotrage.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef RainEvap_INCLUDED
#define RainEvap_INCLUDED

#include "ReservoirMethod.h"

class RainEvap : public ReservoirMethod 
{
public:

	RainEvap( Reservoir* );		// Default constructor

	RainEvap( const RainEvap&, Reservoir* );	
					// Copy constructor.

	virtual ~RainEvap();			// Destructor

	int construct( char**, int );
					// Called from the system parse
					// routine to build the _evap_ts 
					// and the _precip_ts.

	RainEvap* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();

	int solveMethod( TSDate &, int, double** = NULL );		
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// RainEvap object.

private:

	int initialize();		// Initialize data members

	int		_n_evap,	// Length of _evap_ctl
			_n_precip;	// Length of _precip_ctl

	DistributedTS	_evap_ctl,	// evap and precip TS input from a 
			_precip_ctl;	// table in the control file.
	HourTS		*_precip_obs,	// Observed data TS as input from 
			*_evap_obs;	// the database.

};

#endif
