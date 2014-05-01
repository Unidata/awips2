//------------------------------------------------------------------------------
// SetElevation - Object containing all SetElevation scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 14 Apr 1998	Daniel Weiler, RTi	Added first cut at data members and
//					set/gets.
// 24 Sep 1998	DKW, RTi		Moved to the Method-centric idea of
//					input TS storage.
// 12 Nov 2001  James R. VanShaar, RTi  Added setInactiveState
// 12 Nov 2001  JRV, RTi        Added function setCOstring
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef SetElevation_INCLUDED
#define SetElevation_INCLUDED

#include "ReservoirMethod.h"

class SetElevation : public ReservoirMethod 
{
public:
	SetElevation( Reservoir* );	// Default constructor

	SetElevation( const SetElevation&, Reservoir* );
					// Copy constructor.

	virtual ~SetElevation();	// Destructor

	int buildElevationTS( char**, int );

	int construct( char**, int );	// Time series construction.

	SetElevation* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	int setCOstring();	// Prepares carryover string for original 
				// parameter input and CO array sizing

	int setCOstring(TSDate&);	// Prepares adds carryover string to the
					// ResJSys carryover array

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// SetElevation object.

private:
	int initialize();		// Initialize data members

	DistributedTS	_elev_ctl;	// TS read from the control file
	 
	HourTS*		_pool_obs;	// Obs TS as input from the database.

	int 		_n_blend_tbl,	// Number of blending time steps.
			_n_blend_ts,
			_tbl_step,
			_ts_step;
};

#endif
