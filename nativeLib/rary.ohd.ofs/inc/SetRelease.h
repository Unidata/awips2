//------------------------------------------------------------------------------
// SetRelease - Object containing all SetRelease scheme information.
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
// 06 Mar 1998	DKW,	Changed to a DistributedTS.
// 24 Sep 1998	DKW,	Moved to a Method-centric input(obs) TS storage.
// 12 Nov 2001  James R. VanShaar, RTi  Added setInactiveState
// 12 Nov 2001  JRV, RTi        Added function setCOstring
// 09 Mar 2004	JRV, RTi	Added _inWeekScalars.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef SetRelease_INCLUDED
#define SetRelease_INCLUDED

#include "ReservoirMethod.h"

class SetRelease : public ReservoirMethod 
{
public:
	
	SetRelease( Reservoir* );	// Default constructor

	SetRelease( const SetRelease&, Reservoir* );
					// Copy constructor

	virtual ~SetRelease();			// Destructor

	int buildReleaseTS( char**, int );

	int construct( char**, int );
					// Called from the System
					// parse routine to build the 
					// release TSs

	SetRelease* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();

	int print( FILE* );		// Prints SetRelease info.

	int solveMethod( TSDate&, int, double** = NULL );		
					// Solver

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	int setCOstring();	// Prepares carryover string for original 
				// parameter input and CO array sizing

	int setCOstring(TSDate&);	// Prepares adds carryover string to the
					// ResJSys carryover array

	double*		_inWeekScalars;	// list of scaling values used to
					// provide within-week variation.

private:

	int initialize();		// Initialize data members

	int		_n_elev;	// Number of release TSs and elevations

	DistributedTS*	_release_ctl;	// set release timeseries
	HourTS*		_release_obs;	// input observed release TS.

	double*		_elev;	// list of elevation values 
				// corresponding to each release TS

	int		_n_blend_tbl,	// Number of blending time steps.
			_n_blend_ts,	// Blending steps going from TS to table
			_tbl_step,	// Number of time steps since last 
					// table update.
			_ts_step;


};

#endif
