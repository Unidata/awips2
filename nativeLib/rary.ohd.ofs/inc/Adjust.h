//------------------------------------------------------------------------------
// Adjust - Object containing all Adjust scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 30 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 28 Apr 1998	Daniel Weiler, 		First cut at functions and members.
// 24 Sep 1998	Daniel Weiler, 		Added _release_obs.	
// 12 Nov 2001  James R. VanShaar, RTi  Added setInactiveState
// 12 Nov 2001  JRV, RTi        Added function setCOstring
// 14 Oct 2002	JRV, RTi	Added _adjsim, adjsim().
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Adjust_INCLUDED
#define Adjust_INCLUDED

#include "ReservoirMethod.h"

class Adjust : public ReservoirMethod 
{
public:
	Adjust( Reservoir* );	// Default constructor

	Adjust( const Adjust&, Reservoir* );
					// Copy constructor.

	virtual ~Adjust();	// Destructor

	int construct( char**, int );	// Time series construction.

	Adjust* copy( Component* );	// Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// Adjust object.

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	int setCOstring();	// Prepares carryover string for original 
				// parameter input and CO array sizing

	int setCOstring(TSDate&);	// Prepares adds carryover string to the
					// ResJSys carryover array
	
	int setCOstring_AdjsimOff(TSDate&);	// Prepares, adds carryover string
						// for the owning reservoir when
						// the _adjsim parameter is set to
	 					// off.
	
	int adjsim();		// Returns _adjsim;

private:
	int initialize();		// Initialize data members

        int _n_blend,       // Number of blending time steps.
	    _n_tstep;       // Number of time steps since last update.

	double 	_last_obs_rel, // Release value from the last update.
		_deviate;	// Running tally of observed to simulated 
				// difference.

	HourTS	*_release_obs,	// Observed input release TS.
		*_pool_obs;

	TSDate _last_obs;   	// Time step that the release was
				// last updated.

	int _adjsim;		// Determines whether to adjust the
				// simulation values or not.  If not
				// we adjust only carryover when written.
				// 1 = on (default), 0 = off.
};

#endif
