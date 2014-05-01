//------------------------------------------------------------------------------
// Balance - Object containing all Balance scheme information.
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
// 24 Apr 1998	Daniel Weiler, RTi	Added fist cut of data members.
// 12 Jun 2002	James R. VanShaar, RTi	Added _max_rel.
// 13 Jun 2002	JRV, RTi	Added _totBalStor, _ace.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Balance_INCLUDED
#define Balance_INCLUDED

#include "ReservoirMethod.h"

#define	PERCENT	1
#define VOLUME 2

class Balance : public ReservoirMethod 
{
public:
	Balance( Reservoir* );	// Default constructor

	Balance( const Balance&, Reservoir* );
					// Copy constructor.

	virtual ~Balance();	// Destructor

	int construct( char**, int );	// Time series construction.

	Balance* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// Balance object.

private:
	int initialize();		// Initialize data members

	int	_bal_mode,		// Either volume or percent volume
					// balancing.
		_n_res, 		// Number of downstream reservoirs to be 
					// balanced. 
		_owner_pos;

	// The next variables contain the lower and upper bounds, where 
	// applicable, for each reservoir.
	double	*_lower_stor,
		*_upper_stor,
		*_min_rel,
		*_max_rel;

	double _ace;		// Attenuation Coefficient for Evaculation--
				// Multiplies the change in storage required to 
				// reach balance on the owning reservoir, thus
				// slowing how fast the reservoir tries to reach
				// balanced conditions.  This also reduces
				// oscillation due to lag in releases from the 
				// owner and inflows to another reservoir in the
				// balance.
	double _totBalStor;	// Sum over all balancing reservoirs of the
				// balancing storage--
				// 	UpperStorage - LowerStorage

	Reservoir 	**_balance_res;
				// List of pointers to all the reservoirs
				// in the balance system.
};

#endif
