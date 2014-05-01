//------------------------------------------------------------------------------
// Spillway - Object containing all Spillway scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Dec 2002  James R. VanShaar, Riverside Technology, inc.
//					Created initial version.
// 15 Jan 2003	JRV, RTi	Added transferCO().
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Spillway_INCLUDED
#define Spillway_INCLUDED

#include "ReservoirMethod.h"
#include "resj/Table.h"

class Spillway : public ReservoirMethod 
{
public:
	
	Spillway( Reservoir* );	// Default constructor

	Spillway( const Spillway&, Reservoir* );
					// Copy constructor

	virtual ~Spillway();			// Destructor

	int construct( char**, int );

	Spillway* copy( Component* );     // Calls copy constructor.

	char**  elevToStor( char**, int, double, double );
					// Converts the pool elevation / spill
					// data to storage / spill data, 
					// including unit conversions, as 
					// applicable

	int freeDataSpace();

	int initialize();		// Initialize data members

	int print( FILE* );		// Prints Spillway info.

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	int setCOstring();	// Prepares carryover string for original 
				// parameter input and CO array sizing

	int setCOstring(TSDate&);	// Prepares adds carryover string to the
					// ResJSys carryover array

	int solveMethod( TSDate&, int, double** = NULL );		
					// Solver
	
	int transferCO ( Method * methOLD, char * cOLD, char * cNEW,
		int * ipr );
					// Transfers Spillway carryover given
					// the new Spillway method and the 
					// old and new related portions of the 
					// carryover strings

private:

	Table*		_stor_spill_tbl;	// Storage / Spill Table based
						// on input Elevation / Spill
						// pairs.

	int		_intervals;	// Number of mini-timesteps the run
					// timestep will be broken into for
					// spillway calculation.
	
	double*		_withVol_ts;
	double*		_relVol_ts;
	double*		_stor_ts;
	double*		_spillVol_ts;
	double*		_inflVol_ts;

	double		_minSpillStorage;	// Storage associated with 0 
						// spill.
};
#endif
