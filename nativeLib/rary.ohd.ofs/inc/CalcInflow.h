//------------------------------------------------------------------------------
// CalcInflow - Object containing all CalcInflow scheme information.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Feb 2004  James R. VanShaar, RTi  Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef CalcInflow_INCLUDED
#define CalcInflow_INCLUDED

#include "resj/Table.h"
#include "ReservoirMethod.h"

class CalcInflow : public ReservoirMethod 
{
public:
	
	CalcInflow( Reservoir* );	// Default constructor

	CalcInflow( const CalcInflow&, Reservoir* );
					// Copy constructor

	virtual ~CalcInflow();		// Destructor

	int buildTables( char**, int, double );

	int construct( char**, int );
					// Called from the System
					// parse routine to build the 
					// release TSs

	CalcInflow* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();

	int initialize();		// Initialize data members

	int print( FILE* );		// Prints CalcInflow info.

	int solveMethod( TSDate&, int, double** = NULL );		
					// Solver

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	int setCOstring();	// Prepares carryover string for original 
				// parameter input and CO array sizing

	int setCOstring(TSDate&);	// Prepares adds carryover string to the
					// ResJSys carryover array

	int transferCO ( Method * methOLD, char * cOLD, char * cNEW,
		int * ipr );
					// Transfers carryover given
					// the new method and the 
					// old and new related portions of the 
					// carryover strings

	int		_isCopy;	// Tracks copies.
	
private:

	HourTS*		_release_obs;	// input observed release TS.
	HourTS*		_pool_obs;	// input observed pool TS.
	HourTS*		_withdraw_obs;	// input observed withdraw TS.

	HourTS*		_inflow_calc;	// calculated inflow TS.

	int		_outputCalcTS;	// Determines whether we will output
					// the calculated inflow timeseries.

	int 		_useForSim;	// Determines whether to apply calc'd
					// inflows into simulation.

	int		_constrainChange; // Determines whether a MaxChange
					  // table exists.
	
	Table		_maxIncrease;	// Table containing MaxIncrease
					// constraints.

	Table		_maxDecrease;	// Table containing MaxDecrease
					// constraints.

	double		_minInflow;	// Minimum acceptable calculated inflow.

	double		_remainingVol;	// Carryover containing non-applied 
					// inflow volume.
	
	double          _startPool, 
			_startRelease,
		       	_startWithdrawal,
			_startInflow;
	
	double		_meanToInstErr;

	int		_useLoss;	// Determines whether a loss method
					// is included in mass balance.
	
};

#endif
