//------------------------------------------------------------------------------
// Reservoir - encapsulates information about a reservoir topology type.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 06 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 19 Feb 1998	MJR, RTi	Added the getInternalValuePtr function.
// 07 Apr 1998	Daniel Weiler, RTi	Added setStates virtual.
// 16 Apr 1998	DKW, RTi	Added _release, _loss, and _withdraw states,
//				and their corresponding time series.
// 24 Apr 1998	DKW, RTi	Added _pool_obs and _release_obs observed
//				time series.
// 24 Sep 1998	DKW		Moved _pool_obs, _release_obs, _withdraw_obs,
//				_precip and _evap to the Method level.
// 05 Apr 2001  James R. VanShaar, RTi	Added transferCO()
// 05 Apr 2001  JRV, RTi	Added getELEVSTOR()
// 17 May 2001	JRV, RTi	Added various *CO variables and functions to 
//				store values from carryover
// 13 Nov 2001  JRV, RTi        Added function setCOstring(TSDate&)
// 13 Nov 2001  JRV, RTi        Added function setCOstring()
// 13 Nov 2001  JRV, RTi        Added more *CO variables
// 04 Jun 2002	JRV, RTi	Added local state variables: _prevInflow, 
//				_startInflow, _endInflow.
// 05 Jun 2002  JRV, RTi        Added function setEndInflow(TSDate &)
// 06 Jun 2002	JRV, RTi	Added local state variables: _prevPool,
//				_startPool, _endPool, _prevWithdraw, 
//				_startWithdraw, _endWithdraw, _prevRelease, 
//				_startRelease, and _endRelease.
// 07 Jun 2002	JRV, RTi	Added setEndOfTimestepStates.
// 12 Jun 2002	JRV, RTi	Added _prevStorage, _startStorage, _endStorage 
//				to be consistent with states to be used in 
//				Balance method.
// 12 Dec 2002	JRV, RTi	Added valueToMethod.
// 27 Mar 2006  JRV, RTi    Added valueToMethod( TSDate ),
//                          valueToMethod( TSDate, double ).
//                          Deleted valueToMethod( TSDate, double, double ).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Reservoir_INCLUDED
#define Reservoir_INCLUDED

#include "Component.h"
#include "resj/Table.h"

class Reservoir : public Component
{
public:
	Reservoir*	copy();	// Calls copy constructor for Reservoir. 

	//HourTS* getOutputTS( char* );

	int finalizeSolution( TSDate& );	
				// Handles solution cleanup at the end of 
				// each time-step.
	int freeDataSpace();	// Frees dynamically allocated data.

	Table getELEVSTOR(); // Returns the elevation / storage table
				
	void operator= ( const Reservoir& );
				// = Operator.
	virtual void printContents( FILE* );

	Reservoir();		// Default constructor.

	Reservoir( const Reservoir& );
				// Copy constructor.

	virtual ~Reservoir();		// Destructor.

	int setStates( char**, int );	
				// Sets up state data members from 
				// data in the control file.

	Table		_elev_stor_tbl;
				// Elevation vs. Storage table and

	HourTS		*_pool_ts,
			*_release_ts,
			*_spill_ts,
			*_withdraw_ts,
			_loss_ts,
			_storage_ts;

	int		_passflow;	// =1, pass inflow
					// =0, not pass inflow. 
	
	double		_pool,		// local representations of the state
			_release,	// time series above for the previous
			_spill,
			_withdraw,
			_loss,
			_storage;	// time step

	double		_min_release,	// Minimum release and pool values
			_min_pool;	// for this particular reservoir.

	virtual double* getInternalValuePtr( char* );
				// Searches a list of Reservoir key-words
				// and returns a pointer to the data member
				// associated with it.
	
	void valueToMethod ( TSDate date );
	void valueToMethod ( TSDate date, double maxWith );

private:
	int initialize();	// Initializes private data members.

//JRV
public:
	int transferCO ( Component * resOLD, char * cOLD, char * cNEW, 
		int * ipr );
				// Transfers reservoir carryover given 
				// the new reservoir component and the old
				// and new related portions of the carryover
				// strings

	int setCOstring();	// Prepares carryover string for original 
				// parameter input and CO array sizing

	int setCOstring(TSDate&);	// Prepares carryover string for an 
					// existing the ResJSys carry over array

	void setEndInflow(TSDate&);	// Sums inflows at the end of 
					// the current timestep and sets
					// the value of _endInflow, and
					// also puts it in the 
					// _totalInflow timeseries, if 
					// applicable.

	void setEndInflow(TSDate&, double);	
					// Sets the value of _endInflow,
					// and also puts it in the
					// _totalInflow timeseries, if 
					// applicable.

	int setEndOfTimestepStates(TSDate&);	// Reset states for next 
						// timestep and output 
						// carryover, if necessary.

	// The following parameters are / will be filled from the carryover
	//	data.  The values put into these variables, may also be put
	//	into other variables.
	double		_inflowCO;	// Instantaneous inflow for carryover
					//	date/time.
	double		_releaseCO;	// Instantaneous outflow for carryover
					//	date/time.
	double		_withdrawCO;	// Instantaneous withdraw for carryover
					//	date/time.
	double		_poolCO;	// Pool value at carryover date/time.
	double		_meanOutflowCO;	// Mean outflow for carryover date/time.
	double		_prevInflowCO;	// Instantaneous inflow one time step
					// 	previous to carry over 
					//	date/time.
	double		_prevPoolCO;	// Pool value one step previous to 
					// 	carryover date/time.
	double		_prevReleaseCO;	// Instantaneous release one time step
					// 	previous to carry over 
					//	date/time.
	double		_prevWithdrawCO;	// Instantaneous withdraw one 
						// 	time step previous to 
						//	carry over date/time.

	// The following variables were added to enhance Rule capability.
	double		_prevInflow;	// Total instantaneous inflow to the 
					// reservoir at the timestep before
					// the beginning of this timestep--
					// corresponds to the sum of inflows 
					// at _prev_date minus one timestep
					// when cur_date is the timestep 
					// being solved.
	double		_startInflow;	// Total instantaneous inflow to the 
					// reservoir at the beginning of this
					// timestep--corresponds to the sum
					// of inflows at _prev_date when
					// cur_date is the timestep being
					// solved.
	double		_endInflow;	// Total instantaneous inflow to the 
					// reservoir at the end of this
					// timestep--corresponds to the sum
					// of inflows at cur_date or the 
					// timestep being solved.
	double		_prevPool;	// Pool elevation at the timestep before
					// the beginning of this timestep--
					// corresponds to the pool elevation
					// at _prev_date minus one timestep
					// when cur_date is the timestep 
					// being solved.
	double		_startPool;	// Pool elevation at the beginning of 
					// this timestep-- corresponds to the 
					// pool elevation at _prev_date when 
					// cur_date is the timestep being 
					// solved.
	double		_endPool;	// Pool elevation at the end of this
					// timestep--corresponds to the pool
					// elevation at cur_date or the 
					// timestep being solved.
	double		_prevWithdraw;	// Withdrawal at the timestep before
					// the beginning of this timestep--
					// corresponds to the withdrawal
					// at _prev_date minus one timestep
					// when cur_date is the timestep 
					// being solved.
	double		_startWithdraw;	// Withdrawal at the beginning of 
					// this timestep-- corresponds to the 
					// withdrawal at _prev_date when 
					// cur_date is the timestep being 
					// solved.
	double		_endWithdraw;	// Withdrawal at the end of this
					// timestep--corresponds to the 
					// withdrawal at cur_date or the 
					// timestep being solved.
	double		_prevRelease;	// Release at the timestep before
					// the beginning of this timestep--
					// corresponds to the release
					// at _prev_date minus one timestep
					// when cur_date is the timestep 
					// being solved.
	double		_startRelease;	// Release at the beginning of 
					// this timestep-- corresponds to the 
					// release at _prev_date when 
					// cur_date is the timestep being 
					// solved.
	double		_endRelease;	// Release at the end of this
					// timestep--corresponds to the 
					// release at cur_date or the 
					// timestep being solved.

	// The following variables were added to more completely follow state
	// methodology as updated for the Rules, above.
	double		_prevStorage;	// Storage at the timestep before
					// the beginning of this timestep--
					// corresponds to the storage
					// at _prev_date minus one timestep
					// when cur_date is the timestep 
					// being solved.
	double		_startStorage;	// Storage at the beginning of 
					// this timestep-- corresponds to the 
					// storage at _prev_date when 
					// cur_date is the timestep being 
					// solved.
	double		_endStorage;	// Storage at the end of this
					// timestep--corresponds to the 
					// storage at cur_date or the 
					// timestep being solved.
	
};
#endif
