//------------------------------------------------------------------------------
// MaxStage - Object containing all MaxStage scheme information.
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
// 06 May 1998	Daniel Weiler, RTi	Added first version of data members and
//					functions. 
// 01 Jun 2001	James R. VanShaar, RTi	Added _sumLag
// 30 Jan 2002	JRV, RTi	Added _setInactiveState().
// 17 Mar 2003	JRV, RTi	Added getDCP()
// 14 Aug 2007  DJS, RTi	Added _max_discharge for Res. Tools Enhancement
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef MaxStage_INCLUDED
#define MaxStage_INCLUDED

#include "ReservoirMethod.h"
#include "Component.h"
#include "Expression.h"
#include "Solver.h"
#include "Node.h"
#include "Reach.h"

#define MAX_ITER 100	

class MaxStage : public ReservoirMethod 
{
public:
	MaxStage( Reservoir* );	// Default constructor

	MaxStage( const MaxStage&, Reservoir* );
					// Copy constructor.

	virtual ~MaxStage();	// Destructor

	void addTimeStep( int );

	void addTimeStep( int, double );

	int construct( char**, int );	// Time series construction.

	MaxStage* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	Node* getDCP();			// Returns the downstream control point.

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	void setNextDSComp( Component*, int );
					// Sets the _next_ds_comp pointer.

	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// MaxStage object.

	void setAllGroups(Component* , Component* );

	void resetDcp(); //reset _dcp because the constructor create one not link one

private:
	int initialize();		// Initialize data members

	Node*	_dcp;		// Downstream control point for the MaxStage
				// method - restricting this to Nodes for now.

	Component	*_next_ds_comp;
				// This is the next downstream component from
				// the reservoir.
	int		_inflow_pos,	
				// Position in the _next_ds_comp's inflow_ts 
				// array occupied by the _owner of this 
				// method.
			_max_iter;
				// Maximum number of iterations for this method.

	double	_max_stage,	// Max allowable stage at the downstream point.
		_max_discharge, // Max allowable discharge from the reservoir
		_maxflow,       // Max flow (function of _max_stage lookup or
                                //    _max_discharge assignment
		_highestFlow,	// highest flow of Rating Curve/Table for object
		_min_release,	// Minimum release allowable for the reservoir
		_criterion,	// Convergence criterion
		_current_rel;	// Current reservoir release being tested for
				// exceedence of downstream stage.

	Table	_stage_flow_tbl;	
				// stage vs. flow relationship for the dcp.

	int	_n_tstep;	// This number represents the number of time steps
				// that it takes for the release to reach the 
				// downstream control point.

	double _sumLag;		// This will track the total lag time from the
				// controlling reservoir to the downstream
				// control point, for use in assigning _n_tstep

	double _sumK;		// This will track the total K time from the
				// controlling reservoir to the downstream
				// control point, for use in estimating 
				// reservoir release during maxstage iteration.

public:
	HourTS _inFromOwnRes;	// Timeseries used as outflow from 'owning' 
				// reservoir to be assigned as an inflow to the
				// component just downstream
};

#endif
