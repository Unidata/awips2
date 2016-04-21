//------------------------------------------------------------------------------
// Node - encapsulates information about a node topology type.
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
// 07 Apr 1998 	Daniel Weiler, RTI	Added setStates virtual.
// 13 Nov 2001  James R. VanShaar, RTi	Added function setCOstring
// 20 Nov 2001	JRV, RTi	Added _dischargeCO and _previousDischargeCO
// 20 Nov 2001	JRV, RTi	Added transferCO().
// 10 Jun 2002	JRV, RTi	Added and / or updated setEndInflow(), 
//				setEndOfTimestepStates, inflow, and discharge 
//				states for Rules testing.
// 24 Feb 2003	JRV, RTi	Revised parameters passed to be consistent with
// 				Component carryover, despite not being a source
// 				of error.
// 16 Feb 2006  JRV, RTi    Added diversion states.
//                          Commented out unused stage work.
//                          Added diversion time series.
// 16 Mar 2006  JRV, RTi    Added valueToMethod, revised finalizeSolution to be
//                          virtual.
// 14 Aug 2007  DJS, RTi    Added capability to define rating table or rating  
//                          curve id at the node for Reservoir Tools
//                          enhancement project.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Node_INCLUDED
#define Node_INCLUDED

#include "Component.h"
#include "resj/Table.h"

class Node : public Component
{
public:
	Node*	copy();		// Calls copy constructor on "this"

	//HourTS* getOutputTS( char* );

	int buildMinimumTS( char**, int );

        virtual int finalizeSolution( TSDate& ); 
				// Handles solution cleanup at the end of 
				// each time-step.
	int freeDataSpace();	// Frees dynamically allocated data.

	Node();			// Default constructor.

	Node( const Node& );
				// Copy constructor.

	virtual ~Node();		// Destructor.

	void operator= ( const Node& );
				// = Operator.
	virtual void printContents( FILE* );

        int setCOstring();      // Prepares carryover string for original
                                // parameter input and CO array sizing

        int setCOstring(TSDate&);       // Prepares carryover string for an
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

	int setStates( char**, int );

//	HourTS	*_stage_ts;

	int transferCO ( Component * compOLD, char * cOLD, char * cNEW,
		int * ipr );
					// Transfers Node carryover given
					// the new Node component and the old 
					// and new related portions of the 
					// carryover strings

	void valueToMethod ( TSDate date, int reduceDiv );

	HourTS  *_diversion_ts;
			// Diversion time series.
protected:
	virtual double* getInternalValuePtr( char* );
				// Returns a pointer to an internal data
				// member.
private:
	int initialize();	// Initializes private data members.

public:
	// NOTE:  In previous paradigms, the Node simply added inflows and
	//	discharged without further transformation.  Both inflows
	//	and discharges (outflows) are considered below due to the 
	//	possibility of adding a diversion capability to the Node at
	//	some future time.
	double		_prevInflow;	// Total instantaneous inflow to the 
					// node at the timestep before
					// the beginning of this timestep--
					// corresponds to the sum of inflows 
					// at _prev_date minus one timestep
					// when cur_date is the timestep 
					// being solved.
	double		_startInflow;	// Total instantaneous inflow to the 
					// node at the beginning of this
					// timestep--corresponds to the sum
					// of inflows at _prev_date when
					// cur_date is the timestep being
					// solved.
	double		_endInflow;	// Total instantaneous inflow to the 
					// node at the end of this
					// timestep--corresponds to the sum
					// of inflows at cur_date or the 
					// timestep being solved.

        double          _discharge;     // Diversion from the node (after
					// inflow, before discharge).
	double		_prevDischarge;	// Discharge from the node at the 
					// timestep before the beginning of this
					// timestep--at _prev_date minus one 
					// timestep when cur_date is the 
					// timestep being solved.
	double		_startDischarge;// Discharge from the node at the 
					// beginning of this timestep--at 
					// _prev_date when cur_date is the 
					// timestep being solved.
	double		_endDischarge;	// Discharge from the node at the end of
					// this timestep--at cur_date or the 
					// timestep being solved.
        double          _diversion;     // Diversion from the node (after
					// inflow, before discharge).
	double		_prevDiversion;	// Diversion from the node at the 
					// timestep before the beginning of this
					// timestep--at _prev_date minus one 
					// timestep when cur_date is the 
					// timestep being solved.
	double		_startDiversion;// Diversion from the node at the 
					// beginning of this timestep--at 
					// _prev_date when cur_date is the 
					// timestep being solved.
	double		_endDiversion;	// Diversion from the node at the end of
					// this timestep--at cur_date or the 
					// timestep being solved.

	DistributedTS	_min_ctl;	// TS read from the control file
	int		_min;		// Trigger for minimum flow parameterization
	int		_mode;		// Mode of MinRemainder interpolation.
      	bool   		_has_rating_table;
					// The following two params are new as of
					// Aug 2007 and are optional -
					// if they are not defined in an input
					// deck, the system will function
					// as before (i.e. backward compatibility
					// is preserved.
	Table   	_rating_table;  // rating curve table (optional)

        char     	_rating_curve_id[MAXC]; // rating curve id (optional)
};
#endif
