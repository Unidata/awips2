//------------------------------------------------------------------------------
// SetWithdraw - Object containing all SetWithdraw scheme information.
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
// 17 Apr 1998	Daniel Weiler, RTi	Added first cut at data members/
//					functions
// 20 Jun 2001	James R. VanShaar, RTi	Replaced usage of _n_withdraw with
//					_n_elev for consistency with SetRelease
// 12 Nov 2001  James R. VanShaar, RTi  Added setInactiveState
// 12 Nov 2001  JRV, RTi        Added function setCOstring
// 11 Dec 2002	JRV, RTi	Added *_receivingComp, use of Component.h, 
// 				_Comp_ts, _toCompMode, sendToTS.
// 18 Dec 2002	JRV, RTi	Added transferCO().
// 27 Mar 2006  JRV, RTi    Added void sendToTS( TSDate date ),
//                          void sendToTS( TSDate date, double scalar ), and
//                          getMyValueOut( double *in, double *out ).
//                          Eliminated double sendToTS( TSDate, double, double).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef SetWithdraw_INCLUDED
#define SetWithdraw_INCLUDED

#include "Component.h"
#include "ReservoirMethod.h"

class SetWithdraw : public ReservoirMethod 
{
public:
	SetWithdraw( Reservoir* );	// Default constructor

	SetWithdraw( const SetWithdraw&, Reservoir* );
					// Copy constructor.

	virtual ~SetWithdraw();		// Destructor

	int buildWithdrawTS( char**, int );

	int construct( char**, int );	// Time series construction.

	SetWithdraw* copy( Component* );     // Calls copy constructor.

	int freeDataSpace();		// Deletes dynamically allocated data.

	void getMyValueAsOut( double *in, double *out );
	int solveMethod( TSDate &, int, double** = NULL );	
					// Solving algorithm.

	int print( FILE* );		// Prints all the info about the
					// SetWithdraw object.

	void setInactiveState();	// Resets any variables which represent
					// a continuation of method activity 
					// from the previous time step

	int setCOstring();	// Prepares carryover string for original 
				// parameter input and CO array sizing

	int setCOstring(TSDate&);	// Prepares adds carryover string to the
					// ResJSys carryover array

	Component*	_receivingComp;		// Component for toComp 
						// withdrawal.

	int linkCopiedTS( Component *, Component * );

	int transferCO ( Method * methOLD, char * cOLD, char * cNEW,
		int * ipr );
					// Transfers SetWithdraw carryover given
					// the new SetWithdraw method and the 
					// old and new related portions of the 
					// carryover strings

private:
	int initialize();		// Initialize data members

	DistributedTS*	_withdraw_ctl;	// Withdraw TS from the control file.
	HourTS*		_withdraw_obs;	// Input observed withdraw TS.

	int 	_n_elev;		// Number of withdraw TSs and elevations
	//int 	_n_withdraw;		// Number of withdraw TS.		

	double*	_elev;			// List of elevation values 
					// corresponding to each withdraw TS

	int 	_n_blend_tbl,	// Number of blending time steps.
		_n_blend_ts,
		_tbl_step,	// Number of time steps since last update.
		_ts_step;
	
	int _toCompMode;	// # of timesteps to lag transfer of withdrawal
				// to the Component.
	
	void  sendToTS( TSDate date );
	void  sendToTS( TSDate date, double scalar );

protected:
	HourTS	_Comp_ts;		// Component inflow TS at which the 
					// withdrawal will be applied.
};

#endif
