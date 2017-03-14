//------------------------------------------------------------------------------
// Method - Base class for scheme classes.
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
//					Changed 'constructTS' to 'construct' 
//					to make sense for all the methods. Also
//					add the '_is_constructed' flag.
// 31 Mar 1998	DKW	Added the getTimeStep.
// 31 Mar 1998	MJR	Added an integer flag to the solveMethod function
//			for isPrimarySolution.
// 06 Apr 1998  DKW	Added interval and mult for time step.
// 08 Apr 1998	MJR	Added static functions to get and set the dates.
// 12 Nov 2001	James R. VanShaar, RTi
//					Added virtual function setInactiveState.
// 12 Nov 2001	JRV, RTi	Added an integer flag _hasStates
// 12 Nov 2001	JRV, RTi	Added virtual function setCOstring
// 11 Dec 2002	JRV, RTi	Added _Active, _myValue, sendToTS.
// 15 Dec 2002	JRV, RTi	Added linkCopiedTS()
// 24 Dec 2002	JRV, RTi	Added SPILLWAY_METHOD.
// 12 Feb 2004	JRV, RTi	Added INFLOW_METHOD.
// 27 Mar 2006  JRV, RTi    Added sendToTS( TSDate date ),
//                          sendToTS( TSDate date, double scalar ),
//                          getMyValueAsOut( double *in, double *out ),
//                          DIVERS_METHOD.
//                          Deleted virtual double sendToTS( TSDate, double,
//                          double ).
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef Method_INCLUDED
#define Method_INCLUDED

#include "DistributedTS.h"
#include "TSList.h"

#define	LOSS_METHOD 1
#define	WITHD_METHOD 2 
#define	RELEASE_METHOD 3 
#define	SPILLWAY_METHOD 4 
#define	INFLOW_METHOD 5 
#define	DIVERS_METHOD 6 

#define SI 0
#define	ENGLISH 1 

#define	MAX_COMPONENT	100

class Component;

class Method 
{
public:
	Method();			// Default constructor.

	Method( const Method& );	// Copy constructor.

	virtual ~Method();		// Default destructor.

	int	getGroupID();		// returns _group_id

	char*	getID();		// returns the _id.

	char*	getType();		// returns the type string '_type'.

	int 	setID ( char* );	// sets _id.

	double** allocateDataSpace( int rows, int cols );
					// allocates data space for 2-D
					// double array.
	
	// pure virtual functions
	virtual int freeDataSpace() = 0;

	virtual int construct( char**, int ) = 0;

	virtual Method* copy( Component* ) = 0;

	int isConstructed();	// Returns the value of the is constructed flag.

	virtual void getMyValueAsOut( double *in, double *out );

	virtual int solveMethod( TSDate&, int, double** = NULL ) = 0;

	int _Active;		// Integer determining the state of a method.
				// If 0 = expression returned FALSE, the method
				//   was not solved.
				// If 1 = expression returned TRUE, the method
				//   was solved.
				// If -1 = expression returned TRUE, the method
				// was solved, but discarded due to SetMin or 
				// SetMax dominance.

	int _mode;		// Normal or interpolate distributed TS, when 
				// appropriate.
				// For SetRelease: 0 = Normal, no interpolation
				//	1 = Interpolate in time (default if no 
				//		additional parameter given)
				//	2 = Interpolate across elevations
				//	3 = Interpolate across elevations and in
				//		time

	int _hasStates;		// Carryover states are required (1-yes, 0-no)

	double _myValue;	// The value last calculated by the method.

	virtual void setInactiveState();

	virtual int setCOstring();		// Prepares adds carryover string
						// to the ResJSys carryover array

	virtual int setCOstring(TSDate&);	// Prepares adds carryover string
						// to the ResJSys carryover array

	int _ordered;

	virtual int print( FILE* ) = 0;

	virtual Component* getOwner() = 0;

	virtual void  sendToTS( TSDate date );
	virtual void  sendToTS( TSDate date, double scalar );

	virtual int linkCopiedTS( Component *, Component * );

	int		_group_id;	// Used to identify what a method solves
					// for so that the ComboMethods know
					// what is compatible with what.
	// static functions

	static TSDate getForecastDate1();

	static TSDate getForecastDate2();

	static int getTimeInterval();

	static int getTimeMult();

	static double getTimeStepSec();

	static int setForecastDate1( TSDate& );

	static int setForecastDate2( TSDate& );

	static int setTimeStep( int );

	static int setTimeStep( int, int );

	static int getUnitType();
	static void setUnitType( int );

	static double _t_step_in_sec;
private:
	int initialize();

protected:
	char		_id[MAXC],	// Method identifier.	
			_type[MAXC];	// Type of Method.

	int		_is_constructed;// Flag that is TRUE if the method
					// has been constructed...

	static int 	_t_int,		// Time step interval and mult,
			_t_mult,	// (int will be hourly for now)

			_units;		// 0 for metric, 1 for english 

	static TSDate	_forecast_date1,// The start and end dates for the 
			_forecast_date2;// forecast.
			
};

#endif
