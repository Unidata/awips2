//------------------------------------------------------------------------------
// ResJSys - RESJ system object.
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
// 29 May 1998	Daniel Weiler, RTi	Added carryover and timeseries lists
//					for the FORTRAN interface
// 29 May 1998	MJR	Add functions and data members to handle the debug
//			and warning routines.
// 03 Jun 1998	DKW	Added all of the interfacing routines for the 
//			Fortran calling function.
// 04 Apr 2001  James R. VanShaar, RTi	Added carryover transfer functionality
// 08 May 2001  JRV, RTi	Added static integers _ipr and _iodebug for use 
//				in error, warning, status and debug handling
//				as well as the needed set, get methods
// 06 Aug 2002	JRV, RTi	Added Linux mapping of warn and error to warn_
// 				and error_.
// 07 Feb 2006  JRV, RTi    Introduced the _mainum static variable, 
//                          setMAINUM and getMAINUM static member functions.
// 18 Feb 2006  JRV, RTi    Revised to fix co string issue in hindcasting
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#ifndef ResJSys_INCLUDED
#define ResJSys_INCLUDED

#include "ResJ.h"
#include "Component.h"

// The two different run modes are to account for the initial parsing of the 
// control file versus the complete run of the system.

#define	RUN_SIMULATION 1
#define RUN_SYNTAX_CHECK 2

#define TOPOLOGY_READ 0x1
#define RULES_READ 0x2
#define PARAMETERS_READ 0x4

#define DEBUG_INDEX 3
#define WARNING_INDEX 4

#define ResJ_ccwrite resj_ccwrite 
#define ResJ_ccfcwtco resj_ccfcwtco 

#ifdef LINX
  #define ResJ_ffcwtco resjffcwtco_
  #define ResJ_fwrite  resjfwrite_
  #define ERROR error_
  #define WARN warn_
#else
  #define ResJ_ffcwtco resjffcwtco
  #define ResJ_fwrite  resjfwrite
  #define ERROR error 
  #define WARN warn
#endif


extern "C" {
void input_parameters58( char* fname, float* PO, float* CO, 
	int* iusec, int* ipr, int* iodebug, int* ibug, int* ierr );
void prettyprint58( char* fname, int* iodebug, int* ipr, int* ierr );
void punch58( char* fname, float* CO, int* iusec, 
	int* ipu, int* ipr, int* ibug, int* ierr );
int resj_ccwrite( int*, char*, int* );
int resj_ccfcwtco( int*, int*, float*, int* );
void execute58( char* fname, float* CO, float* PO, float* D, 
	int* d_index, int* co_da, int* co_hr, int* num_co, int* st_da,
	int* st_hr, int* end_da, int* end_hr, 
	int* ifillc, int* iusec, int* ibug, int* ipr, int* iodebug, int* ierr);

// Added to effect carryover transfer: JRV-3/3/01
    //  Changed name to carryovertransfer58: DWS-9/21/01
int carryovertransfer58 ( float* COLD, float* CNEW, float* POLD, float* PNEW,
        char* resjFOLD, char* resjFNEW, int* ibug, int* ipr, int* iodbug);

#ifdef LINX
  void resjffcwtco_( int*, int*, float*, int* );
  void resjfwrite_( int*, char*, int* );
	void error_(void);
	void warn_(void);
#else
  void resjffcwtco( int*, int*, float*, int* );
  void resjfwrite( int*, char*, int* );
  void error(void);
  void warn(void);
#endif

}

class ResJSys
{
public:
	void operator= ( const ResJSys& );
				// = overload. 

	ResJSys();		// Default constructor.

	ResJSys( const ResJSys& );
				// Copy constructor.

	~ResJSys();		// Destructor.

	static int addPOString( char* );
	static int addCOString( char* );
				// Builds the _po_list and _co_list.

	Component* getComponent ( void );
				// Searches ResJSys for a given component
				// based on type and name
	
	static char* getDebugString( int* );
				// Returns the debug messages as one large
				// string.

	static char* getPOString( int*, int* );
	static char* getCOString( int* );
	static char** getCOArray( int*, int* );
				// Returns the _po_list, _co_list, 
				// _co_array_str.
	Component* getRoot ( void ) { return _root; };
				// Returns the root component of the system
	static int resetCOString();
				// Sets _co_list to NULL after the
				// contents have been saved somewhere
				// non-volatile.

	static int deleteCOString();
				// Just destroys _co_list

	TS** getResJOutput( char**, int );
				// Looks for the output TS IDs and returns
				// pointers to them.
	
	static char* getWarningString( int* );
				// Returns the warning messages as one large
				// string.

	static int isCarryoverDate( TSDate& );
				// Returns 1 if the incoming date matches a
				// date in the _co_dates array.
	int run( char* );
				// Main controlling function. This function 
				// is responsible for parsing the appropriate
				// section of the control file for RESJ 
				// commands.

	int run( char*, TSDate&, TSDate& );

	int run( char*, int, char**, float** );

	int setStartDate( int, int, int, int yr=0 );
	int setStartDate( TSDate& );
	int setEndDate( int, int, int, int yr=0 );
	int setEndDate( TSDate& );
	static int setCODates( int*, int*, int*, int*, int );

	static int saveDebugMessages( int, char*, char* );
				// Routine which receives all the debug
				// messages below a certain level.
	static int saveWarningMessages( int, char*, char* );
				// Routine which receives all the warning
				// messages below a certain level.

	static int setDebugSaveLevel( int );
				// Function to set the debug save level.

	static int setWarningSaveLevel( int );
				// Function to set the warning save level.

private:
	int freeDataSpace();	// Frees up the instance data space.

	static int freeDataSpaceStatic();
				// Frees up the static data space.

	int initialize();	// Initializes instance data members.

	static int initializeStatic();	// Initializes inst

	int parseParameters( char**, int );
				// Main parsing routine for the parameters
				// section.
	int parseConstants( char**, int );
				// Parses out CONSTANT values
	int parseRules( char**, int, char**, int );
				// Main parsing routine for the rules
				// section.
	int parseTimeSeries( char**, int );
				// Main parsing routine for the timeseries
				// section.
	int parseTopology( char**, int );
				// Main parsing routine for the topology 
				// information.
	int runSolver();	// This function runs the solver...

	// Instance data members...

	char		_control_file[MAXC];

	Component	*_root;

	TSDate		_t1;	// The start date of the simulation.

	TSDate		_t2;	// The end date of the simulation.

	static TSDate*		_co_dates;
				// Array of dates that the Carryover should be
				// saved.

	static int	_num_co_dates;
				// Number of dates in the _co_dates array.

	static char**  	_co_array_str;
				// List of strings corresponding to the 
				// carryover values saved off at
				// the _co_dates.
	static int	_num_co_str;
				// Counter used to keep track of what position
				// in the _co_array_str list we are writing to.

	// Static data members...

	static char*	_co_list;	// Carryover list needed by FORTRAN>

	static int	_co_size;	// Size of _co_list.

	static char*	_debug_str;	// The string which contains
					// the debug and status messages.

	static int	_debug_str_len;	// The length of _debug_str;

	static int	_iodebug;	// Unit number of debug printout device
					// needed to print to NWS (FORTRAN).

	static int	_ipr;		// Unit number of standard print device
					// needed to print to NWS (FORTRAN).

	static char*	_po_list;	// Timeseries list needed by FORTRAN.

	static char*	_warning_str;	// The string which contains
					// the warning messages.
	
	static int	_warning_str_len;
					// The length of _warning_str.

	static int	_po_size;	// Size of _po_list;

	static int	_ref_count;	// The number of ResJSys objects that
					// have been constructed. This is 
					// incremented in the Constructor and
					// decremented in the Destructor and is
					// used for initializing and 
					// cleaning up the static data members.

// Added to allow access to print devices: JRV-5/8/01
public:
	static int setIPR_IODEBUG ( int, int );
				// Function to assign standard and debug
				// print devices

	static int getIPR ();
				// Accesses the standard print device unit
				// number

	static int getIODEBUG ();
				// Accesses the debug print device unit
				// number

// Added to prevent duplicate definition of time series in carryover_transfer
	static void set_TSdefined ();
	static int get_TSdefined ();

        static int setMAINUM ( int );
        static int getMAINUM ();

private:
	static int _TSdefined;

        static int _mainum;

        static int _writeLastCO;  // Trigger for carryover writing, esp. for
				  // hindcasting issues.
};

#endif
