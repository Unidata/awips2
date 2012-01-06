//------------------------------------------------------------------------------
// Component - base class for different types of topology.
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
// 13 Jan 1998  Daniel Weiler, RTi	Added some set/get functions.
// 18 Feb 1998	MJR	Removed some extra routines and variables that are
//			no longer used. Also added routines to deal with 
//			constants being set.
// 20 Feb 1998	MJR	Added the print function to help with debugging.
// 04 Mar 1998	MJR	Added the checkMethods function to make sure that
//			each method has been constructed. Also change the 
//			getMethod function so that it takes three id's, the
//			method type, the component, and the method id.
// 31 Mar 1998	MJR	Add the solve() and finalizeSolution() functions to
//			the class.
// 02 Apr 1998	DKW	Added first version of the inflow/outflow wiring.
// 07 Apr 1998	DKW	Added setStates virtual.
// 15 Apr 1998	DKW	Added finalize function which calls finalizeSolution
//			recursively.
// 27 Apr 1998  DKW	Added mean release TS. (outflow? JRV)
// 05 May 1998  DKW	Added prev_date.
// 05 May 1998  DKW	Added buildSubTree.
// 03 Jun 1998	DKW	Added Carryover date stuff
// 24 May 2001	James R. VanShaar, Rti	Added resetInflowTS() 
// 13 Nov 2001  JRV, RTi        Added virtual function setCOstring(TSDate &)
// 13 Nov 2001  JRV, RTi        Added virtual function setCOstring()
// 05 Jun 2002	JRV, RTi	Added virtual function setEndInflow(TSDate &)
// 11 Dec 2002	JRV, RTi	Added _SolutionOrder, setSolutionOrder, 
// 				getSolutionNumber.
// 13 Dec 2002	JRV, RTi	Added _specialTieOut, _specialTieIn, 
// 				buildSubTree_specialTieIn, 
// 				builtSubTree_specialTieOut, getMethod( char * ).
// 19 Feb 2004	JRV, RTi	Added _totalInflow timeseries.
// 19 Feb 2006  JRV, RTi    Added buildSubTree_CompIDList( Component *ucp,
//                              char ** IDList, int* nList )
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#ifndef Component_INCLUDED
#define Component_INCLUDED

#include "ResJ.h"
#include "Expression.h"
#include "HourTS.h"
#include "Method.h"

#define PRETRAV 1
#define POSTTRAV 2 

#define MAX_METHOD 200

class Component 
{
public:
	int addSon( Component* );
				// Adds a son to the component and sets the
				// father pointer.
	Component();		// Default constructor.

	Component( const Component& );

	virtual ~Component();		// Destructor.

	int addConstant( char*, char* );
	int addConstant( char*, double );
				// Routines to attach constant values to
				// a component.

	int addExpression( Expression*, char*, char*, char* );

	int addMethod( Expression*, Method* );

	Component* buildSubTree( Component*, Method*, TSDate &, HourTS & );
	//Component* buildSubTree( Component*, Method* );
				// BUilds sub-topology used by Max Stage.

	char** buildSubTree_CompIDList( Component*, char**, int* );

	int buildSubTree_specialTieOut( Component*, Component* );

	int buildSubTree_specialTieIn( );

	int checkMaxStageUS( Component* );
				// Checks to see if there are any MaxStage 
				// methods directly upstream of this.

	int checkMethods();	// Recursively checks all the methods of the
				// component.

	virtual Component*	copy() = 0;

	virtual int finalizeSolution( TSDate& ) = 0;

	int finalize( TSDate& );	
				// Takes care of solver cleanup at the end of 
				// each time step for the component.
	Component* findRoot();	// Traverses the tree upward from any
				// Component in a tree and returns the root..
	int freeDataSpace();	// Frees object's memory.

	Component* getComponentPtr( char* );

	char* getID( ); 	// Returns the ID from the topo number.

	HourTS** getInflowTS();

	HourTS* getOutflowTS();

	HourTS* getOutputTS( char* );
				// Gets the appropriate TS depending on the 
				// char*;  i.e. release, outflow, etc.

	Method* getMethod( char*, char*, char* );
				// Returns a method pointer from the 
				// component and method IDs.

	Method* getMethod( char* );
				// Returns a method pointer using a method ID.
	
	int getSolutionNumber();	// Returns the value of _SolutionNumber.

	char* getType();	// Returns the type string.

	double*	getValuePtr( char* );
				// Returns the pointer to a double that is
				// identified by the passed in string.
	void operator= ( const Component& );
				// = overload.
	void print( FILE* );
				// Prints the contents of the component.
	int setID( char* );	// Sets the component's identification.

	int resetInflowTS( HourTS* );

	virtual int setCOstring();	// Prepares carryover string for 
					// original parameter input and CO array
					// sizing

	virtual int setCOstring(TSDate&);	// Prepares carryover string for
						// an existing the ResJSys carry
						// over array

	int setInflowTS( HourTS* );

	void setOutflowVal( TSDate&, double );

	virtual int setStates( char**, int ) = 0;

	double sumInflow( TSDate& );	
				// Sums the list of inflows if the Component
				// has multiple upstream feeders.

	double getTotalInflow( TSDate& );	
				// Sums the list of inflows if the Component
				// has multiple upstream feeders.

	int _SolutionNumber;	// A number representing the order in which 
				// Components will be solved for a given 
				// timestep.

	int solve( TSDate&, int );
				// Function which solves each of the 
				// expression-method pairs if necessary.

	int verifyExpressions();
				// Calls verify recursively on the expression 
				// tree.

	char	_type[MAXC];	// The type of component.

	Component	**_son_list;
				// Dynamically allocated array of sons...
	Component 	*_father;
				// Father pointer.
	char 		**_const_id_list,
				// List of constant identifiers.
			_id[MAXC];
				// Identifier of this component.
	double		*_const_val_list;

	Expression**	_expr_list;

	int 		_n_const,
				// Number of constants..
			_n_meth,// Number of expressions and methods in the
				// _expr_list and _method_list
			_n_son;	// Number of sons in the _son_list.

	int 		_in_count;
				// Number of inflow TS in _inflow

	Method**	_method_list;

	HourTS		**_inflow_ts, 	
				// Inflow which is a pointer to the upstream
			_outflow_ts,	
				// Component's outflow (possibly multiple).
			_mean_outflow_ts,
				// Mean outflow.
			_totalInflow;

			// WARNING: The next is hardcoded here and elsewhere
			// in the code!!
	HourTS		*_output_ts[15];
				// Array of TS pointers that will be written
				// back to the Process Database.
	int		_n_output_ts;

	int		setPrevDate( TSDate& );
	TSDate		_prev_date,
			getPrevDate();
				// Previous date used in the solve methods.


	virtual double* getInternalValuePtr( char* ) = 0;
				// Returns the pointer to a private data
				// member based on the key-word passed in.
	void printComponent( FILE* );
				// Prints the contents of the component.
	virtual void printContents( FILE* ) = 0;
				// Prints the contents of the derived classes.
	static int setCOFlag( int );
	static int _is_co_date;
				// If this is true, save carryover on every
				// component. 

        static double   _lfactor,       // length conversion factor
                        _ffactor,       // flow conversion factor
                        _vfactor,       // volume conversion factor
                        _cfactor;       // length/flow conversion factor

	int _conserve_mass;

	virtual int transferCO( Component * resOLD, char * cOLD, char * cNEW,
                int * ipr );
                                // Transfers component carryover given
                                // the old and new components and the old
                                // and new related portions of the carryover
                                // strings

private:
	int initialize();	//initialize private data members.

public:
	virtual void setEndInflow(TSDate&);	// Sums inflows at the end of 
						// the current timestep and sets
						// the value of _endInflow, and
						// also puts it in the 
						// _totalInflow timeseries, if 
						// applicable.

	virtual void setEndInflow(TSDate&, double);	
						// Sets the value of _endInflow,
						// and also puts it in the
						// _totalInflow timeseries, if 
						// applicable.

	virtual int setEndOfTimestepStates(TSDate&);	// Reset states for next
							// timestep and output 
							// carryover, if 
							// necessary.

	int setSolutionOrder( int );	// Defines _SolutionNumber based on the
					// order in which Components will be 
					// solved for a given timestep.

	int _specialTieOut;	// Trigger used for handling of flows
				// in finalizeSolution.  Specifically
				// currently used for SetWithdraw 
				// ToComp approach.
				// Value of 0, no special tie to Comp.
	
	int _specialTieIn;	// Trigger to know if one or more of the 
				// inflows is from a _specialTieOut.

};

#endif
