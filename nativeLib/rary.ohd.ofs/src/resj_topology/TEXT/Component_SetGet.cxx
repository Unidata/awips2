//------------------------------------------------------------------------------
// Component :: SetGet 	Various set/get routines.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 18 Feb 1998	Matthew J. Rutherford, RTi
//					Removed some extra functions that
//					aren't needed anymore.
// 19 Feb 1998	DKW	Added functionality.
// 04 Mar 1998	MJR	Changed the prototype for the getMethod function so that
//			it takes the method type also.
// 02 Apr 1998  DKW 	Inflow/outflow time series set/gets
// 03 Jun 1998  DKW 	Added Carryover flag, getOutputTS.	
// 27 Jul 2001	James R. VanShaar, RTi	Updated addExpression to include 
//					reordering of method_list according
//					to the order they appear in RULES.
// 13 Nov 2001	JRV, RTi	Added setCOstring(TSDate &)
// 13 Nov 2001	JRV, RTi	Added setCOstring()
// 05 Jun 2002  JRV, RTi        Added function setEndInflow(TSDate &)
// 06 Jun 2002	JRV, RTi	Created initial version of 
//				setEndOfTimestepStates by copying stuff from 
//				Component::solve().
// 11 Dec 2002	JRV, RTi	Added setSolutionOrder, getSolutionNumber.
// 				Also reordered the file alphabetically.
// 19 Feb 2004	JRV, RTi	Added 
// 				Component :: setEndInflow ( TSDate&, double )
// 				Revised 
// 				Component :: setEndInflow ( TSDate& date )
// 17 Apr 2006	JRV, RTi	Modified getValuePtr( char* id ) to handle
//				unit conversion of constants here.  It used to
//				be handled back in 
//				double* Expression :: getConstantPtr().
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: addConstant( char* id, char* value )
{
	return( addConstant( id, (double)atof( value ) ) );
}

int Component :: addConstant( char* id, double value )
{
	char	routine[]="Component :: addConstant";
	int	i=0;

	// First we have to make sure that the supplied constant is not a 
	// reserved word...

/*
	if( getInternalValuePtr( id ) ){
		PrintWarning( 1, routine,
			"\"%s\" is a internal variable for %s, cannot set the "
			"value.", id, _id );
		return( STATUS_FAILURE );
	}

*/
	// Next we need to check if this constant has been added yet...

	for( i=0; i<_n_const; i++ ){
		if( !strcmp( _const_id_list[i], id ) ){
			PrintDebug( 10, routine,
				"Constant \"%s\" already set to %f. Updating "
				"to %f.", id, _const_val_list[i], value );
			_const_val_list[i]	= value;
			return( STATUS_SUCCESS );
		}
	}

	// We came out of the loop without finding anything, so we need to add 
	// it.

	_const_id_list = AddToStringList( _const_id_list, id, &_n_const );

	_const_val_list = (double*)realloc( (double*)_const_val_list, 
		_n_const * sizeof( double ) );

	if( !_const_val_list ){
		PrintWarning( 1, routine,
			"Unable to allocate memory for the %dth double.", 
			_n_const );
		return( STATUS_FAILURE );
	}

	_const_val_list[_n_const-1] = value;

	PrintDebug( 10, routine, "Set \"%s\"=%f at list position %d.", 
		_const_id_list[_n_const-1], _const_val_list[_n_const-1], 
		(_n_const-1) );

	return( STATUS_SUCCESS );
}

int Component :: addExpression( Expression* expr, char* type, char* comp_id,
	char* meth_id )
{
	// JRV: Added break in i loop and ordering of methods according
	//	to rules.

	char routine[]="Component :: addExpression";
	int i, j;

	for( i = 0; i < _n_meth; i++ ) {
		if( !strcasecmp( type, _method_list[i]->getType() ) &&
		!strcasecmp( comp_id, _id ) &&
		!strcasecmp( meth_id, _method_list[i]->getID() ) ) {
			if( _method_list[i] == NULL ) {
				PrintWarning( 1, routine, 
				"Cannot add expression to "
				"NULL method." );
				return( STATUS_FAILURE );
			}

			if( _expr_list[i] != NULL ) {
				PrintWarning( 1, routine, 
					"Previous expression %s for method %s will "
					"be overwritten with %s.", 
					_expr_list[i]->toString(), meth_id,
					expr->toString() );
			}
			_expr_list[i] = expr;
			break;
		}	
	}

	// Now reorder from how the method was read in from parameters section
	// to how it is read in the rules section.
	for( j = 0; j <= i; j++ ) {
		if( !_method_list[j]->_ordered ) {
			Method *tempMeth = NULL;
			tempMeth = _method_list[j];
			Expression *tempExpr = NULL;
			tempExpr = _expr_list[j];

			_method_list[j] = _method_list[i];
			_expr_list[j] = _expr_list[i];

			_method_list[i] = tempMeth;
			_expr_list[i] = tempExpr;

			tempMeth = NULL;
			tempExpr = NULL;

			_method_list[j]->_ordered = 1;

			break;
		}
	}
	
	
	return( STATUS_SUCCESS );
}

int Component :: addMethod( Expression* expr, Method* meth )
{
	char routine[]="Component :: addMethod";

	if( _n_meth == 100 ) {
		PrintWarning( 1, routine, "Exceeding max number of expressions "
			"and methods.");
		return( STATUS_FAILURE );
	}	

	if( meth == NULL ) {
		PrintWarning( 1, routine, "Cannot add NULL or method to list.");
		return( STATUS_FAILURE );
	}

	_expr_list[ _n_meth ] = expr;
	_method_list[ _n_meth ] = meth;

	// Increment the static counter
	_n_meth++;

	return( STATUS_SUCCESS );
}

int Component :: addSon( Component* son )
{
        char    routine[]="Component :: addSon";

	if( son == NULL ) {
		PrintWarning( 2, routine, 
		"Cannot add NULL pointer to son list.");
		return( 2 );
	}

	_n_son = _n_son + 1;

	// allocate one more pointer's worth of memory to the _son_list array

	_son_list = (Component**)realloc( (Component**)_son_list, 
		sizeof(Component*)*_n_son );

	if( _son_list == NULL ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %dth Component*.", _n_son );
		return( STATUS_FAILURE );
	}

	// add son to the array.
	_son_list[_n_son-1] = son;

	// Set the father of son. If the father of the son has already
	// been set, this is a bad topology as a son cannot have more than
	// one father
	if( _son_list[_n_son-1]->_father == NULL ) {
		_son_list[_n_son-1]->_father = this;
	}
	else {
		PrintWarning( 1, routine, "Illegal topology - Component "
		"%s cannot have multiple downstream links.", 
		_son_list[_n_son-1]->_id );
		return( STATUS_FAILURE );
	}

	return( STATUS_SUCCESS );
}

Component* Component :: getComponentPtr( char* ident )
{
	char routine[]="Component :: getComponentPtr";
	Component	*ptr=NULL;
	int i;

	if( ident == NULL ){
		PrintWarning( 1, routine,
		"Incoming Identifier is NULL." );
		return( NULL );
	}
	// First we check this ID against the incoming identifier...

	if( !strcmp( _id, ident ) ) {
		return( this );
	}

	// If it wasn't this one, then traverse the sons and call this 
	// routine recursively.

	for( i=0; i<_n_son; i++ ) {
		ptr = _son_list[i]->getComponentPtr( ident );
		if( ptr != NULL ){
			return( ptr );
		}
	}

	// It wasn't this one of any of the sons so return NULL.

	return( NULL );
}

char* Component :: getID()
{
	char routine[]="Component :: getID";

	return(_id);
}

HourTS** Component :: getInflowTS() 
{
	return( _inflow_ts );
}

Method* Component :: getMethod( char* meth_id )
{
	char routine[] = "Component :: getMethod";
	int i;
	Component* comp=NULL;
	Method* upstreamMeth = NULL;
	Method* localMeth = NULL;

	// Check all upstream components
	for( i=0; i<_n_son; i++ ){
		if( upstreamMeth == NULL ) {
			upstreamMeth = _son_list[i]->getMethod( meth_id );
		}
		else if( _son_list[i]->getMethod( meth_id ) ){
			// We got a second on on the other son
			PrintWarning( 1, routine, "Multiple methods with ID "
				"\"%s\" found on multiple components.  PLEASE "
				"REVISE PARAMETERIZATION!  Future versions may "
				"not support this.  Current carryovertransfer "
				"may not function correctly.", meth_id );
		}
	}

	// Check the current component
	for( i = 0; i < _n_meth; i++ ) {
		if( !strcmp( meth_id, _method_list[i]->getID() ) ) {
			// We found one.
			if( localMeth != NULL ) {
				// We have two on the same compoent!
				PrintWarning( 1, routine, "Multiple methods "
					"with ID \"%s\" found on component "
					"_id.  PLEASE REVISE PARAMETERIZATION! "
					"Future versions may not support this.  "
					"Current carryovertransfer may not "
					"function correctly.", meth_id );
			}
			else {
				localMeth = _method_list[i];
			}
		}
	}
	
	// Check to ensure we don't have a match on an upstream component and
	// the current component.
	if( upstreamMeth != NULL ) {
		if( localMeth != NULL ) {
			// We have methods on different components with the 
			// same ID
			PrintWarning( 1, routine, "Multiple methods with ID "
				"\"%s\" found on multiple components.  PLEASE "
				"REVISE PARAMETERIZATION!  Future versions may "
				"not support this.  Current carryovertransfer "
				"may not function correctly.", meth_id );
			return upstreamMeth;
		}
		else {
			return upstreamMeth;
		}
	}
	
	return localMeth;
}

Method* Component :: getMethod( char* meth_type, char* comp_id, char* meth_id )
{
	char routine[] = "Component :: getMethod";
	int i;
	Component* comp=NULL;

	comp = getComponentPtr( comp_id );

	if( comp == NULL ){
		PrintWarning( 1, routine,
		"Component \"%s\" does not exist in the topology tree.",
		comp_id );
		return( NULL );
	}

	int nMeth = comp->_n_meth;
	for( i = 0; i < comp->_n_meth; i++ ) {
		char *idX = comp->_method_list[i]->getID();
		char *typeX = comp->_method_list[i]->getType();
		if( !strcmp( meth_id, idX ) && !strcmp( meth_type, typeX ) ) {
			return( comp->_method_list[i] );
		}
	}

	// If we get here we didn't find it
	PrintDebug( 2, routine, 
		"Troubles getting %s %s on %s", meth_type, meth_id, comp_id );
	return( (Method*)NULL );
}

HourTS* Component :: getOutflowTS() 
{
	return( &_outflow_ts );
}

HourTS* Component :: getOutputTS( char* type )
{
	char routine[]="Component :: getOutputTS", **list = NULL, 
		**idlist = NULL;
	int i, nlist = 0, nidlist = 0;
	HourTS* tspt = NULL;

	for( i = 0; i < _n_son; i++ ) {
		tspt = _son_list[i]->getOutputTS( type );
		if( tspt != NULL ) {
			return( tspt ); 
		}
	}

	if( type == NULL || !strcmp( type, "" ) ) {
		PrintWarning( 2, routine, "Cannot get OutputTS - NULL data "
		"type." );
		return( NULL );
	}

	list = BreakStringList( type, " \t\n", DELIM_SKIP_BLANKS, &nlist ); 
	if( list == NULL || nlist != 3 ) {
		list = FreeStringList( list );
		return( NULL );
	}
        for( i = 0; i < _n_output_ts; i++ ) {
		idlist = BreakStringList( 
			(_output_ts[i]->getIdentifier()).getIdentifier(), ".", 
			0, &nidlist ); 
		if( idlist == NULL || nidlist < 4 ) {
			list = FreeStringList( list );
			idlist = FreeStringList( idlist );
			return( NULL );
		}
		if( !strcasecmp( list[0], idlist[0] ) &&
			!strcasecmp( list[1], idlist[2] ) &&
			( atoi( list[2] ) == atoi( idlist[3] ) ) ) {
			list = FreeStringList( list );
			idlist = FreeStringList( idlist );
			return( _output_ts[i] );
		}
		idlist = FreeStringList( idlist );
	}

	// If we got here then we didn't find it
	list = FreeStringList( list );
	return( NULL );
}

TSDate Component :: getPrevDate() 
{
	return( _prev_date );
}

int Component :: getSolutionNumber ()
{
	return( _SolutionNumber );
}

double Component :: getTotalInflow( TSDate& date )
{
	return( _totalInflow.getDataValue( date ) );
}

char* Component :: getType()
{
	return( _type );
}

double* Component :: getValuePtr( char* id )
{
	char	routine[]="Component :: getValuePtr";
	double	*value_ptr=NULL;
	int	i=0;

	// First we need to get pointer to internal value

	value_ptr = getInternalValuePtr( id );

	if( value_ptr != NULL ){
		PrintDebug( 10, routine,
		"Returning pointer to internal value for \"%s\".", id );
		return( value_ptr );
	}
	// It wasn't a reserved word so we need to check our constant list...

	value_ptr = NULL;
	for( i=0; i<_n_const; i++ ){
		if( !strcmp( _const_id_list[i], id ) ){
			PrintDebug( 15, routine,
			"Found \"%s\" at position %d.", id, i );
			value_ptr = &_const_val_list[i];
			// We found it in the constant list
			// Let's convert it here before we pass it back.
			double cfactor = Component::_cfactor;
			(*value_ptr) = (*value_ptr) * cfactor;
			break;
		}
	}

	return( value_ptr );
}

int Component :: setID( char* ident )
{
	char routine[]="Component :: setID";

	if( ident == NULL ){
		PrintWarning( 1, routine, "NULL identifier passed in." );
		return( STATUS_FAILURE );
	}
	if( !strcmp( ident, "" ) ) {
		PrintWarning( 2, routine, "Blank component identifier." );
		return(STATUS_SUCCESS);
	}
	strcpy( _id, ident );
	
	return(STATUS_SUCCESS);
}

int Component :: resetInflowTS( HourTS* in ) 
{
	// This function is intended for use when an upstream component needs 
	// to link its outflow into a component over an existing inflow 
	// timeseries.  This situation will occur according to the new pattern 
	// of building a subTree:  The components are copied, including a call 
	// to initialize which creates its new outflow timeseries, and including
	// a call to "Component :: Component( const Component& comp )" which now
	// copies all inflow time series.  Then the new outflow timeseries needs
	// to be assigned as the inflow timeseries where it was in the original
	// system.  The memory requirements should have been taken care of 
	// already.

	char routine[]="Component :: resetInflow";

	// Checking to see of this is the most upstream component. If it is,
	// then in must be coming from an input TS, i.e. it is non-NULL
	if( in == NULL && _n_son == 0 ) {
		PrintWarning( 2, routine, "Cannot set most upstream Component"
		" \"%s\" inflow to NULL.", _id );
		return( STATUS_FAILURE );
	}

	// _inflow_ts will have pointers to all the inflow timeseries' for the 
	// component.  Each timeseries defined by TSINPUT (from the parameter 
	// file) will have associated with it a fully defined identifier.  Those
	// that represent outflows from an upstream component do not have a 
	// fully defined identifier.
	// The procedure, therefore, is to search through

	// were included in _inflow_ts by sending a pointerby 'setInflowTS' 
	// where the inflow

	// Get Identification of 'in'
	TSIdent outflowID = in->getIdentifier();

	// Match the Identification to an existing timeseries in _inflow_ts

	// Assign a pointer the timeseries 'in' (corresponding to an outflow 
	// from an upstream component) into the existing inflow timeseries.

/****
	// Checking to see of this is the most upstream component. If it is,
	// then in must be coming from an input TS, i.e. it is non-NULL
	if( in == NULL && _n_son == 0 ) {
		PrintWarning( 2, routine, "Cannot set most upstream Component"
		" \"%s\" inflow to NULL.", _id );
		return( STATUS_FAILURE );
	}
	_in_count++;
	_inflow_ts = (HourTS**)realloc( (HourTS**)_inflow_ts, 
		(_in_count*sizeof( HourTS* ) ) );
	if( _inflow_ts == NULL ) {
		PrintWarning( 1, routine, "Could not allocate %d HourTS "
		"pointers.", _in_count );
		return( STATUS_FAILURE );
	}
	_inflow_ts[ _in_count-1 ] = in;
****/

	return( STATUS_SUCCESS );
}

int Component :: setCOFlag( int flag )
{
	if( flag == 0 ) {
		_is_co_date = 0;
	}
	else {
		_is_co_date = 1;
	}
	return( STATUS_SUCCESS );
}

int Component :: setCOstring( )
{
	return( STATUS_SUCCESS );
}

int Component :: setCOstring( TSDate& date )
{
	return( STATUS_SUCCESS );
}

void Component :: setEndInflow ( TSDate& date )
{
	_totalInflow.setDataValue( date, sumInflow( date ) );
	return;
}

void Component :: setEndInflow ( TSDate& date, double value )
{
	_totalInflow.setDataValue( date, value );
	return;
}

// Reset states for next timestep and output carryover, if necessary.
int Component :: setEndOfTimestepStates( TSDate& date )
{
	char	routine[]="Component :: setEndOfTimestepStates";
	int	i=0;

	// We have to recursively call the function on the 
	// sons...
	for( i=0; i<_n_son; i++ ){
		if( _son_list[i]->setEndOfTimestepStates( date ) ){
			return( STATUS_FAILURE );
		}
	}

	// Check for the need to write carryover
	if( _is_co_date ) {
		// write Component carryover
		setCOstring(date);
		// write method carryover
		for( i=0; i<_n_meth; i++ ) {
			if( _method_list[i]->_hasStates ) {
				_method_list[i]->setCOstring(date);
			}
		}
	}

	// Set the previous date on this component for use at the next time 
	//	step.
	setPrevDate( date );
	
	return( STATUS_SUCCESS );
}

int Component :: setInflowTS( HourTS* in ) 
{
	char routine[]="Component :: setInflow";

	// Checking to see of this is the most upstream component. If it is,
	// then in must be coming from an input TS, i.e. it is non-NULL
	if( in == NULL && _n_son == 0 ) {
		PrintWarning( 1, routine, "Cannot set most upstream Component"
			" \"%s\" inflow to NULL.", _id );
		return( STATUS_FAILURE );
	}
	if( in == NULL ) {
		PrintWarning( 1, routine, "Cannot set inflow timeseries to "
			"NULL." );
		return( STATUS_FAILURE );
	}
	_in_count++;
	_inflow_ts = (HourTS**)realloc( (HourTS**)_inflow_ts, 
		(_in_count*sizeof( HourTS* ) ) );
	if( _inflow_ts == NULL ) {
		PrintWarning( 1, routine, "Could not allocate %d HourTS "
			"pointers.", _in_count );
		return( STATUS_FAILURE );
	}
	_inflow_ts[ _in_count-1 ] = in;
	TSIdent tempJRV = in->getIdentifier();

	return( STATUS_SUCCESS );
}

void Component :: setOutflowVal( TSDate& date, double value ) 
{
	_outflow_ts.setDataValue( date, value );
}

int Component :: setPrevDate( TSDate& prev ) 
{
	_prev_date = prev;
	return( STATUS_SUCCESS );
}

int Component :: setSolutionOrder( int lastNumber )
{
	char routine[]="Component :: setSolutionOrder";
	int i;
	
	for( i=0; i<_n_son; i++ ){
		lastNumber = _son_list[i]->setSolutionOrder( lastNumber );
	}

	lastNumber++;
	_SolutionNumber = lastNumber;

	return (lastNumber);

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Component_SetGet.cxx,v 1.8 2006/10/26 15:18:05 hsu Exp $";}
/*  ===================================================  */

}
