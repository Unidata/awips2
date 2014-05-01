//------------------------------------------------------------------------------
// Expression Set/Get routines.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 18 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 07 Aug 2001	James R. VanShaar, RTi	Fixed setCurrentDate to properly strip
//					off year.
// 06 Jun 2002	JRV, RTi	Added cfactor adjustment for inflows, 
//				withdrawals and expanded its capability for all 
//				pools and releases.
// 07 Jun 2002	JRV, RTi	Handled NULL pointer returned from 
//				comp->getValuePtr
// 10 Jun 2002	JRV, RTi	Addec cfactor adjustment for Node DISCHARGE.
// 14 Nov 2003	JRV, RTi	Employed use of floor for getting at value in
// 				setCurrentDate( TSDate & ).
// 17 apr 2006	JRV, RTi	Moved "(*pvalue) = (*pvalue) * cfactor;" to
//				double* Component :: getValuePtr( char* id )
// 18 apr 2006	JRV, RTi	Revised usage of floor in setCurrentDate().
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Expression.h"
#include "Component.h"

// Declare the static variables...

Component* Expression::_component_root=NULL;
double Expression::_current_date_value=0.0;

int Expression :: setComponentRoot( Component* root )
{
	_component_root = root;

	return( STATUS_SUCCESS );
}

int Expression :: setCurrentDate( TSDate& date )
{
	_current_date_value = date.toDouble();
	// We have to strip off the years here since all the dates in the
	// Expression derived code are year independant...

//cfan	int value = floor( _current_date_value );
	double value = floor( _current_date_value );   //cfan

	_current_date_value -= value;

	return( STATUS_SUCCESS );
}

#include "Component.h"
double* Expression :: getConstantPtr( char* id )
{
	char	**list=NULL, routine[]="Expression::getConstantPtr";
	Component	*comp=NULL;
	double	*pvalue=NULL, value, cfactor;
	int	nlist=0;

	if( id == NULL ){
		PrintWarning( 1, routine,
		"Incoming constant string is NULL." );
		return( NULL );
	}

	if( _component_root == NULL ){
		PrintWarning( 1, routine,
		"The Expression component root has not been set yet." );
		return( NULL );
	}

	// We need to break the identifier by '.'.

	list = BreakStringList( id, ".", DELIM_SKIP_BLANKS, &nlist );

	if( !list || nlist != 2 ){
		PrintWarning( 1, routine,
		"Malformed constant \"%s\".", id );
		if( list ){
			list = FreeStringList( list );
		}
		return( NULL );
	}

	// The first item in the string list is the component identifier
	// so we need to find that.

	comp = _component_root->getComponentPtr( list[0] );

	if( comp == NULL ){
		PrintWarning( 1, routine,
		"Unable to find component matching \"%s\".", list[0] );
		list = FreeStringList( list );
		return( NULL );
	}

	// Ok, so we found the component, now we need to do a lookup and 
	// get the double value associated with the variable which is stored
	// as the second item in list.

	// Convert to upper case so we do not have to worry about cases
	ToUpper( list[1] );

	pvalue = comp->getValuePtr( list[1] );
	if( pvalue == NULL ){
		PrintWarning( 1, routine,
			"Unable to handle Rule keyword \"%s\".  Nonstandard "
			"and no constant of same name.", id );
		list = FreeStringList( list );
		return( pvalue );
	}

	cfactor = Component::_cfactor;
	// Move this to double* Component :: getValuePtr( char* id )
	//(*pvalue) = (*pvalue) * cfactor;

	if( NULL != strstr( list[1], "POOL" ) ) {
        	Component::_cfactor = Component::_lfactor;
	}
	else if( NULL != strstr( list[1], "INFLOW" ) ) {
                Component::_cfactor = Component::_ffactor;
	}
	else if( NULL != strstr( list[1], "DISCHARGE" ) ) {
                Component::_cfactor = Component::_ffactor;
	}
	else if( NULL != strstr( list[1], "RELEASE" ) ) {
                Component::_cfactor = Component::_ffactor;
	}
	else if( NULL != strstr( list[1], "WITHDRAW" ) ) {
        	Component::_cfactor = Component::_ffactor;
	}
	else {
        	// Component::_cfactor = 1.0;
	}	
	list = FreeStringList( list );

	if( pvalue == NULL ){
		PrintWarning( 1, routine,
		"Unable to find location of \"%s\".", id );
	}
	return( pvalue );
}

void Expression :: print( FILE* fp )
{
	if( fp ){
		fprintf( fp, "%s\n", toString() );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/Expression_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: Expression_SetGet.cxx,v 1.9 2006/10/26 15:20:23 hsu Exp $";}
/*  ===================================================  */

}
