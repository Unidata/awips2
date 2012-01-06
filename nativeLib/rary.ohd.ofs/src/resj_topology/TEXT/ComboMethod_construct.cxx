//------------------------------------------------------------------------------
// ComboMethod :: construct - function constructs the list of methods to be
//				acted on as a group by SetMin, SetMax, etc.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 22 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ComboMethod.h"

int ComboMethod :: construct( char** meth_list, int nmeth )
{
	char routine[] = "ComboMethod :: construct", **sub_list = NULL, 
		owner_id[MAXC];
	int i, nsub_list = 0, group_type, count = 0;
	Method*	method = NULL;

	// Build the group list of methods that need to be solved...
	for( i = 0; i < nmeth; i++ ) {
		if( strlen( meth_list[i] ) == 0 || meth_list[i][0] == '#' ){
			continue;
		}
		sub_list = BreakStringList( meth_list[i], " ", 
			DELIM_SKIP_BLANKS, &nsub_list );
		if( !sub_list || nsub_list != 3 ) {
			PrintWarning( 1, routine, "Troubles building group "
			"member info for %s.", _id );
			if( sub_list ) {
				sub_list = FreeStringList( sub_list );
			}
			return( STATUS_FAILURE );
		}

		method = _owner->getMethod( sub_list[0], 
			sub_list[1], sub_list[2] );
		if( !method ) {
			PrintWarning( 1, routine, "Could not get Method "
			"pointer for %s.", sub_list[2] );
			sub_list = FreeStringList( sub_list ); 
			return( STATUS_FAILURE );
		}

		// Check the group type and owner ID of the first method in the
		// group. All subsequent methods must be of matching type and 
		// owner. Also, set the _group_id of "this". 
		if( i == 0 ) {
			group_type = method->getGroupID();
			if( group_type == 0 ) {
				PrintWarning( 1, routine, 
				"Group Type not set on %s.", _id );
				return( STATUS_FAILURE );
			}
			strcpy( owner_id, method->getOwner()->_id );
			_group_id = group_type;
		}
		else if( method->getGroupID() != group_type || 
			strcasecmp( method->getOwner()->_id, owner_id ) ) {
			PrintWarning( 1, routine, 
			"Method %s (type %d) not of compatible type with "
			"rest of group (type %d) for %s - not adding.", 
			method->getID(), method->getGroupID(), group_type, 
			_id );
			sub_list = FreeStringList( sub_list ); 
			continue;
		}
		_group[i] = method;
		count++;
		sub_list = FreeStringList( sub_list ); 
	}
	_group_n = count;

	// Check to make sure we got the vital data to solve...
	if( _group_n < 1 ) {
		PrintWarning( 1, routine, "Must have at least one reservoir "
		"in order to Balance using %s on %s.", _id, _owner->_id );
		return( STATUS_FAILURE );
	}
	_is_constructed = 1;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/ComboMethod_construct.cxx,v $";
 static char rcs_id2[] = "$Id: ComboMethod_construct.cxx,v 1.3 2006/10/26 15:12:40 hsu Exp $";}
/*  ===================================================  */

}
