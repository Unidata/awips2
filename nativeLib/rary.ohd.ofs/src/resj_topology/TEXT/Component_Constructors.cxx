//------------------------------------------------------------------------------
// Component :: Component - constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 23 May 2001	James R. VanShaar, RTi	Modified copying Constants so as to 
//					appropriately size the new 
//					_const_id_list as it is filled
// 23 May 2001	James R. VanShaar, RTi	Modified so as to copy all TSINPUT 
//					defined inflow timeseries regardless of
//					component's position in the system.
// 24 May 2001	James R. VanShaar, RTi	Modified so as not to copy any TSINPUT 
//					related information, neither the 
//					timeseries nor the timeseries count 
//					(_in_count).
// 24 Dec 2002	JRV, RTi	Added special Tie relationship handling
// 19 Feb 2004	JRV, RTi	Added copying of _totalInflow.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

Component :: Component()  
{
	initialize();
}

Component :: Component( const Component& comp ) 
{
	char	routine[]="Component :: Component(const Component&)";
	int i;

	initialize();

	// Copy char*s
	strcpy( _id, comp._id );

	/****** Erroneous thinking... Replaced
	// We only want to grab the comp's inflow ts if it is the most
	// upstream component of its branch.
	if( comp._n_son == 0 ) {
		setInflowTS( comp._inflow_ts[0] );
	}
	******/

	/**********
	// We want to grab all inflow time series that were defined using 
	//	TSINPUT.  This is identifiable because each of these have fully
	//	defined identifiers. (Those not falling into this category were
	//	defined by assigning outflows from upstream components as 
	//	inflows to this component.)
	TSIdent identifier;
	for ( i = 0; i < comp._in_count; i++ ) {
		identifier = comp._inflow_ts[i]->getIdentifier();
		char *id = identifier.getIdentifier();
		// The following returns true if they are not the same
		if ( strcmp( id, "..." ) ) {
			setInflowTS( comp._inflow_ts[i] );
		}
	}
	**********/

	// ints
	/****
	_in_count = comp._in_count;
	****/
	_n_meth = comp._n_meth;
	_n_const = comp._n_const;
	_conserve_mass = comp._conserve_mass;

	// TS copies - inflow pointers get set when the topology builds...
	_totalInflow=comp._totalInflow;
		

	// Constants
	int temp_length;
	for( i = 0; i < _n_const; i++ ) {
		temp_length = i;
		_const_id_list = AddToStringList( _const_id_list, 
			comp._const_id_list[i], &temp_length );
		//strcpy( _const_id_list[i], comp._const_id_list[i] );
	}
	// TSDates
	_prev_date = comp._prev_date;

	// Methods and Expressions
	for( i = 0; i < comp._n_meth; i++ ) {
		_method_list[i] = comp._method_list[i]->copy( this );
		if( comp._expr_list[i] != NULL ) {
			_expr_list[i] = comp._expr_list[i]->copy();
		}
	}

	// Handle special Tie relationships.
	_specialTieOut = comp._specialTieOut;
	_specialTieIn = comp._specialTieIn;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Component_Constructors.cxx,v 1.6 2006/10/26 15:17:56 hsu Exp $";}
/*  ===================================================  */

}
