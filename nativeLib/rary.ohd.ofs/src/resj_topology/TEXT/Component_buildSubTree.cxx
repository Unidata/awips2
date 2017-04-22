//------------------------------------------------------------------------------
// Component :: buildSubTree - traverses upward and builds a sub topology
// 				beginning at the component that this is first
//				called on and ending when it finds a Component
//				that matches the ucp or it goes to the upstream-
//				most point.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 May 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 23 May 2001	James R. VanShaar, RTi	Eliminated _in_count assignments as they
//					are appropriately handled within the
//					setInflowTS functions.
// 24 May 2001	JRV, RTi	Modify index sent to 'setNextDSComp' to point
//				to appropriate timeseries without assuming
//				symmetry in son and inflow timeseries indexing.
// 24 May 2001	JRV, RTi	Allowed copy and 'linkage' of all inflow
//				timeseries for the subTree.
// 16 Dec 2002	JRV, RTi	Added special consideration of _specialTie time
// 				series.
// 				Created function buildSubTree_SpecialTieOut.
// 19 Feb 2006  JRV, RTi    Added buildSubTree_CompIDList( Component *ucp,
//                              char ** IDList, int* nList )
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
// Notes:
//	This function accomplishes a number of important tasks among which are:
//	1-	Copy component topology from the downstream control point (dcp)
//		up to the component receiving the outflow from the reservoir 
//		'owning' the method (the reservoir is also known as the upstream
//		control point (ucp), and the component downstream is the 
//		NextDSComp), including all branches that enter the system 
//		between the dcp and the NextDSComp.
//	2-	Identify which (of potentially many) inflow timeseries to the
//		NextDSComp represents outflow from the ucp.
//	3-	Ensure appropriate linkage of outflow from upstream components
//		to inflow at downstream components.
//	4-	Include in the copied component any other inflows not related
//		to topological linkages.
//
//	The following information may be useful for understanding how all this
//	takes place.
//	A- 	During the copying of a given component (occuring in the
//		Constructors, etc.) no inflow timeseries are copied.
//	B-	Here in this function, topology allows us to copy and link
//		all outflows from upstream components to sub_comp as inflows.
//		These inflows may include a fully defined identifier, which is
//		completed if the upstream component specifies a TSOUTPUT for the
//		timeseries, or it include an incomplete / empty identifier, if 
//		the outflow timeseries is created during execution.
//	C-	Because only topologically related timeseries may contain empty
//		identifiers, all others are comparable by name, etc.
//------------------------------------------------------------------------------

#include "Component.h"
#include "MaxStage.h"
#include "LagK.h"
#include "SetWithdraw.h"

//Component* Component :: buildSubTree( Component* ucp, Method* meth )
Component* Component :: buildSubTree( Component* ucp, Method* meth,
	TSDate &cur_date, HourTS &inFromOwnRes )
{
	char	routine[]="Component :: buildSubTree";
	char temp[MAXC];
	int i; 
	Component* sub_comp = NULL;
	MaxStage* mxstg = NULL;
	int prevSteps, t_mult, t_int;
	TSDate t1;
	LagK* lag_meth;
	int son_is_null=0;

	// Check to see that there is a valid upstream control point. 
	// If there is, then use it. If there is not, then build the sub tree 
	// to the upstream-most point.
	if( this == ucp && ucp != NULL ) {
		TSIdent ts_ident = getOutflowTS()->getIdentifier();
		// This means that "this" is the Reservoir that called the 
		// MaxStage method. Return right away.
		return( NULL );
	}

	sub_comp = copy();
	// sub_comp->_in_count = 0;	// copy() used to copy _in_count, and
					// the inflow timeseries only if the 
					// component was an upstream-most one.
	mxstg = (MaxStage*)meth;

	TSIdent ts_ident, ts_identX;
	for( i = 0; i < _n_son; i++ ) {
		// This loop includes a process to link upstream outflows as 
		//	inflows to the sub_comp
		//if( sub_comp->addSon( _son_list[i]->buildSubTree( ucp, meth ) )
		if( sub_comp->addSon( _son_list[i]->buildSubTree( ucp, meth,
			cur_date, inFromOwnRes ) ) == 2 ) {
// printf(" buildtree subcompname=%s ucp=%s methodname=%s methid=%s \n", sub_comp->getID(), ucp->getID(), meth->getType(), meth->getID());
			// This case occurs when the above call to buildSubTree
			//	is the upstream control point (ucp) or 'owning'
			//	reservoir.  The addSon function sees the NULL
			//	returned and specifically returns a '2' to this
			// 	point.

/*********
			// Determine if we already have the son's outflows as
			//	part of our inflows, and if so, where.
			TSIdent identifier;
			char *id;
			int index = MISSING;
			for ( i = 0; i < comp._in_count; i++ ) {
				identifier = _son_list[i]->getOutflowTS()->
					getIdentifier();
				id = identifier.getIdentifier();
				// The following is exeucted if they are the 
				// same
				if ( !(strcmp( id, "..." )) ) {
					index = i;
					break;
				}
			}

			// Did we find a match?
			if ( index != MISSING ) {
				// Yes, we found a match.
				
				continue;
			}

			// No, we did not find a match.
*********/

			// Define within the MaxStage method this component as
			// 	the component immediately downstream of the
			//	'owning' reservoir and identify which of this
			//	component's timeseries represents outflow from
			//	the reservoir.  (The timeseries indexed by the
			//	current value of sub_comp->_in_count will be 
			//	defined below in the call to setInflowTS.)
			mxstg->setNextDSComp( sub_comp, (sub_comp->_in_count) );

			// Dummy in a place for the inflow timeseries for 
			//	sub_comp representing the outflow from the
			//	'owning' reservoir.  The actual values used in 
			//	the Max Stage will be set by the MaxStage method
			//	itself.
			sub_comp->setInflowTS( &inFromOwnRes );
			//sub_comp->setInflowTS( _son_list[i]->getOutflowTS() );

			// Get and copy the identifier for future use
			ts_ident = _son_list[i]->getOutflowTS()->
				getIdentifier();
			ts_identX = sub_comp->_inflow_ts
				[(sub_comp->_in_count)-1]->getIdentifier();
			sub_comp->_inflow_ts[(sub_comp->_in_count)-1]->
				setIdentifier( ts_ident );

			// continue;
			son_is_null=2;
		}
		else {
			sub_comp->setInflowTS( sub_comp->_son_list[i]->
				getOutflowTS() );

			// Get and copy the identifier for future use
			ts_ident = _son_list[i]->getOutflowTS()->
				getIdentifier();
			ts_identX = sub_comp->_inflow_ts
				[(sub_comp->_in_count)-1]->getIdentifier();
			sub_comp->_inflow_ts[(sub_comp->_in_count)-1]->
				setIdentifier( ts_ident );
		}

		// Because this outflow timeseries is new, it is also empty.  
		// This may cause problems when a component looks towards its 
		// inflow timseries (which may be this outflow timeseries) for 
		// values for previous timesteps.  Therefore, effect a copy of 
		// potentially needed data values.
		// NOTE: Carryover values such as _releaseCO, inflow array for 
		// 	LagK, etc. are copied in the constructors.

		// Reservoir: Will not need more than 2 previous timesteps
		// Node: Will not need more than 2 previous timesteps
		prevSteps = 2;
		t_mult = Method::getTimeMult();

		// Reach: Will not need more than 
		//	(int)_lag / _t_mult + 2 previous timesteps
		if ( !strcmp( sub_comp->getType(), "REACH" ) ) {
			// For now we are going to assume that Lag and 
			// K is the only routing method available for a
			// reach and we will use the lag time of the 
			// first Lag and K method in the method list
			lag_meth = (LagK*)(sub_comp->_method_list[0]);
			int intLag = (int) (lag_meth->getLag());
			prevSteps = ( (int)(lag_meth->getLag()) / t_mult ) + 2;
		}

		t1 = cur_date;
		t_int = Method::getTimeInterval();
		t1.addInterval( t_int, -1 * (t_mult * prevSteps) );
		// Adjust to simulation beginning if t1 is earlier
		if ( t1 < Method::getForecastDate1() ) {
			t1 = Method::getForecastDate1();
		}

		// Beginning at t1, cycle through cur_date copying inflows
		// from original component
		double inflowVal;
		for( ; t1 < cur_date; t1.addInterval( t_int, t_mult ) ) {
			inflowVal = _son_list[i]->getOutflowTS()->
				getDataValue(t1);
			sub_comp->_inflow_ts[i]->setDataValue( t1, inflowVal );
		}

		if ( sub_comp->_n_son == 0 || son_is_null==2) {
			// Don't check the lag time
			son_is_null=0;
			continue;
		}

		int ucpUP = _son_list[i]->checkMaxStageUS( ucp ); // 0 if yes
		int Ctype = strcasecmp( "REACH", sub_comp->_son_list[i]->
			getType() ); // 0 if yes
		if (!ucpUP && !Ctype) { 
			// For now we are going to assume that Lag and K is the
			// only routing method available for a reach and we will
			// use the lag time of the first Lag and K method in the
			// method list
			lag_meth = (LagK*)(sub_comp->_son_list[i]->
				_method_list[0]);
			int newLag = lag_meth->getLag();
//			mxstg->addTimeStep( lag_meth->getLag() );
			mxstg->addTimeStep( lag_meth->getLag(), lag_meth->getK() );
		}
	}

	// When we get to this point, all inflows from upstream components have
	//	been properly defined, with appropriate linkages.
	// Now we need to include in our subTree component any other inflows
	//	that may be defined.  We will do this by cycling through the
	//	original component, checking to see if each timeseries has
	//	already been transferred to the copied component.  If it has not
	// 	then we will copy it.
	int j, matchj;
	TSIdent idTemp1, idTemp2;
	for ( i = 0; i < _in_count; i++ ) {
		idTemp1 = _inflow_ts[i]->getIdentifier();

		// Check to see if the current timeseries we are about to copy
		// is a specialTieIn timeseries.
		sprintf (temp, "%s", idTemp1.getAlias());
		if( !strncasecmp( temp, ">>SpecialTie<<SetWithdraw-", 26 ) ) {
			// The timeseries was constructed by SetWithdraw for
			// the ToComp construct
			// We will rely on other code to handle this.  See
			// MaxStage::solve, shortly below where buildSubTree
			// was called.
			continue;
		}


		matchj = -999;
		for ( j = 0; j < sub_comp->_in_count; j++ ) {
			idTemp2 = sub_comp->_inflow_ts[j]->getIdentifier();
			if( idTemp1.equals( idTemp2 ) ) {
				matchj = j;
				break;
			}
		}
		// Did we find a match?
		if ( matchj != -999 ) {
			// Yes, we found a match.
			continue;
		}

		// No, we did not find a match.
		// Because these are TSInput timeseries and cannot be modified
		// we will assign them to the sub_comp using a pointer.
		sub_comp->setInflowTS( _inflow_ts[i] );
		sub_comp->_inflow_ts[(sub_comp->_in_count)-1]->
			setIdentifier( idTemp1 );
	}

	// If we get to this point and the inflow ts count for this object
	// is zero, this means that "this" is the end of a branch in the tree.
	// Because every component must have at least one inflow_ts (remember
	// we set the in_count to zero above for bookkeeping reasons), we need
	// to set it explicitly

	// REMOVED
	// The old version of copy() used to copy the inflow timeseries' for
	//	upstream-most components.  I suppose this was to re-assign
	//	_in_count from 0 to 1 since there really was at least 1.  No
	//	consideration was taken into account for multiple inflows under
	//	this setup.
	//if( sub_comp->_in_count == 0 ) {
		//sub_comp->_in_count = 1;
	//}


	// Finally, return a copy of "this"self. If the solution gets to
	// this point, that means "this" is either the most upstream point
	// or that it has successfully added all of its sons.
	return( sub_comp );
}

// This method returns an array of Component pointers that are part of the
// subtree for a given MaxStage method.  This list is used in controlling
// interaction with Lookup3 methods.
// It is called by the Method's downstream control point.
// Component *ucp is a pointer to the method's owner (upstream control point).
// nList is the number of Component IDs in the list.

//------------------------------------------------------------------------------
// Component :: buildSubTree_CompIDList - Build a character string array of
//                                        containing the Component identifiers
//                                        in a MaxStage method.
//------------------------------------------------------------------------------
// This routine operates recursively to work though a MaxStage subtree, creating
// an array (list) of component identifiers belonging to the subtree.
// There are two possible results:
//  - Return a null character array, if the current recursion call belongs to
//    a component not in the MaxStage method subtree.
//  - Return an array of character strings, including one more added by this
//    method's execution.
//------------------------------------------------------------------------------
// Return: 
//     char **
// Calls:
//     Component :: buildSubTree_CompIDList( Component *ucp, char **IDlist,
//        int *nList ), recursively.
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Build a character string array containing the Component identifiers in a
MaxStage method.  It should be called first by the downstream control point of
the MaxStage method.  It will recursively call up the subtree.
@return none
@param ucp The upstream control point or the reservoir owning the MAxStage
method in question.
@param IDlist The character string array containing component identifiers
@param nList The number of component identifiers in IDlist.
*/

char** Component :: buildSubTree_CompIDList( Component *ucp, char ** IDList,
    int* nList )
{
    int i;
    if( this == ucp && ucp != NULL ) {
        // This means that "this" is the Reservoir that called the MaxStage
        //method. Return right away.
        return( NULL );
    }

    for( i = 0; i < _n_son; i++ )
    {
        // Call this method again
        IDList = _son_list[i]->buildSubTree_CompIDList( ucp, IDList, nList );
    }

    IDList = AddToStringList( IDList, _id, nList );
    return IDList;
}

int Component :: buildSubTree_specialTieOut ( Component *sub_root, 
	Component *CtrlPt )
{
	// This will be called by the root of the new Tree
	
	char	routine[]="Component :: buildSubTree_specialTieOut";
	char oldOwnerName[MAXC];
	Component *oldOwningRes;
	int i, length, Old_n_meth; 
	Method** Old_method_list;
	SetWithdraw * SetWith;

	// Traverse the new subTree looking for or _specialTieOut.
	// If _specialTieOut, go to the methods to copy and connect appropriate
	// time series information.
	for( i = 0; i < _n_son; i++ ) {
		if( _son_list[i]->buildSubTree_specialTieOut( sub_root, 
			CtrlPt ) ) {
			// We have some type of error
		}
	}
	
	// We have reached a point where the current component has no more
	// unchecked (or any) sons.
	// Test to see if we are concerned about _specialTieOut
	if( _specialTieOut ) {
		// Find the companion owner in the old tree
		length = strlen( _id );
//kwz		strncpy( oldOwnerName, _id, length-2 );
//kwz		oldOwnerName[length-2] = '\0';
		strncpy( oldOwnerName, _id, length);
		oldOwnerName[length] = '\0';

		oldOwningRes = CtrlPt->getComponentPtr( oldOwnerName );

		// Work through the methods of the old component
		Old_n_meth = oldOwningRes->_n_meth; 
		Old_method_list = oldOwningRes->_method_list;
		for( i = 0; i< Old_n_meth; i++ ) {
			// Currently we only have withdraw tie backs
			if( !strncmp( _method_list[i]->getType(), 
				"SETWITHDRAW", 11 ) ) {
				// Check to see if the method had a TieBack
				// comp->_method_list[i]->getType())
				SetWith = (SetWithdraw *)Old_method_list[i];
				if( SetWith->_receivingComp ) {
					// We send the new component and the new
					// tree root into the old method
					Component *Comp = getComponentPtr(_id);
					int success = SetWith->linkCopiedTS( 
							Comp, sub_root);
						//getComponentPtr(_id), sub_root);
				}
			}
		}
	}

	return ( STATUS_SUCCESS );
}

int Component :: buildSubTree_specialTieIn ( )
{
	// This will be called by the root of the new Tree
	
	char	routine[]="Component :: buildSubTree_specialTieIn";
	char alias[MAXC], Ties[MAXC];
	int i, numTies;

	// Traverse the new subTree looking for or _specialTieIn.
	// If _specialTieIn, go to the methods to copy and connect appropriate
	// time series information.
	for( i = 0; i < _n_son; i++ ) {
		if( _son_list[i]->buildSubTree_specialTieIn( ) ) {
			// We have some type of error
			return ( STATUS_FAILURE );
		}
	}
	
	// We have reached a point where the current component has no more
	// unchecked (or any) sons.
	// Test to see if we are concerned about _specialTieIn
	if( _specialTieIn ) {
		numTies = 0;
		// Cycle through the inflow timeseries, counting which are
		// associated with a specialTieIn
		for( i = 0; i < _in_count; i++ ) {
			if( _inflow_ts[i] == NULL ) {
				PrintError( routine, "Troubles checking "
					"inflow timeseries for MaxStage %s "
					"subtree.", _id );
				return ( STATUS_FAILURE );
			}
	
			TSIdent tempJRV = _inflow_ts[i]->getIdentifier();
			sprintf( alias, tempJRV.getAlias() );
			if( !strncasecmp( alias, ">>SpecialTie<<SetWithdraw-", 
				26 ) ) {
				// We found a tie back.
				numTies++;
				if( numTies == _specialTieIn ) {
					// We've found them all, skip the rest
					return ( STATUS_SUCCESS );
				}
				// Keep track of matches in case we miss one.
				sprintf( Ties, "%s\n\0", Ties );
				strcat( Ties, &alias[26] );
			}

		}
		if( numTies != _specialTieIn ) {
			// We missed at least one.  Scream and holler!
			PrintError( routine, "Copied component %s is missing "
				"one or more special tie inflows.  Likely this "
				"is due to the SetMax subTree not containing "
				"the originating component.  The following "
				"ties were made: %s", _id, Ties );
			return ( STATUS_FAILURE );
		}
	}

	return ( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_buildSubTree.cxx,v $";
 static char rcs_id2[] = "$Id: Component_buildSubTree.cxx,v 1.8 2006/10/26 15:18:09 hsu Exp $";}
/*  ===================================================  */

}
