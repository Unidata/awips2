//------------------------------------------------------------------------------
// SetMax :: solveMethod - Algorithm that solves the SetMax method.
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
// 05 Jun 2001  JRV, RTi        Assign new storage based on new pool following
//                              completion of loss method (Ensure full updating
//                              of states).
// 05 Jun 2001  JRV, RTi        Removed update of pool following withdrawal
//                              method as it is inconsistent with documentation
//                              and intended functionality.
//      ***********CLARIFICATION***************
//      Due to the solution method of updating states only at then end of
//      a timestep solution, states are kept constant throughout the calls
//      to the sub methods (except for Loss methods).  For example, SetMax
//      acting on 2 or more SetWithdraw methods will use the same pool elevation
//      to calculate the withdrawals.
// 05 Jun 2001  JRV, RTi        Include handling of MISSING values returned from
//                              called methods
// 11 Dec 2002	JRV, RTi	Added assignment of _Active.
// 2003-11-24 Luiz Teixeira, RTi - Add code to free the memory allocated for the
//                              local member 'prev' if returning STATUS_FAILURE.
// 16 Feb 2006  JRV, RTi    Generalized the method for different component
//                          types.  (It Was Reservoir only.)
// 16 Feb 2006  JRV, RTi    Added handling of DIVERS_METHOD, and Lookup3.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "SetMax.h"

int SetMax :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="SetMax :: solveMethod";
	double *prev[2], max = 0.0, pool = MISSING;
	int i, maxMethod;

	// Set _Active to 1
	_Active = 1;

	for( i = 0; i < 2; i++ ) {
		prev[i] = new double;
		*prev[i] = 0.0;
	}
	// Execute the methods in the group and then select the 
	// maximum return values from those methods.
	for( i = 0; i < _group_n; i++ ) {
		if( _group[i]->solveMethod( cur_date, isPrimarySolution,
			prev ) ) {
			PrintWarning( 1, routine, "Troubles solving "
				"%s for SetMax Method %s." , _group[i]->getID(),
				 _id );
			
			// Free the memory associated with the elements...
			for( i = 0; i < 2; i++ ) { 	// LT 2003-11-24
				delete prev[i];   	// LT 2003-11-24        
			}                         	// LT 2003-11-24    
			    
			return( STATUS_FAILURE );	
		}

		// Is the method just solved a Lookup3 method?
		// If the method is Lookup3, we don't want to check for MISSING
                if( strcmp( "LOOKUP3",_group[i]->getType() ) ) {
			// No, check to ensure the return is not MISSING
			if( *prev[0] == MISSING ) {
                       		 // Solution was unsuccessful
				_group[i]->_Active = -1;
				continue;
                	}
		}

		if( i == 0 ) {
			max = *prev[0];
			maxMethod = 0;
		}
		else if( *prev[0] >= max ) {
			max = *prev[0];
			_group[maxMethod]->_Active = -1;
			maxMethod = i;
		}
		else if( *prev[0] < max ) {
			_group[i]->_Active = -1;
		}

                if( _group_id == LOSS_METHOD ) {
			pool = *prev[1];
		}
	}

	// Free the memory associated with the elements...
	for( i = 0; i < 2; i++ ) {
		delete prev[i];
	}

	if( group_val != NULL ) {
		*group_val[0] = max;
		if( pool != MISSING ) {
			*group_val[1] = pool;
		}
// Need to save the value then return
//		return( STATUS_SUCCESS );
	}

	if( _group_id == RELEASE_METHOD ) {
		// Check to see that this release is reasonable...
                if( max < ((Reservoir*)_owner)->_min_release ) {
                        PrintWarning( 1, routine, "SetMax %s on %s produced "
				"release that is less than the reservoir's "
				"minimum release (%f < %f). Setting release to "
				"minimum release.", _id, _owner->_id, max, 
				((Reservoir*)_owner)->_min_release );

                        max = ((Reservoir*)_owner)->_min_release;
                }
		((Reservoir*)_owner)->_release = max;
		// NOTE: Validity of a release in terms of min_pool will be
		//      checked in Reservoir::finalizeSolution()
	}
	else if( _group_id == LOSS_METHOD ) {
		((Reservoir*)_owner)->_loss = max;
		((Reservoir*)_owner)->_pool = pool;
		((Reservoir*)_owner)->_storage =
			((Reservoir*)_owner)->_elev_stor_tbl.lookup( pool,
			GETCOLUMN_2, ALLOW_BOUNDS );
	}
	else if ( _group_id == WITHD_METHOD ) {
		((Reservoir*)_owner)->_withdraw = max;
		// NOTE: Validity of a withdrawal in terms of min_pool will be
		//      checked in Reservoir::finalizeSolution()

	}
	else if ( _group_id == DIVERS_METHOD ) {
		((Node *)_owner)->_diversion = max;
		// NOTE: Validity of a diversion in terms of minimum remainder
		//	will be checked in Node::finalizeSolution()
	}
//  printf(" !RRR SetMax resname=%s group_id=%d release=%f \n", _owner->getID(), _group_id, _owner->_release);

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetMax_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: SetMax_solveMethod.cxx,v 1.7 2006/10/26 15:33:37 hsu Exp $";}
/*  ===================================================  */

}
