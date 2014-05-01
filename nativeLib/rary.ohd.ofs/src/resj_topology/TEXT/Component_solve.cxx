//------------------------------------------------------------------------------
// Component :: solve - Main solution function for the component.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 31 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 23 Apr 1998  Daniel Weiler, RTi	Skipped NULL expressions.
// 07 Oct 1998	DKW, RTi		Fine tuned all sorts of things.
// 12 Nov 2001	James R. VanShaar, RTi	Added call to setInactiveState
// 12 Nov 2001	JRV, RTi	Moved handling of Method carry over from 
//				*Method*_solve to here.
// 13 Nov 2001	JRV, RTi	Moved handling of Component carry over from
//				*Component*_finalizeSolution to here.
// 04 Jun 2002	JRV, RTi	Added calculation of _endInflow.
// 07 Jun 2002	JRV, RTi	Modified to use setEndOfTimestepStates.
// 11 Nov 2003	JRV, RTi	Fixed eliminated setInactiveState call when
// 				expression is NULL (as when method is part of a
// 				combo method).  *** Eventually needs thorough
// 				testing with SetWithdraw toComp feature.
// 2003-11-26 Luiz Teixeira, RTi - Removed the line 
//				'_method_list[i]->setInactiveState()'
//				from the 'if( _expr_list[i] == NULL ) {...}'
//				block. Method should not be set inactive under 
//				this condition.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: solve( TSDate& date, int isPrimary )
{
	char	routine[]="Component :: solve";
	int	i=0;

	// We have to recursively call the solve function on the 
	// sons...
	for( i=0; i<_n_son; i++ ){
		if( _son_list[i]->solve( date, isPrimary ) ){
			return( STATUS_FAILURE );
		}
	}
	// Now we have to solve ourseleves.

	_conserve_mass = 1;

	setEndInflow( date );

	for( i=0; i<_n_meth; i++ ) {
		// We have to check the associated expression...
		if( _expr_list[i] == NULL ) {
			PrintDebug( 5, routine, "Expr. is NULL, skipping "
				"%s.%s.%s \n", _id, _method_list[i]->getType(), 
				_method_list[i]->getID() );
			// DO NOT _method_list[i]->setInactiveState() HERE.
			continue;
		}
		if( _expr_list[i]->evaluate() == 0.0 ){
			// Method is not active this time step.
			// Negate previous conditions (such as blends) that 
			// should be reinitialized due to the inactivity of the 
			// method
			_method_list[i]->setInactiveState();
			
			PrintDebug( 15, routine,
				"%s is FALSE, skipping %s.%s.%s...\n", 
				_expr_list[i]->toString(), _id, 
				_method_list[i]->getType(),
				_method_list[i]->getID() );
			continue;
		}


		PrintDebug( 15, routine,
			" %s is TRUE, evaluating %s.%s.%s ... \n", 
			_expr_list[i]->toString(), _id, _method_list[i]->
				getType(), _method_list[i]->getID() );

		if( _method_list[i]->solveMethod( date, isPrimary ) ){
			PrintWarning( 1, routine, "Troubles solving method %s.",
				_method_list[i]->getID() );
				//"- presumably having memory allocation "
				//"problems."
			return( STATUS_FAILURE );
		}
	}

	// Now we must finalize our solution for this time step...
	if( finalizeSolution( date ) ) {
		PrintWarning( 1, routine, "Troubles finalizing "
			"solution for %s.", _id );
	}

/********
	setEndOfTimestepStates( date );
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
********/
	
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_solve.cxx,v $";
 static char rcs_id2[] = "$Id: Component_solve.cxx,v 1.7 2006/10/26 15:18:42 hsu Exp $";}
/*  ===================================================  */

}
