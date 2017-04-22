//------------------------------------------------------------------------------
// CalcInflow::print - printd instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 12 Feb 2004	James R. VanShaar, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "CalcInflow.h"

int CalcInflow::print( FILE* fp )
{
	char routine[] = "CalcInflow::print";
	int i;
	TSDate date, start;
	TSIdent id;


	if( fp == NULL ) {
		PrintWarning( 1, routine, "Cannot print CalcInflow info -"
		" null FILE*.");
		return( STATUS_FAILURE );
	}

	// Print self id
 	fprintf( fp, "Start of \"%s %s %s\" information.\n\n", _type, 
		_owner->_id, _id );

	// Print timeseries information
	if( _pool_obs ) {
		id=_pool_obs->getIdentifier();
		fprintf( fp, "Observed Pool Elevation TS\n\tAlias: %s\n"
			"\tFull ID: %s %s %s\n", id.getAlias(), 
			id.getIdentifier(), id.getInterval() );
	}

	if( _release_obs ) {
		id=_release_obs->getIdentifier();
		fprintf( fp, "Observed Release TS\n\tAlias: %s\n"
			"\tFull ID: %s %s %s\n", id.getAlias(), 
			id.getIdentifier(), id.getInterval() );
	}

	if( _withdraw_obs ) {
		id=_withdraw_obs->getIdentifier();
		fprintf( fp, "Observed Withdraw TS\n\tAlias: %s\n"
			"\tFull ID: %s %s %s\n", id.getAlias(), 
			id.getIdentifier(), id.getInterval() );
	}
	else {
		fprintf( fp, "No Withdraw TS used.\n" );
	}

	if( _outputCalcTS ) {
		id=_inflow_calc->getIdentifier();
		fprintf( fp, "Calculated Inflow TS\n\tAlias: %s\n"
			"\tFull ID: %s %s %s\n", id.getAlias(), 
			id.getIdentifier(), id.getInterval() );
	}
	else {
		fprintf( fp, "Calculated Inflow will not be output.\n" );
	}

	if( _useForSim ) {
		fprintf( fp, "Calculated Inflow will be used for simulation, "
			"when possible.\n" );
	}
	else {
		fprintf( fp, "Calculated Inflow will not be used for "
			"simulation." );
	}

	// Handle Change Constraints
	if( _constrainChange ) {
		int rows = _maxIncrease.getNRows();
		fprintf( fp, "Increases in calculated inflow are "
			"constrained:\n\tPrevInflow\tMaxIncrease\n" );
		for( i=0; i<rows; i++ ) {
			fprintf( fp, "\t%f\t%f\n", _maxIncrease.lookup(i,0),
				_maxIncrease.lookup(i,1) );
		}

		fprintf( fp, "Decreases in calculated inflow are "
			"constrained:\n\tPrevInflow\tMaxIncrease\n" );
		for( i=0; i<rows; i++ ) {
			fprintf( fp, "\t%f\t%f\n", _maxDecrease.lookup(i,0),
				_maxDecrease.lookup(i,1) );
		}

		// Interpolation scheme
		if( _mode == INTERPOLATE ) {
			fprintf( fp, "Max Change Tables are interpolated.\n" );
		}
		else {
			fprintf( fp, "Max Change Tables are not "
				"interpolated.\n" );
		}
	}
	else {
		fprintf( fp, "There are no constraints on the change in "
			"calculated inflow.\n" );
	}

	// MinInflow
	if( _minInflow == MISSING ) {
		fprintf( fp, "No minimum inflow constraint exists.\n" );
	}
	else {
		fprintf( fp, "Minimum inflow is constrained to: %f.\n",
		      _minInflow );
	}

	// RemainingVol (Do we print this?)

	// _myValue (StartCalcInflow--Do we print this?)
	

	// Loss method use
	if( _useLoss ) {
		fprintf( fp, "Reservoir loss is included in the mass balance "
			"calculation of inflows.\n" );
	}

	// End identifier
 	fprintf( fp, "End of \"%s %s %s\" information.\n\n", _type, _owner->_id, 
		_id );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/CalcInflow_print.cxx,v $";
 static char rcs_id2[] = "$Id: CalcInflow_print.cxx,v 1.2 2006/10/26 15:11:50 hsu Exp $";}
/*  ===================================================  */

}
