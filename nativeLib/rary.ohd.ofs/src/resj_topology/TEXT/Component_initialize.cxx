//------------------------------------------------------------------------------
// Component :: initialize 	Initialize private data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 13 Dec 2002	James R. VanShaar, RTi	Added _specialTieOut and _specialTieIn.
// 19 Feb 2004	JRV, RTi	Added _totalInflow, initialized _output_ts to
// 				NULL.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

int Component :: initialize()
{
	char routine[]="Component :: initialize";
	int i;
	TSDate temp;

	_son_list 	= NULL;
	_father 	= NULL;
	_n_son 		= 0;
	_in_count	= 0;
	_id[0]		= '\0';
	_type[0]	= '\0';
	_conserve_mass = 1;

	_prev_date = Method :: getForecastDate1();
	_prev_date.addInterval( Method :: getTimeInterval(), 
		-1*Method :: getTimeMult() );

	_expr_list = new Expression* [MAX_METHOD];

	if( !_expr_list ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %d Expression*.", MAX_METHOD );
		return( STATUS_FAILURE );
	}

	_method_list = new Method* [MAX_METHOD];

	if( !_method_list ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %d Method*.", MAX_METHOD );
		return( STATUS_FAILURE );
	}

	for( i = 0; i < MAX_METHOD; i++ ) {
		_method_list[i] = NULL;
		_expr_list[i] = NULL;
	}

	_inflow_ts = NULL;
	// NOTE: The following "15" is based on a hardcoded maximum found in 
	// Component.h
	for( i=0; i<15; i++ ) {
		_output_ts[i]=NULL;
	}
	_n_output_ts = 0;

	// Allocate and set up the _outflow TS...
	_outflow_ts.setDate1( Method :: getForecastDate1() );
	_outflow_ts.setDate2( Method :: getForecastDate2() );
	_outflow_ts.setDataInterval( Method :: getTimeInterval(), 
		Method :: getTimeMult() );
	if( _outflow_ts.allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for outflow time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	// Allocate and set up the _mean_outflow TS...
	_mean_outflow_ts.setDate1( Method :: getForecastDate1() );
	_mean_outflow_ts.setDate2( Method :: getForecastDate2() );
	_mean_outflow_ts.setDataInterval( Method :: getTimeInterval(), 
		Method :: getTimeMult() );
	if( _mean_outflow_ts.allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for outflow time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	// Allocate and set up the _totalInflow TS...
	_totalInflow.setDate1( Method :: getForecastDate1() );
	_totalInflow.setDate2( Method :: getForecastDate2() );
	_totalInflow.setDataInterval( Method :: getTimeInterval(), 
		Method :: getTimeMult() );
	if( _totalInflow.allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space"
		" for outflow time series on %s.", _id );
		return( STATUS_FAILURE );
	}

	_n_meth = 0;

	_const_id_list = NULL;

	_const_val_list = NULL;

	_n_const = 0;

	_specialTieOut = 0;
	_specialTieIn = 0;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Component_initialize.cxx,v 1.6 2006/10/26 15:18:36 hsu Exp $";}
/*  ===================================================  */

}
