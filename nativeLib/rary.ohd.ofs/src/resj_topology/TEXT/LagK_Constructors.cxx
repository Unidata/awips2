//------------------------------------------------------------------------------
// LagK :: LagK - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 19 Mar 1998	Daniel Weiler, Riverside Technology, inc
//				Created initial version.
// 19 Nov 2001	James R. VanShaar, RTi	Added handling of _outflowCO.
// 13 Jan 2003	JRV, RTi	Corrected oversight of certain variables in the
// 				copy-type Constructor.
// 12 Jan 2003	JRV, RTi	Added _n_OutStorVals
// 29 Jan 2003	JRV, RTi	Fixed bug in copying of _co_inflow array.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "LagK.h"

LagK :: LagK( Reach* owner ) : ReachMethod( owner )
{
	initialize();
}

LagK :: LagK( const LagK& meth, Reach* owner ) : 
	ReachMethod( meth, owner )
{
	char	routine[]="LagK :: LagK";
	int i;

	initialize();

	// Copy the integers first...
	_n_kval = meth._n_kval;
	_n_lagval = meth._n_lagval;
	_n_OutStorVals = meth._n_OutStorVals;
	_lag = meth._lag;
	_kconst = meth._kconst;
	_k_mode = meth._k_mode;
	_lag_mode = meth._lag_mode;
	_interp = meth._interp;
	_sizeInflowCO = meth._sizeInflowCO;

	// doubles...
	_co_inflow = new double[ _sizeInflowCO ];
	for( i = 0; i < _sizeInflowCO; i++ ) {
		_co_inflow[i] = meth._co_inflow[i];
	}
	_outflowCO = meth._outflowCO;
	_laggedInflow = meth._laggedInflow;
	_storageCO = meth._storageCO;
	_transLossCoef = meth._transLossCoef;
	_transLossLevel = meth._transLossLevel;

	// Tables...
	if( meth._n_kval > 0 )
	{
		_out_k_tbl = meth._out_k_tbl;
	}
	if( meth._lag_mode == VARIABLE_LAG )
	{
		_in_lag_tbl = meth._in_lag_tbl;
	}
	// Copy only if it exists
	if( _n_OutStorVals ) {
		_stor_out_tbl = meth._stor_out_tbl;
		_stor_out_tbl4 = meth._stor_out_tbl4;
	}


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/LagK_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: LagK_Constructors.cxx,v 1.6 2006/10/26 15:22:17 hsu Exp $";}
/*  ===================================================  */

}
