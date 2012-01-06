//------------------------------------------------------------------------------
// Spillway :: Spillway - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 24 Dec 2002	James R. VanShaar, Riverside Technology
//					Created initial version.
// 14 Jan 2003	JRV, RTi	Added usage of _myValue.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Spillway.h"
#include "Reservoir.h"

Spillway :: Spillway( Reservoir* owner ) : ReservoirMethod( owner )
{
	initialize();
}

Spillway :: Spillway( const Spillway& meth, Reservoir* owner )
	: ReservoirMethod( meth, owner )
{
	char	routine[]="Spillway :: Spillway";
	int i;

	initialize();

	// Copy the integers
	_intervals = meth._intervals;

	// Copy the doubles;
	_minSpillStorage = meth._minSpillStorage;
	_myValue = meth._myValue;

	// Copy the Table;
	_stor_spill_tbl = new Table( *(meth._stor_spill_tbl) );

	// Allocate space for the arrays (psuedo time series used in solution)
	_withVol_ts = new double[ _intervals ];
	if( !_withVol_ts ) {
		PrintWarning( 1, routine, "Unable to allocate %d floats.",
			_intervals );
	}
	_relVol_ts = new double[ _intervals ];
	if( !_relVol_ts ) {
		PrintWarning( 1, routine, "Unable to allocate %d floats.",
			_intervals );
	}
	_stor_ts = new double[ _intervals ];
	if( !_stor_ts ) {
		PrintWarning( 1, routine, "Unable to allocate %d floats.",
			_intervals );
	}
	_spillVol_ts = new double[ _intervals ];
	if( !_spillVol_ts ) {
		PrintWarning( 1, routine, "Unable to allocate %d floats.",
			_intervals );
	}
	_inflVol_ts = new double[ _intervals ];
	if( !_inflVol_ts ) {
		PrintWarning( 1, routine, "Unable to allocate %d floats.",
			_intervals );
	}
	
	// Copy the arrays (psuedo time series used in solution)
	for( i = 0; i < _intervals; i++ ) {
		_withVol_ts[i] = meth._withVol_ts[i];
		_relVol_ts[i] = meth._relVol_ts[i];
		_stor_ts[i] = meth._stor_ts[i];
		_spillVol_ts[i] = meth._spillVol_ts[i];
		_inflVol_ts[i] = meth._inflVol_ts[i];
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Spillway_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Spillway_Constructors.cxx,v 1.2 2006/10/26 15:35:55 hsu Exp $";}
/*  ===================================================  */

}
