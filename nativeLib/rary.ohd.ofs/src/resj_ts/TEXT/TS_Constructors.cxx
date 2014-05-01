// ----------------------------------------------------------------------------
// TS::constructors - member functions for TS base class
// ----------------------------------------------------------------------------

#include "resj/TS.h"

// TS constructor
//
// Notes:	(1)	Need to spend time on this
//
TS :: TS ( void )
{	char routine[]="TS.TS";

	PrintDebug( 50, routine, "constructing" );

	init();
}

// TS constructor - construct from file
//
// Notes:	(1)	Need to spend time on this
//
TS :: TS ( char *input )
{	char	routine[] = "TS.TS(char*)";

	PrintWarning ( 1, routine,
	"Constructing from file (%s) is not enabled", input );
	init();
}

//------------------------------------------------------------------------------
// TS :: TS - Cop Constructor
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 05 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
TS :: TS ( const TS& ts )
{
	char	routine[] = "TS::TS(const TS&)";

	init();

	// We need to to a piece-by-piece copy of all the data members.

	_date1				= ts._date1;
	_date2				= ts._date2;
	_date1_original 		= ts._date1_original;
	_date2_original 		= ts._date2_original;
	_data_interval_base 		= ts._data_interval_base;
	_data_interval_mult 		= ts._data_interval_mult;
	_data_interval_base_original 	= ts._data_interval_base_original;
	_data_interval_mult_original 	= ts._data_interval_mult_original;

	// Call functions to set the following information since they 
	// all have dynamically allocated memory.
	setDataType( ts._data_type );
	setDataUnits( ts._data_units );
	setDataUnitsOriginal( ts._data_units_original );
	setVersion( ts._version );

	_id 				= ts._id;

	_sequence_number		= ts._sequence_number;

	_dirty				= ts._dirty;

	_data_limits			= ts._data_limits;

	_is_instantaneous		= ts._is_instantaneous;

	_missing			= ts._missing;
	_missingl			= ts._missingl;
	_missingu			= ts._missingu;

	setDescription( ts._description );
	setComments( ts._comments );
	setGenesis( ts._genesis );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: TS_Constructors.cxx,v 1.2 2000/05/19 13:35:20 dws Exp $";}
/*  ===================================================  */

}
