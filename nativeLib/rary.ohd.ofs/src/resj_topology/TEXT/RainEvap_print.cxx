//------------------------------------------------------------------------------
// RainEvap :: print - printd instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "RainEvap.h"
#include "TSDateIterator.h"

int RainEvap :: print( FILE* fp )
{
	char routine[] = "RainEvap :: print";
	int i;
	TSDate	date;

	if( fp == NULL ) {
		PrintWarning( 1, routine, "Cannot print RainEvap info -"
		" null FILE*.");
		return( STATUS_FAILURE );
	}

	// Print self id fnd owner first
	fprintf( fp, "RainEvap method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );

	fprintf( fp, "Preciptation TimeSeries:\n");

	for( date = _precip_ctl.getDate1(); date <= _precip_ctl.getDate2(); 
		date.addInterval( _t_int, _t_mult ) ){
		PrintDebug( 10, "Date: %s  Precip from ctl file: %f\n", date.toString(),
			_precip_ctl.getDataValue( date, 1 ) );
	}
		
	fprintf( fp, "Evaporation TimeSeries:\n");
	for( date = _evap_ctl.getDate1(); date <= _evap_ctl.getDate2(); 
		date.addInterval( _t_int, _t_mult ) ){
		PrintDebug( 10, "Date: %s  Evap form ctl file: %f\n", date.toString(),
			_evap_ctl.getDataValue( date, 1 ) );
	}

	fprintf( fp, "End of \"%s\" information.\n\n", _id );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/RainEvap_print.cxx,v $";
 static char rcs_id2[] = "$Id: RainEvap_print.cxx,v 1.3 2006/10/26 15:29:32 hsu Exp $";}
/*  ===================================================  */

}
