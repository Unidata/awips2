//------------------------------------------------------------------------------
// TSIdent::setScenario - set the scenario part of the identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Split code out of SetGet.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// scenario	I	Scenario to use in identifier.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setScenario( char *scenario )
{	char	routine[] = "TSIdent.setScenario";
	int	dl = 50;

	if ( !scenario ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting scenario to \"%s\"...", scenario );

	if( _scenario ) {
		delete [] _scenario;
	}

	_scenario = new char[strlen( scenario )+1];

	if( !_scenario ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for scenario \"%s\"", scenario );
		return 1;
	}

	strcpy( _scenario, scenario );

	setIdentifier ();

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setScenario.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setScenario.cxx,v 1.2 2000/05/19 13:08:22 dws Exp $";}
/*  ===================================================  */

}
