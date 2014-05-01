//------------------------------------------------------------------------------
// TSIdent::setMainSource - set the main source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the main source and then triggers
//			a reset of the full source.
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// main_source I	Main source part of source.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setMainSource ( char *main_source )
{	char	routine[] = "TSIdent.setMainSource";
	int	dl = 50;

	if ( !main_source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting main source to \"%s\"",
	main_source );

	if( _main_source ) {
        	delete [] _main_source;
	}

	_main_source = new char[strlen( main_source )+1];

	if( !_main_source ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for main source \"%s\"",
		main_source );
		return 1;
	}

	strcpy( _main_source, main_source );

	setSource();

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setMainSource.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setMainSource.cxx,v 1.2 2000/05/19 13:08:22 dws Exp $";}
/*  ===================================================  */

}
