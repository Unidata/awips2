//------------------------------------------------------------------------------
// TSIdent.setSubSource - set the sub-source part of the source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the subsource and then calls
//			setsource() to reset the full source.
//------------------------------------------------------------------------------
// History:
// 
// 17 Sep 1997	Steven A. Malers, RTi	Initial version.
// 08 Jan 1997	SAM, RTi		Update to be consistent with Java.
//------------------------------------------------------------------------------
// Variable	I/O	Description		
//
// sub_source	I	Sub-source to set.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent :: setSubSource ( char *sub_source )
{	char	routine[]="TSIdent.setSubSource";
	int	dl = 50;

	if ( !sub_source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting sub-source to \"%s\"",
	sub_source );

	if( _sub_source ){
        	delete [] _sub_source;
	}

	_sub_source = new char[strlen( sub_source )+1];

	if( !_sub_source ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for sub-source \"%s\"",
		sub_source );
                return 1;
	}

	strcpy( _sub_source, sub_source );

	setSource();

        return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setSubSource.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setSubSource.cxx,v 1.2 2000/05/19 13:08:22 dws Exp $";}
/*  ===================================================  */

}
