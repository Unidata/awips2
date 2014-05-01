//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"

ResJSys :: ResJSys()
{
	initialize();
	if( !_ref_count ){
		initializeStatic();
	}
	_ref_count++;
}

ResJSys :: ResJSys( const ResJSys& rjs )
{
	char	routine[]="ResJSys :: ResJSys";

	initialize();
	if( !_ref_count ){
		initializeStatic();
	}
	_ref_count++;

	PrintWarning( 1, routine,
	"The \"%s\" routine is not currently enabled.", routine );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_Constructors.cxx,v 1.3 2006/10/26 15:30:38 hsu Exp $";}
/*  ===================================================  */

}
