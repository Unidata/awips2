//------------------------------------------------------------------------------
// Component :: print - recursively prints out a component.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

void Component :: print( FILE* fp )
{
	if( !fp ){
		return;
	}

	for( int i=0; i<_n_son; i++ ){
		_son_list[i]->print( fp );
	}
	printComponent( fp );
	printContents( fp );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_print.cxx,v $";
 static char rcs_id2[] = "$Id: Component_print.cxx,v 1.2 2006/10/26 15:18:38 hsu Exp $";}
/*  ===================================================  */

}
