//------------------------------------------------------------------------------
// Component :: operator - operators.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 18 Feb 1998	Matthew J. Rutherford, RTi
//				Changed things around a bit so that the son
//				list is copied dynamically.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Component.h"

void Component :: operator= ( const Component& tree )
{
	char	routine[]="Component :: operator=";
	/*
	int	i;

	for( i=0; i<tree._n_son; i++ ){
		if( addSon( tree._son_list[i] ) ){
			PrintWarning( 1, routine,
			"Troubles adding %dth son to list in = operator.", 
			i+1 );
			return;
		}
	}
	_father = tree._father;

	strcpy( _id, tree._id );
	*/
	PrintWarning( 1, routine,
	"The \"%s\" routine is not yet supported.", routine );

	return;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Component_Operators.cxx,v $";
 static char rcs_id2[] = "$Id: Component_Operators.cxx,v 1.3 2006/10/26 15:18:02 hsu Exp $";}
/*  ===================================================  */

}
