//------------------------------------------------------------------------------
// Node :: copy - constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 05 May 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Node.h"

Node* Node :: copy() 
{
	Node* copy_node = NULL;

	copy_node = new Node( *this );

//	strcat( copy_node->_id, "_2" );
	return( copy_node );
	

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Node_copy.cxx,v $";
 static char rcs_id2[] = "$Id: Node_copy.cxx,v 1.3 2006/10/26 15:27:54 hsu Exp $";}
/*  ===================================================  */

}
