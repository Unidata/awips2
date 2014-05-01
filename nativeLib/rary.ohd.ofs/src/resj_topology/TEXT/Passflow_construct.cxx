//------------------------------------------------------------------------------
// Passflow::construct - reads in necessary data for the Passflow method. 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
//  23 AUG 2004	KSH, OHD	Added this method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "Passflow.h"
#include <stdio.h>

int Passflow :: construct ( char** re_list, int n_items )  
{
	char routine[] = "Passflow::construct", **list = NULL, 
		**value_list=NULL;

	_is_constructed = 1;
	return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Passflow_construct.cxx,v $";
 static char rcs_id2[] = "$Id: Passflow_construct.cxx,v 1.2 2006/10/26 15:28:51 hsu Exp $";}
/*  ===================================================  */

}		
