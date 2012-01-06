//------------------------------------------------------------------------------
// Table :: initialize - initializes private data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 07 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "resj/Table.h"

int Table :: initialize()
{
	_id[0] = '\0';

	_col_1 = NULL;

	_col_2 = NULL;

	_rows = 0;

	return( STATUS_FAILURE );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils2/RCS/Table_initialize.cxx,v $";
 static char rcs_id2[] = "$Id: Table_initialize.cxx,v 1.3 2006/10/26 15:37:02 hsu Exp $";}
/*  ===================================================  */

}
