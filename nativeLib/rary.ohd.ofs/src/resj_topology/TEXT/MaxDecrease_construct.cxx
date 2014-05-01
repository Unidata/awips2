//------------------------------------------------------------------------------
// MaxDecrease::construct - reads in necessary data for the MaxDecrease method. 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 21 Nov 2001	JRV, RTi	Improved error handling
// 2003-11-21 Luiz Teixeira, RTi - Added list = FreeStringList( list ) at the 
//  				end of the main for loop in the construct method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "MaxDecrease.h"
#include "TSUtil.h"
#include <stdio.h>

int MaxDecrease :: construct ( char** re_list, int n_items )  
{
	char routine[] = "MaxDecrease::construct", **list = NULL,
		**value_list=NULL;
	int i, nlist, n_value;
	double ffactor = 1.0;
	int numErrs = 0;

	if( Method::getUnitType() == ENGLISH ) {
		ffactor = 0.028317;	
	}

	for( i = 0; i < n_items; i++ ) {
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( re_list[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );
		if( nlist == 0 || list == NULL ) {
			PrintError( routine, "Troubles getting data for %s %s.",
				_type, _id );
			list = FreeStringList( list );
			return( STATUS_FAILURE );
		}

		// This reads in the increase over the previous 24 hr period.
		if( !strcasecmp( list[0], "DECREASE" ) ) { 
                        if( nlist < 2 ) {
				// The following is a warning since we might see
				// INCREASE later
				PrintWarning( 1, routine, "Value required "
					"immediately after %s keyword %s.", 
					_type, list[0]);
				list = FreeStringList( list );
				continue;
			}
			_max_decrease = atof( list[1] ) * ffactor;
			if( _max_decrease > 0.0 ) {
				_max_decrease = -1.0 * _max_decrease; 
			}
			list = FreeStringList( list );
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}
	}

	// Check to see if we got the necessaries
	if( _max_decrease == MISSING ) {
		numErrs++;
		PrintError( routine,
			"Keyword DECREASE and Value required but not "
			"found in %s %s ", _type, _id );
	}

	if( numErrs > 0 ) {
		return(STATUS_FAILURE);
	}

	_is_constructed = 1;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxDecrease_construct.cxx,v $";
 static char rcs_id2[] = "$Id: MaxDecrease_construct.cxx,v 1.6 2006/10/26 15:25:57 hsu Exp $";}
/*  ===================================================  */

}		


