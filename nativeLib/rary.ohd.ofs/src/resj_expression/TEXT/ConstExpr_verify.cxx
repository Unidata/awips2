//------------------------------------------------------------------------------
// ConstExpr  ::  verify - verifies all the constants and variables
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
// 02 Mar 1998	MJR	Put in support for the DATE macro.
// 02 Jul 2002	James R. VanShaar, RTi	Fixed handling of dates to use proper
//					date formulation (underscores and no
//					spaces)
// 2003-11-21 Luiz Teixeira, RTi - Added code to free memory ( list )
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ConstExpr.h"

int ConstExpr  ::  verify()
{
	char	**list=NULL, routine[]="ConstExpr :: verify";
	int	nlist=0;

	if( _is_verified ){
		return( STATUS_SUCCESS );
	}

	int len = strlen( _constant );

	int digit_cnt	= 0;
	int dot_cnt	= 0;
	int date_chr_cnt= 0;

	// We need to loop across the string and figure out what we are
	// dealing with. If any of the characters are not digits and if
	// there are more than one '.' then we must not be dealing with a
	// value.
	for( int i=0; i<len; i++ ){
		if( isdigit( _constant[i] ) ){
			digit_cnt++;
		}
		if( _constant[i] == '.' ){
			dot_cnt++;
		}
		if( 	_constant[i] == '/' || _constant[i] == '_' ||
			_constant[i] == ':' ){
			date_chr_cnt++;
		}
	}

	// Ok, now we know....

	if( ((digit_cnt+dot_cnt) != len) || dot_cnt > 1 ){
		// We have a constant that is not a number, it could either
		// be the DATE macro, a string representation of a date, or
		// a variable...
		if( !strcmp( _constant, "DATE" ) ){
			PrintDebug( 10, routine,
			"Attaching to the DATE variable..." );
			// We have a date so we just need to wire this
			// together to point at the static variable that
			// represents today.
			_value = &_current_date_value;
		}
		// It could also be the string representation of a date...
		else if( (date_chr_cnt + digit_cnt) == len && date_chr_cnt ){
			// All the characters in the variable are
			// either numbers or '/', ':', or ' ' so we have a date...
			list = BreakStringList( _constant, "_:/", 0, &nlist );

			if( !list ){
				PrintWarning( 1, routine,
				"Troubles breaking \"%s\" by /.", 
				_constant );
				return( STATUS_FAILURE );

			}
			// The date could be any of these formats:
			// mm/dd
			// mm/dd_hh
			// mm/dd_hh:mm

			if( nlist < 2 || nlist > 4 ){
				PrintWarning( 1, routine,
				"Date \"%s\" is not valid format.",
				_constant );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}
			// Ok, we need to allocate memory for this
			// value locally.

			_value = new double;

			if( !_value ){
				PrintWarning( 1, routine,
				"Unable to allocate memory for a double." );
				list = FreeStringList( list );
				return( STATUS_FAILURE );
			}

			_is_local_value = 1;

			TSDate	date;

			if( nlist == 4 ){
				date.setMinute( atoi( list[3] ) );
			}
			if( nlist >= 3 ){
				date.setHour( atoi( list[2] ) );
			}
			date.setDay( atoi( list[1] ) );
			date.setMonth( atoi( list[0] ) );

			// We have difficulty determining whether we should
			// consider leap year in assigning this value.  We
			// will assume that we shouldn't, resulting in getting
			// it right 3 of 4 times.
			date.setYear( 1 );

			*_value = date.toDouble() - 1;
			
			// Freeing memory - 2003-11-21 LT
			if ( list ) {
				list = FreeStringList( list );
			}
		}
		else {
			PrintDebug( 10, routine,
			"Looking up variable \"%s\".", _constant );
			// We have to implement some way of reconciling this 
			// variable ...
			_value = Expression :: getConstantPtr( _constant );

			if( _value == NULL ){
				PrintWarning( 1, routine,
				"Unable to lookup value for \"%s\".", 
				_constant );
				return( STATUS_FAILURE );
			}
		}
		_is_verified = 1;
		return( STATUS_SUCCESS );
	}

	PrintDebug( 10, routine,
	"Evaluating numeric constant %s.", _constant );

	_value = new double;

	if( !_value ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for a double." );
		return( STATUS_FAILURE );
	}
	_is_local_value = 1;
	(*_value) = (double)atof( _constant );

	_is_verified = 1;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/ConstExpr_verify.cxx,v $";
 static char rcs_id2[] = "$Id: ConstExpr_verify.cxx,v 1.6 2006/10/26 15:19:22 hsu Exp $";}
/*  ===================================================  */

}
