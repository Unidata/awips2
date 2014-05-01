//------------------------------------------------------------------------------
// TSInterval.getName
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the character name corresponding
//			to an interval.  In the future, we may want to
//			overload to allow abbreviations, etc., and perhaps to
//			concatenate the multiplier.
//		(2)	Also need a static array to cross-reference the
//			integer intervals with the character string names.
//------------------------------------------------------------------------------
// History:
// 
// 04 Feb 1998	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "TSInterval.h"
#include "resj/TS.h"

char * TSInterval :: getName ( int interval )
{	char name[32];

	name[0] = '\0';
	if ( interval == TS::INTERVAL_YEAR ) {
		strcpy ( name, "Year" );
	}
	else if ( interval == TS::INTERVAL_MONTH ) {
		strcpy ( name, "Month" );
	}
	else if ( interval == TS::INTERVAL_DAY ) {
		strcpy ( name, "Day" );
	}
	else if ( interval == TS::INTERVAL_DAY ) {
		strcpy ( name, "Day" );
	}
	else if ( interval == TS::INTERVAL_HOUR ) {
		strcpy ( name, "Hour" );
	}
	else if ( interval == TS::INTERVAL_MINUTE ) {
		strcpy ( name, "Minute" );
	}
	else if ( interval == TS::INTERVAL_SECOND ) {
		strcpy ( name, "Second" );
	}
	char *pt = name;
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_getName.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_getName.cxx,v 1.3 2000/05/19 13:33:56 dws Exp $";}
/*  ===================================================  */

	return pt;

}
