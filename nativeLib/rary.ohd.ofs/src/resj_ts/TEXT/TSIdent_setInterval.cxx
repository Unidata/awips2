//------------------------------------------------------------------------------
// TSIdent.setInterval - set TS identifier interval
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the interval.  Depending on the
//			arguments, it may have to parse the interval into its
//			components.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Overload to be more useful.
// 08 Jan 1998	SAM, RTi		Update to use TSInterval to be
//					compatible with Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"
#include "TSInterval.h"

int TSIdent :: setInterval ( char *interval_string )
{	char		interval_base_string[256],
			routine[]="TSIdent::setInterval(char*)";
	int		dl = 50;
	TSInterval	tsinterval;

	if ( !interval_string ) {
		return 1;
	}

	PrintDebug ( dl, routine, "Setting interval to \"%s\"...",
	interval_string );

	if ( !interval_string[0] ) {
		// First split the string into its base and multiplier...

		tsinterval = TSInterval::parseInterval ( interval_string );

		// Now set the base and multiplier...

		_interval_base = tsinterval.getBase();
		_interval_mult = tsinterval.getMultiplier();
	}
	// Else, don't do anything (leave as zero initialized values).

	// Now set the interval string.  Use the given interval base string
	// because we need to preserve existing file names, etc.

	setIntervalString ( interval_string );
	setIdentifier();

	return 0;
}

// Notes:	(1)	Set the interval from the base and multiplier, both
//			as integers.
//		(2)	Need to put in checks at some point to check for valid
//			multipliers given the base, but for now assume that
//			any positive multiplier is valid.
int TSIdent :: setInterval ( int interval_base, int interval_mult )
{	char	routine[] = "TSIdent.setInterval";

	if ( interval_mult <= 0 ) {
		PrintWarning ( 2, routine,
		"Interval multiplier (%d) must be greater than zero",
		interval_mult );
		return 1;
	}
	if (	(interval_base != TS::INTERVAL_SECOND) &&
		(interval_base != TS::INTERVAL_MINUTE) &&
		(interval_base != TS::INTERVAL_HOUR) &&
		(interval_base != TS::INTERVAL_DAY) &&
		(interval_base != TS::INTERVAL_WEEK) &&
		(interval_base != TS::INTERVAL_MONTH) &&
		(interval_base != TS::INTERVAL_YEAR) &&
		(interval_base != TS::INTERVAL_IRREGULAR) ) {
		PrintWarning ( 2, routine,
		"Base interval (%d) is not recognized", interval_base );
		return 1;
	}
	_interval_base = interval_base;
	_interval_mult = interval_mult;

	// Now we need to set the string representation of the interval...

	char	interval_string[256];
	if (	(interval_base != TS::INTERVAL_IRREGULAR) &&
		(interval_mult != 1) ) {
		sprintf ( interval_string, "%d", interval_mult );
	}

	if ( interval_base == TS::INTERVAL_SECOND ) {
		strcat ( interval_string, "sec" );
	}
	else if	( interval_base == TS::INTERVAL_MINUTE ) {
		strcat ( interval_string, "min" );
	}
	else if	( interval_base == TS::INTERVAL_HOUR ) {
		strcat ( interval_string, "hour" );
	}
	else if	( interval_base == TS::INTERVAL_DAY ) {
		strcat ( interval_string, "day" );
	}
	else if	( interval_base == TS::INTERVAL_WEEK ) {
		strcat ( interval_string, "week" );
	}
	else if	( interval_base == TS::INTERVAL_MONTH ) {
		strcat ( interval_string, "month" );
	}
	else if	( interval_base == TS::INTERVAL_YEAR ) {
		strcat ( interval_string, "year" );
	}
	else if	( interval_base == TS::INTERVAL_IRREGULAR ) {
		strcat ( interval_string, "irreg" );
	}

	setIntervalString ( interval_string );
	setIdentifier ();
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_setInterval.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_setInterval.cxx,v 1.4 2000/05/19 13:33:20 dws Exp $";}
/*  ===================================================  */

	return 0;

}
