//------------------------------------------------------------------------------
// TSInterval.parseInterval - parses an interval string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine takes a string like "12min" and splits
//			it into the base and multiplier for the data interval.
//			Several different long and abbreviated names intervals
//			accepted.
//------------------------------------------------------------------------------
// History:
// 
// 03 Dec 96	Matthew J. Rutherford,	Initial version for ESPADP.
//		Riverside Technology,
//		inc.
// 12 Jun 97	Steven A. Malers, RTi	Copy to TS++ and add minute
//					capability.
// 17 Sep 1997	SAM, RTi		Rename TSIntervalFromString to
//					TSIdent::parseInterval and also pass
//					back the interval base as a string.
//					Use new static data flags.
// 07 Jan 1998	SAM, RTi		Update to agree with Java.  Return a
//					TSInterval.  This code is being
//					moved to the TSInterval class.
// 08 Jan 1998	SAM, RTi		Code is now in TSInterval.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// base		O	Base interval.
// base_string	O	Base interval as a string.
// digit	L	The part of "string" that is the multiplier.
// digit_count	L	The number of characters in "digit".
// dl		L	Debug level for this routine.
// i		L	Loop counter for characters in "string".
// interval_string L	String containing interval to be split.
// length	L	Length of "string".
// mult		O	Interval multiplier.
// routine	L	Name of this routine.
//------------------------------------------------------------------------------

#include "ResJ.h"
#include "resj/TS.h"
#include "TSInterval.h"
#include <string.h>
#include <ctype.h>

TSInterval TSInterval :: parseInterval ( char* interval_string )
{	char		digit[250], routine[] = "TSInterval.parseInterval";
	int		digit_count=0, dl = 30, i=0, length;
	TSInterval	tsinterval;

	length = strlen( interval_string );

	//
	// Need to strip of any leading digits.
	//

	while( i < length ){
		if( isdigit(interval_string[i]) ){
			digit_count++;
			i++;
		}
		else {	// We have reached the end of the digit part
			// of the string.
			break;
		}
	}

	if( digit_count == 0 ){
		//
		// The string had no leading digits, interpret as one.
		//
		tsinterval.setMultiplier ( 1 );
	}
	else if( digit_count == length ){
		//
		// The whole string is a digit.
		//
		tsinterval.setBase ( TS::INTERVAL_HOUR );
		tsinterval.setMultiplier ( atoi( interval_string ) );
		
		PrintDebug( dl, routine, "%d Hourly",
		tsinterval.getMultiplier() );
		return tsinterval;
	}
	else {	strncpy( digit, interval_string, digit_count );

		digit[digit_count] = '\0';

		tsinterval.setMultiplier ( atoi( digit ) );
	}

	PrintDebug ( dl, routine, "Multiplier: %d",
	tsinterval.getMultiplier() );

	//
	// Now parse out the Base interval
	//

	char base_string[256];
	base_string[0] = '\0';
	if(	!ESPstrncasecmp( &interval_string[digit_count], "minute", 6) ) {
		strncpy ( base_string, &interval_string[digit_count], 6 );
		base_string[6] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MINUTE );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "min", 3 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MINUTE );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "hour", 4 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 4 );
		base_string[4] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_HOUR );
	}
	else if(!ESPstrncasecmp( &interval_string[digit_count], "hr", 2 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 2 );
		base_string[2] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_HOUR );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "day", 3 ) ){
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_DAY );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "week", 4) ){
		strncpy ( base_string, &interval_string[digit_count], 4 );
		base_string[4] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_WEEK );
	}
	else if(!ESPstrncasecmp( &interval_string[digit_count], "wk", 2) ){
		strncpy ( base_string, &interval_string[digit_count], 2 );
		base_string[2] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_WEEK );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "month", 5 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 5 );
		base_string[5] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MONTH );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "mon", 3 ) ){
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MONTH );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "year", 4 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 4 );
		base_string[4] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_YEAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "yr", 2 ) ){
		strncpy ( base_string, &interval_string[digit_count], 2 );
		base_string[2] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_YEAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "irregular", 9 ) ){
		strncpy ( base_string, &interval_string[digit_count], 9 );
		base_string[9] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_IRREGULAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "irreg", 5 ) ){
		strncpy ( base_string, &interval_string[digit_count], 5 );
		base_string[5] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_IRREGULAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "irr", 3 ) ){
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_IRREGULAR );
	}
	else {	PrintWarning( 2, routine,
		"Unrecognized Interval \"%s\", returning HOURLY", 
		&interval_string[digit_count] );
		tsinterval.setBase ( TS::INTERVAL_HOUR );
	}

	PrintDebug( dl, routine, "Base: %d (%s), Mult: %d",
	tsinterval.getBase (),
	tsinterval.getBaseString (),
	tsinterval.getMultiplier () ); 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_parseInterval.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_parseInterval.cxx,v 1.3 2000/05/19 13:35:19 dws Exp $";}
/*  ===================================================  */

	return tsinterval;

}
