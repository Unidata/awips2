//------------------------------------------------------------------------------
// TSDateGetDateFromString - converts char string into TSDate
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This will probably be changed when the NWS reviews batchmode.
//
//------------------------------------------------------------------------------
// History:
// 
// 03 Dec 1996	Matthew J. Rutherford, Riverside Technology, inc.
// 17 Feb 1998  Daniel Weiler, RTi	Converted for use in Res-J.
// 20 Apr 1998  DKW, RTi		Reworked to be a lot more flexible.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// date		L	TSDate that is being created.
// date_string	I	incoming date string in format 120396.
// routine	L	name of function.
// tmp		L	string used in parsing.
//
//------------------------------------------------------------------------------

#include "TSUtil.h"

TSDate TSUtil :: getTSDateFromString( char* date_string )
{
	char		tmp[250], routine[] = "TSUtil::getTSDateFromString";
	int		hour_set = 0, minute_set = 0, year_set2 = 0, 
			year_set4 = 0;
	TSDate		date;

	strncpy( tmp, date_string, 2 );
	tmp[2] = '\0';

	date.setMonth( atoi( tmp ) );

	strncpy( tmp, &date_string[3], 2 );
	tmp[2] = '\0';

	date.setDay( atoi( tmp ) );


	if( strlen( date_string ) == 8 ){
		//
		// the date is MM/DD/YY or MM/DD HH
		//
		if( date_string[5] == '_' ) {
			strncpy( tmp, &date_string[6], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
		else {
			sprintf( tmp, "19%s", &date_string[6] );
			tmp[4] = '\0';
			date.setYear( atoi( tmp ) );
		}
	}
	else if( strlen( date_string ) == 10 ){
		//
		// the date is MM/DD/YYYY or MM/DD/YY H
		//
		if( date_string[5] == '_' ) {
			sprintf( tmp, "0%s", &date_string[6] );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
		else {
			strncpy( tmp, &date_string[6], 4 );
			tmp[4] = '\0';
			date.setYear( atoi( tmp ) );

			strncpy( tmp, &date_string[9], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
	}
	else if( strlen( date_string ) == 11 ){
		//
		// the date is MM/DD/YY HH or MM/DD HH:MM
		//
		if( date_string[5] == '_' ) {
			strncpy( tmp, &date_string[6], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );

			sprintf( tmp, &date_string[9], 2 );
			tmp[2] = '\0';
			date.setMinute( atoi( tmp ) );
		}
		else {
			sprintf( tmp, "19%s", &date_string[6] );
			tmp[4] = '\0';
			date.setYear( atoi( tmp ) );

			strncpy( tmp, &date_string[9], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
	}
	else if( strlen( date_string ) == 14 ){
		//
		// the date is MM/DD/YY HH:MM
		//
		strncpy( tmp, &date_string[12], 2 );
		tmp[2] = '\0';
		date.setMinute( atoi( tmp ) );

		strncpy( tmp, &date_string[9], 2 );
		tmp[2] = '\0';
		date.setHour( atoi( tmp ) );
	}
	else {
		PrintWarning( 10, routine, 
		"Invalid Date String %s, returning empty date", date_string );
		return( date );
	}

	PrintDebug( 10, routine, 
	"Converted %s to: %d/%d %02d:%02d",
	date_string, date.getMonth(), date.getDay(), date.getHour(), 
	date.getMinute() );

	return( date );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSUtil_getTSDateFromString.cxx,v $";
 static char rcs_id2[] = "$Id: TSUtil_getTSDateFromString.cxx,v 1.1 1999/02/18 15:20:27 dws Exp $";}
/*  ===================================================  */

}
