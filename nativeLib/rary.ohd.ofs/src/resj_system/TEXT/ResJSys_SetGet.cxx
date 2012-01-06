//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Mar 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 28 May 1998	DKW,		Added all of the Carryover* functions. These
//				are used by the FORTRAN interface to ResJ, so
//				if they are a little non-intuitive, remember
//				that FORTRAN is involved.
// 29 May 1998	Matthew J. Rutherford, RTi
//				Added the functions used for dealing with the
//				messaging storage.
// 03 Jun 1998	DKW		Added setStartDate and setEndDate.
// 11 Apr 2001  James R. VanShaar, RTi
//				Added _co_size = 0 in resetCOString()
// 08 Jul 2002	JRV, RTi	Corrected return value for setIPR_IODEBUG
// ** *** 2003	JRV, RTi	Fixed setCODates logic for no user specified
// 				carryover dates.
// 07 Feb 2006  JRV, RTi    Added setMAINUM and getMAINUM static member
//                          functions.
// 18 Feb 2006  JRV, RTi    Revised getCOArray to also modify the writeSize
//                          variable.
//                          Revised ResJSys :: setCODates to set a trigger value
//                          (_writeLastCO) defining whether the last CO array
//                          will be written back to NWSRFS.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include "Method.h"

char* ResJSys :: getCOString( int* size )
{
	*size = _co_size;	
	return( _co_list );
}

int ResJSys :: addCOString( char* string )
{
	char routine[] = "ResJSys :: addCOString";
	int length, prev_len = 0;

	// Make sure we got something...
	if( string == NULL ) {
		PrintWarning( 2, routine, "Cannot add null string to carryover "
		"CO array for FORTRAN interface." );
		return( STATUS_FAILURE );
	}

	// First we need to realloc the string's length 
	if( _co_list == NULL ) {
		length  = strlen( string );
		prev_len = 0;
	}
	else {
		length  = strlen( string ) + strlen( _co_list );
		prev_len = strlen( _co_list );
	}
		
	_co_list = (char*)realloc( _co_list, ( length + 1 ) * sizeof( char ) );
	/*
	if( _co_size == 0 ) {
		_co_list[ 0 ] = '\0';  
	}
	*/
	_co_list[ prev_len ] = '\0';

	if( _co_list == NULL ) {
		PrintWarning( 1, routine, "Troubles allocating %d chars.",
			length + 1 );
		return( STATUS_FAILURE );
	}
	strcat( _co_list, string );
	_co_list[ length ] = '\0';

	// The incoming string always has a -999 specified in the
	// position that corresponds to the next component's id position
	// in the co array. If we are dealing with any component other
	// the first one (_co_size = 0), then we need to manually set the 
	// position of the current ID.
	if( _co_size > 0 ) {
		char* ptr = strstr( _co_list, "-999" ); 
		if( ptr == NULL ) {
			PrintWarning( 2, routine, "Troubles inserting position"
			"of next Component in CO array." );
			return( STATUS_FAILURE );
		}
		char pos[5];
		sprintf( pos, "%4d", prev_len );
		strncpy( ptr, pos, 4 );
	}

	// Keep the counter going...
	_co_size++;
	
	return( STATUS_SUCCESS );
}

int ResJSys :: resetCOString( )
{
	char routine [] = "ResJSys :: resetCOString";

	if( _co_list == NULL ) {
		return( STATUS_SUCCESS );
	}
	// First copy the contents of the _co_list into the list of
	// co strings, _co_array_str
	_co_array_str[_num_co_str] = new char[ strlen(_co_list)+1 ];
	strcpy( _co_array_str[_num_co_str], _co_list );
	_num_co_str++;

	free( _co_list );
	_co_list = NULL;
	_co_size = 0;
	// Note: Absence of this re-initialization in previous versions caused
	// 	incorrect indexing of string sets.  This would occur if a
	//	carryover run wrote more than one set of carryover.  Upon 
	//	beginning to write carryover for the second carryover date,
	//	because _co_size would still be non-zero, the -999 in the
	//	first string set would be replaced with the value of prev_len
	//	(which would be 0) effectively pointing to the beginning of
	//	itself.  Normally, no indexing would be done on the first
	//	string set until appending the second.  The index would replace
	//	the first -999 with the value pointing to the beginning of the
	//	second.  The second -999 would remain until a third string set
	// 	is appended.  SEE addCOString() above for how this all plays
	//	out.

	return( STATUS_SUCCESS );
}

int ResJSys :: deleteCOString( )
{
	if( _co_list ) {
		free( _co_list );
	}
	_co_list = NULL;
	_co_size = 0;
	return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// ResJSys :: getCOArray - Returns the carryover array list, including an
//            integer number of the arrays, and the number of those arrays
//            expected to be sent back to NWSRFS.
//------------------------------------------------------------------------------
// This routine, called by the execute58, passes back a string list.  Each
// string contains one carryover array string.  The method also modifies an
// integer variable defining how many of the strings exist in the array and
// how many of them should be passed to NWSRFS based on the program that called
// Res-J.
//------------------------------------------------------------------------------
// Return: 
//     char** String list, each string being a carryover array for a given date.
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Determines the number of carryover arrays to pass back to NWSRFS and returns the
list of arrays, and the size of the array.
@return char** String list--an array of carryover arrays.
@param size Integer pointer to be assigned the number of carryover strings in the string list.
@param writeSize Integer pointer to be assigned the number of carryover strings to be written to NWSRFS.
*/

char** ResJSys :: getCOArray( int* size, int* writeSize )
{
	// Add one here because we are explicitly setting the end date as 
	// a carryover save date.
	*size = _num_co_str;
	if( _writeLastCO ) {
		*writeSize = _num_co_str;
	}
	else {
		*writeSize = _num_co_str - 1;
	}
	return( _co_array_str );
}

char* ResJSys :: getPOString( int* size, int* t_mult )
{
	*size = _po_size;	
	*t_mult = Method :: getTimeMult();
	return( _po_list );
}

int ResJSys :: addPOString( char* string )
{
	char routine[] = "ResJSys :: addPOString";
	int length;

	if( string == NULL ) {
		PrintWarning( 2, routine, "Cannot add null string to time "
		"series PO array for FORTRAN interface." );
		return( STATUS_FAILURE );
	}
	// First we need to realloc the string's length 
	if( _po_list == NULL ) {
		length  = strlen( string );
	}
	else {
		length  = strlen( string ) + strlen( _po_list );
	}
	_po_list = (char*)realloc( _po_list, ( length + 1 ) * sizeof( char ) );
	if( _po_size == 0 ) {
		_po_list[ 0 ] = '\0';  
	}
	if( _po_list == NULL ) {
		PrintWarning( 1, routine, "Troubles allocating %d chars.",
			length + 1 );
		return( STATUS_FAILURE );
	}
	strcat( _po_list, string );
	_po_list[ length ] = '\0';

	// Keep the counter going...
	_po_size++;

	return( STATUS_SUCCESS );
}

char* ResJSys :: getDebugString( int* size )
{
	char routine[] = "ResJSys :: getDebugString";
	if( !size ){
		PrintWarning( 1, routine,
		"Incoming Size variable is NULL." );
		return( "" );
	}
	(*size)		= _debug_str_len;
	return( _debug_str );
}

int ResJSys :: getIODEBUG ()
{
	char routine[] = "ResJSys :: getIODEBUG";
	return _iodebug;
}

int ResJSys :: getIPR ()
{
	char routine[] = "ResJSys :: getIPR";
	return _ipr;
}

//------------------------------------------------------------------------------
// ResJSys :: getMAINUM - Returns the value of _mainum, referring to which
//            NWSRFS program called RES-J.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer _mainum.
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Returns the value of _mainum, referring to which NWSRFS program called RES-J.
@return int _mainum
*/
int ResJSys :: getMAINUM ()
{
	//char routine[] = "ResJSys :: getMAINUM";
	return _mainum;
}

int ResJSys :: get_TSdefined ( )
{
	char routine[] = "ResJSys :: get_TSdefined";
	return _TSdefined;
}

char* ResJSys :: getWarningString( int* size )
{
	char routine[] = "ResJSys :: getWarningString";
	if( !size ){
		PrintWarning( 1, routine,
		"Incoming Size variable is NULL." );
		return( "" );
	}
	(*size)		= _warning_str_len;
	return( _warning_str );
}

int ResJSys :: setDebugSaveLevel( int level )
{
	char routine[] = "ResJSys :: setDebugSaveLevel";
	SetStatusLevel( level, DEBUG_INDEX );
	return( SetDebugLevel( level, DEBUG_INDEX ) );
}

int ResJSys :: setIPR_IODEBUG ( int ipr, int iodebug )
{
	char routine[] = "ResJSys :: setIPR_IODEBUG";
	_ipr = ipr;
	_iodebug = iodebug;
	return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// ResJSys :: setMAINUM - Sets the value of _mainum, referring to which
//            NWSRFS program called RES-J.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer referring to successful setting of _mainum variable.
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Sets the value of _mainum, referring to which NWSRFS program called RES-J.
@param mainum Integer value to be assigned to _mainum
*/
int ResJSys :: setMAINUM ( int mainum )
{
	//char routine[] = "ResJSys :: setMAINUM";
	_mainum = mainum;
	return( STATUS_SUCCESS );
}

void ResJSys :: set_TSdefined ( )
{
	char routine[] = "ResJSys :: set_TSdefined";
	_TSdefined = 1;
}

int ResJSys :: setWarningSaveLevel( int level )
{
	return( SetWarningLevel( level, WARNING_INDEX ) );
}

int ResJSys :: setStartDate( int hr, int da, int mo, int yr )
{
	_t1.setYear( yr );
	_t1.setMonth( mo );
	_t1.setDay( da );
	_t1.setHour( hr );

	Method :: setForecastDate1( _t1 );
	return( STATUS_SUCCESS );
}

int ResJSys :: setStartDate( TSDate& date )
{
	_t1 = date; 
	Method :: setForecastDate1( _t1 );
	return( STATUS_SUCCESS );
}

int ResJSys :: setEndDate( int hr, int da, int mo, int yr )
{
	_t2.setYear( yr );
	_t2.setMonth( mo );
	_t2.setDay( da );
	_t2.setHour( hr );
	Method :: setForecastDate2( _t2 );
	return( STATUS_SUCCESS );
}

int ResJSys :: setEndDate( TSDate& date )
{
	_t2 = date; 
	Method :: setForecastDate2( _t2 );
	return( STATUS_SUCCESS );
}

int ResJSys :: setCODates( int* hr, int* da, int* mo, int* yr, int num_co )
{
	char routine[] = "ResJSys :: setCODates";

	if( !hr || !da || !mo ) {
		PrintWarning( 2, routine, "Cannot set carryover save dates -"
		" missing hour, day, or month information." );
		return( STATUS_FAILURE );
	}

	// Add one for the end date of the simulation
	_num_co_dates = num_co + 1;

	_co_dates = new TSDate[ _num_co_dates ];
	_co_array_str = new char*[ _num_co_dates ];
	if( _co_dates == NULL ) {
		PrintWarning( 2, routine, "Could not allocate %d TSDates.",
			num_co );
		return( STATUS_FAILURE );
	}
	if( _co_array_str == NULL ) {
		PrintWarning( 2, routine, "Could not allocate %d char*s.",
			num_co );
		return( STATUS_FAILURE );
	}

	for( int i = 0; i < _num_co_dates-1; i++ ) {
		if( !yr ) {	
			_co_dates[i].setYear( 0 );
		}
		else {
			_co_dates[i].setYear( yr[i] );
		}
		_co_dates[i].setMonth( mo[i] );
		_co_dates[i].setDay( da[i] );
		_co_dates[i].setHour( hr[i] );
	}

	// Check to see if the end of the simulation is also a carryover date
	TSDate date;
	date = Method :: getForecastDate2();
	// Remember that _co_dates begins with index 0, 
	// but _num_co_dates with 1.
	// Therefore, if the user specified 2 carryover dates, by the time we
	// get to this point, _num_co_dates will be equal to 3, and the last
	// user specified date will be found in _co_dates at index 1.
	if( _num_co_dates == 1 ) {
		// There are no user specified carry over dates.  The only
		// one to worry about is algorithm-made, at the end of the 
		// simulation period.
		_co_dates[0].setMonth( date.getMonth() );
		_co_dates[0].setDay( date.getDay() );
		_co_dates[0].setYear( date.getYear() );
		_co_dates[0].setHour( date.getHour() );
	}
	else {
		if( date != _co_dates[_num_co_dates-2] ) {
			_co_dates[_num_co_dates-1].setMonth( date.getMonth() );
			_co_dates[_num_co_dates-1].setDay( date.getDay() );
			_co_dates[_num_co_dates-1].setYear( date.getYear() );
			_co_dates[_num_co_dates-1].setHour( date.getHour() );
		}
		else {
			_num_co_dates -= 1;
			_writeLastCO = 1;
		}
	}

	return( STATUS_SUCCESS );
}

TS** ResJSys :: getResJOutput( char** list, int nlist )
{
	char routine[] = "ResJSys :: getOutputTS";
	TS** ts_list = NULL;
	int i;

	ts_list = new TS*[nlist];
	if( ts_list == NULL ) {	
		PrintWarning( 1, routine, "Could not allocate %d TS*.", nlist);
		return( NULL );
	}
	for( i = 0; i < nlist; i++ ) {	
		ts_list[i] = _root->getOutputTS( list[i] );
		if( ts_list[i] == NULL ) {
			PrintWarning( 1, routine, "Could not get output time series "
			"%s.", list[i] );
			return( (TS**)NULL );
		}
	}
	return( ts_list );

/*  ==============  Statements containing RCS keywords:  */

/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_SetGet.cxx,v 1.7 2006/10/26 15:30:51 hsu Exp $";}
/*  ===================================================  */

}
