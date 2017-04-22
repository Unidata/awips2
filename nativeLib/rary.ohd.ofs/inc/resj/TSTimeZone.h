//------------------------------------------------------------------------------
// TSTimeZone - class to encapsulate time zone informatin.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This class is used as both a data storage mechanism to
//		store the abbreviation, number, and description of a time
//		zone, and as a location for some time-zone conversion routines.
//------------------------------------------------------------------------------
// History:
// 
// 01 Oct 1997	Matthew J. Rutherford,	Created class description.
//		RTi
// 05 Jan 1998	Steven A. Malers, RTi	Update to be compatible with Java code.
//------------------------------------------------------------------------------

#ifndef TSTimeZone_INCLUDED
#define TSTimeZone_INCLUDED

#include "ResJ.h"
#include <string.h>

typedef struct {
	char	*tz, *desc;
	int	ntz, dsflag;
} TSTimeZoneData;

class TSTimeZone
{
public:
	TSTimeZone ();				// Standard constructor.

	virtual ~TSTimeZone ( );		// Destructor.

	void operator= ( const TSTimeZone& );	// overload =

	int operator== (const TSTimeZone&);	// overload ==

	// Instance Methods //

	int calculateOffsetHours( char * );
					// the incoming tz to the internal one.
	char* getCode();		// Returns the TZ abbreviation.

	char* getDescription();	// Returns the TZ description.

	int getDSFlag();		// Routine to get the daylight savings
					// flag.
	int getNumber();		// Returns the TZ number.

	int setCode( char* );		// Sets the TZ abbreviation.

	int setNumber( int, int = 0 );	// Sets the TZ number.

	const char* toString();		// Converts the internal values to
					// a string.
	// Class Methods //

	static int calculateOffsetHours( char*, char* );
					// Calculate the number of hours from 
					// one time zone to another.
	static char* getDefinedCode( int, int = 0 );
					// Returns a defined abbreviation for
					// a zone number and optionally for
					// a zone number and daylight savings
					// flag.
	static char** getDefinedZones();// Routine to return a string list of
					// the recognized zones.
	static int getDefinedDSFlag( char* );
	static int getDefinedNumber( char* );
					// Returns a defined zone number for
					// an abbreviation.
	static char* getDefinedDescription( char* );
					// Returns a defined description
					// for either an abbreviation or for
					// a zone number.

	static const TSTimeZoneData TimeZoneData[];

private:
	static const int	RESET_WITH_CODE;
	static const int	RESET_WITH_NUMBER;

	int		init( );

	int		reset( int );	// Resets the stored values based
					// on one of the fields.

	int		_ntz;		// Time zone number for standard
					// time (0=Z).
	int		_dsflag;	// Daylight savings flag (1 if DS).

	char		_tz[8];	// Time zone abbreviation (ie EST).
};

#endif
