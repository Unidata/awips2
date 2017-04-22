// ----------------------------------------------------------------------------
// TSIdent - time series identifier class
// ----------------------------------------------------------------------------
// Notes:	(1)	A time series is identified by five parts:
//
//			Location[_sub].Source[_sub].Type.Interval.Scenario
//
//			Where location is typically a station or basin
//			identifier, source is typically some type of
//			abbreviation, type is the NWS data type, interval
//			is the number of hours or "month" or "year" and
//			scenario is a scenario identifier (the rest of the
//			identifier).  The Location and Source can each have
//			subsources.
// ----------------------------------------------------------------------------
// History:
//
// Apr 96	Steven A. Malers, RTi	Start developing the class based on
//					lots of previous work.
// 11 Nov 1996	Matthew J. Rutherford,	Changed to dynamic allocation.
//		RTi
// 21 May 1997	MJR, RTi		Added getBaseMultInterval.
// 16 Sep 1997	SAM, RTi		Did a substantial overhaul in order to
//					get the code ready for a Java port.
//					This involved adding new functions for
//					the sub/main location and source and
//					carrying around integer and string
//					versions of data, where appropriate.
// 07 Jan 1998	SAM, RTi		Add _alias so time series can be used
//					in expressions without having to use
//					the entire identifier.  Move
//					parseInterval to the TSInterval class.
// ----------------------------------------------------------------------------

#ifndef TSIdent_INCLUDED
#define TSIdent_INCLUDED

// The following are coordinated because they can be passed into a top-level
// function (like a constructor) as well as the individual set functions.

#define NO_SUB_LOCATION		0x1	// Mask for setLocation()
					// Do not use a sub-location.
#define NO_SUB_SOURCE		0x2	// Mask for setSource()
					// Do not use a sub-source.

#define SEPARATOR		"."	// Separator for identifier
					// parts
#define LOCATION_SEPARATOR	"_"	// Separator character for the
					// location information.
#define SOURCE_SEPARATOR	"_"	// Separator character for the
					// source information.

#include <string.h>
#include "ResJ.h"

class TSIdent
{
public:
	// Static functions...

	static int getIdentifierFromParts ( char *, char *, char *, char *,
					char *, char * );
					// Get an identifier from its.
	static TSIdent parseIdentifier ( char * );
					// Parse an identifier into parts.
	static TSIdent parseIdentifier ( char *, unsigned int );
					// Parse an identifier into parts using
					// flags.

	// The basic member functions...

	TSIdent ( void );		// Standard constructor.
	TSIdent ( unsigned int );	// Construct using behavior mask;
	TSIdent ( char * );		// Construct using the 5-part
					// identifier.
	TSIdent ( char *, unsigned int );
					// Construct using the 5-part identifier
					// and the behavior mask.
	TSIdent ( char *, char *, char *, char *, char * );
					// Construct using the 5-parts
					// separately.
	TSIdent ( char *, char *, char *, char *, char *, unsigned int );
					// Construct using the 5-parts
					// separately and the behavior mask.
	TSIdent ( const TSIdent& );	// Copy Constructor.

	~TSIdent ( void );		// Destructor.

	TSIdent& operator= ( const TSIdent& );	// Overload = operator.
	int operator== ( const TSIdent& );	// Overload == operator.
	int equals ( const TSIdent& );		// Named version of ==.
	int operator!= ( const TSIdent& );	// Overload != operator.
	int notEquals ( const TSIdent& );	// Named version of !=.
	int operator= ( char* );		// Overload =.
	operator char *();			// a (char*) cast.

	// Member functions specific to this class...

	// Member functions to return data members...

	char *getAlias ( void );	// Get the alias.
	unsigned int getBehaviorMask ( void );
					// Getthe behavior mask.
	char *getIdentifier( void );	// Get the whole identifier.

	char *getLocation ( void );	// Get the location.
	char *getMainLocation( void );	// Get the main location.
	char *getSubLocation( void );	// Get the sub-location.

	char *getSource( void );	// Get the source.
	char *getMainSource( void );	// Get the main source.
	char *getSubSource( void );	// Get the sub-source.

	char *getType( void );		// Get the Type

	char *getInterval( void );	// Get the interval as a string.
	int getIntervalBase ( void );	// Get the interval base as integer.
	int getIntervalMult ( void );	// Get the interval multiplier as
					// integer.

	char *getScenario( void );	// Get the scenario

	// Member functions to set data members...

	int setAlias ( char * );	// Set the alias.
	int setBehaviorMask ( unsigned int );
					// Set the behavior mask.
	int setIdentifier( char * );	// Set the whole identifier.
	int setIdentifier ( char *, char *, char *, char *, char * );
					// Set the whole identifier using the
					// component parts.
	int setIdentifier ( void );	// Reset the identifier based on the
					// current identifier components.

	int setLocation ( void );	// Reset the location using the main and
					// sub-locations.
	int setLocation ( char * );	// Set the location.
	int setLocation ( char *, char * );
					// Set the location using the main and
					// sub-location.
	int setMainLocation( char* );	// Set the main location.
	int setSubLocation( char* );	// Set the sub-location.

	int setSource ( void );		// Reset the source using the main and
					// sub-source.
	int setSource( char* );		// Set the source.
	int setSource ( char *, char * );
					// Set the source using the main and
					// sub-source.
	int setMainSource( char* );	// Set the main source.
	int setSubSource( char* );	// Set the sub-source.

	int setType( char* );		// Set the data type.

	int setInterval( char* );	// Set the interval.
	int setInterval( int, int );	// Set the interval using the base and
					// the multiplier.

	int setScenario( char* );	// Set the scenario.

	char *toString ( void );	// Return a string representation of
					// the TSIdent.

private:
	// Private member functions for object house-keeping...

	int init ( void );		// Initialize the data members
	int setFullIdentifier( char* );	// Set the full identifier.
	int setFullLocation( char* );	// Set the full location.
	int setFullSource( char* );	// Set the full source.
	int setIntervalString ( char * );
					// Set the interval string.

	// Data members...

	char	*_identifier;		// The whole identifier.
	char	*_alias;		// A short alias for the time series
					// identifier.

	char	*_full_location;	// The location (combining the main
					// location and the sub-location).
	char	*_main_location;	// The main location.
	char	*_sub_location;		// The sub-location.

	char	*_full_source;		// The time series source (combining
					// the main source and the sub-source).
	char	*_main_source;		// The main source.
	char	*_sub_source;		// The sub-source.

	char	*_type;			// The time series data type.

	char	*_interval_string;	// The time series interval as a
					// string.
	int	_interval_base;		// The base data interval.
	int	_interval_mult;		// The data interval multiplier.

	char	*_scenario;		// The time series scenario.

	unsigned int _behavior_mask;	// Mask that controls behavior (e.g.,
					// how sub-fields are handled).
}; // End of TSIdent class definition

#endif // TSIdent_INCLUDED
