//------------------------------------------------------------------------------
// TS::formatOutput - format a time series for output
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is used to write a time series to a simple format.
//			If the format corresponds to a specific time series
//			type in a derived class, use that class'
//			writePersistent routine.
//------------------------------------------------------------------------------
// History:
// 
// 30 Sep 1997	Steven A. Malers,	First version to replace some of the
//		Riverside Technology,	TSPrint* routines.
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

// Format as strings...
char ** TS :: formatOutput ( int format, unsigned long int flags )
{	char	routine[]="TS::formatOutput(int,long)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in derived classes" );

	// put in the following so that we don't get warnings about variables
	// not getting used...
	if ( format ) {
		; // do nothing
	}
	if ( flags ) {
		; // do nothing
	}

	return (char **)NULL;
}

// Format and write to a file...
int TS :: formatOutput ( ostream &out, int format, unsigned long int flags )
{	char	routine[]="TS::formatOutput(ostream&,int,long)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in derived classes" );

	// put in the following so that we don't get warnings about variables
	// not getting used...
	if ( out ) {
		; // do nothing
	}
	if ( format ) {
		; // do nothing
	}
	if ( flags ) {
		; // do nothing
	}

	return( STATUS_FAILURE );
}

int TS :: formatOutput ( char *fname, int format, unsigned long int flags )
{	char		routine[] = "TS::formatOutput(char*,int,long)";

	// put in the following so that we don't get warnings about variables
	// not getting used...
	if ( fname ) {
		; // do nothing
	}
	if ( format ) {
		; // do nothing
	}
	if ( flags ) {
		; // do nothing
	}

	return( STATUS_FAILURE );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_formatOutput.cxx,v $";
 static char rcs_id2[] = "$Id: TS_formatOutput.cxx,v 1.2 2000/05/19 13:35:21 dws Exp $";}
/*  ===================================================  */

}
