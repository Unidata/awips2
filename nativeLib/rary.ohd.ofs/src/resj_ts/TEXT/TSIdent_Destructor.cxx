//------------------------------------------------------------------------------
// TSIdent destructor
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is the destructor for the TSIdent class.  At this
//			time, it deletes all strings that have been allocated.
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Update to delete data members for
//					enhanced TSIdent class.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

TSIdent :: ~TSIdent( void )
{
	if( _alias ) {
		delete [] _alias;
	}
	if( _identifier ) {
		delete [] _identifier;
	}
	if( _full_location ) {
		delete [] _full_location;
	}
	if( _main_location ) {
		delete [] _main_location;
	}
	if( _sub_location ) {
		delete [] _sub_location;
	}
	if( _full_source ) {
		delete [] _full_source;
	}
	if( _main_source ) {
		delete [] _main_source;
	}
	if( _sub_source ) {
		delete [] _sub_source;
	}
	if( _type ) {
		delete [] _type;
	}
	if( _interval_string ) {
		delete [] _interval_string;
	}
	if( _scenario ) {
		delete [] _scenario;
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSIdent_Destructor.cxx,v $";
 static char rcs_id2[] = "$Id: TSIdent_Destructor.cxx,v 1.2 2000/05/19 13:06:32 dws Exp $";}
/*  ===================================================  */

}
