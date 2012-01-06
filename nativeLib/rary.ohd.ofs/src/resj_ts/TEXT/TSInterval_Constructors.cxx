// ----------------------------------------------------------------------------
// TSInterval Constructors
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	
//
// ----------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Steven A. Malers,	Port from Java version.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
// Variables:		I/O	Description		
//
// ----------------------------------------------------------------------------

#include "TSInterval.h"

TSInterval :: TSInterval ( void )
{
	init ();
}

// ----------------------------------------------------------------------------
// TSLimits Copy Constructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	
//
// ----------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	SAM, RTi	Port from Java.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description		
//
// t		I	incoming date
// ----------------------------------------------------------------------------

TSInterval :: TSInterval ( const TSInterval& interval )
{	char	routine[] = "TSInterval.CopyConstructor";

	init ();

 	_interval_base = interval._interval_base;
 	_interval_mult = interval._interval_mult;
 	setBaseString ( interval._interval_base_string );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSInterval_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: TSInterval_Constructors.cxx,v 1.1 1999/02/18 15:19:44 dws Exp $";}
/*  ===================================================  */

}
