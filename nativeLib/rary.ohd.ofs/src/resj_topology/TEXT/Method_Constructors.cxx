//------------------------------------------------------------------------------
// Method :: Method - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 05 May 1998	DKW		Enabled copy constructor.
// 13 Dec 2002	James R. VanShaar, RTi	Added _myValue.
// 14 Jan 2003	JRV, RTi	Added copy of _Active in copy-type Constructor
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Method.h"

Method :: Method()
{
	initialize();
}

Method :: Method( const Method& meth )
{
	char	routine[]="Method :: Method";

	initialize();

	// copy the char*s
	strcpy( _id, meth._id );

	// integers
	_is_constructed = meth._is_constructed;
	_t_int = meth._t_int;
	_t_mult = meth._t_mult;
	_mode = meth._mode;
	_Active = meth._Active;
	_group_id = meth._group_id;

	// doubles
	_t_step_in_sec = meth._t_step_in_sec;
	_myValue = meth._myValue;

	// TSDates
	_forecast_date1 = meth._forecast_date1;
	_forecast_date2 = meth._forecast_date2;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Method_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Method_Constructors.cxx,v 1.5 2006/10/26 15:27:10 hsu Exp $";}
/*  ===================================================  */

}
