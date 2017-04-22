/* ----------------------------------------------------------------------------
** MonthAbbreviation - get abbreviated name for month
** ----------------------------------------------------------------------------
** Copyright:   See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996  Steven A. Malers, RTi   Split out of the HMTD.c file.
** 07 Oct 1996	SAM, RTi		Include HMExtern.h to get to global
**					data.
** 03 Jan 1997	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** month	I	Integer month (1-12)
** monthname	O	Abbreviation for month name.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

char *MonthAbbreviation ( int month )
{	if ( (month < 1) || (month > 12) )
		return ESPmonthabbr[0];
	else	return ESPmonthabbr[month - 1];

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/MonthAbbreviation.c,v $";
 static char rcs_id2[] = "$Id: MonthAbbreviation.c,v 1.2 2000/05/18 13:08:32 dws Exp $";}
/*  ===================================================  */

}
