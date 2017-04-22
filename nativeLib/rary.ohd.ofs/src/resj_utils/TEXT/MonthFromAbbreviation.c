/* ----------------------------------------------------------------------------
** MonthFromAbbreviation - get month number from abbreviated name
** ----------------------------------------------------------------------------
** Copyright:   See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996  Steven A. Malers, RTi   Split out of the HMTD.c file.
** 07 Oct 1996	SAM, RTi		Add HMExtern.h include to get to
**					global data.
** 03 Jan 1997	CEN, RTi		Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** i		L	Counter for months.
** monthabbr	O	Abbreviated month name.
** ----------------------------------------------------------------------------
*/

#include "resj/Extern.h"

int MonthFromAbbreviation ( char *monthabbr )
{	int	i = 0;

	while ( *ESPmonthabbr[i] ) {
		if ( !ESPstrncasecmp(monthabbr,ESPmonthabbr[i],3) )
			return (i + 1);
		++i;
	}
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/MonthFromAbbreviation.c,v $";
 static char rcs_id2[] = "$Id: MonthFromAbbreviation.c,v 1.2 2000/05/18 13:08:32 dws Exp $";}
/*  ===================================================  */

}
