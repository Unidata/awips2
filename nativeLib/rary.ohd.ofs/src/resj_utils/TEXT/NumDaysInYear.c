/* ----------------------------------------------------------------------------
** HMNumDaysInYear - get the number of days in a year
** ----------------------------------------------------------------------------
** Copyright:   See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996  Steven A. Malers, RTi   Split out of the HMTD.c file.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** month	.... integer (1-12) representing month
** ndays	.... number of days in year
** year		.... year
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int NumDaysInYear ( int year )
{	int	ndays;

	if ( IsLeapYear(year) )
		ndays = 366;
	else	ndays = 365;
	return ndays;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/NumDaysInYear.c,v $";
 static char rcs_id2[] = "$Id: NumDaysInYear.c,v 1.1 1999/02/18 15:17:02 dws Exp $";}
/*  ===================================================  */

}
