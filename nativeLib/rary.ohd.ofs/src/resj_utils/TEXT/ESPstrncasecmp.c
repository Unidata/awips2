/* ----------------------------------------------------------------------------
** ESPstrncasecmp - compare strings ignoring case (needed to match UNIX version)
** ----------------------------------------------------------------------------
** Notes:	(1)	Note the size limit for strings.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split out of the HMUtil.c file.
** 27 Sep 1996	Catherine E. Nutting, RTi	Copied from HMData
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** i		L	Character counter.
** s1, s2	I	Two strings.
** sl1, sl2	L	Lowercase strings.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int ESPstrncasecmp ( char *s1, char *s2, int n )
{	char	sl1[256], sl2[256];
	int	i = 0;

	while ( *s1 ) {
		sl1[i++] = tolower ( *s1 );
		++s1;
	}
	sl1[i] = '\0';
	i = 0;
	while ( *s2 ) {
		sl2[i++] = tolower ( *s2 );
		++s2;
	}
	sl2[i] = '\0';

	return strncmp(sl1, sl2, n);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/ESPstrncasecmp.c,v $";
 static char rcs_id2[] = "$Id: ESPstrncasecmp.c,v 1.1 1999/02/18 15:16:39 dws Exp $";}
/*  ===================================================  */

}
