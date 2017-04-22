/* ----------------------------------------------------------------------------
** ESPstrcasecmp - compare strings ignoring case (needed to match UNIX version)
** ----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
** ----------------------------------------------------------------------------
** Notes:	(1)	Note the size limit for strings.
** ----------------------------------------------------------------------------
** History:
**
** 06 Sep 1996	Steven A. Malers, RTi	Split out of HMUtil.c file.
** 07 Oct 1996	SAM, RTi		Add <ctype.h> to prototype functions.
** 16 Oct 1996	SAM, RTi		Change to allocate memory dynamically.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** i		L	Character counter.
** len1, len2	L	Lengths of two strings.
** n		I	Number of characters to compare.
** s1, s2	I	Two strings.
** sl1, sl2	I	Lowercase strings.
** status	L	Return status from strcmp.
** ----------------------------------------------------------------------------
*/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "ResJ.h"

int ESPstrcasecmp ( char *s1, char *s2 )
{	char	routine[] = "ESPstrcasecmp", *sl1, *sl2;
	int	i, len1, len2, status;

	/*
	** Make sure that the strings coming in exist...
	*/

	if ( !s1 ) {
		return 1;
	}
	if ( !s2 ) {
		return 1;
	}

	/*
	** Now allocate memory for the lowercase strings...
	*/

	len1 = strlen ( s1 );
	len2 = strlen ( s2 );

	sl1 = (char *)(malloc((len1+1)*sizeof(char)));
	if ( !sl1 ) {
		PrintWarning ( 10, routine,
		"Trouble allocating for lowercase string 1 (%d characters)",
		(len1 + 1) );
		return 1;
	}
	sl2 = (char *)(malloc((len2+1)*sizeof(char)));
	if ( !sl2 ) {
		PrintWarning ( 10, routine,
		"Trouble allocating for lowercase string 2 (%d characters)",
		(len2 + 1) );
		free ( sl1 );
		return 1;
	}

	i = 0;
	while ( s1[i] ) {
		sl1[i] = tolower ( s1[i] );
		++i;
	}
	sl1[i] = '\0';
	i = 0;
	while ( s2[i] ) {
		sl2[i] = tolower ( s2[i] );
		++i;
	}
	sl2[i] = '\0';

	status = strcmp(sl1, sl2);
	free ( sl1 );
	free ( sl2 );
	return status;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/ESPstrcasecmp.c,v $";
 static char rcs_id2[] = "$Id: ESPstrcasecmp.c,v 1.1 1999/02/18 15:16:38 dws Exp $";}
/*  ===================================================  */

}
