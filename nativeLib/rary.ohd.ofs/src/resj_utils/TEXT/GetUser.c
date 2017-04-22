#include "ResJ.h"

/* ----------------------------------------------------------------------------
** HMGetUser - get the login name of the user who is running the current program
** ----------------------------------------------------------------------------
** notes:	(1)	Because of the way that getenv is implemented on
**			various machines, it is possible that a NULL address
**			is returned (not a NULL pointer).  Consequently,
**			call "getenv" twice to make an assignment.
**		(2)	Try the environment variables USER and LOGNAME when
**			resolving the user name.
** ----------------------------------------------------------------------------
** Variable	I/O	Description
**
** user		O	User login name.
** ----------------------------------------------------------------------------
*/
int GetUser ( char *user )
{	*user = '\0';

	if ( getenv("USER") )
		strcpy ( user, getenv("USER") );
	if ( !*user ) {
		if ( getenv("LOGNAME") )
			strcpy ( user, getenv("LOGNAME") );
	}
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetUser.c,v $";
 static char rcs_id2[] = "$Id: GetUser.c,v 1.1 1999/02/18 15:16:53 dws Exp $";}
/*  ===================================================  */

}
