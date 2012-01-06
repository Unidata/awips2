#include <stdlib.h>

int get_user ( char *user, int *len_user )
{
	strcpy ( user, getenv("LOGNAME") );
        *len_user = strlen( user );
	return 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/calb/src/mcp3_shared/RCS/get_user.c,v $";
 static char rcs_id2[] = "$Id: get_user.c,v 1.2 2006/04/20 22:19:13 wkwock Exp $";}
/*  ===================================================  */

}
