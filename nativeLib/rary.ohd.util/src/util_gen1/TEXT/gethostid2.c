#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void gethostid2(long* hostid)
{
    *hostid = (long)gethostid();

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/gethostid2.c,v $";
 static char rcs_id2[] = "$Id: gethostid2.c,v 1.1 2004/05/03 21:36:05 hank Exp $";}
/*  ===================================================  */

}
