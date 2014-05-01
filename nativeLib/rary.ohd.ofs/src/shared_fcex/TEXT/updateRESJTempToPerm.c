#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* EJM - RTi - 08/08/98
   This routine updates for each RES-J Operation the value in the P array 
   that indicates if the RES-J file is permanent.
*/

int updateresjtemptoperm( float *p ) {

  int i;

  i = 0;
  while ( p[i] > 0 ) {
    if ( (p[i] > 57.90) && (p[i] < 58.9))
    	p[i+7] = 1;
/* skip to next Operation */	
    i = p[ i + 1 ] - 1;
    if (i == -1) break;
    }

  return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/updateRESJTempToPerm.c,v $";
 static char rcs_id2[] = "$Id: updateRESJTempToPerm.c,v 1.2 2000/03/15 14:22:36 scv Exp $";}
/*  ===================================================  */

}
           
