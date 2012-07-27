#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_retsubgcrs ( char *prj, int kx, int ky,
                   float lllat, float lllon, float urlat, float urlon,
                   float angl1, float angl2, float angl3, int *iret )
/************************************************************************
 *									*
 * db_retsubgcrs               						*
 *									*
 ************************************************************************/
{
    int ier;
    char nav[320];
/*---------------------------------------------------------------------*/
    if ( subgCrsClbkPtr == NULL ) {
       ier = -17;
       return;
    }
    
   /*
    * Initialization
    */
    sprintf ( nav,"%s;%d;%d;%f;%f;%f;%f;%f;%f;%f",
                       prj, kx, ky, lllat,lllon,urlat,urlon,
                       angl1,angl2,angl3);
    subgCrsClbkPtr(nav);

    return;
}
