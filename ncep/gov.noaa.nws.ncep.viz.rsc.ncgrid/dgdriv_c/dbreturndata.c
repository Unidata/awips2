#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_returndata(float *result, int *nrez)
{
    char     diagMessage[720];   
    int ier, jj;
/*----------------------------------------------------------------------*/
    diagMessage[0] = '\0';
    sprintf (diagMessage, "%s %d", "back from Java nrez=", *nrez);
    db_msgcave ("db_returndata", "debug", diagMessage, &ier);
    if ( *nrez != 0 && result != NULL ) {
       sprintf (diagMessage, "%s %f", "back from Java result[0]=", result[0]);
       db_msgcave ("db_returndata", "debug", diagMessage, &ier);
       if ( *nrez > 0 ) {
         rdataBackSize = *nrez;
         sprintf (diagMessage, "%s %d", "set rdataBackSize to ", rdataBackSize);
         db_msgcave ("db_returndata", "debug", diagMessage, &ier);
         G_MALLOC ( rdataBack, float, rdataBackSize,
                 "Error allocating rdataBack grid" );
         for( jj=0; jj < rdataBackSize; jj++ ) {
            rdataBack[jj] = result[jj];
         }
       }
       else {
         rdataBack = NULL;
         rdataBackSize = 0;
         db_msgcave ("db_returndata", "error", "!!! *back from Java result= NULL* !!!!", &ier);
       }
    }
    else {
       rdataBack = NULL;
       rdataBackSize = 0;
       db_msgcave ("db_returndata", "error", "!!! *back from Java result= NULL* !!!!", &ier);
    }
    return;
}
