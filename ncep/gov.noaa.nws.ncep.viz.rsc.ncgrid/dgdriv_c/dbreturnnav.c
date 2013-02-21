#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_returnnav(char *result)
/************************************************************************
 *                                                                      *
 * db_returnnav                                                         *
 *                                                                      *
 * m.gamazaychikov/CWS	06/11   Created                                 *
 ************************************************************************/
{
    char     diagMessage[720];
    int      ier;
/*----------------------------------------------------------------------*/
    diagMessage[0] = '\0';
    sprintf (diagMessage, "%s %s", "back from Java result=", result);
    db_msgcave ("db_returnnav", "debug", diagMessage, &ier);
   
    if ( result != NULL ) {
       if ( strlen(result) > 0 ) {
          G_MALLOC ( navStrBack, char, strlen(result)+1, "Error allocating navStrBack" );
          sprintf (navStrBack, "%s", result);
          navStrLength = strlen(result);
          sprintf (diagMessage, "%s %s", "set navStrBack=", navStrBack);
          db_msgcave ("db_returnnav", "debug", diagMessage, &ier);
       }
       else {
          navStrBack = NULL;
          navStrLength = 0;
          db_msgcave ("db_returnnav", "error", "!!! *back from Java result= NULL* !!!", &ier);
       }
    }
    else {
       navStrBack = NULL;
       navStrLength = 0;
       db_msgcave ("db_returnnav", "error", "!!! *back from Java result= NULL* !!!", &ier);
    }
    return;
}
