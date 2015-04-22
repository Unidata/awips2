#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_returnfhrs(char *result)
/************************************************************************
 *                                                                      *
 * db_returnfhrs                                                         *
 *                                                                      *
 * m.gamazaychikov/CWS	06/11   Created                                 *
 ************************************************************************/
{
    int      ier;
    char     diagMessage[2000];
/*----------------------------------------------------------------------*/
    diagMessage[0] = '\0';
    sprintf (diagMessage, "%s %s", "back from Java result=", result);
    db_msgcave ("db_returnfhrs", "debug", diagMessage, &ier);
   
    if ( result != NULL ) {
       if ( strlen(result) > 0 ) {
          G_MALLOC ( fhrsStrBack, char, strlen(result)+1, "Error allocating fhrsStrBack" );
          sprintf (fhrsStrBack, "%s", result);
          fhrsStrLength = strlen(result);
          sprintf (diagMessage, "%s %s", "set fhrsStrBack=", fhrsStrBack);
          db_msgcave ("db_returnfhrs", "debug", diagMessage, &ier);
       }
       else {
          fhrsStrBack = NULL;
          fhrsStrLength = 0;
          db_msgcave ("db_returnfhrs", "error", "!!! *back from Java result= NULL* !!!!", &ier);
       }
    }
    else {
       fhrsStrBack = NULL;
       fhrsStrLength = 0;
       db_msgcave ("db_returnfhrs", "error", "!!! *back from Java result= NULL* !!!!", &ier);
    }
    return;
}
