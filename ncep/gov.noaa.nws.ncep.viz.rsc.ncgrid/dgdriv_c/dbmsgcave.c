#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_msgcave (char *functionName, char *logLevel, char *message, int *iret)
{
   int      diagMessageLength=2000;
   char     diagMessage[diagMessageLength];
/*----------------------------------------------------------------------*/
    *iret = 0;
    if ( diagClbkPtr != NULL ) {
       diagMessage[0] = '\0';
       if ( (int)strlen(message) > diagMessageLength ) {
          snprintf (diagMessage, diagMessageLength-1, "%s|%s: %s", functionName, logLevel, message);
       }
       else {
          sprintf (diagMessage, "%s|%s:: %s", functionName, logLevel, message);
       }
    diagClbkPtr(diagMessage);
    }
    return;
}
