#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_returnduri(char *result)
{
   int      ier, diagMessageLength=2000;
   char     diagMessage[diagMessageLength];
/*----------------------------------------------------------------------*/
    diagMessage[0] = '\0';
    sprintf (diagMessage, "%s %s", "back from Java result=", result);
    db_msgcave ("db_returnduri", "debug", diagMessage, &ier);
   
    if ( result != NULL ) {
       if ( strlen(result) > 0 ) {
          G_MALLOC ( duriStrBack, char, strlen(result)+1, "Error allocating duriStrBack" );
          sprintf (duriStrBack, "%s", result);
          duriStrLength = strlen(result);
          sprintf (diagMessage, "%s %s ", "set duriStrBack= ", duriStrBack);
          db_msgcave ("db_returnduri", "debug", diagMessage, &ier);
       }
       else {
          duriStrBack = NULL;
          duriStrLength = 0;
/*          db_msgcave ("db_returnduri", "error", "!!! *back from Java result= NULL* !!!!", &ier);*/
       }
    }
    else {
       duriStrBack = NULL;
       duriStrLength = 0;
/*       db_msgcave ("db_returnduri", "error", "!!! *back from Java result= NULL* !!!!", &ier);*/
    }
    /*
    */
    return;

}
