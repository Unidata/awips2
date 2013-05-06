#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_returnflnm(char *result)
{
   int      diagMessageLength=2000, ier;
   char     diagMessage[diagMessageLength];
   char     *flnmStr;
/*----------------------------------------------------------------------*/
    diagMessage[0] = '\0';
//    sprintf (diagMessage, "%s %s", "back from Java result=", result);
//    db_msgcave ("db_returnflnm", "debug", diagMessage, &ier);
   
    if ( flnmStrBack != NULL ) {
       G_FREE( flnmStrBack, char );
    }
    if ( result != NULL ) {
       if ( strlen(result) > 0 ) {
          G_MALLOC ( flnmStrBack, char, strlen(result)+1, "Error allocating flnmStrBack" );
          sprintf (flnmStrBack, "%s", result);
          flnmStrLength = strlen(result);
//          sprintf (diagMessage, "%s %s ", "set flnmStrBack= ", flnmStrBack);
//          db_msgcave ("db_returnflnm", "debug", diagMessage, &ier);
       }
       else {
          flnmStrBack = NULL;
          flnmStrLength = 0;
          db_msgcave ("db_returnflnm", "error", "!!! *back from Java result= NULL* !!!", &ier);
       }
    }
    else {
       flnmStrBack = NULL;
       flnmStrLength = 0;
       db_msgcave ("db_returnflnm", "error", "!!! *back from Java result= NULL* !!!", &ier);
    }
    /*
    */
    return;

}
