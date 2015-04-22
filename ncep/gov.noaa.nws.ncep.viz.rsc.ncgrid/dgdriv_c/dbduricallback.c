#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_duriCallback(void (*aDuriCallback)(char*))
{
   int ierm;
   char     diagMessage[720];

   duriClbkPtr = aDuriCallback;
   sprintf (diagMessage, "%s%p", "set the pointer to the callback function duriClbkPtr=", duriClbkPtr);
   db_msgcave ("db_duriCallback", "debug", diagMessage, &ierm);
   return(0);
}
