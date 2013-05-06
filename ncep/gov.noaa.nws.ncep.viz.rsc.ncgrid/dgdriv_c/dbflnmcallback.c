#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_flnmCallback(void (*aFlnmCallback)(char*))
{
   int  ierm;
   char diagMessage[720];

   flnmClbkPtr = aFlnmCallback;
   sprintf (diagMessage, "%s%p", "set the pointer to the callback function flnmClbkPtr=", flnmClbkPtr);
   db_msgcave ("db_flnmCallback", "debug", diagMessage, &ierm);
   return(0);
}
