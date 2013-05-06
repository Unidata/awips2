#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_fhrsCallback(void (*aFhrsCallback)(void))
{
   int  ierm;
   char diagMessage[720];

   fhrsClbkPtr = aFhrsCallback;
   sprintf (diagMessage, "%s%p", "set the pointer to the callback function fhrsClbkPtr=", fhrsClbkPtr);
   db_msgcave ("db_fhrsCallback", "debug", diagMessage, &ierm);
   return(0);
}
