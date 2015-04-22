#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_diagCallback(void (*aDiagCallback)(char*))
{
   int 	ierm;
   char diagMessage[720];

   diagClbkPtr = aDiagCallback;
   sprintf (diagMessage, "%s%p", "set the pointer to the callback function diagClbkPtr=", diagClbkPtr);
   db_msgcave ("db_diagCallback", "debug", diagMessage, &ierm);
   return(0);
}
