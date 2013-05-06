#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_dataCallback(void (*aDataCallback)(char*))
{
   int 	ierm;
   char diagMessage[720];

   dataClbkPtr = aDataCallback;
   sprintf (diagMessage, "%s%p", "set the pointer to the callback function dataClbkPtr=", dataClbkPtr);
   db_msgcave ("db_dataCallback", "debug", diagMessage, &ierm);
   return(0);
}
