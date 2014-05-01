#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_navCallback(void (*aNavCallback)(char *))
{
   int 	ierm;
   char	diagMessage[720];

   navClbkPtr = aNavCallback;
   sprintf (diagMessage, "%s%p", "set the pointer to the callback function navClbkPtr=", navClbkPtr);
   db_msgcave ("db_navCallback", "debug", diagMessage, &ierm);
   return(0);
}
