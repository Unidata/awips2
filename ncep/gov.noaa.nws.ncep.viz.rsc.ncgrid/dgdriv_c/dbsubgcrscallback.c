#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

int db_subgCrsCallback(void (*aSubgCrsCallback)(char*))
{

   subgCrsClbkPtr = aSubgCrsCallback;
   return(0);
}
