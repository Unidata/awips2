#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_wsetevtname ( const char *evtname, int *iret )
{
    db_setevtname ( (char *) evtname, iret, strlen(evtname));
    return;
}
