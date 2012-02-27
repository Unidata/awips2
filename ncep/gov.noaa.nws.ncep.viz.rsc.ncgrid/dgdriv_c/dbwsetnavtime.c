#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/time.h>

void db_wsetnavtime ( const char *timeNav, int *iret )
{
    db_setnavtime ( (char *) timeNav, iret, strlen(timeNav));
    
    return;
}
