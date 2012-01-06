/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

/* AODT library function */
extern int aodtv64_inserthistoryrec(int *,int *);

int aodtv64_historyrecordinsert(int *retmods,int *retrecs )
/* Subroutine to return next valid history file record structure
   within analysis dates defined by user in API.
   Output : retmods : number of records modified
            retrecs : total number of records written
   Return : 61 : Overwrote a record
            62 : Inserted a record in middle of history file
	    63 : Inserted a record in an empty history file
	    64 : Inserted a record at the end of a history file
*/
{
  int    iok,iret,modified,ioflag;

  iok=aodtv64_inserthistoryrec(&modified,&ioflag);
  *retrecs=iok;        /* number of records written */
  *retmods=modified;   /* number of records modified */  
  iret=60+ioflag;      /* insert/overwrite flag */

  return iret;
}
