/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_commenthistoryrec( char * );

int aodtv64_historyaddcomment( char *inchar,int *modrec )
/* add comment to history file record 
   Inputs : inchar - character string containing comment to be added
   Outputs: modrec - record modifed in history file
   Return : -4 : error finding date/time in history file to be modified
	    68 : successful modification
*/
{
  int iok,iret;

  iok=aodtv64_commenthistoryrec(inchar);

  /* set return error code */
  if(iok>=0) {
    *modrec=iok;
    iret=68;
  } else {
    *modrec=-1;
    iret=-4;
  }

  return iret;
}
