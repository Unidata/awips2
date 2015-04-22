/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int aodtv64_historydeleterec(int *deleted,int *modified)
/* Subroutine to delete records within history file structure/file dependent
   upon the date/time values previously entered into the AODT library
   Inputs : none
   Outputs: number of deleted and modified records within history structure/file
   Return : 66 : successful delete of record(s)
*/
{
  int    iok,iret,mods;

  iok=aodtv64_deletehistoryrec(&mods);
  *deleted=iok;
  *modified=mods;
  iret=66;

  return iret;
}
