/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

/* external AODT library function call */
extern int aodtv64_writehistoryfile(void);

int aodtv64_historywritefile( int *retrecs )
/* Subroutine to write history structure to output file
   Inputs : none
   Outputs: number of records written to file
   Return : -2 : error creating history file
            -3 : error writing to history file
	    67 : successful write of history file
*/
{
  int    iok,iret;

  iok=aodtv64_writehistoryfile();

  *retrecs=iok;   /* number of records written */
  if(iok>0) iret=67;

  return iret;
}
