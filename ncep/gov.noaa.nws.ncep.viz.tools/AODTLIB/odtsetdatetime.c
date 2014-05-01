/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int aodtv64_setdatetime( int it1, int it2, char *id1, char *id2, int idelete ) 
/* set history file examination boundary dates/times within AODT library memory
   Inputs : AODT library start and end times/dates for history file examination
            also history file delete flag (for setting starttime_v64 and endtime_v64 variables
   Outputs: none
   Return : -61 : error with date/time and delete command
              0 : o.k.
*/
{
  int iret,iok;

  iret=0;
  iok=aodtv64_datetime(it1,it2,id1,id2,idelete);
  if(iok!=0) iret=-61;
  return iret;
}

