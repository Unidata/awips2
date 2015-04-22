/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_gettemps(int);

int aodtv64_seteyecloudtemp(void)
/* Routine to search for, idenfify, and set the eye and cloud temperature values
   for the AODT library.  Temperatuers are set within AODT library.
   Inputs : none
   Outputs: none
   Return : -51 : eye, CWcloud, or warmest temperature <-100C or >+40C
              0 : o.k.
*/
{
  int iok;

  iok=aodtv64_gettemps(keyerM_v64);

  return iok;
}
