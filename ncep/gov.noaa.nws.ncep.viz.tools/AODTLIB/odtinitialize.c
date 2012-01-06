/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs.h"

/* AODT library function */
extern int aodtv64_initcurrent(int);

int aodtv64_initialize(void)
/* Subroutine to initialize history structure variables for current analysis
   Inputs : none
   Outputs: none
   Return : 0 : o.k.
*/
{
  int iok;
  iok=aodtv64_initcurrent(0);  /* initialize all variables */

  return iok;
}
