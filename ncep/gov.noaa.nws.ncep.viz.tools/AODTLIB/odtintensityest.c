/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_calcintensity(void);

int aodtv64_intensity(void)
/* Routine to calculate intensity values and store values in AODT library.
   Inputs : none
   Outputs: none
   Return : 71 : storm is over land
             0 : o.k.
*/
{
  int iok;

  iok=aodtv64_calcintensity();

  return iok;
}
