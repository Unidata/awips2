/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_gettopovalue( int *topo )
/* return topography (land) value to application from AODT library
   Inputs : none
   Outputs: AODT topography value (1-land, 2-ocean)
   Return : -33 : invalid TOPO value 
              0 : o.k.
*/
{
  int iret;

  /* set TOPO value to return variable */
  *topo=odtcurrent_v64->IR.land;
  iret=0;
  if((*topo<0)||(*topo>2)) iret=-33;

  return iret;
}

int aodtv64_settopovalue( float curtopo )
/* assign SST value to AODT library variable
   Inputs : AODT topography value (1-land, 2-ocean)
   Outputs: none
   Return : -21 : invalid storm center location
            -33 : invalid TOPO value 
              0 : o.k.
*/
{
  int iret;

  /* check for valid storm center location */
  if((odtcurrent_v64->IR.latitude<-999.0)||(odtcurrent_v64->IR.latitude<-999.0)) {
    iret=-21;
  } else {
    /* set topography flag structure element for AODT library  */
    odtcurrent_v64->IR.land=curtopo;
    iret=0;
    if((odtcurrent_v64->IR.land<0)||(odtcurrent_v64->IR.land>2)) iret=-33;
  }

  return iret;
}
