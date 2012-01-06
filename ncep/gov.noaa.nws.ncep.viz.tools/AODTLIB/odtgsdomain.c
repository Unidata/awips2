/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getdomain( int *domain )
/* return current ocean domain variable to application from AODT library
   Inputs : none
   Outputs: AODT library ocean domain flag
   Return :  72 : automatically determined storm basin
             73 : manually determined storm basin
*/
{
  int iret;

  /* set ocean domain flag to return variable */
  *domain=idomain_v64;
  if(ixdomain_v64==0) {
    iret=72;
  } else {
    iret=73;
  }

  return iret;
}

int aodtv64_setdomain( int indomain )
/* set current ocean domain variable within AODT library memory
   Inputs : domain flag value from input
   Outputs: none
   Return : -81 : error deterimining storm basin
*/
{
  int domain;
  float xlon;

  /* obtain current storm center longitude */
  xlon=odtcurrent_v64->IR.longitude;


  if((xlon<-180.0)||(xlon>180.0)) return -81;

  ixdomain_v64=indomain;
  /* determine oceanic domain */
  if(indomain==0) {
    /* automatically determined storm basin */
    if(xlon>=0.0) {
      domain=0;    /* atlantic and east pacific to 180W/dateline */
    } else {
      domain=1;    /* west pacific and other regions */
    }
  } else {
    /* manually determined storm basin */
    domain=indomain-1;
  }

  /* assign ocean domain flag value to AODT library variable */
  idomain_v64=domain;

  return 0;
}
