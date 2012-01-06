/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getsstvalue( float *sst )
/* return SST file name and SST value to application from AODT library
   Inputs : none
   Outputs: AODT SST value
   Return :  115 : invalid SST value
               0 : o.k.
*/
{
  int iret;

  /* set SST value to return variable */
  *sst=odtcurrent_v64->IR.sst;
  iret=0;
  if(*sst<-90.0) iret=115;

  return iret;
}

int aodtv64_setsstvalue( float cursst )
/* read SST value from file and assign to AODT library variable
   Inputs : AODT library SST value
   Outputs: none
   Return : -21 : invalid storm center location
            115 : invalid SST value 
              0 : o.k.
*/
{
  int iret;

  /* check for valid storm center location */
  if((odtcurrent_v64->IR.latitude<-999.0)||(odtcurrent_v64->IR.latitude<-999.0)) {
    iret=-21;
  } else {
    /* set sst structure element for AODT library*/
    odtcurrent_v64->IR.sst=cursst;
    iret=0;
    if(odtcurrent_v64->IR.sst<-90.0) iret=115;
  }

  return iret;
}
