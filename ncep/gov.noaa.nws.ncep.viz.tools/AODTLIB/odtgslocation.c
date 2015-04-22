/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getlocation( float *olat, float *olon, int *opos )
/* return current storm center location to application from AODT library
   Inputs : none
   Outputs: AODT library current storm center latitude and longitude values
            and location positioning method : 1-forecast interpolation
	                                      2-laplacian technique
					      3-warm spot
					      4-extrapolation
   Return : -21 : invalid storm center position
             21 : user selected storm center position
             22 : auto selected storm center position
*/
{
  int iret;

  iret=0;
  /* set storm center latitude value to return variable */
  *olat=odtcurrent_v64->IR.latitude;
  /* set storm center longitude value to return variable */
  *olon=odtcurrent_v64->IR.longitude;
  /* set storm center positioning flag to return variable */
  *opos=odtcurrent_v64->IR.autopos;

  iret=21; /* user selected image location */
  /* check for invalid storm center location values */
  if((*olon<-180.)||(*olon>180.)) iret=-21;
  if((*olat<-90.)||(*olat>90.)) iret=-21;
  if(*opos>=1) iret=22;

  return iret;
}

int aodtv64_setlocation( float ilat, float ilon, int ipos )
/* set current storm center location within from AODT library memory
   Inputs : AODT library current storm center latitude and longitude values
            and location positioning method : 1-forecast interpolation
	                                      2-laplacian technique
					      3-warm spot
					      4-extrapolation
   Outputs: none
   Return : -21 : invalid storm center position
             21 : user selected storm center position
             22 : auto selected storm center position
*/
{
  int iret;

  /* assign current storm center latitude value to AODT library variable */
  odtcurrent_v64->IR.latitude=ilat;
  /* assign current storm center longitude value to AODT library variable */
  odtcurrent_v64->IR.longitude=ilon;
  /* assign current storm center positioning flag to AODT library variable */
  odtcurrent_v64->IR.autopos=ipos;
  if((odtcurrent_v64->IR.longitude<-180.)||(odtcurrent_v64->IR.longitude>180.)) iret=-21;
  if((odtcurrent_v64->IR.latitude<-90.)||(odtcurrent_v64->IR.latitude>90.)) iret=-21;

  iret=21; /* user selected image location */
  if(ipos>=1) iret=22;


  return iret;
}
