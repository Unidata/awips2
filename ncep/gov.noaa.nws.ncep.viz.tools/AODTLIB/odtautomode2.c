/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
 
/* AODT library function */
extern int aodtv64_automode2(float,float,float *,float *,int *);

int aodtv64_runautomode2(float flat,float flon,float *lat, float *lon,int *pos)
/* Subroutine to run final AUTO-AODT routine.  This routine will
   return the "best" determined position to API for the storm center.
   Inputs : forecast latitude and longitude
   Outputs: location : latitude and longitude
            positioning method : 1 - interpoation
	                         2 - laplacian
				 3 - warm spot
				 4 - extrapolation
				 0 - error
   Return : 51 : good position
*/
{
  int xpos;
  float xlat,xlon;

  aodtv64_automode2(flat,flon,&xlat,&xlon,&xpos);
  *lat=xlat;
  *lon=xlon;
  *pos=xpos;

  return 51;
}
