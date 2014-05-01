/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_automode1(float *,float *,int *);

int aodtv64_runautomode1(float *lat, float *lon,int *pos)
/* Subroutine to run initial AUTO-AODT routine.  This routine
   will return interpolated forecast file position to API.
   Inputs : nonoe
   Outputs: position : latitude and longitude
            positioning method (1=interpolation, 4=extrapolation, 0=error)
   Return : -43 : Error w/ forecast file open and BAD extrapolation
            -44 : Invalid forecast file and BAD extrapolation
            -45 : Error w/ forecast file read and BAD extrapolation
            -46 : Error w/ forecast interpolation and BAD extrapolation
             42 : GOOD INTERPOLATION
             43 : Error w/ forecast file open but GOOD EXTRAPOLATION
             44 : Invalid forecast file but GOOD extrapolation
             45 : Error w/ forecast file read but GOOD extrapolation
             46 : Error w/ forecast interpolation but GOOD EXTRAPOLATION
*/
{
  int iok,ipos;
  float xlat,xlon;

  iok=aodtv64_automode1(&xlat,&xlon,&ipos);
  *lat=xlat;
  *lon=xlon;
  *pos=ipos;

  return iok;
}
