/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

extern int aodtv64_readtopo( char *, float, float, int * ); 

int aodtv64_readtopofile( char *tfile, float lat, float lon, int *topo ) 
/* read topography file and return topograph (land) value to application
   Inputs  : tfile - topography file name
             mlat  - latitude of data point in question
             mlon  - longitude of data point in question
   Outputs : topo  - <0=error,1=land,2=water
   Return  : -31 : error accessing topography file
             -32 : error reading topography file
              71 : cyclone is over land
               0 : cyclone is over water
*/
{
  int iret,itopo;
  char *retstrng;
 
  retstrng=(char *)calloc((size_t)100,sizeof(char)); 
  iret=aodtv64_readtopo(tfile,lat,lon,&itopo);
  strcpy(tfile,retstrng); 
  tfile[strlen(retstrng)]='\0'; 
  *topo=itopo;

  free(retstrng);
  return iret;
}
