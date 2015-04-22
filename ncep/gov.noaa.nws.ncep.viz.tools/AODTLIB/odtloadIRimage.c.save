/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_loadIRimage(float **temps,float **lats,float **lons,int numx,int numy )
/* Subroutine to load IR image data grid values (temperatures and positions) into 
   data structure for AODT library
   Inputs : temperature, latitude, and longitude arrays centered on storm position location
            along with number of columns (x) and rows (y) in grid
   Outputs: none (areadata_v64 structure passed via global variable)
   Return : 0 : o.k.
*/
{
  int ixx,iyy;

  /* allocate space for data */
  if(areadata_v64 != (struct datagrid *)NULL) free(areadata_v64);
  areadata_v64=(struct datagrid *)malloc(sizeof(struct datagrid));

  /* load structure elements */
  for(ixx=0;ixx<numx;ixx++) {
    for(iyy=0;iyy<numy;iyy++) {
      areadata_v64->temp[iyy][ixx]=temps[iyy][ixx];
      areadata_v64->lat[iyy][ixx]=lats[iyy][ixx];
      areadata_v64->lon[iyy][ixx]=lons[iyy][ixx];
    }
  }
  areadata_v64->numx=numx;
  areadata_v64->numy=numy;

  return 0;
}
