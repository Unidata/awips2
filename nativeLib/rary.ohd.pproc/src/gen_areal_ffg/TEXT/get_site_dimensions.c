#include "gen_areal_ffg.h"
#include "llgd.h"

/*---------------------------------------------------------------------------------*/
/*   routine to read HRAP coordinates of SW corner of site and dimensions of site  */
/*                                                                                 */
/*   this info is in .../geo_data/xxx/ascii/coord_xxx.dat                          */
/*                      where xxx = site id                                        */
/*---------------------------------------------------------------------------------*/

void get_site_dimensions(char *siteid, int *xor, int *yor, int *xsize, int *ysize,
                         float *lat00, float *lon00)

{
   FILE  *file;
   int    xr, yr, mxx, mxy, len;
   int iflg=0, npts=1, istat;
   float lat, lon, xxr, yyr;
   char   geo_dir[128],filename[256];

/*-------------------*/
/*  create filename  */
/*-------------------*/

     len = strlen("geo_data");
     get_apps_defaults("geo_data",&len,geo_dir,&len);

     sprintf(filename,"%s/%s/ascii/coord_%s.dat",geo_dir,siteid,siteid);

 /*-------------------------------------------------------*/
 /*   read coordinates of rectangle surrounding site area */
 /*   read coordinates of SouthWest corner of site area   */
 /*   coordinates are on national HRAP grid               */
 /*-------------------------------------------------------*/

 if((file = fopen(filename,"r")) == NULL)
 {
    printf("ERROR: %s file not found -",filename);
    printf(" program stopping \n");
    exit(0);
 }

 fscanf(file,"%d",&xr);
 fscanf(file,"%d",&yr);
 fscanf(file,"%d",&mxx);
 fscanf(file,"%d",&mxy);

 *xor = xr; *yor = yr; *xsize = mxx; *ysize = mxy;

 fclose(file);

 /*-------------------------------------------------------*/
 /*  generate lat/lon of NorthWest corner                 */
 /*-------------------------------------------------------*/

 xxr = xr;
 yyr = yr + mxy;

 LLGD(&lon,&lat,&npts,&xxr,&yyr,&iflg,&istat);
 *lat00 = lat;
 *lon00 = (-1) * lon;

 /* printf("site info: xor = %d  yor = %d  xsize = %d  ysize = %d  lat00 = %f  lon00 = %f\n",
         *xor, *yor, *xsize, *ysize, *lat00, *lon00); */

}
