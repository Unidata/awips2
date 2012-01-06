#include "gen_areal_ffg.h"
#include "select_ffg_file.h"
#include "ffconv.h"
#include "llgd.h"
#include "netcdf.h"

/*-----------------------------------------------------------------------------------*/
/*   this function creates the gridded FFG mosaic                                    */
/*   it opens and reads the netCDF files containing the gridded FFG data for         */
/*       each RFC                                                                    */
/*   it also mallocs space for mosaic arrays (first time only)                       */
/*                                                                                   */
/*   Input:                                                                          */
/*   input_dir = directory containing the gridded FFG netCDF files                   */
/*   irfc = RFC number (from parsing RFC name list)                                  */
/*   duration = duration in units of hours                                           */
/*                                                                                   */
/*   Arrays:                                                                         */
/*   rfc_ffg_grid =    gridded ffg array read from netcdf file for RFC               */
/*                     (type unsigned char) (one dim array)                          */
/*                                                                                   */
/*   mosaic_ffg_float = mosaic ffg array (type float)                                */
/*   mosaic_ffg_uchar = mosaic ffg array (type unsigned char) (one dim array)        */
/*                      - this is the array written to the netCDF file               */
/*                                                                                   */
/*   Calling routine:  main                                                          */
/*                                                                                   */
/*   Functions called:                                                               */
/*   ffconv - converts unsigned char to float value according to netcdf file formula */
/*-----------------------------------------------------------------------------------*/

void create_ffg_mosaic(int irfc, int idur, int duration, int *nmalloc,
                       int xor, int yor, int wfo_xsize, int wfo_ysize)
{

static int prev_dur=0;
int xoffset, yoffset, yloc, xloc, yoff, indwfo, indrfc;
int xrl, yrl, rfc_xsize, rfc_ysize, rfc_size;
int i, arrayid;
int iflg=1, istat, status;
int xdim, ydim, ixh=0, iyh=0, ix, iy, xru, yru;
long xs = 0, ys = 0;
float *plat00, *plon00, lat00, lon00, xhrap, yhrap;
char netcdf_fname[256];

/*--------------------------------*/
/*   netcdf specific variables    */
/*--------------------------------*/

int ncid, l_len;
nc_type l_type;
/* unsigned char *fillv, fillval; */

/*----------------------------------------------------*/
/*   malloc space for mosaic arrays (first time only) */
/*   initialize mosaic_ffg_float array to value of miss_value_float (=-99.)  */
/*   initialize mosaic_ffg_uchar array to all 0       */
/*----------------------------------------------------*/

if(*nmalloc == 1)
{

   mosaic_ffg_float = (float **) malloc (wfo_ysize*sizeof(float *));
   for (i=0; i<wfo_ysize; i++)
   {
      mosaic_ffg_float[i] = (float *) malloc (wfo_xsize*sizeof(float));

      if(!mosaic_ffg_float[i])
      {
        printf("malloc of space for mosaic_ffg_float array failed -- ");
        printf("program stopping\n");
        exit(4);
      }
   }

   mosaic_ffg_uchar = (unsigned char *) malloc ((wfo_ysize*wfo_xsize)*sizeof(unsigned char));

   if(!mosaic_ffg_uchar)
   {
     printf("malloc of space for mosaic_ffg_uchar array failed -- ");
     printf("program stopping\n");
     exit(4);
   }

   *nmalloc = *nmalloc + 1;
}

/*-----------------------------------------------------*/
/*  initialize arrays (for each duration calculation)  */
/*-----------------------------------------------------*/

if(prev_dur != duration)
{
   for(iy=0; iy<wfo_ysize; iy++)
   {
      for(ix=0; ix<wfo_xsize; ix++)
      {
         mosaic_ffg_float[iy][ix] = miss_value_float;
      }
   }

   memset(mosaic_ffg_uchar, fill_value_uchar, (wfo_xsize*wfo_ysize));

   prev_dur = duration;
}

/*----------------------------------------------------------*/
/*   get full pathname of file containing RFC gridded FFG   */
/*----------------------------------------------------------*/

strcpy(netcdf_fname,filenames_array[irfc]);
printf("opening file = %s\n",netcdf_fname);

/*--------------------------------*/
/*  open netcdf file for reading  */
/*--------------------------------*/

status = nc_open(netcdf_fname,NC_NOWRITE,&ncid);
if(status != NC_NOERR)
{
   printf("error opening netcdf file -- program stopping\n");
   exit(1);
}

/*--------------------------------*/
/*  get dimensions of RFC  area   */
/*--------------------------------*/

xdim = ncdimid(ncid, "x");
ydim = ncdimid(ncid, "y");

if(xdim == -1 || ydim ==  -1)
{
   printf("error in ncdimid -- program stopping\n");
   exit(1);
}

ncdiminq(ncid, xdim, (char *)NULL, &xs);
ncdiminq(ncid, ydim, (char *)NULL, &ys);

rfc_xsize = xs;
rfc_ysize = ys;

if(rfc_xsize <= 0 || rfc_ysize <= 0)
{
   printf("error in ncdiminq -- program stopping\n");
   exit(1);
}

if(idur == 0) printf("rfc_xsize = %d  rfc_ysize = %d\n",rfc_xsize,rfc_ysize);
rfc_size = rfc_xsize * rfc_ysize;

/*------------------------------------------------*/
/*  read lat/lon of upper left corner of RFC area */
/*------------------------------------------------*/

ncattinq(ncid, NC_GLOBAL, "lat00", &l_type, &l_len);
ncattinq(ncid, NC_GLOBAL, "lon00", &l_type, &l_len);

plat00 = (float *) malloc(l_len * nctypelen(l_type));
plon00 = (float *) malloc(l_len * nctypelen(l_type));

ncattget(ncid, NC_GLOBAL, "lat00", plat00);
ncattget(ncid, NC_GLOBAL, "lon00", plon00);

lat00 = *plat00;
lon00 = *plon00;

if(idur == 0) printf("lat00 = %f  lon00 = %f\n",lat00,lon00);

/*----------------------------------------------------------------*/
/*  transform lat/lon of upper left corner of RFC area  to HRAP   */
/*  use llgd function from util_gen1 lib                          */
/*  (ixh,iyh) = HRAP coord of upper left (NW) corner of RFC area  */
/*----------------------------------------------------------------*/

lon00 = (-1) * lon00;
LLGD(&lon00,&lat00,&iflg,&xhrap,&yhrap,&iflg,&istat);

if(istat != 0)
{
   printf("error attempting to transform lat/lon to hrap - error code = %d\n",istat);
}
else
{
   ixh = xhrap;
   iyh = yhrap;
/*   printf("upper left (NW) corner of RFC area:  x = %d  y = %d\n",ixh,iyh); */
}

/*-----------------------------------------------------------------*/
/*  calculate HRAP coord of upper right (NE) corner of RFC area    */
/*  (xru,yru) = HRAP coord of upper right (NE) corner of RFC area  */
/*-----------------------------------------------------------------*/

xru  = ixh + rfc_xsize;
yru  = iyh;

printf("upper right (NE) corner of RFC area:  x = %d  y = %d\n",xru,yru);

/*------------------------------------------------------------------*/
/*  calculate HRAP coord of lower left  (SW) corner of RFC area     */
/*  (xrl,yrl) = HRAP coord of lower left  (SW) corner  of RFC area  */
/*------------------------------------------------------------------*/

xrl  = ixh;
yrl  = iyh - rfc_ysize;

printf("lower left (SW) corner of RFC area:  x = %d  y = %d\n",xrl,yrl);

/*------------------------------------------*/
/*  malloc space for FFG gridded array      */
/*------------------------------------------*/

rfc_ffg_uchar = (unsigned char *) malloc ((rfc_size)*sizeof(unsigned char));

if(!rfc_ffg_uchar)
{
    printf("malloc of space for rfc_ffg_uchar array failed -- ");
    printf("program stopping\n");
    exit(2);
}

/*---------------------------------------------------------*/
/*  read gridded FFG array from file                       */
/*  data values are unsigned char quantized from 0 - 253   */
/*  first value read is NW (upper left) corner of grid     */
/*---------------------------------------------------------*/

arrayid = ncvarid(ncid, "image");

status = nc_get_var_uchar(ncid, arrayid, rfc_ffg_uchar);
if(status != NC_NOERR)
{
   printf("error reading array from netcdf file -- program stopping\n");
   exit(3);
}

/*------------------------------------------------------------*/
/*  transform gridded FFG values from unsigned char to float  */
/*  map gridded FFG array elements into the mosiac array      */
/*    for both float and unsigned char versions of array      */
/*  unsigned char version will be written to a netCDF file    */
/*------------------------------------------------------------*/

/*-----------------------------------------------------------------*/
/*  xoffset < 0 occurs when western edge of site's area is west    */
/*     of western edge of RFC area                                 */
/*  for example, site = PBZ and RFC = MARFC                        */
/*                                                                 */
/*  yoffset < 0 occurs when northern edge of site's area is north  */
/*     of northern edge of RFC area                                */
/*  for example, site = ICT and RFC = ABRFC                        */
/*-----------------------------------------------------------------*/

xoffset = xor - xrl;
yoffset = yru - (yor + wfo_ysize);

for(iy=0; iy<wfo_ysize; iy++)
{

   yloc = (yor + wfo_ysize) - iy;
   yoff = yoffset + iy;

   for(ix=0; ix<wfo_xsize; ix++)
   {

      indwfo = (wfo_xsize * iy) + ix;
      indrfc = (rfc_xsize * yoff) + (ix + xoffset);

      if(yoff >= 0 && (yloc <= yru && yloc > yrl))
      {

         xloc = xor + ix;
         if(xloc <= xru && xloc >= xrl)   
         {

            indwfo = (wfo_xsize * iy) + ix;
            indrfc = (rfc_xsize * yoff) + (ix + xoffset);

            if(indrfc > rfc_size)
            {
               printf("array index (%d) exceeds rfc size (%d) -- program stopping\n",
                        indrfc, rfc_size);
               printf("ix = %d  iy = %d  \n",ix,iy);
               printf("yoff = %d  \n",yoff);
               printf("xoffset = %d  yoffset = %d  \n",xoffset,yoffset);
               exit(4);
            }

            /*---------------------------------------------------------------------------*/
            /*  need additional check for non-missing data because of overlapping areas  */
            /*   and use of bit-map to set all bins outsie of area to missing            */
            /*  0,0 point in mosaic_ffg_float array corresponds to SW corner of grid     */
            /*  0 element of mosaic_ffg_uchar array corresponds to NW corner of grid     */
            /*---------------------------------------------------------------------------*/

            if(rfc_ffg_uchar[indrfc] != fill_value_uchar)
            {

               mosaic_ffg_float[wfo_ysize - iy - 1][ix] = ffconv(rfc_ffg_uchar[indrfc]);

               mosaic_ffg_uchar[indwfo] = rfc_ffg_uchar[indrfc];

            }

         }

      }

   }
}

/*----------------------------------*/
/*  free array space for RFC array  */
/*----------------------------------*/

free(rfc_ffg_uchar);

/*--------------------------------*/
/*  close the netcdf file         */
/*--------------------------------*/

status = nc_close(ncid);
if(status != NC_NOERR)
{
   printf("error closing netcdf file\n");
   exit(4);
}

}
