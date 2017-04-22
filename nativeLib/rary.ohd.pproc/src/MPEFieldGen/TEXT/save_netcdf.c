#include <stdlib.h>
#include "read_xmrg.h"
#include "stage3.h"
#include "netcdf.h"

/******************************************************************/
/*   FUNCTION NAME:  save_netcdf()                                */
/*       FUNCTION:   creates netCDF files                         */
/*                   precip array is read from xmrg file          */
/*******************************************************************

Function type:
   void

Called by functions:
   writeQPE (mpe_fieldgen)
   save_rfcwide(MPE Editor)

Functions called:
    read_xmrg
    netCDF functions
*******************************************************************/

extern void read_xmrg ( const int * mx , const int * my , const int * irow ,
                 const char * fname , const int * length , int * ifile ,
                 short * mosaic ) ;

void MPEFieldGen_saveNetCDF( const int max_x ,
				const int  max_y,
				const char * fname,
				const char * filename_xmrg ,
				long int *irc)
{

 /* netCDF id */
 int  ncid;                  

 /* dimension ids */
 int  hrapy_dim, hrapx_dim;
 int  hrap_dimids[2];

 /* variable ids */
 int  amountofprecip_id;
 int  lat_id, lon_id;
 int  true_lat_id, true_lon_id;
 int  dateofdata_id, dateofcreation_id;
 int  hrap_xor_id, hrap_yor_id;
   
 
 /* variable shapes */
 int         datedims[1];
 int         latlondims[1];
 long        amountofprecip_edges[2];
 static long amountofprecip_start[] = {0,0};
 long        hrap_index[] = {0};
 long        latlon_start[] = {0};
 long        latlon_edge[] = {4};
 long        truelatlon[] = {0};
 long        dateedge[] = {0};
 long        dateedge2[] = {9};
 
 
 float       xor_hrap[1], yor_hrap[1];
 int         i,j,k,ierr, len, lenofstr, status;
 char        unixcmd[500], external_dir[100];
 char        name[128], netcdfloc[100], buf[20];
 char        fnm[30], fnmx[128];
 
 
 float         lats[4], lons[4], true_lat[1], true_lon[1];
 char          datenc[12], datenc2[12];
 date_struct   ncdate;
 struct tm     *t_local;
 time_t        tnow;

 short **temp;
 const int lenfnx = strlen(filename_xmrg);
 const int lenfn = strlen(fname);


 *irc = 0;

    /*-------------------------------*/
    /* malloc space for precip array */
    /*-------------------------------*/

    precip = (short int *)malloc((max_x * max_y)*sizeof(short int)); 
    if(!precip)
    {
       *irc = 1;
       return;
    }

    /*-------------------------------------------*/
    /* read xmrg file and populate precip array  */
    /*-------------------------------------------*/

    temp = (short **)malloc(max_y*sizeof(short *));
    for (i=0; i<max_y; i++)
      temp[i] = (short *)malloc(max_x*sizeof(short));

    memset(fnmx, '\0', 128);
    strncpy(fnmx, filename_xmrg, lenfnx);
    len = lenfnx;
 
    for (i=0; i<max_y; i++)
    {

      read_xmrg(&max_x,&max_y,&i,fnmx,&len,&ierr,temp[i]);
      if (ierr != 0)
      {
         *irc = 2;
         return;
      }
    }

    k=0;
    for (j=0;j<max_y;j++)
    for (i=0;i<max_x;i++)
    {
      precip[k] = temp[j][i];
      k++;
    }

    /*--------------------------------------*/
    /* read tokens from .Apps_defaults      */
    /*--------------------------------------*/

    len = strlen("mpe_qpe_netcdf_dir");
    get_apps_defaults("mpe_qpe_netcdf_dir", &len, external_dir, &len);

    len = strlen("st3_netcdf_loc");
    get_apps_defaults("st3_netcdf_loc",&len,netcdfloc,&lenofstr); 

    len = strlen("st3_netcdf_swlat");
    get_apps_defaults("st3_netcdf_swlat",&len,buf,&len);  
    lats[0] = atof(buf);
    len = strlen("st3_netcdf_swlon");
    get_apps_defaults("st3_netcdf_swlon",&len,buf,&len);  
    lons[0] = atof(buf);
    len = strlen("st3_netcdf_selat");
    get_apps_defaults("st3_netcdf_selat",&len,buf,&len);  
    lats[1] = atof(buf);
    len = strlen("st3_netcdf_selon");
    get_apps_defaults("st3_netcdf_selon",&len,buf,&len);  
    lons[1] = atof(buf); 
    len = strlen("st3_netcdf_nelat");
    get_apps_defaults("st3_netcdf_nelat",&len,buf,&len);  
    lats[2] = atof(buf);
    len = strlen("st3_netcdf_nelon");
    get_apps_defaults("st3_netcdf_nelon",&len,buf,&len);  
    lons[2] = atof(buf);
    len = strlen("st3_netcdf_nwlat");
    get_apps_defaults("st3_netcdf_nwlat",&len,buf,&len);  
    lats[3] = atof(buf);
    len = strlen("st3_netcdf_nwlon");
    get_apps_defaults("st3_netcdf_nwlon",&len,buf,&len);  
    lons[3] = atof(buf);
          
    memset(fnm, '\0', lenfn);
    strncpy(fnm, fname, lenfn);
 
    true_lat[0] = 60.0;
    true_lon[0] = 105.0;
    xor_hrap[0] = (float)XOR/1.0;
    yor_hrap[0] = (float)YOR/1.0;
    
   for(i=0;i<10;i++) cdate[i] = fnm[i];
   cdate[10] = 0;
       
   sprintf(datenc, "%sZ", cdate);
   
   time(&tnow);
   t_local = gmtime(&tnow);
   ncdate.hour = t_local->tm_hour ;
   ncdate.month = t_local->tm_mon + 1;
   ncdate.day = t_local->tm_mday;
   ncdate.year = t_local->tm_year + 1900;
   sprintf(datenc2, "%02d%02d%04d%02dZ", ncdate.month, ncdate.day, ncdate.year, ncdate.hour);
    
   sprintf(name, "%s/mpe.netcdf", external_dir);
   
   /*------------------------------*/
   /*  create netCDF file          */
   /*------------------------------*/

   status = nc_create(name, NC_CLOBBER, &ncid);
   if(status != NC_NOERR)
   {
      *irc = 3;
      return;
   }
   
   /* define dimensions */
   
   hrapx_dim = ncdimdef(ncid, "hrapx", (long)max_x);
   hrapy_dim = ncdimdef(ncid, "hrapy", (long)max_y);
   
   hrap_dimids[0] = hrapy_dim;
   hrap_dimids[1] = hrapx_dim;
   
      
   /* define variables */ 
   amountofprecip_id = ncvardef (ncid, "amountofprecip", NC_SHORT, 2, hrap_dimids);
   latlondims[0] = ncdimdef(ncid, "latlong", 4L);
   lat_id = ncvardef(ncid, "lat", NC_FLOAT, 1, latlondims);
   lon_id = ncvardef(ncid, "lon", NC_FLOAT, 1, latlondims);
   true_lat_id = ncvardef(ncid, "true_lat", NC_FLOAT, 0, hrap_dimids);
   true_lon_id = ncvardef(ncid, "true_lon", NC_FLOAT, 0, hrap_dimids);
   datedims[0] = ncdimdef(ncid, "dates", 11L);
   dateofdata_id = ncvardef(ncid, "timeofdata", NC_CHAR, 1, datedims); 
   dateofcreation_id =  ncvardef(ncid, "timeofcreation", NC_CHAR, 1, datedims); 
   hrap_xor_id = ncvardef(ncid, "hrap_xor", NC_FLOAT, 0, hrap_dimids);
   hrap_yor_id = ncvardef(ncid, "hrap_yor", NC_FLOAT, 0, hrap_dimids);
   
   ncattput (ncid, amountofprecip_id, "long_name", NC_CHAR, 20, (void *)"hourly precipitation");
   ncattput (ncid, amountofprecip_id, "units", NC_CHAR, 16, (void *)"hundredths of mm");
   ncattput (ncid, amountofprecip_id, "grid", NC_CHAR, 25, (void *)"hrap_grid=1/40th lfm grid");
   ncattput (ncid, amountofprecip_id, "resolution", NC_CHAR, 7, (void *)"4km*4km");
   ncattput (ncid, amountofprecip_id, "dateofdata", NC_CHAR, 11, (void *)datenc);
   ncattput (ncid, amountofprecip_id, "dateofcreation", NC_CHAR, 11, (void *)datenc2);
   ncattput (ncid, amountofprecip_id, "source", NC_CHAR, lenofstr, (void *)netcdfloc);
   ncattput (ncid, amountofprecip_id, "comments", NC_CHAR, 36, (void *)"preliminary data...subject to change");
   ncattput (ncid, hrap_xor_id, "comments", NC_CHAR, 34, (void *)"offset in x direction of hrap grid");
   ncattput (ncid, hrap_yor_id, "comments", NC_CHAR, 34, (void *)"offset in y direction of hrap grid");
   ncattput (ncid, lat_id, "order", NC_CHAR, 43, (void *)"bottom_left,bottom_right,top_right,top_left"); 
   ncattput (ncid, lon_id, "order", NC_CHAR, 43, (void *)"bottom_left,bottom_right,top_right,top_left"); 

   /* leave define mode */
   ncendef (ncid);

   amountofprecip_edges[0] = max_y;
   amountofprecip_edges[1] = max_x;

   ncvarput(ncid, dateofdata_id, dateedge,dateedge2, (void *)datenc);
   ncvarput(ncid, dateofcreation_id, dateedge, dateedge2, (void *)datenc2); 
   ncvarput1(ncid, true_lat_id, truelatlon, (void *)true_lat);
   ncvarput1(ncid, true_lon_id, truelatlon, (void *)true_lon); 
   ncvarput(ncid, lat_id, latlon_start, latlon_edge, (void *)lats);
   ncvarput(ncid, lon_id, latlon_start, latlon_edge, (void *)lons);
   ncvarput1(ncid, hrap_xor_id, hrap_index, (void *)xor_hrap);
   ncvarput1(ncid, hrap_yor_id, hrap_index, (void *)yor_hrap);
   ncvarput(ncid, amountofprecip_id, amountofprecip_start, amountofprecip_edges, (void *) precip);
   ncclose (ncid);
 
   free (precip);

   sprintf(unixcmd, "cd %s\nmv mpe.netcdf %s\n",external_dir,fnm);
   system(unixcmd);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/save_netcdf.c,v $";
 static char rcs_id2[] = "$Id: save_netcdf.c,v 1.1 2007/10/15 12:19:16 dsa Exp $";}
/*  ===================================================  */

}
