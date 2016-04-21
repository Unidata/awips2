#include <stdlib.h>

#include "empe_fieldgen.h"
#include "netcdf.h"
#include "stage3.h"

/******************************************************************/
/*   FUNCTION NAME:  save_netcdf()                                */
/*       FUNCTION:   creates netCDF files                         */
/*******************************************************************

Function type:
   void

Called by functions:
   write_formatted_xmrg

Functions called:
    netCDF functions
*******************************************************************/

void saveNetCDF( const int max_x ,
                 const int  max_y,
                 const char * strDateTime ,
                 const char * fname,
                 double ** pMosaic ,
                 const char * proc_flag ,
                 const char * external_dir ,
                 long int *irc)
{

    /* netCDF id */
    int  ncid;                  

    /* dimension ids */
    int  hrapy_dim, hrapx_dim;
    int  hrap_dimids[2];

    /* variable ids */
    int  process_id;

    int  lat_id, lon_id;
    int  true_lat_id, true_lon_id;
    int  dateofdata_id, dateofcreation_id;
    int  hrap_xor_id, hrap_yor_id;

    /* variable shapes */
    int         datedims[1];
    int         latlondims[1];
    long        process_edges[2];
    static long process_start[] = {0,0};
    long        hrap_index[] = {0};
    long        latlon_start[] = {0};
    long        latlon_edge[] = {4};
    long        truelatlon[] = {0};
    long        dateedge[] = {0};
    long        dateedge2[] = {9};

    float       xor_hrap[1], yor_hrap[1];
    int         i,j,k,len, lenofstr, status;
    char        unixcmd[500] = {'\0'};
    char        proc_name[128] = {'\0'};
    char        name[128] = {'\0'};
    char        long_name[128] = {'\0'};
    char        units[128] = {'\0'} ;
    char        grid_label[128] = {'\0'};
    char        res_label[128] = {'\0'};
    char        netcdfloc[100] = {'\0'};
    char        buf[20] = {'\0'};
    char        fnm[30] = {'\0'};

    float         lats[4], lons[4], true_lat[1], true_lon[1];
    char          datenc[ANSI_YEARSEC_TIME_LEN + 2] = {'\0'};
    char          datenc2[ANSI_YEARSEC_TIME_LEN + 2] = {'\0'};
    struct tm     *t_local;
    time_t        tnow;

    const int lenfn = strlen(fname);

    *irc = 0;

    /*-------------------------------*/
    /* malloc space for precip array */
    /* and fill data into the array  */
    /*-------------------------------*/

    precip = (short int *)malloc((max_x * max_y)*sizeof(short int)); 
    if(!precip)
    {
       *irc = 1;
       return;
    }

    k=0;
    for (j = 0; j < max_y; j++)
    {
        for (i = 0; i < max_x; i++)    
        {
            precip[k] = (short)pMosaic[j][i];
            k++;
        }
    }

    /*--------------------------------------*/
    /* read tokens from .Apps_defaults      */
    /*--------------------------------------*/

    if (strstr(proc_flag,"DHR"))
    {
       strcpy(proc_name,"preciprate");
       strcpy(long_name,"precipitation rate");
       strcpy(units,"mm/hr");
       strcpy(grid_label,"1/4 hrap grid");
       strcpy(res_label,"1km*1km");
    }
    else if (strstr(proc_flag,"DSP"))
    {
       strcpy(proc_name,"stormtotalprecip");
       strcpy(long_name,"storm total precipitation");
       strcpy(units,"mm");
       strcpy(grid_label,"1/4 hrap grid");
       strcpy(res_label,"1km*1km");
    }
    else
    {
       strcpy(proc_name,"amountofprecip");
       strcpy(long_name,"hourly precipitation");
       strcpy(units,"hundredths of mm");
       strcpy(grid_label,"hrap_grid=1/40th lfm grid");
       strcpy(res_label,"4km*4km");
       
    }   

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
          
    strncpy(fnm, fname, lenfn);
 
    true_lat[0] = 60.0;
    true_lon[0] = 105.0;
    xor_hrap[0] = (float)XOR/1.0;
    yor_hrap[0] = (float)YOR/1.0;
    
   sprintf(datenc, "%sZ", strDateTime);
   
   time(&tnow);
   t_local = gmtime(&tnow);
   sprintf(datenc2, "%02d%02d%04d%02d%02d%02dZ",
                     t_local->tm_mon + 1,
                     t_local->tm_mday,
                     t_local->tm_year + 1900,
                     t_local->tm_hour,
                     t_local->tm_min,
                     t_local->tm_sec);
    
   sprintf(name, "%s/hpe.netcdf", external_dir);
   
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
   process_id = ncvardef (ncid, proc_name, NC_SHORT, 2, hrap_dimids);
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
   
   ncattput (ncid, process_id, "long_name", NC_CHAR, 20, (void *)long_name);
   ncattput (ncid, process_id, "units", NC_CHAR, 16, (void *)units);
   ncattput (ncid, process_id, "grid", NC_CHAR, 25, (void *)grid_label);
   ncattput (ncid, process_id, "resolution", NC_CHAR, 7, (void *)res_label);
   ncattput (ncid, process_id, "dateofdata", NC_CHAR, 13, (void *)datenc);
   ncattput (ncid, process_id, "dateofcreation", NC_CHAR, 15, (void *)datenc2);
   ncattput (ncid, process_id, "source", NC_CHAR, lenofstr, (void *)netcdfloc);
   ncattput (ncid, process_id, "comments", NC_CHAR, 36, (void *)"preliminary data...subject to change");
   ncattput (ncid, hrap_xor_id, "comments", NC_CHAR, 34, (void *)"offset in x direction of hrap grid");
   ncattput (ncid, hrap_yor_id, "comments", NC_CHAR, 34, (void *)"offset in y direction of hrap grid");
   ncattput (ncid, lat_id, "order", NC_CHAR, 43, (void *)"bottom_left,bottom_right,top_right,top_left"); 
   ncattput (ncid, lon_id, "order", NC_CHAR, 43, (void *)"bottom_left,bottom_right,top_right,top_left"); 

   /* leave define mode */
   ncendef (ncid);

   process_edges[0] = max_y;
   process_edges[1] = max_x;

   ncvarput(ncid, dateofdata_id, dateedge,dateedge2, (void *)datenc);
   ncvarput(ncid, dateofcreation_id, dateedge, dateedge2, (void *)datenc2); 
   ncvarput1(ncid, true_lat_id, truelatlon, (void *)true_lat);
   ncvarput1(ncid, true_lon_id, truelatlon, (void *)true_lon); 
   ncvarput(ncid, lat_id, latlon_start, latlon_edge, (void *)lats);
   ncvarput(ncid, lon_id, latlon_start, latlon_edge, (void *)lons);
   ncvarput1(ncid, hrap_xor_id, hrap_index, (void *)xor_hrap);
   ncvarput1(ncid, hrap_yor_id, hrap_index, (void *)yor_hrap);
   ncvarput(ncid, process_id, process_start, process_edges, (void *) precip);
   ncclose (ncid);
 
   free (precip);

   sprintf(unixcmd, "cd %s\nmv hpe.netcdf %s\n",external_dir,fnm);
   system(unixcmd);

   sprintf ( message , "STATUS: file written to: %s/%s" , 
                       external_dir, fnm );
   printLogMessage( message );

}
