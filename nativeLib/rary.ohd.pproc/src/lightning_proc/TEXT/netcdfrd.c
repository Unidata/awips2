#include "netcdf.h" 
#include "light_data.h"

/*  
    Read lightning data from netcdf file and insert 
    them into "light" array.                
*/

/*******************************************************/
int ntcdfrd ( char *fn)
/******************************************************/
{
   int cdfid;
   int ndims;
   int nvars;
   int ngatts;
   int recdim;
   int strikeNum_id;
   long minute_id;
   long n_strikeNum;
   long n_minute;
   float lat;
   float lon;
   double time;
   float sigStr;
   long mult;
   int lat_vid;
   int lon_vid;
   int time_vid;
   int sigStr_vid;
   int mult_vid;

   long start[1];
   long count[1];
   
   int r;
   int rr=0;
 
   /*   print version number */

   printf( "\n%s %s - %s\n", lightproc_name, lightproc_ver, lightproc_date );

   /*   open the Netcdf file*/
   cdfid=ncopen(fn,NC_NOWRITE);
   if (cdfid==-1)
   {
      printf("Could not open the netcdf file: %s\n", fn);
      exit(1);
   }
   else
   {
      printf ("Netcdf file %s was opened successfully.\n",fn);
   }
    
   /* Inquire about the Netcdf file: No.of dimensions, No.of variables,
                                   No. of global attributes etc.*/
				   
   ncinquire (cdfid, &ndims, &nvars, &ngatts, &recdim);

   printf ("Number of dimensions for this netcdf file is: %d\n",ndims);
   printf ("Number of variables for this netcdf file is: %d\n",nvars);
   printf ("Number of global attributes for this netcdf file is: %d\n",ngatts);
   printf ("ID of unlimited dimension for this netcdf file is: %d\n",recdim);

   /*  Get the dimension ids */

   strikeNum_id=ncdimid(cdfid,"strikeNum");
   ncdiminq(cdfid,strikeNum_id,(char *) 0, &n_strikeNum);
   printf("Number of lightning strikes within US is: %ld\n", n_strikeNum); 
   minute_id=ncdimid(cdfid,"minute");
   ncdiminq(cdfid,minute_id, (char *) 0, &n_minute);

   /*  Get the variable ids */

   lat_vid=ncvarid(cdfid,"lat");
   lon_vid=ncvarid(cdfid,"lon");
   time_vid=ncvarid(cdfid,"time");
   sigStr_vid=ncvarid(cdfid,"sigStr");
   mult_vid=ncvarid(cdfid,"mult");
    
   /* print the variable ids */
/*
   printf("lat_vid: %d\n",lat_vid);
   printf("lon_vid: %d\n",lon_vid);
   printf("time_vid: %d\n",time_vid);
   printf("sigStr_vid: %d\n",sigStr_vid);
   printf("mult_vid: %d\n",mult_vid);
*/
   
   for (r=0; r<n_strikeNum; r++)
   {
      start[0]=r;
      count[0]=1;
      ncvarget(cdfid,lat_vid,start,count,&lat);
      ncvarget(cdfid,lon_vid,start,count,&lon);
      ncvarget(cdfid,time_vid,start,count,&time);
      ncvarget(cdfid,sigStr_vid,start,count,&sigStr);
      ncvarget(cdfid,mult_vid,start,count,&mult);
   
      light[rr].value=sigStr;
      light[rr].lat=lat;
      light[rr].lon=lon;
      light[rr].time=time;

      rr++;
   }
   
   return rr;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
