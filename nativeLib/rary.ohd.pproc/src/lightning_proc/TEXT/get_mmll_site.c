#include "light_data.h"
#include "llgd.h"
#include "stage3.h"
#include "stage3_globals.h"

/*******************************************************************
  Based on XOR, YOR, MAXX, MAXY compute the max. and min. 
  lat. & lon. at a specific site (RFC/WFO) region.
*******************************************************************/
  
/*****************************************************************/   		  
void get_mmll_site()
/*********************************************************************/		  
{
   float x,y,flon,flat;
   int nbpts, illgd,istat;
   
   FILE  *file;
   int    len;
   char   pdir[128],infile1[128];
   char   RFC[9];
      
   nbpts = 1;
   illgd = 0;

/*--------------------------------------------------------------------*/
/*     Read XOR, YOR, MAXX, MAXY from geo_data directory              */     
/*--------------------------------------------------------------------*/

/*  create directory name for geographic data  */

   len = strlen("geo_data");
   get_apps_defaults("geo_data",&len,pdir,&len);
   
   len = strlen("st3_rfc");
   get_apps_defaults("st3_rfc",&len,RFC,&len);

   sprintf(infile1,"%s/%s/ascii/coord_%s.dat",pdir,RFC,RFC);

 /*
    read coordinates of rectangle surrounding RFC area
    coordinates are on national HRAP grid
 */

   if((file = fopen(infile1,"r")) == NULL)
   {
     printf("ERROR: coord_%s.dat file not found -",RFC);
     printf(" program stopping \n");
     exit(0);
   }

   fscanf(file,"%d",&XOR);
   fscanf(file,"%d",&YOR);
   fscanf(file,"%d",&MAXX);
   fscanf(file,"%d",&MAXY);

   fclose(file);

   printf("XOR=%d    YOR=%d    MAXX=%d    MAXY=%d\n",XOR,YOR,MAXX,MAXY);

   llx = XOR;
   lly = YOR;
   nx = MAXX;
   ny = MAXY;

   min_lat = 90.0;
   min_lon = 180.0;
   max_lat = 0.0;
   max_lon = 0.0;   

/* Convert HRAP gridpoint to latitude and longitude */                
   x =(float)llx;
   y =(float)lly;
   
   LLGD(&flon,&flat,&nbpts,&x,&y,&illgd,&istat);   
   
   if(flat < min_lat) min_lat = flat;
   if(flat > max_lat) max_lat = flat;   
   if(flon < min_lon) min_lon = flon;    
   if(flon > max_lon) max_lon = flon;   

   x = (float)(llx + nx);
   y = (float)(lly);

   LLGD(&flon,&flat,&nbpts,&x,&y,&illgd,&istat);   
   
   if(flat < min_lat) min_lat = flat;
   if(flat > max_lat) max_lat = flat;   
   if(flon < min_lon) min_lon = flon;    
   if(flon > max_lon) max_lon = flon;   

   x = (float)(llx);
   y = (float)(lly + ny);

   LLGD(&flon,&flat,&nbpts,&x,&y,&illgd,&istat);   
   
   if(flat < min_lat) min_lat = flat;
   if(flat > max_lat) max_lat = flat;   
   if(flon < min_lon) min_lon = flon;    
   if(flon > max_lon) max_lon = flon;   
       
   x = (float)(llx + nx);
   y = (float)(lly + ny);

   LLGD(&flon,&flat,&nbpts,&x,&y,&illgd,&istat);   
   
   if(flat < min_lat) min_lat = flat;
   if(flat > max_lat) max_lat = flat;   
   if(flon < min_lon) min_lon = flon;    
   if(flon > max_lon) max_lon = flon;   
   
   max_lat = max_lat + 1.0;
   max_lon = max_lon + 1.0;
   min_lat = min_lat - 1.0;
   min_lon = min_lon - 1.0;
   
   printf("Minimum lat and lon are:  %8.4f    %8.4f\n",min_lat,min_lon);   
   printf("Maximum lat and lon are:  %8.4f    %8.4f\n",max_lat,max_lon);
}

