#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

float get_lmaxmin(float lat,float lon,int mon,int type)

{

extern struct maxmin *maxmin;
int kk,jj,ix,iy;
double distance,value,dist1,dist2,dist,lat1,lon1;
float mxn;
float conv=.0174;

if(maxmin==NULL)
              return(-9999);

iy=((maxmin->max_lat-lat)/maxmin->delta_lat+.00001);
ix=((maxmin->max_lon-lon)/maxmin->delta_lon+.00001);

if(ix < 0 || iy < 0 || ix >= maxmin->maxi-1 || iy>= maxmin->maxj-1) 
                            return(-9999.0);

distance=0.0;
value=0.0;

for(kk=ix;kk<=ix+1;kk++) {
      
for(jj=iy;jj<=iy+1;jj++) {

           lat1=maxmin->max_lat-maxmin->delta_lat*jj;
 
           lon1=maxmin->max_lon-maxmin->delta_lon*kk;
           
           dist1=(lon1-lon)*cos((lat1+lat)/2*conv);

           dist2=lat-lat1;

           dist=pow(dist1,2)+pow(dist2,2);

           if(dist < 0.00001)
                      dist=.00001;

           dist=1/dist;

           if(type==0 && maxmin->maxvalue[mon][kk][jj] <= -50)
                                      continue;          
           
           if(type==1 && maxmin->minvalue[mon][kk][jj] <= -50)
                                      continue;          
           
           if(type==0)
           value=value+dist*(float)maxmin->maxvalue[mon][kk][jj];
	   
           if(type==1)
           value=value+dist*(float)maxmin->minvalue[mon][kk][jj];

           distance=distance+dist;

	     }
             }
 
if(distance==0)
        return(-9999.0);
           
mxn=value/distance;

return(mxn);


}







