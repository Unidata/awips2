#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

float get_lisohyet(float lat,float lon,int mon)

{

extern struct isoh *isoh;
int kk,jj,ix,iy;
double distance,value,dist1,dist2,dist,lat1,lon1;
float isohyet;
float conv=.0174;
 
iy=((isoh->max_lat-lat)/isoh->delta_lat+.00001);
ix=((isoh->max_lon-lon)/isoh->delta_lon+.00001);

if(ix < 0 || iy < 0 || ix >= isoh->maxi-1 || iy>= isoh->maxj-1) 
                            return(-1.0);

distance=0.0;
value=0.0;

for(kk=ix;kk<=ix+1;kk++) {
      
for(jj=iy;jj<=iy+1;jj++) {

           lat1=isoh->max_lat-isoh->delta_lat*jj;
 
           lon1=isoh->max_lon-isoh->delta_lon*kk;
           
           dist1=(lon1-lon)*cos((lat1+lat)/2*conv);

           dist2=lat-lat1;

           dist=pow(dist1,2)+pow(dist2,2);

           if(dist < 0.00001)
                      dist=.00001;

           dist=1/dist;

           if(isoh->value[mon][kk][jj] <= 0)
                                      continue;          
           
           value=value+dist*(float)isoh->value[mon][kk][jj];
           distance=distance+dist;

	     }
             }
 
if(distance==0)
        return(-1.0);
           
isohyet=value/distance;

return(isohyet);


}







