#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

void get_lat_lon(int newx,int newy,float *lat,float *lon)
{

extern struct dval dval;
double conv=.0174,c,lonf;
double t1,t2,t3,t4,t5,lat1,lat2;

if(newx==6400)
         newx=6401;

t1=.0001;

lonf= atan(((double)newy-dval.yo)/(t1))/conv-90.;

t1=(double)newx-dval.xo;

*lon= atan(((double)newy-dval.yo)/(t1))/conv;

if(*lon > 0)
          *lon=-180+*lon-2*lonf;

*lon=*lon+90+dval.lo;

c=dval.a*cos((*lon-dval.lo-90)*conv)/((double)newx-dval.xo);
c=pow(c,2);

t1=1/(1+c);
t2=1-c;
t3=pow(2*t1,2);
t4=4*t2*t1;
t5=pow(t3-t4,.5)/2;

if(fabs(-t1+t5) > 1.0)
          lat1=-9999;
else
          lat1=asin(-t1+t5)/conv;

if(fabs(-t1-t5) > 1.0)
          lat2=-9999;
else
          lat2=asin(-t1-t5)/conv;

if(lat1 > 0 && lat1 < 90)
                 *lat=lat1;

else
                 *lat=lat2;

return;

}







