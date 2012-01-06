#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vis_assign.h"

int use_goodness = 0;

void use_goodness_values(int ug)
{
    use_goodness = ug;
}

void vis_assign(float lats[], float lons[], int goodness[],
           float dist[], int ns)
{

   float * xx, * yy, * zz;
   float * gfac;
   unsigned char * assigned;
   int i;
   float dtr,lt,ln;
   float dx,dy,dz;
   float d,dd;
   int na;
   int mingood,maxgood;
   int minspec,maxspec;
   float xmin,xmax,ymin,ymax,zmin,zmax;
   float floor;
   float w1,w2;
   int npass;
   int best;
   float disfac,bestdf,bestdis;

   if (ns<=0) return;

/* printf("ns %d\n",ns); */

   xx = (float *) malloc(17*ns);
   yy = xx+ns;
   zz = yy+ns;
   gfac = zz+ns;
   assigned = (unsigned char *)(gfac+ns);

/* printf("memory allocated %d\n",xx); */

   dtr = atan(1.0)/45.0;
   mingood = 0x6FFFFFFF;
   maxgood = -0x6FFFFFFF;
   minspec = 0x7FFFFFFF;
   maxspec = 0x6FFFFFFF;
   xmin = ymin = zmin = 1.0;
   xmax = ymax = zmax = -1.0;
   na = 0;
   for (i=0; i<ns; i++) {
      lt = dtr*lats[i];
      ln = dtr*lons[i];
      xx[i] = cos(lt)*cos(ln);
      yy[i] = cos(lt)*sin(ln);
      zz[i] = sin(lt);
      if (use_goodness && goodness[i]>0 && goodness[i]<=22222) {
          assigned[i] = 1;
          dist[i] = 2*sin(goodness[i]/12740.0);
          goodness[i] += 0x6FFFFFFF;
          na++;
      } else {
          assigned[i] = 0;
          dist[i] = 2.0;
      }/*endif*/
      if (xx[i]>xmax) xmax = xx[i];
      if (xx[i]<xmin) xmin = xx[i];
      if (yy[i]>ymax) ymax = yy[i];
      if (yy[i]<ymin) ymin = yy[i];
      if (zz[i]>zmax) zmax = zz[i];
      if (zz[i]<zmin) zmin = zz[i];
      if (goodness[i]>=0x6FFFFFFF) {
          if (goodness[i]>maxspec) maxspec = goodness[i];
          if (goodness[i]<minspec) minspec = goodness[i];
      } else {
          if (goodness[i]>maxgood) maxgood = goodness[i];
          if (goodness[i]<mingood) mingood = goodness[i];
      }/*endif*/
   }/*end for*/

/* printf("mingood,maxgood %d %d\n",mingood,maxgood);
   printf("minspec,maxspec %d %d\n",minspec,maxspec);
   printf("xmin,xmax %f %f\n",xmin,xmax);
   printf("ymin,ymax %f %f\n",ymin,ymax);
   printf("zmin,zmax %f %f\n",zmin,zmax); */

   dx = xmax-xmin;
   dy = ymax-ymin;
   dz = zmax-zmin;
   floor = dx;
   if (dy>floor) floor = dy;
   if (dz>floor) floor = dz;
   floor /= 4;
   npass = 0;

   for (i=0; i<ns; i++)
      if (assigned[i])
          continue;
      else if (goodness[i]>=0x6FFFFFFF) {
          if (maxspec==minspec)
              w1 = 0.5;
          else
              w1 = (goodness[i]-minspec)/(maxspec-minspec);
          w2 = 1.0-w1;
          gfac[i] = exp(log(4.0)*w1+log(2.0)*w2);
      } else {
          if (maxgood==mingood)
              w1 = 0.5;
          else
              w1 = (goodness[i]-mingood)/(maxgood-mingood);
          w2 = 1.0-w1;
          gfac[i] = exp(log(1.0)*w1+log(0.1)*w2);
      }/*endif*/

   for (npass=0; na<ns ; npass++,floor/=2) {
      if (npass==10) floor = -1.0;

   /* printf("floor %f\n",floor); */

      while (na<ns) {

          best = -1;
          bestdf = floor;
          for (i=0; i<ns; i++) {
             if (assigned[i]) continue;
           /*if (dist[i]<floor) continue;
             disfac=dist[i]*gfac[i];*/
             disfac=pow(dist[i],3)*gfac[i];
             if (disfac<bestdf) continue;
             best = i;
             bestdf = disfac;
             bestdis = dist[i];
          }/*end for i*/
          if (best<0) break;

          for (i=0; i<ns; i++) {
             if (!assigned[i]) continue;
             dd = dist[best];
             if (dist[i]<dd) dd = dist[i];
             dx = xx[i]-xx[best];
             if (dx>dd || -dx>dd) continue;
             dy = yy[i]-yy[best];
             if (dy>dd || -dy>dd) continue;
             dz = zz[i]-zz[best];
             if (dz>dd || -dz>dd) continue;
             d = sqrt(dx*dx+dy*dy+dz*dz);
             if (d<dd) dist[best] = d;
          }/*end for i*/

          assigned[best] = 1;
          na++;

          for (i=0; i<ns; i++) {
             if (assigned[i]) continue;
             dd = dist[best];
             if (dist[i]<dd) dd = dist[i];
             dx = xx[i]-xx[best];
             if (dx>dd || -dx>dd) continue;
             dy = yy[i]-yy[best];
             if (dy>dd || -dy>dd) continue;
             dz = zz[i]-zz[best];
             if (dz>dd || -dz>dd) continue;
             d = sqrt(dx*dx+dy*dy+dz*dz);
             if (d<dd) dist[i] = d;
          }/*end for i*/

      }/*end while*/

   }/*end for npass*/

   free(xx);

   for (i=0; i<ns; i++)
      dist[i] = 6370*2*asin(dist[i]/2);

} /* end vis_assign */
