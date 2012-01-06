#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "va_advanced.h"

#define BREAKAXIS 5
#define BREAKALL 25
#define BREAKAXIST 8
#define BREAKALLT 64

static float aspect = 0;

static int min_literal = 1;
static int max_literal = 0;

static int dist_pass = 0;
static int recomp = 0;

static float * xx, * yy, * zz;
static unsigned char * assigned;
static float * lats, * lons;
static float * dist;
static float * gooddist;
static int * goodness;
static double * goodcomp;

static float weighting = 0.0;
static int just_goodness = 0;

static float dtr;

/* This routine allows the client to place more importance on either user   */
/* preferences or spatial uniformity in determining the visibility          */
/* thresholds.  0 means only consider user preferences, 1 means only        */
/* consider spatial uniformity.  0 is the default, values between 0 and 1   */
/* are meaningful.                                                          */
void va_weighting(float wgt)
{
    weighting = wgt;
}

/* Normally, when selecting which station to include next, two things are   */
/* considered.  First, spatial uniformity, which means use the station      */
/* furthest from any station currently included.  Second, user preferences  */
/* which are usually expressed in terms of the distance to the nearest      */
/* other station with at least as great a goodness value.  Expressing both  */
/* in terms of a distance means that these two considerations can easily be */
/* blended.  This routine allows one to just consider the goodness value as */
/* is for determining the next station to include.                          */
void va_just_goodness(int yes)
{
    just_goodness = yes;
}


/* When this value is set to greater than one, it is assumed that the items */
/* being plotted using this progressive strategy are this factor wider than */
/* they are tall.                                                           */
void va_aspect(float a)
{
    aspect = a;
}

/* Goodness values between these two numbers are taken literally as         */
/* progressive disclosure distances.  By default no goodness values will    */
/* be treated this way.                                                     */
void va_literal(int mn, int mx)
{
    min_literal = mn;
    max_literal = mx;
}

/* If this is set to true, then valid progressive disclosure distances in    */
/* the array input_dist will be preserved.                                   */
void va_dist_pass(int dp)
{
    dist_pass = dp;
}

/* If this is set to true, then valid progressive disclosure distances in    */
/* the array input_dist will be converted to goodness values.                */
void va_recomp(int rc)
{
    recomp = rc;
}

/* The job of this routine is to convert goodness values into a precursor   */
/* to progressive disclosure distances, namely the distance to the nearest  */
/* other station which has at least as great a goodness value.              */
static int goodness_to_dist(float minlat, float maxlat,
                            float minlon, float maxlon,
                            int * active, int na)
{
   float dis1;
   int best1;
   int * subactive[BREAKALL];
   int ns[BREAKALL];
   float mnlts[BREAKALL], mxlts[BREAKALL];
   float mnlns[BREAKALL], mxlns[BREAKALL];
   int **subim, **subjm, **submm;
   int *nsim, *nsjm, *nsmm;
   int * topactive;
   int nt;
   int i,j,k,kk,a,g,gg;
   float dlt,dln;
   float mnlt,mxlt,mnln,mxln;
   float dx,dy,dz,d,dd;

   if (na<=0) return -1;

   if (na>BREAKALL*2) {

       /* We still have too many stations in this sub area for this
          algorithm to work efficiently, divide this working area
          further into subareas. */
       nt = 0;
       topactive = (int *)malloc(4*BREAKALL);
       dlt = (maxlat-minlat)/(1+BREAKAXIS);
       dln = (maxlon-minlon)/(1+BREAKAXIS);
       mnlt = minlat;
       mxlt = mnlt+2*dlt;
       for (k=0,j=0; j<BREAKAXIS; j++) {
           mnln = minlon;
           mxln = mnln+2*dln;
           for (i=0; i<BREAKAXIS; i++,k++) {
               subactive[k] = (int *)malloc(4*na);
               ns[k] = 0;
               mnlns[k] = mnln;
               mxlns[k] = mxln;
               mnlts[k] = mnlt;
               mxlts[k] = mxlt;
               mnln += dln;
               mxln += dln;
           }/*end for i*/
           mnlt += dlt;
           mxlt += dlt;
       }/*end for j*/

       subim = subactive-1;
       subjm = subactive-BREAKAXIS;
       submm = subjm-1;
       nsim = ns-1;
       nsjm = ns-BREAKAXIS;
       nsmm = nsjm-1;

       /* Assign each station to its proper sub area. */
       for (a=0; a<na; a++) {
           g = active[a];
           i = (int)((lons[g]-minlon)/dln);
           if (i<0) i = 0;
           if (i>BREAKAXIS) i = BREAKAXIS;
           j = (int)((lats[g]-minlat)/dlt);
           if (j<0) j = 0;
           if (j>BREAKAXIS) j = BREAKAXIS;
           k = j*BREAKAXIS+i;
           if (i<=0) {
               if (j>0)
                   subjm[k][nsjm[k]++] = g;
               if (j<BREAKAXIS)
                   subactive[k][ns[k]++] = g;
           } else if (i>=BREAKAXIS) {
               if (j>0)
                   submm[k][nsmm[k]++] = g;
               if (j<BREAKAXIS)
                   subim[k][nsim[k]++] = g;
           } else {
               if (j>0) {
                   subjm[k][nsjm[k]++] = g;
                   submm[k][nsmm[k]++] = g;
               }/*endif*/
               if (j<BREAKAXIS) {
                   subactive[k][ns[k]++] = g;
                   subim[k][nsim[k]++] = g;
               }/*endif*/
           }/*endif*/
       }/*end for a*/

       /* Do the goodness value to goodness distance conversions separately
          for the stations in each sub area. Accumulate the indices of the
          station with the highest goodness distance from each sub area. */
       for (k=0; k<BREAKALL; k++) {
           kk = goodness_to_dist(mnlts[k],mxlts[k],mnlns[k],mxlns[k],
                                 subactive[k],ns[k]);
           if (kk>=0) topactive[nt++] = kk;
           free(subactive[k]);
       }/*end for gg*/

   } else {

       nt = na;
       topactive = active;

   }/*endif*/

   best1 = -1;
   dis1 = -1;

   /* We are finding the distance to the nearest 
      station that is at least as desirable. */
   for (k=0; k<nt-1; k++) {
        for (kk=k+1; kk<nt; kk++) {
           g = topactive[k];
           gg = topactive[kk];
           if (g==gg) continue;
           if (goodcomp[g]<goodcomp[gg])
               dd = gooddist[g];
           else if (goodcomp[gg]<goodcomp[g])
               dd = gooddist[gg];
           else if (gooddist[g]>gooddist[gg])
               dd = gooddist[g];
           else
               dd = gooddist[gg];
           dx = xx[g]-xx[gg];
           if (dx>dd || -dx>dd) continue;
           dy = yy[g]-yy[gg];
           if (dy>dd || -dy>dd) continue;
           dz = zz[g]-zz[gg];
           if (dz>dd || -dz>dd) continue;
           d = sqrt(dx*dx+dy*dy+dz*dz);
           if (d>dd) continue;
           if (goodcomp[g]<=goodcomp[gg] && d<gooddist[g])
               gooddist[g] = d;
           if (goodcomp[gg]<=goodcomp[g] && d<gooddist[gg])
               gooddist[gg] = d;
       }/*end for kk*/
   }/*end for k*/

   /* Report station with the furthest distance to the station
      that is at least as desirable. */
   for (k=0; k<nt; k++) {
       g = topactive[k];
       if (gooddist[g]<dis1) continue;
       dis1 = gooddist[g];
       best1 = g;
   }/*end for*/

   if (active!=topactive) free(topactive);
   return best1;
}

/* This routine performs the primary function of this module for the client. */
/* The basic output of this module is a set of progressive disclosure        */
/* distances in the array `input_dist'.  The basic input, beyond the         */
/* lat/lon locations of the points, is the goodness values in                */
/* `input_goodness', which are relative client preferences as to which       */
/* station should show up first when zooming.  The goodness values are only  */
/* hints; the routine makes the final determination of the progressive       */
/* disclosure distances based primarily on two rules.  The first rule is     */
/* that once a point is revealed by zooming, further zooming will not cause  */
/* it to dissappear.  The second rule is that stations do not overlap each   */
/* other when plotted, which of course it the main reason for the existence  */
/* of this routine.  The progressive disclosure distances are the distance   */
/* in kilometers to the nearest other station which is at least as visible.  */
/* In order to apply these distances to a progressive disclosure strategy,   */
/* the client needs knowledge of the the physical size of the objects being  */
/* plotted on the screen.                                                    */
void va_advanced(float * input_lats, float * input_lons, int * input_goodness,
                 float * input_dist, int input_ns)
{

   int i,g,gg,k,kk,na;
   int ns;
   float lt,ln;
   float dx,dy,dz,d,dd;
   double d2,dd2;
   float minlat,minlon,maxlat,maxlon;
   int * active;
   int * unassigned;
   double big;
   float xxa,yya,zza;
   float xxn,yyn,zzn;
   float xxe,yye;
   float myaspect;

   /* No points, give up. */
   if (input_ns<=0) return;

   /* set internal variables for the input arguments, allocate any memory
      we will need. */
   ns = input_ns;
   lats = input_lats;
   lons = input_lons;
   goodness = input_goodness;
   dist = input_dist;
   xx = (float *) malloc(4*ns);
   yy = (float *) malloc(4*ns);
   zz = (float *) malloc(4*ns);
   gooddist = (float *) malloc(4*ns);
   assigned = (unsigned char *) malloc(ns);
   active = (int *) malloc(4*ns);
   i = 0x7FFFFFFF;
   big = (double)i;

   /* goodcomp is double because it must have the precision of a float and
      the range of integral values of an int.  It is workspace within
      which we convert goodness values to goodness distances. */
   goodcomp = (double *) malloc(8*ns);

   /* Here we calculate a unit vector on the earths surface which is used
      to make quick relative comparisons of point to point distances.
      Here we also figure out the entire range of lats and lons in the
      data set and we handle whether input valid progressive disclosure
      distances are preserved, ignored, or turned into goodness values.
      Stations with no initial progressive disclosure distance get a
      relative distance of 2.0, which when converted to a real distance
      is halfway round the globe. */
   minlat = maxlat = lats[0];
   minlon = maxlon = lons[0];
   xxa = yya = zza = 0.0;
   dtr = atan(1.0)/45.0;
   for (i=0; i<ns; i++) {
      active[i] = i;
      lt = dtr*lats[i];
      ln = dtr*lons[i];
      xx[i] = cos(lt)*cos(ln);
      yy[i] = cos(lt)*sin(ln);
      zz[i] = sin(lt);
      xxa += xx[i];
      yya += yy[i];
      zza += zz[i];
      if ((dist_pass || recomp) && dist[i]>=0) {
          assigned[i] = 1;
          dist[i] = 2*sin(dist[i]/12740.0);
          goodcomp[i] = (1+dist[i])*big;
      } else if (goodness[i]>=min_literal && goodness[i]<=max_literal) {
          assigned[i] = 1;
          dist[i] = 2*sin(goodness[i]/12740.0);
          goodcomp[i] = (1+dist[i])*big;
      } else {
          assigned[i] = 0;
          dist[i] = 2.0;
          goodcomp[i] = goodness[i];
      }/*endif*/
      if (recomp)
          {
          assigned[i] = 0;
          dist[i] = 2.0;
          }
      gooddist[i] = 2.0;
      if (lats[i]<minlat)
          minlat = lats[i];
      else if (lats[i]>maxlat)
          maxlat = lats[i];
      if (lons[i]<minlon)
          minlon = lons[i];
      else if (lons[i]>maxlon)
          maxlon = lons[i];
   }/*end for*/

   /* try to account for the aspect ratio of what is being plotted if the
      area of coverage is small. */
   myaspect = 1.0;
   while (aspect>1) {

       /* unit vector of mean point */
       xxa /= ns;
       yya /= ns;
       zza /= ns;
       d = sqrt(xxa*xxa+yya*yya+zza*zza);
       xxa /= d;
       yya /= d;
       zza /= d;

       /* east unit vector at mean point */
       xxe = -yya;
       yye = xxa;
       d = sqrt(xxe*xxe+yye*yye);
       xxe /= d;
       yye /= d;

       /* get the greatest east-west separation from center */
       dx = 0;
       dd = 1.0;
       for (i=0; i<ns; i++) {
           d = xxe*xx[i]+yye*yy[i];
           if (d<0) d = -d;
           if (d>dx) dx = d;
           d = xxa*xx[i]+yya*yy[i]+zza*zz[i];
           if (d<dd) dd = d;
       }/*end for*/

       /* calculate working aspect based on the separation. */
       dx *= dx;
       dd = 1.0-dd*dd;
       if (dd>dx) dx = dd;
       dd = 1.0-2*dx;
       if (dd<=0.0) break;
       myaspect = pow(aspect,dd);

       /* north unit vector at mean point (AxE) */
       xxn = -zza*yye;
       yyn = zza*xxe;
       zzn = xxa*yye-yya*xxe;

       /* artificially sqeeze points in the east-west dimension */
       for (i=0; i<ns; i++) {
           dx = (xxe*xx[i]+yye*yy[i])/myaspect;
           dy = (xxn*xx[i]+yyn*yy[i]+zzn*zz[i]);
           dz = (xxa*xx[i]+yya*yy[i]+zza*zz[i]);
           xx[i] = dx;
           yy[i] = dy;
           zz[i] = dz;
       }/*end for*/

       break;
   }/*end while*/

   /* This is the area within which we will work as we turn relative
      goodness values into goodness distances. */
   d = (maxlat-minlat)/200;
   minlat -= d;
   maxlat += d;
   d = (maxlon-minlon)/200;
   minlon -= d;
   maxlon += d;

   /* Convert goodness values into goodness distances. */
   if (!just_goodness)
       goodness_to_dist(minlat,maxlat,minlon,maxlon,active,ns);
   free(goodcomp);

   /* Categorize stations as preassigned or not.  Active becomes a map
      for the order in which we consider stations for final resolution;
      preassigned stations first, then all others. */
   unassigned = (int *) malloc(4*ns);
   na = kk = 0;
   for (k=0; k<ns; k++)
       if (assigned[k])
           active[na++] = k;
       else
           unassigned[kk++] = k;
   for (k=na,kk=0; k<ns; k++,kk++)
       active[k] = unassigned[kk];
   free(unassigned);

   /* This loop is not technically through each station we need to assign,
      but just for the number we need to assign. */
   for (k=0; k<ns; k++) {

       if (k>=na) {

           /* Determine next non-preassigned station for final resolution. */
           dd2 = -big;
           g = -1;
           for (kk=na; kk<ns; kk++) {
               gg = active[kk];
               if (assigned[gg]) continue;

               /* Here is where we balance user preference versus spatial
                  uniformity. */
               if (just_goodness)
                   d2 = goodness[gg];
               else
                   d2 = dist[gg]*weighting+(1-weighting)*gooddist[gg];
               if (d2<=dd2) continue;

               g = gg;
               dd2 = d2;
           }/*end for*/
           if (g<0) break;

       } else
           /* still working on preassigned stations. */
           g = active[k];

       /* Station g has been selected for final resolution.  For all other
          stations that have not yet been been resolved, reduce their
          relative distance to the distance between it and station g, unless
          its distance is already less. */
       assigned[g] = 1;
       for (kk=0; kk<ns; kk++) {
           gg = active[kk];
           if (assigned[gg]) continue;
           dd = dist[gg];
           if (dist[g]<dd) dd = dist[g];
           dx = xx[g]-xx[gg];
           if (dx>=dd || -dx>=dd) continue;
           dy = yy[g]-yy[gg];
           if (dy>=dd || -dy>=dd) continue;
           dz = zz[g]-zz[gg];
           if (dz>=dd || -dz>=dd) continue;
           d = sqrt(dx*dx+dy*dy+dz*dz);
           if (d>=dd) continue;
           dist[gg] = d;
       }/*end for*/

   }/*end for*/

   /* convert point to point distances in unit vector space to real distances
      in kilometers. */
   for (i=0; i<ns; i++)
      dist[i] = myaspect*6370*2*asin(dist[i]/2);

   /* Free up all the rest of the internally allocated memory. */
   free(xx);
   free(yy);
   free(zz);
   free(gooddist);
   free(assigned);
   free(active);

} /* end va_advanced */
