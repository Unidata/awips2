#include "cmapf.h"


int kcllxy ( maparam * stcprm, lat_lon xltlng[], x_y xypts[], int npts,
             int index[],int kndx) {
int kstr,kindex=1;
vector_3d map0,map1;
double y0,y1,xmid;
   index[0]=0;
   map1 = basegtom(stcprm,ll_geog(xltlng[0].lat,xltlng[0].lng));
   map_xy(stcprm, map1,&xypts[0].x,&xypts[0].y);
   for (kstr=1;kstr<npts;kstr++) {
      map0 = map1;
      map1 = basegtom(stcprm,ll_geog(xltlng[kstr].lat,xltlng[kstr].lng));
      map_xy(stcprm, map1,&xypts[kstr].x,&xypts[kstr].y);
      y0 = map0.v[1];
      y1 = map1.v[1];
      if ((y1<0. && y0 >=0.) || (y0<0. && y1 >=0.)) {
         xmid = (y1*map0.v[0] - y0*map1.v[0])/(y1-y0);
         if (xmid <= 0.) {
         /*line between map0 and map1 crosses y=0 where it projects to the cut*/
            if (kindex <kndx) {
               index[kindex]=kstr;
               kindex++;
            }
         }
      }
   }

   if (kindex<kndx) {
      index[kindex] = npts;
      return kindex+1;
   } else {
      index[kndx-1] = npts;
      return -kindex;
   }
}

int kcwrap ( maparam * stcprm, lat_lon xltlng[], x_y xypts[] ){
vector_3d map[2],mapcut;
x_y xieta[3];
int k;
double y0,y1,norm=0.;
   map[0] = basegtom(stcprm, ll_geog(xltlng[0].lat, xltlng[0].lng) );
   map[1] = basegtom(stcprm, ll_geog(xltlng[1].lat, xltlng[1].lng) );
   map_xy(stcprm, map[0], &xypts[0].x,&xypts[0].y);
   map_xy(stcprm, map[1], &xypts[3].x,&xypts[3].y);
   y0 = map[0].v[1];
   y1 = map[1].v[1];
   if ( ( y0<0. && y1<0.) || (y0>=0. && y1 >=0.) ) {
      return 0;
   }
   if ( fabs( stcprm->gamma) >= 1. ) {
   /* stereographic projection has no cut */
      return 0;
   }
   for (k=0;k<3;k++) {
      mapcut.v[k] = (y0*map[1].v[k] - y1 * map[0].v[k] ) / (y0 - y1) ;
      norm += mapcut.v[k] * mapcut.v[k];
   }
   if ((mapcut.v[0] > 0.) || (norm == 0.) ) {
      return 0 ;
   } else {
      norm = sqrt(norm);
      for (k=0;k<3;k++) {
         mapcut.v[k] /= norm;
      }
   }
   map_xe( stcprm, mapcut,xieta,1);
   if (y0 < y1) {
      xe_xy( stcprm,xieta[1].x,xieta[1].y,& xypts[2].x,& xypts[2].y) ;
      xe_xy( stcprm,xieta[2].x,xieta[2].y,& xypts[1].x,& xypts[1].y) ;
      return 1;
   } else {
      xe_xy( stcprm,xieta[1].x,xieta[1].y,& xypts[1].x,& xypts[1].y) ;
      xe_xy( stcprm,xieta[2].x,xieta[2].y,& xypts[2].x,& xypts[2].y) ;
      return -1;
   }
}
