#include "meteoLib.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define MAX_WAVE_NUMBER 15

void FTN_MANGLE (dist_filter) (float * input, float * npts, float * output, 
                               int * mnx, int * nx, int * ny) 
    {

    static float * weights = 0;
    static float lastNpts = -99999.;

    int i,j,ii,jj;
    int i1,i2,j1,j2;
    int d,n,m,dd;
    int offset;
    int dwgtx,dnx,dskip;
    float * fpiMid;
    float * fpi;
    float * fpo;
    float * wp;
    float waveno;
    float tot;

    // copy the data if needed
    if (*npts<=1.0)
        {
        dnx = *mnx-*nx;
        fpiMid = input;
        fpo = output;
        for (jj=0; jj<*ny; jj++, fpiMid+=dnx, fpo+=dnx)
          for (ii=0; ii<*nx; ii++, fpiMid++, fpo++)
            *fpo = *fpiMid;
        return;
        }
    else
        {
        dnx = *mnx-*nx;
        fpo = output;
        for (jj=0; jj<*ny; jj++, fpo+=dnx)
          for (ii=0; ii<*nx; ii++, fpo++)
            *fpo = 1e37;
        }

    n = (int)(*npts + 0.99);
    if (n<2)
        n = 2;
    else if (n>MAX_WAVE_NUMBER)
        n = MAX_WAVE_NUMBER;
    d = (n+1)/2;
    dd = d+d;
    m = dd+1;

    // calculate weight table if needed.
    if (lastNpts!=*npts)
        {
        lastNpts = *npts;
        waveno = 3.14159265/(*npts);
        if (weights) free(weights);
        weights = (float*)malloc(sizeof(float)*m*m);
        tot = 0;
        for (wp=weights,j=-d; j<=d; j++)
          for (i=-d; i<=d; i++,wp++)
            {
            if (i)
                *wp = sin(waveno*i)/(waveno*i);
            else
                *wp = 1;
            if (j)
                *wp *= sin(waveno*j)/(waveno*j);
            else
                *wp *= 1;
            tot += *wp;
            }
        // divide by sum of the weights
        for (wp=weights,j=-d; j<=d; j++)
          for (i=-d; i<=d; i++,wp++)
            *wp /= tot;
        }

    // loop for doing those where weights fall totally within grid
    dwgtx = *mnx-m;
    dnx = (*mnx-*nx)+dd;
    offset = (*mnx*d)+d;
    fpiMid = input+offset;
    fpo = output+offset;
    for (jj=dd; jj<*ny; jj++, fpiMid+=dnx, fpo+=dnx)
      for (ii=dd; ii<*nx; ii++, fpiMid++, fpo++)
          {
          if (*fpiMid>99998.0)
              {
              *fpo = 1e37;
              continue;
              }
          *fpo = 0;
          tot = 1;
          wp = weights;
          fpi = fpiMid-offset;
          for (j=0; j<m; j++, fpi+=dwgtx)
            for (i=0; i<m; i++,wp++,fpi++)
                 if (*fpi>99998.0)
                     tot -= *wp;
                 else
                     *fpo += *fpi * *wp;
          if (tot<0.95)
              *fpo = 1e37;
          else
              *fpo /= tot;
          }//for ii

    // loop for doing those where weights fall partially outside grid
    fpiMid = input;
    fpo = output;
    dnx = *mnx-*nx;
    dskip = *nx-dd;
    ii = *nx+d;
    jj = *ny+d;
    j1 = -d;
    for (j2=d; j2<jj; j1++,j2++, fpiMid+=dnx,fpo+=dnx)
        {
        i1 = -d;
        for (i2=d; i2<ii; i1++,i2++, fpiMid++,fpo++)
            {
            if (i1==0 && j1>=0 && j2<*ny)
                {
                i1 += dskip;
                i2 += dskip;
                fpiMid += dskip;
                fpo += dskip;
                }
            if (*fpiMid>99998.0)
                {
                *fpo = 1e37;
                continue;
                }
            *fpo = 0;
            tot = 1;
            wp = weights;
            fpi = fpiMid-offset;
            for (j=j1; j<=j2; j++, fpi+=dwgtx)
              for (i=i1; i<=i2; i++,wp++,fpi++)
                 if (i>=0 && i<*nx && j>=0 && j<*ny && *fpi<99998.0)
                     *fpo += *fpi * *wp;
                 else
                     tot -= *wp;
            if (tot<0.95)
                *fpo = 1e37;
            else
                *fpo /= tot;
            }//for i2
        }//for j2

    }
                  
