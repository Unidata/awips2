
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "meteoLib.h"

void heliComp(const float ** u, const float ** v, float * umot, float * vmot, 
              int mnx, int nx, int ny, int nz, 
              float * heli)
    {

    const float *u1, *v1, *u2, *v2;
    float *eptr, *hptr;
    int k,nn,n2,dd,i,nxm;

    dd = mnx-nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    nxm = nx-1;

    // Zero our destination array.
    hptr = heli;
    eptr = heli+nn;
    while (hptr<eptr) *(hptr++) = 1e37;

    // First do our motion, lower bulk shear computation.
    u1 = u[0];
    v1 = v[0]; // first row of v matrix
    u2 = u[nz-1];
    v2 = v[nz-1];
    if (dd==0)
        i = 0x7FFFFFFF;
    else
        i = nx;
    for (hptr=heli; hptr<eptr; umot++,vmot++,u1++,v1++,u2++,v2++,hptr++)
        {
        if (--i<0)
            {
            umot += dd;
            vmot += dd;
            u1 += dd;
            v1 += dd; // moving dd columns over in the current row
            u2 += dd;
            v2 += dd;
            hptr += dd;
            i = nxm;
            }
        if (*umot>1e36 || *vmot>1e36 ||
            *u1>1e36 || *v1>1e36 || *u2>1e36 || *v2>1e36) continue;
        *hptr = (*v2-*v1)*(*umot)+(*u1-*u2)*(*vmot);
        }

    // Now add in the contribution from the shear with the low level winds.
    for (k=1; k<nz; k++)
        {
        u1 = u[k-1];
        v1 = v[k-1];
        u2 = u[k];
        v2 = v[k]; // Get the Kth row of v...
        if (dd==0)
            i = 0x7FFFFFFF;
        else
            i = nx;
        for (hptr=heli; hptr<eptr; u1++,v1++,u2++,v2++,hptr++)
            {
            if (--i<0)
                {
                u1 += dd;
                v1 += dd;
                u2 += dd;
                v2 += dd;
                hptr += dd;
                i = nxm;
                }
            if (*hptr>1e36 ||
                *u1>1e36 || *v1>1e36 || *u2>1e36 || *v2>1e36) continue;
            *hptr += (*u2)*(*v1)-(*u1)*(*v2);
            }
        }    

    }
