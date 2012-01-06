
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "meteoLib.h"

/* vc3d and param3d are lists of pointers to horizontal slabs containing   */
/* 3D data for a vertical coordinate and a parameter with which we want to */
/* define a quasi-horizontal surface.  mnx is the inner dimension of the   */
/* slab arrays, nx and ny are the dimensions of the slab grids, and nz is  */
/* the number of slabs.  param is a specific value of the same parameter   */
/* in param3d which defines the vertical location of the quasi horizontal  */
/* surface.  If sense<0, then the vertical coordinate is considered to be  */
/* increasing downward (like pressure).  If abs(sense)<=1, then the lowest */
/* occurence of the specific value is identified, otherwise the highest is */
/* identified.  The output variable vc2d is the value of the vertical      */
/* coordinate on the quasi-horizontal surface.   A param value of 1e35     */
/* means define the quasi-horizontal surface with the max value of the     */
/* parameter, -1e35 means define it with the min value.                    */

void defineSlice(const float ** vc3d, const float ** param3d,
                 int mnx, int nx, int ny, int nz, float param, int sense,
                 float * vc2d)
    {
    float *vc0, *par0;
    const float *vcptr, *parptr;
    float *eptr, *work;
    int k,nn,n2,nw,dd,i,found,nxm, dvc, dpar;
    int lowest, mxmn, earlyout, any;

    dd = mnx-nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    nw = n2*2;
    nxm = nx-1;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = vc2d+nn;
    while (--eptr>=vc2d) *eptr = 1e37;

    vc0 = work;
    par0 = vc0+n2;
    eptr = vc2d+nn;

    /* Determine whether we identify max, min, or the highest or   */
    /* the lowest occurence.  */
    if (param>9e34)
        mxmn = 1;
    else if (param<-9e34)
        mxmn = -1;
    else
        mxmn = 0;
    if (sense<=-2 || sense>=2)
        lowest = 0;
    else
        lowest = 1;
    if (lowest==1 && mxmn==0)
        earlyout = 1;
    else
        earlyout = 0;

    /* Loop through each vertical slab we have. */
    dvc = dpar = 0;
    for (k=0; k<nz; k++)
        {
        vcptr = vc3d[k];
        parptr = param3d[k];
        if (k>0)
            {
            dvc = vc3d[k-1]-vcptr;
            dpar = param3d[k-1]-parptr;
            }
        if (dd==0)
            i = 0x7FFFFFFF;
        else
            i = nx;

        /* Loop through current vertical slab */
        for (any=0; vc2d<eptr; vc2d++,vc0++,par0++,vcptr++,parptr++)
            {

            /* Deal with slab array being larger than slab grid.*/
            if (--i<0)
                {
                vcptr += dd;
                parptr += dd;
                vc2d += dd;
                i = nxm;
                }

            /* Accounting for whether we can exit early. */
            if (earlyout && *vc2d<1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* If data undefined, only record first defined vert coord.*/
            if (*parptr>1e36)
                {
                if (*vc0<1e36) continue;
                *vc0 = *vcptr;
                if (mxmn==1) *par0 = -1e37;
                continue;
                }

            /* Special case of first usable vert coord value.*/
            if (*vc0>1e36)
                {
                *vc0 = *vcptr;
                *par0 = *parptr;
                if (mxmn || *parptr==param) *vc2d = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of looking for max or min or our parameter. */
            if (mxmn)
                {
                *vc0 = *vcptr;
                if (*parptr==*par0)
                    {
                    if (lowest) continue;
                    }
                else if (mxmn==1)
                    {
                    if (*parptr<*par0) continue;
                    }
                else if (*parptr>*par0)
                    continue;
                *par0 = *parptr;
                *vc2d = *vcptr;
                continue;
                }

            /* Special case of top exactly matching parameter. */
            if (*parptr==param)
                {
                *vc2d = *vcptr;
                if (earlyout) continue;
                *vc0 = *vcptr;
                *par0 = *parptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*par0>1e36)
                {
                if (parptr[dpar]>1e36 || vcptr[dvc]>1e36 ||
                    (sense>0)==(*vcptr<vcptr[dvc]))
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                *vc0 = vcptr[dvc];
                *par0 = parptr[dpar];
                }

            /* If data values for this layer bracket param value, interp vc2d.*/
            if (*par0<*parptr)
                {
                if (param>=*par0 && param<=*parptr)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            else if (*par0>*parptr)
                {
                if (param>=*parptr && param<=*par0)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            *vc0 = *vcptr;
            *par0 = *parptr;
            }

        if (earlyout && !any) break;
        vc2d -= nn;
        vc0 -= n2;
        par0 -= n2;
        }

    free(work);
    }


/* In this version, dimvc is the dimensionality of the vc3d slabs, and    */
/* dimpar is the dimensionality of the param3d slabs.  We assume that the */
/* slab in vc2d0 has maximum dimensionality of all dimvc and dimpar.      */
/* Dimensionality is 0=const, 1=linear (nx X 1) and 2 = 2 dimensional.    */
void defineSliceD(const float ** vc3d, int * dimvc, const float ** param3d, int * dimpar,
                  int mnx, int nx, int ny, int nz, float param, int sense,
                  float * vc2d0)
    {
    float *vc0, *par0, *vc2d;
    float *eptr, *work, lastvc, *vcptrM, lastpar;
    const float *parptr, *vcptr, *parptrM;
    int k,nn,n2,nw,dd,i,j,nxm,d3a,d3b,dim2,d2,prevvc,prevpar;
    int lowest, mxmn, earlyout, any;

    dim2 = 0;
    for (k=0; k<nz && dim2<2; k++)
        {
        if (dimvc[k]>dim2) dim2 = dimvc[k];
        if (dimpar[k]>dim2) dim2 = dimpar[k];
        }

    dd = mnx-nx;
    d2 = dim2==2 ? dd : -nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    nw = n2*2;
    nxm = nx-1;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = vc2d0+nn;
    while (--eptr>=vc2d0) *eptr = 1e37;

    /* Determine whether we identify max, min, or the highest or   */
    /* the lowest occurence.  */
    if (param>9e34)
        mxmn = 1;
    else if (param<-9e34)
        mxmn = -1;
    else
        mxmn = 0;
    if (sense<=-2 || sense>=2)
        lowest = 0;
    else
        lowest = 1;
    if (lowest==1 && mxmn==0)
        earlyout = 1;
    else
        earlyout = 0;

    /* Loop through each vertical slab we have. */
    prevvc = dimvc[0];
    vcptrM = vc3d[0];
    prevpar = dimpar[0];
    parptrM = param3d[0];
    for (k=0; k<nz; k++)
        {
        vcptr = vc3d[k];
        parptr = param3d[k];
        if (k>0)
            {
            prevvc = dimvc[k-1];
            vcptrM = vc3d[k-1];
            prevpar = dimpar[k-1];
            parptrM = param3d[k-1];
            }
        lastvc = *vcptrM;
        lastpar = *parptrM;
        d3a = dimvc[k]==2 ? dd : -nx;
        d3b = dimpar[k]==2 ? dd : -nx;
        vc2d = vc2d0;
        vc0 = work;
        par0 = work+n2;

        /* Loop through current vertical slab */
        if (dimvc[k] && dimpar[k])
          for (any=j=0; j<ny; j++,vc2d+=d2,vcptr+=d3a,parptr+=d3b)
           for (i=0; i<nx; i++,vc2d++,vc0++,par0++,vcptr++,parptr++)
            {

            /* Accounting for whether we can exit early. */
            if (earlyout && *vc2d<1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* If data undefined, only record first defined vert coord.*/
            if (*parptr>1e36)
                {
                if (*vc0<1e36) continue;
                *vc0 = *vcptr;
                if (mxmn==1) *par0 = -1e37;
                continue;
                }

            /* Special case of first usable vert coord value.*/
            if (*vc0>1e36)
                {
                *vc0 = *vcptr;
                *par0 = *parptr;
                if (mxmn || *parptr==param) *vc2d = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of looking for max or min or our parameter. */
            if (mxmn)
                {
                *vc0 = *vcptr;
                if (*parptr==*par0)
                    {
                    if (lowest) continue;
                    }
                else if (mxmn==1)
                    {
                    if (*parptr<*par0) continue;
                    }
                else if (*parptr>*par0)
                    continue;
                *par0 = *parptr;
                *vc2d = *vcptr;
                continue;
                }

            /* Special case of top exactly matching parameter. */
            if (*parptr==param)
                {
                *vc2d = *vcptr;
                if (earlyout) continue;
                *vc0 = *vcptr;
                *par0 = *parptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*par0>1e36)
                {
                if (prevpar==1)
                    lastpar = parptrM[i];
                else if (prevpar==2)
                    lastpar = parptrM[i+j*mnx];
                if (lastpar>1e36)
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                if (prevvc==1)
                    lastvc = vcptrM[i];
                else if (prevvc==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                *vc0 = lastvc;
                *par0 = lastpar;
                }

            /* If data values for this layer bracket param value, interp vc2d.*/
            if (*par0<*parptr)
                {
                if (param>=*par0 && param<=*parptr)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            else if (*par0>*parptr)
                {
                if (param>=*parptr && param<=*par0)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            *vc0 = *vcptr;
            *par0 = *parptr;
            }

        else if (!dimvc[k] && !dimpar[k])
          for (any=j=0; j<ny; j++,vc2d+=d2)
           for (i=0; i<nx; i++,vc2d++,vc0++,par0++)
            {

            /* Accounting for whether we can exit early. */
            if (earlyout && *vc2d<1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* If data undefined, only record first defined vert coord.*/
            if (*parptr>1e36)
                {
                if (*vc0<1e36) continue;
                *vc0 = *vcptr;
                if (mxmn==1) *par0 = -1e37;
                continue;
                }

            /* Special case of first usable vert coord value.*/
            if (*vc0>1e36)
                {
                *vc0 = *vcptr;
                *par0 = *parptr;
                if (mxmn || *parptr==param) *vc2d = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of looking for max or min or our parameter. */
            if (mxmn)
                {
                *vc0 = *vcptr;
                if (*parptr==*par0)
                    {
                    if (lowest) continue;
                    }
                else if (mxmn==1)
                    {
                    if (*parptr<*par0) continue;
                    }
                else if (*parptr>*par0)
                    continue;
                *par0 = *parptr;
                *vc2d = *vcptr;
                continue;
                }

            /* Special case of top exactly matching parameter. */
            if (*parptr==param)
                {
                *vc2d = *vcptr;
                if (earlyout) continue;
                *vc0 = *vcptr;
                *par0 = *parptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*par0>1e36)
                {
                if (prevpar==1)
                    lastpar = parptrM[i];
                else if (prevpar==2)
                    lastpar = parptrM[i+j*mnx];
                if (lastpar>1e36)
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                if (prevvc==1)
                    lastvc = vcptrM[i];
                else if (prevvc==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                *vc0 = lastvc;
                *par0 = lastpar;
                }

            /* If data values for this layer bracket param value, interp vc2d.*/
            if (*par0<*parptr)
                {
                if (param>=*par0 && param<=*parptr)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            else if (*par0>*parptr)
                {
                if (param>=*parptr && param<=*par0)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            *vc0 = *vcptr;
            *par0 = *parptr;
            }

        else if (dimvc[k])
          for (any=j=0; j<ny; j++,vc2d+=d2,vcptr+=d3a)
           for (i=0; i<nx; i++,vc2d++,vc0++,par0++,vcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (earlyout && *vc2d<1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* If data undefined, only record first defined vert coord.*/
            if (*parptr>1e36)
                {
                if (*vc0<1e36) continue;
                *vc0 = *vcptr;
                if (mxmn==1) *par0 = -1e37;
                continue;
                }

            /* Special case of first usable vert coord value.*/
            if (*vc0>1e36)
                {
                *vc0 = *vcptr;
                *par0 = *parptr;
                if (mxmn || *parptr==param) *vc2d = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of looking for max or min or our parameter. */
            if (mxmn)
                {
                *vc0 = *vcptr;
                if (*parptr==*par0)
                    {
                    if (lowest) continue;
                    }
                else if (mxmn==1)
                    {
                    if (*parptr<*par0) continue;
                    }
                else if (*parptr>*par0)
                    continue;
                *par0 = *parptr;
                *vc2d = *vcptr;
                continue;
                }

            /* Special case of top exactly matching parameter. */
            if (*parptr==param)
                {
                *vc2d = *vcptr;
                if (earlyout) continue;
                *vc0 = *vcptr;
                *par0 = *parptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*par0>1e36)
                {
                if (prevpar==1)
                    lastpar = parptrM[i];
                else if (prevpar==2)
                    lastpar = parptrM[i+j*mnx];
                if (lastpar>1e36)
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                if (prevvc==1)
                    lastvc = vcptrM[i];
                else if (prevvc==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                *vc0 = lastvc;
                *par0 = lastpar;
                }

            /* If data values for this layer bracket param value, interp vc2d.*/
            if (*par0<*parptr)
                {
                if (param>=*par0 && param<=*parptr)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            else if (*par0>*parptr)
                {
                if (param>=*parptr && param<=*par0)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            *vc0 = *vcptr;
            *par0 = *parptr;
            }

        else
          for (any=j=0; j<ny; j++,vc2d+=d2,parptr+=d3b)
           for (i=0; i<nx; i++,vc2d++,vc0++,par0++,parptr++)
            {

            /* Accounting for whether we can exit early. */
            if (earlyout && *vc2d<1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* If data undefined, only record first defined vert coord.*/
            if (*parptr>1e36)
                {
                if (*vc0<1e36) continue;
                *vc0 = *vcptr;
                if (mxmn==1) *par0 = -1e37;
                continue;
                }

            /* Special case of first usable vert coord value.*/
            if (*vc0>1e36)
                {
                *vc0 = *vcptr;
                *par0 = *parptr;
                if (mxmn || *parptr==param) *vc2d = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of looking for max or min or our parameter. */
            if (mxmn)
                {
                *vc0 = *vcptr;
                if (*parptr==*par0)
                    {
                    if (lowest) continue;
                    }
                else if (mxmn==1)
                    {
                    if (*parptr<*par0) continue;
                    }
                else if (*parptr>*par0)
                    continue;
                *par0 = *parptr;
                *vc2d = *vcptr;
                continue;
                }

            /* Special case of top exactly matching parameter. */
            if (*parptr==param)
                {
                *vc2d = *vcptr;
                if (earlyout) continue;
                *vc0 = *vcptr;
                *par0 = *parptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*par0>1e36)
                {
                if (prevpar==1)
                    lastpar = parptrM[i];
                else if (prevpar==2)
                    lastpar = parptrM[i+j*mnx];
                if (lastpar>1e36)
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                if (prevvc==1)
                    lastvc = vcptrM[i];
                else if (prevvc==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *par0 = *parptr;
                    continue;
                    }
                *vc0 = lastvc;
                *par0 = lastpar;
                }

            /* If data values for this layer bracket param value, interp vc2d.*/
            if (*par0<*parptr)
                {
                if (param>=*par0 && param<=*parptr)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            else if (*par0>*parptr)
                {
                if (param>=*parptr && param<=*par0)
                    *vc2d = *vc0+(param-*par0)*(*vcptr-*vc0)/(*parptr-*par0);
                }
            *vc0 = *vcptr;
            *par0 = *parptr;
            }


        if (earlyout && !any) break;
        vc0 -= n2;
        par0 -= n2;
        }

    free(work);
    }

/* vc3d and slice3d are lists of pointers to horizontal slabs containing   */
/* 3D data for a vertical coordinate and a parameter which we want to      */
/* interpolate to a quasi-horizontal surface.  vc2d is a slab of vertical  */
/* coordinate values that define the vertical location of that surface.    */
/* mnx is the inner dimension of the slab arrays, nx and ny are the        */
/* dimensions of the slab grids, and nz is the number of slabs.            */
/* If sense<0, then the vertical coordinate is considered to be increasing */
/* downward (like pressure).  If abs(sense)<=1, then the lowest occurence  */
/* of the specific value is identified, otherwise the highest is           */
/* identified.  The output variable slice is the value of the parameter in */
/* slice3d interpolated to the quasi-horizontal surface.                   */

void createSlice(const float ** vc3d, float * vc2d, const float ** slice3d,
                 int mnx, int nx, int ny, int nz, int sense,
                 float * slice)
    {
    float *vc0, *slc0;
    const float *vcptr, *slcptr;
    float *eptr, *work;
    int k,nn,n2,nw,dd,i,nxm,any,dvc,dslc;

    dd = mnx-nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    nw = n2*2;
    nxm = nx-1;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = slice+nn;
    while (--eptr>=slice) *eptr = 1e37;

    vc0 = work;
    slc0 = vc0+n2;
    eptr = slice+nn;

    /* Loop through each vertical slab we have. */
    dvc = dslc = 0;
    for (k=0; k<nz; k++)
        {
        vcptr = vc3d[k];
        slcptr = slice3d[k];
        if (k>0)
            {
            dvc = vc3d[k-1]-vcptr;
            dslc = slice3d[k-1]-slcptr;
            }
        if (dd==0)
            i = 0x7FFFFFFF;
        else
            i = nx;

        /* Loop through current vertical slab */
        for (any=0; slice<eptr; slice++,vc2d++,vc0++,slc0++,vcptr++,slcptr++)
            {

            /* Deal with slab array being larger than slab grid.*/
            if (--i<0)
                {
                slice += dd;
                vcptr += dd;
                slcptr += dd;
                vc2d += dd;
                i = nxm;
                }

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *slice<1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vcptr)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vcptr)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    *slice = *slcptr;
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of this parameter value missing. */
            if (*slcptr>1e36)
                {
                if (*slc0<1e36) continue;
                if (sense<0 && *vcptr<*vc2d || sense>0 && *vcptr>*vc2d)
                    *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                *slice = *slcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36)
                {
                if (slcptr[dslc]>1e36 || vcptr[dvc]>1e36 ||
                    (sense>0)==(*vcptr<vcptr[dvc]))
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                *vc0 = vcptr[dvc];
                *slc0 = slcptr[dslc];
                }

            /* If vc values for this layer bracket vc2d, interp slice. */
            if (*vcptr>*vc0)
                {
                if (*vcptr>*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }
            else if (*vcptr<*vc0)
                {
                if (*vcptr<*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }

            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }
        if (!any) break;
        slice -= nn;
        vc2d -= nn;
        vc0 -= n2;
        slc0 -= n2;
        }

    free(work);
    }

/* In this version, dim3 is the dimensionality of the vc3d slabs, and      */
/* dim2 is the dimensionality of the vc2d slab.  We assume that the slabs  */
/* in slice3d and slice are uniformly fully 2 dimensional.  Dimensionality */
/* is 0=const, 1=linear (nx X 1) and 2 = 2 dimensional.  */
void createSliceD(float ** vc3d, int * dim3,
                  float * vc2d0, int dim2, float ** slice3d,
                  int mnx, int nx, int ny, int nz, int sense,
                  float * slice0)
    {
    float *vc0, *slc0, *slice;
    float *vcptr, *vc2d, *slcptr, *eptr, *work, lastvc, *vcptrM;
    int k,nn,n2,nw,dd,d3,d2,i,j,nxm,any,dslc,prev3;

    dd = mnx-nx;
    d2 = dim2==2 ? dd : -nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    nw = n2*2;
    nxm = nx-1;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = slice0+nn;
    while (--eptr>=slice0) *eptr = 1e37;

    /* Loop through each vertical slab we have. */
    dslc = 0;
    prev3 = dim3[0];
    vcptrM = vc3d[0];
    for (k=0; k<nz; k++)
        {
        vc2d = vc2d0;
        vcptr = vc3d[k];
        slcptr = slice3d[k];
        if (k>0)
            {
            prev3 = dim3[k-1];
            vcptrM = vc3d[k-1];
            dslc = slice3d[k-1]-slcptr;
            }
        lastvc = *vcptrM;
        d3 = dim3[k]==2 ? dd : -nx;
        vc0 = work;
        slc0 = work+n2;
        slice = slice0;

        /* Loop through current vertical slab, 4 cases to account for */
        /* combinations of constant dimensionality. */
        if (dim2 && dim3[k])
          for (any=j=0; j<ny; j++,slice+=dd,vcptr+=d3,slcptr+=dd,vc2d+=d2)
           for (i=0; i<nx; i++,slice++,vc2d++,vc0++,slc0++,vcptr++,slcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *slice<1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vcptr)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vcptr)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    *slice = *slcptr;
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of this parameter value missing. */
            if (*slcptr>1e36)
                {
                if (*slc0<1e36) continue;
                if (sense<0 && *vcptr<*vc2d || sense>0 && *vcptr>*vc2d)
                    *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                *slice = *slcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36)
                {
                if (k==0 || slcptr[dslc]>1e36)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                *vc0 = lastvc;
                *slc0 = slcptr[dslc];
                }

            /* If vc values for this layer bracket vc2d, interp slice. */
            if (*vcptr>*vc0)
                {
                if (*vcptr>*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }
            else if (*vcptr<*vc0)
                {
                if (*vcptr<*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }

            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        else if (!dim2 && !dim3[k])
          for (any=j=0; j<ny; j++,slice+=dd,slcptr+=dd)
           for (i=0; i<nx; i++,slice++,vc0++,slc0++,slcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *slice<1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vcptr)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vcptr)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    *slice = *slcptr;
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of this parameter value missing. */
            if (*slcptr>1e36)
                {
                if (*slc0<1e36) continue;
                if (sense<0 && *vcptr<*vc2d || sense>0 && *vcptr>*vc2d)
                    *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                *slice = *slcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36)
                {
                if (k==0 || slcptr[dslc]>1e36)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                *vc0 = lastvc;
                *slc0 = slcptr[dslc];
                }

            /* If vc values for this layer bracket vc2d, interp slice. */
            if (*vcptr>*vc0)
                {
                if (*vcptr>*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }
            else if (*vcptr<*vc0)
                {
                if (*vcptr<*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }

            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        else if (dim2)
          for (any=j=0; j<ny; j++,slice+=dd,slcptr+=dd,vc2d+=d2)
           for (i=0; i<nx; i++,slice++,vc2d++,vc0++,slc0++,slcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *slice<1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vcptr)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vcptr)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    *slice = *slcptr;
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of this parameter value missing. */
            if (*slcptr>1e36)
                {
                if (*slc0<1e36) continue;
                if (sense<0 && *vcptr<*vc2d || sense>0 && *vcptr>*vc2d)
                    *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                *slice = *slcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36)
                {
                if (k==0 || slcptr[dslc]>1e36)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                *vc0 = lastvc;
                *slc0 = slcptr[dslc];
                }

            /* If vc values for this layer bracket vc2d, interp slice. */
            if (*vcptr>*vc0)
                {
                if (*vcptr>*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }
            else if (*vcptr<*vc0)
                {
                if (*vcptr<*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }

            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        else
          for (any=j=0; j<ny; j++,slice+=dd,vcptr+=d3,slcptr+=dd)
           for (i=0; i<nx; i++,slice++,vc0++,slc0++,vcptr++,slcptr++)
            {

            /* Deal with slab array being larger than slab grid.*/
            if (--i<0)
                {
                slice += dd;
                vcptr += d3;
                slcptr += dd;
                i = nxm;
                }

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *slice<1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vcptr)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vcptr)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    *slice = *slcptr;
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.*/
            if (sense<0)
                {
                if (*vcptr>*vc0) continue;
                }
            else if (*vcptr<*vc0)
                continue;

            /* Case of this parameter value missing. */
            if (*slcptr>1e36)
                {
                if (*slc0<1e36) continue;
                if (sense<0 && *vcptr<*vc2d || sense>0 && *vcptr>*vc2d)
                    *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                *slice = *slcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36)
                {
                if (k==0 || slcptr[dslc]>1e36)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc>1e36 || (sense>0)==(*vcptr<lastvc))
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    continue;
                    }
                *vc0 = lastvc;
                *slc0 = slcptr[dslc];
                }

            /* If vc values for this layer bracket vc2d, interp slice. */
            if (*vcptr>*vc0)
                {
                if (*vcptr>*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }
            else if (*vcptr<*vc0)
                {
                if (*vcptr<*vc2d)
                    *slice = *slc0+(*vc2d-*vc0)*(*slcptr-*slc0)/(*vcptr-*vc0);
                }

            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        if (!any) break;
        }

    free(work);
    }

/* sampleSlice is analogous to the createSlice above, except that instead   */
/* of interpolating the data in slice3d to the nearest slice, the data      */
/* from the nearest point in the vertical is copied to the quasi-horizontal */
/* surface.  The extra argument hyb, if hyb!=0, means interpolate if that   */
/* is possible, otherwise copy.                                             */

void sampleSlice(const float ** vc3d, float * vc2d, const float ** slice3d,
                 int mnx, int nx, int ny, int nz, int sense, int hyb,
                 float * slice)
    {

    float *vc0, *slc0;
    const float *vcptr, *slcptr;
    float *eptr, *work, wgt;
    int k,nn,n2,nw,dd,i,nxm,any,dvc,dslc;

    dd = mnx-nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    nw = n2*2;
    nxm = nx-1;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = slice+nn;
    while (--eptr>=slice) *eptr = 1e37;

    vc0 = work;
    slc0 = vc0+n2;
    eptr = slice+nn;

    /* Loop through each vertical slab we have. */
    dvc = dslc = 0;
    for (k=0; k<nz; k++)
        {
        vcptr = vc3d[k];
        slcptr = slice3d[k];
        if (k>0)
            {
            dvc = vc3d[k-1]-vcptr;
            dslc = slice3d[k-1]-slcptr;
            }
        if (dd==0)
            i = 0x7FFFFFFF;
        else
            i = nx;

        /* Loop through current vertical slab */
        for (any=0; slice<eptr; slice++,vc2d++,vc0++,slc0++,vcptr++,slcptr++)
            {

            /* Deal with slab array being larger than slab grid.*/
            if (--i<0)
                {
                slice += dd;
                vcptr += dd;
                slcptr += dd;
                vc2d += dd;
                i = nxm;
                }

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.  Sense */
            /* violation above slice means ground is abv slice and we're done.*/
            if (sense<0)
                {
                if (*vcptr>*vc0)
                     {
                     if (*vc2d<*vc0) continue;
                     *slice = 1e37;
                     *vc0 = -1e37;
                     continue;
                     }
                }
            else if (*vcptr<*vc0)
                {
                if (*vc2d>*vc0) continue;
                *slice = 1e37;
                *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                if (*slcptr<1e37)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                continue;
                }

            /* If consecutive missing params are both above slice, we're done.*/
            if (*slcptr>1e36 && *slc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vc0)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vc0)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                *vc0 = *vcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36 && *slcptr<1e36 &&
                slcptr[dslc]<1e36 && vcptr[dvc]<1e36 &&
                (sense>0)==(*vcptr>vcptr[dvc]))
                {
                *vc0 = vcptr[dvc];
                *slc0 = slcptr[dslc];
                }

            /* Rare case of equal vert coord values. */
            if (*vcptr==*vc0)
                {
                *slc0 = *slcptr;
                continue;
                }

            /* If vc values for layer bracket vc2d & hyb is true, interp */
            /* slice. Otherwise copy value within half of interval to slice. */
            wgt = (*vc2d-*vc0)/(*vcptr-*vc0);
            if (wgt<0)
                {
                if (wgt>-0.5) *slice = *slc0;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1)
                {
                if (hyb && *slcptr<1e36 && *slc0<1e36)
                    *slice = *slc0+(*slcptr-*slc0)*wgt;
                else if (wgt<0.5)
                    *slice = *slc0;
                else
                    *slice = *slcptr;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1.5) *slice = *slcptr;
            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        if (!any) break;
        slice -= nn;
        vc2d -= nn;
        vc0 -= n2;
        slc0 -= n2;
        }

    free(work);
    }

/* In this version, dim3 is the dimensionality of the vc3d slabs, and      */
/* dim2 is the dimensionality of the vc2d slab.  We assume that the slabs  */
/* in slice3d and slice are uniformly fully 2 dimensional.  Dimensionality */
/* is 0=const, 1=linear (nx X 1) and 2 = 2 dimensional. */
void sampleSliceD(const float ** vc3d, int * dim3,
                  float * vc2d0, int dim2, const float ** slice3d,
                  int mnx, int nx, int ny, int nz, int sense, int hyb,
                  float * slice0)
    {

    float *vc0, *slc0, *slice;
    const float *vcptr, *slcptr;
    float *eptr, *work,*vc2d, wgt, lastvc, *vcptrM;
    int k,nn,n2,nw,dd,d3,d2,i,j,nxm,any,dslc,prev3;

    dd = mnx-nx;
    d2 = dim2==2 ? dd : -nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    nw = n2*2;
    nxm = nx-1;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = slice0+nn;
    while (--eptr>=slice0) *eptr = 1e37;

    /* Loop through each vertical slab we have. */
    dslc = 0;
    prev3 = dim3[0];
    vcptrM = vc3d[0];
    for (k=0; k<nz; k++)
        {
        vc2d = vc2d0;
        vcptr = vc3d[k];
        slcptr = slice3d[k];
        if (k>0)
            {
            prev3 = dim3[k-1];
            vcptrM = vc3d[k-1];
            dslc = slice3d[k-1]-slcptr;
            }
        lastvc = *vcptrM;
        d3 = dim3[k]==2 ? dd : -nx;
        vc0 = work;
        slc0 = work+n2;
        slice = slice0;

        /* Loop through current vertical slab , 4 cases to account for */
        /* combinations of constant dimensionality. */
        if (dim2 && dim3[k])
          for (any=j=0; j<ny; j++,slice+=dd,vcptr+=d3,slcptr+=dd,vc2d+=d2)
           for (i=0; i<nx; i++,slice++,vc2d++,vc0++,slc0++,vcptr++,slcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.  Sense */
            /* violation above slice means ground is abv slice and we're done.*/
            if (sense<0)
                {
                if (*vcptr>*vc0)
                     {
                     if (*vc2d<*vc0) continue;
                     *slice = 1e37;
                     *vc0 = -1e37;
                     continue;
                     }
                }
            else if (*vcptr<*vc0)
                {
                if (*vc2d>*vc0) continue;
                *slice = 1e37;
                *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                if (*slcptr<1e37)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                continue;
                }

            /* If consecutive missing params are both above slice, we're done.*/
            if (*slcptr>1e36 && *slc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vc0)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vc0)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                *vc0 = *vcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36 && *slcptr<1e36 &&
                slcptr[dslc]<1e36)
                {
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc<1e36 && (sense>0)==(*vcptr>lastvc))
                    {
                    *vc0 = lastvc;
                    *slc0 = slcptr[dslc];
                    }
                }

            /* Rare case of equal vert coord values. */
            if (*vcptr==*vc0)
                {
                *slc0 = *slcptr;
                continue;
                }

            /* If vc values for layer bracket vc2d & hyb is true, interp */
            /* slice. Otherwise copy value within half of interval to slice. */
            wgt = (*vc2d-*vc0)/(*vcptr-*vc0);
            if (wgt<0)
                {
                if (wgt>-0.5) *slice = *slc0;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1)
                {
                if (hyb && *slcptr<1e36 && *slc0<1e36)
                    *slice = *slc0+(*slcptr-*slc0)*wgt;
                else if (wgt<0.5)
                    *slice = *slc0;
                else
                    *slice = *slcptr;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1.5) *slice = *slcptr;
            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        else if (!dim2 && !dim3[k])
          for (any=j=0; j<ny; j++,slice+=dd,slcptr+=dd)
           for (i=0; i<nx; i++,slice++,vc0++,slc0++,slcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.  Sense */
            /* violation above slice means ground is abv slice and we're done*/
            if (sense<0)
                {
                if (*vcptr>*vc0)
                     {
                     if (*vc2d<*vc0) continue;
                     *slice = 1e37;
                     *vc0 = -1e37;
                     continue;
                     }
                }
            else if (*vcptr<*vc0)
                {
                if (*vc2d>*vc0) continue;
                *slice = 1e37;
                *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                if (*slcptr<1e37)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                continue;
                }

            /* If consecutive missing params are both above slice, we're done.*/
            if (*slcptr>1e36 && *slc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vc0)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vc0)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                *vc0 = *vcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36 && *slcptr<1e36 &&
                slcptr[dslc]<1e36)
                {
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc<1e36 && (sense>0)==(*vcptr>lastvc))
                    {
                    *vc0 = lastvc;
                    *slc0 = slcptr[dslc];
                    }
                }

            /* Rare case of equal vert coord values. */
            if (*vcptr==*vc0)
                {
                *slc0 = *slcptr;
                continue;
                }

            /* If vc values for layer bracket vc2d & hyb is true, interp */
            /* slice. Otherwise copy value within half of interval to slice. */
            wgt = (*vc2d-*vc0)/(*vcptr-*vc0);
            if (wgt<0)
                {
                if (wgt>-0.5) *slice = *slc0;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1)
                {
                if (hyb && *slcptr<1e36 && *slc0<1e36)
                    *slice = *slc0+(*slcptr-*slc0)*wgt;
                else if (wgt<0.5)
                    *slice = *slc0;
                else
                    *slice = *slcptr;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1.5) *slice = *slcptr;
            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        else if (dim2)
          for (any=j=0; j<ny; j++,slice+=dd,slcptr+=dd,vc2d+=d2)
           for (i=0; i<nx; i++,slice++,vc2d++,vc0++,slc0++,slcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.  Sense */
            /* violation above slice means ground is abv slice and we're done.*/
            if (sense<0)
                {
                if (*vcptr>*vc0)
                     {
                     if (*vc2d<*vc0) continue;
                     *slice = 1e37;
                     *vc0 = -1e37;
                     continue;
                     }
                }
            else if (*vcptr<*vc0)
                {
                if (*vc2d>*vc0) continue;
                *slice = 1e37;
                *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                if (*slcptr<1e37)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                continue;
                }

            /* If consecutive missing params are both above slice, we're done.*/
            if (*slcptr>1e36 && *slc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vc0)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vc0)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                *vc0 = *vcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36 && *slcptr<1e36 &&
                slcptr[dslc]<1e36)
                {
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc<1e36 && (sense>0)==(*vcptr>lastvc))
                    {
                    *vc0 = lastvc;
                    *slc0 = slcptr[dslc];
                    }
                }

            /* Rare case of equal vert coord values. */
            if (*vcptr==*vc0)
                {
                *slc0 = *slcptr;
                continue;
                }

            /* If vc values for layer bracket vc2d & hyb is true, interp */
            /* slice. Otherwise copy value within half of interval to slice. */
            wgt = (*vc2d-*vc0)/(*vcptr-*vc0);
            if (wgt<0)
                {
                if (wgt>-0.5) *slice = *slc0;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1)
                {
                if (hyb && *slcptr<1e36 && *slc0<1e36)
                    *slice = *slc0+(*slcptr-*slc0)*wgt;
                else if (wgt<0.5)
                    *slice = *slc0;
                else
                    *slice = *slcptr;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1.5) *slice = *slcptr;
            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        else
          for (any=j=0; j<ny; j++,slice+=dd,vcptr+=d3,slcptr+=dd)
           for (i=0; i<nx; i++,slice++,vc0++,slc0++,vcptr++,slcptr++)
            {

            /* Accounting for whether we can exit early. */
            if (*vc2d>1e36 || *vc0<-1e36) continue;
            any = 1;
            if (*vcptr>1e36) continue;

            /* Special case of first vertical coord value */
            if (*vc0>1e36)
                {
                if (*vc2d!=*vcptr)
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                else if (*slcptr<1e36)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    *vc0 = *vcptr;
                continue;
                }

            /* Verify that the sense of the current layer is correct.  Sense */
            /* violation above slice means ground is abv slice and we're done.*/
            if (sense<0)
                {
                if (*vcptr>*vc0)
                     {
                     if (*vc2d<*vc0) continue;
                     *slice = 1e37;
                     *vc0 = -1e37;
                     continue;
                     }
                }
            else if (*vcptr<*vc0)
                {
                if (*vc2d>*vc0) continue;
                *slice = 1e37;
                *vc0 = -1e37;
                continue;
                }

            /* Case of top exactly matching vert coord. */
            if (*vc2d==*vcptr)
                {
                if (*slcptr<1e37)
                    {
                    *slice = *slcptr;
                    *vc0 = -1e37;
                    }
                else
                    {
                    *vc0 = *vcptr;
                    *slc0 = *slcptr;
                    }
                continue;
                }

            /* If consecutive missing params are both above slice, we're done.*/
            if (*slcptr>1e36 && *slc0>1e36)
                {
                if (sense>0)
                    {
                    if (*vc2d<*vc0)
                        {
                        *vc0 = -1e37;
                        continue;
                        }
                    }
                else if (*vc2d>*vc0)
                    {
                    *vc0 = -1e37;
                    continue;
                    }
                *vc0 = *vcptr;
                continue;
                }

            /* Try to use 3d level just below ground for bottom */
            if (*slc0>1e36 && *slcptr<1e36 &&
                slcptr[dslc]<1e36)
                {
                if (prev3==1)
                    lastvc = vcptrM[i];
                else if (prev3==2)
                    lastvc = vcptrM[i+j*mnx];
                if (lastvc<1e36 && (sense>0)==(*vcptr>lastvc))
                    {
                    *vc0 = lastvc;
                    *slc0 = slcptr[dslc];
                    }
                }

            /* Rare case of equal vert coord values. */
            if (*vcptr==*vc0)
                {
                *slc0 = *slcptr;
                continue;
                }

            /* If vc values for layer bracket vc2d & hyb is true, interp */
            /* slice. Otherwise copy value within half of interval to slice.*/
            wgt = (*vc2d-*vc0)/(*vcptr-*vc0);
            if (wgt<0)
                {
                if (wgt>-0.5) *slice = *slc0;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1)
                {
                if (hyb && *slcptr<1e36 && *slc0<1e36)
                    *slice = *slc0+(*slcptr-*slc0)*wgt;
                else if (wgt<0.5)
                    *slice = *slc0;
                else
                    *slice = *slcptr;
                *vc0 = -1e37;
                continue;
                }
            if (wgt<1.5) *slice = *slcptr;
            *vc0 = *vcptr;
            *slc0 = *slcptr;
            }

        }

    free(work);
    }


void defineSlices(float * vc3d, int senseA,
                  float * param3d, int senseB,
                  int nx, int ny, int nz,
                  float * paramC, int nc, float * vcC)
    {
    float *vc0, *par0;
    float *eptr, *work, *vci;
    float a1,a0;
    int *next, *np;
    int k,nn,nw,nt,found;

    nn = nx*ny;
    nw = nn*2;
    nt = nn*nc;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = vcC+nt;
    while (--eptr>=vcC) *eptr = 1e37;
    k = nn*sizeof(int);
    np = (int*)malloc(k);
    memset(np, 0, k);

    vc0 = work;
    par0 = vc0+nn;
    eptr = vcC+nn;
    next = np;
    found = 0;

    for (k=0; k<nz; k++)
        {
        for (; vcC<eptr; vcC++,vc0++,par0++,vc3d++,param3d++,next++)
            {
            if (*next>=nc) continue;
            if (*vc3d>1e36 || *param3d>1e36) continue;
            if (*vc0>1e36)
                {
                *vc0 = *vc3d;
                *par0 = *param3d;
                continue;
                }
            if (senseA>0)
                {
                if (*vc3d<*vc0) continue;
                }
            else
                {
                if (*vc3d>*vc0) continue;
                }
            if (senseB>0)
                {
                if (*param3d<*par0)
                    {
                    *vc0 = *vc3d;
                    continue;
                    }
                while (*next<nc && paramC[*next]<*par0) ++*next;
                if (*next>=nc)
                    {
                    found++;
                    continue;
                    }
                if (paramC[*next]<=*param3d)
                    {
                    vci = vcC+(*next * nn);
                    a1 = (*vc3d-*vc0)/(*param3d-*par0);
                    a0 = *vc0-(*par0 * a1);
                    while (*next<nc && paramC[*next]<=*param3d)
                        {
                        *vci = paramC[*next]*a1+a0;
                        vci += nn;
                        ++*next;
                        }
                    }
                }
            else
                {
                if (*param3d>*par0)
                    {
                    *vc0 = *vc3d;
                    continue;
                    }
                while (*next<nc && paramC[*next]>*par0) ++*next;
                if (*next>=nc)
                    {
                    found++;
                    continue;
                    }
                if (paramC[*next]>=*param3d)
                    {
                    vci = vcC+(*next * nn);
                    a1 = (*vc3d-*vc0)/(*param3d-*par0);
                    a0 = *vc0-(*par0 * a1);
                    while (*next<nc && paramC[*next]>=*param3d)
                        {
                        *vci = paramC[*next]*a1+a0;
                        vci += nn;
                        ++*next;
                        }
                    }
                }
            if (*next>=nc) found++;
            *vc0 = *vc3d;
            *par0 = *param3d;
            }
        if (found>=nn) break;
        vcC -= nn;
        vc0 -= nn;
        par0 -= nn;
        next -= nn;
        }

    free(work);
    free(np);
    }


void createSlices(float * vc3d, float * param3d, int sense,
                  int nx, int ny, int nz,
                  float * vcC, int nc, float * paramC)
    {
    float *vc0, *par0;
    float *eptr, *work;
    float a1,a0;
    int *next, *np;
    int k,nn,nw,nt,found;

    nn = nx*ny;
    nw = nn*2;
    nt = nn*nc;

    work = (float*)malloc(nw*sizeof(float));
    eptr = work+nw;
    while (--eptr>=work) *eptr = 1e37;
    eptr = paramC+nt;
    while (--eptr>=paramC) *eptr = 1e37;
    k = nn*sizeof(int);
    np = (int*)malloc(k);
    memset(np, 0, k);

    vc0 = work;
    par0 = vc0+nn;
    eptr = vcC+nn;
    next = np;
    found = 0;

    for (k=0; k<nz; k++)
        {
        for (; vcC<eptr; paramC++,vcC++,vc0++,par0++,vc3d++,param3d++,next++)
            {
            if (*next>=nt) continue;
            if (*vc3d>1e36 || *param3d>1e36) continue;
            if (*vc0>1e36)
                {
                *vc0 = *vc3d;
                *par0 = *param3d;
                continue;
                }
            if (sense>0)
                {
                if (*vc3d<*vc0) continue;
                while (*next<nt && (vcC[*next]>1e36 || vcC[*next]<*vc0))
                    *next += nn;
                if (*next>=nt)
                    {
                    found++;
                    continue;
                    }
                if (vcC[*next]<=*vc3d)
                    {
                    a1 = (*param3d-*par0)/(*vc3d-*vc0);
                    a0 = *par0-(*vc0 * a1);
                    do
                        {
                        paramC[*next] = vcC[*next]*a1+a0;
                        *next += nn;
                        }
                    while (*next<nt && vcC[*next]<=*vc3d);
                    }
                }
            else
                {
                if (*vc3d>*vc0) continue;
                while (*next<nt && vcC[*next]>*vc0) *next += nn;
                if (*next>=nt)
                    {
                    found++;
                    continue;
                    }
                if (vcC[*next]>=*vc3d)
                    {
                    a1 = (*param3d-*par0)/(*vc3d-*vc0);
                    a0 = *par0-(*vc0 * a1);
                    do
                        {
                        paramC[*next] = vcC[*next]*a1+a0;
                        *next += nn;
                        }
                    while (*next<nt && vcC[*next]<1e36 && vcC[*next]>=*vc3d);
                    }
                }
            if (*next>=nt) found++;
            *vc0 = *vc3d;
            *par0 = *param3d;
            }
        if (found>=nn) break;
        vcC -= nn;
        paramC -= nn;
        vc0 -= nn;
        par0 -= nn;
        next -= nn;
        }

    free(work);
    free(np);
    }
