
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "meteoLib.h"

// We input theta and specific humidity for initial parcel because these are
// things that can be arithemitically averaged for a mixed layer.
// If usetv=1, buoyancy is done with virtual temp, if usetv=0 with temp.
// If usetv=0, must supply temps in tve_dat.
int capeFunc(float usetv, const float ** p_dat, const float ** tve_dat,
              float * p0, float * th0, float * sh0, 
              int mnx, int nx, int ny, int nz, 
              float * cape_dat, float * cin_dat)
    {
    // These pointers point to our dynamic storarge.
    // These pointers point to our dynamic storarge.
    float *tvp_st, *tec_st, *tvc_st, *pc_st, *pp1_st, *pmd_st, *md_st;
    tvp_st = tec_st = tvc_st = pc_st = pp1_st = pmd_st = md_st = 0;

    // Pointer to output data and end of loop pointer.
    const float *eptr;
    float *cap, *cin;

    // Working pointers inside our loops.
    const float *pp, *tve; 
    float *tt,*tvp, *qq, *pc, *tec, *tvc, *pmd, *md;
    float *pp1, *tvp1, *neg, *pos, *pp0;

    float t0, td, tdc, b, up, dn, dlnp;
    int k,nn,n2,n3,dd,i,nxm;
    float c0 = 26.66082;
    float c1 = 0.0091379024;
    float c2 = 6106.396;
    float c_1 = 223.1986;
    float c_2 = 0.0182758048;
    float kapa = 0.286;
    float kapa_1 = 3.498257;

    dd = mnx-nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    n3 = n2*nz;
    nxm = nx-1;

    // Calculate the parcel equivalent temp, virtual temp, and press at LCL.
    // Make working copy of sfc press, use as press below current 3d pressure.
    pp0 = p0;
    tt = th0;
    qq = sh0;
    tec = tec_st = (float*)malloc(n2*sizeof(float));
	if (tec == NULL || tec_st == NULL)
	{
		return 1;
	}
    tvc = tvc_st = (float*)malloc(n2*sizeof(float));
	if (tvc == NULL || tvc_st == NULL)
	{
		return 1;
	}
    pc = pc_st = (float*)malloc(n2*sizeof(float));
	if (pc == NULL || pc_st == NULL)
	{
		return 1;
	}
    pp1 = pp1_st = (float*)malloc(n2*sizeof(float));
	if (pp1 == NULL || pp1_st == NULL)
	{
		return 1;
	}
    eptr = pp0+nn;
    if (dd==0)
	{
        i = 0x7FFFFFFF;
	}
    else
	{
        i = nx;
	}
    for (; pp0<eptr; pp0++,pp1++,tt++,qq++,pc++,tec++,tvc++)
	{
        *pp1 = *pp0;
        if (--i<0)
        {
            pp0 += dd;
            tt += dd;
            qq += dd;
            i = nxm;
        }
        if (*pp0>1e36 || *tt>1e36 || *qq>1e36 || *qq<0.0005)
        {
            *tec = *tvc = *pc = 1e37;
            continue;
        }
        t0 = (*tt)*pow((*pp0)/1000,kapa);
        b = c0-log( (*pp0)/(622./(*qq) + 0.378) );
        td = (b-sqrt(b*b-c_1))/c_2;
        tdc = td-(t0-td)*(-0.37329638+41.178204/t0+0.0015945203*td);
        *pc = (*pp0)*pow(tdc/t0,kapa_1);
        *tec = adiabatic_te(&tdc,pc);
        *tvc = td*(1+usetv*0.000608*(*qq));
    }

    // Initialize md and pmd, which will be pressure of and max Te delta.
    md_st = (float*)malloc(n2*sizeof(float));
	if (md_st == NULL)
	{
		return 1;
	}
    pmd_st = (float*)malloc(n2*sizeof(float));
	if (pmd_st == NULL)
	{
		return 1;
	}
    memset(md_st, 0, n2*sizeof(float));
    memset(pmd_st, 0, n2*sizeof(float));

    // Now calculate the virtual temperature of the parcel at the pressures
    // in the input data.  Then difference it from the environmental temp,
    // which has been tweaked to not be cooler than dry adiabatic from the
    // parcel start.  Record the level of max parcel difference.
    tvp = tvp_st = (float*)malloc(n3*sizeof(float));
	if (tvp == NULL || tvp_st == NULL)
	{
		return 1;
	}
    for (k=0; k<nz; k++)
    {
        pp1 = pp1_st;
        pp = p_dat[k];
        tve = tve_dat[k];
        tec = tec_st;
        tvc = tvc_st;
        pc = pc_st;
        md = md_st;
        pmd = pmd_st;
        eptr = pp+nn;
        if (dd==0)
		{
            i = 0x7FFFFFFF;
		}
        else
		{
            i = nx;
		}
        for (; pp<eptr; pp1++,pp++,pc++,tec++,tvc++,tvp++,tve++,md++,pmd++)
        {
            if (--i<0)
            {
                pp += dd;
                tve += dd;
                i = nxm;
            }
            if (*pc>1e36 || *pp>1e36 || *tve>1e36)
            {
                *tvp = 1e37;
                continue;
            }
            t0 = (*tvc)*pow((*pp)/(*pc),kapa);
            if (*pp>=*pc)
			{
                *tvp = t0;
			}
            else
            {
                td = (*tec)*pow((*pp)/(*pc),kapa);
                *tvp = td = temp_of_te(&td, pp);
                if (usetv>0)
                    *tvp *= (*pp)/( *pp-exp(25.687958917-c1*td-c2/td) );
            }
            if (*tve<t0)
			{
                *tvp -= t0;
			}
            else
			{
                *tvp -= *tve;
			}
            if (*pp>*pc || *tvp<*md) continue;
            *md = *tvp;
            *pmd = *pp;
		}
    }

    // This loop performs the actual cape and cin calculation. Here we will
    // reuse storage for virt temp, equiv temp, and max delta for prev parcel
    // temp, neg and pos.  neg and pos are pending negative and positive
    // contributions we have not yet added into the cape and cin yet.
    tvp = tvp_st;
    for (k=0; k<nz; k++)
	{
        pp0 = p0;
        pc = pc_st;
        pmd = pmd_st;
        pp1 = pp1_st;
        pp = p_dat[k];
        tvp1 = tvc_st;
        neg = tec_st;
        pos = md_st;
        cap = cape_dat;
        cin = cin_dat;
        eptr = pp+nn;
        if (dd==0)
		{
            i = 0x7FFFFFFF;
		}
        else
		{
            i = nx;
		}
        for (; pp<eptr; pp0++,pc++,pmd++,pp1++,pp++,tvp1++,tvp++,
			cap++,cin++,pos++,neg++)
        {
            if (--i<0)
            {
                pp0 += dd;
                pp += dd;
                cap += dd;
                cin += dd;
                i = nxm;
            }
            if (k==0)
            {
                *cin = *cap = 1e37;
                *pos = *neg = 0;
            }
            else if (*pp0>1e36)
                continue;
            else if (*pp1>1e36 || *tvp1>1e36)
                ;
            else if (*pp>=*pp1 || *tvp>1e36)
                continue;
            else if (*pp>=*pp0)
                ;
            else 
            {
                // Now we finally have the data we need for calculating
                // the cape/cin contribution for this layer.
                if (*cap>1e36) *cap = *cin = 0;
                if (*pmd==0) continue; // No parcel delta>0, we're done.

                // First deal with possibility of bottom lvl being below the
                // initial parcel.
                if (*pp1>*pp0)
                {
                    dlnp = log((*pp0)/(*pp));
                    dn = 0;
                }
                else
                {
                    dlnp = log((*pp1)/(*pp));
                    dn = dlnp*287*(*tvp1);
                }

                // Now deal with the fact that not allowing superadiabatic
                // layers means no cape below condensation pressure.
                if (*pp1>=*pc)
                {
                    if (dn>0) dn = 0;
                    if (*tvp<=0)
                        up = dlnp*287*(*tvp);
                    else if (*pp>=*pc)
                        up = 0;
                     else
                        up = log((*pc)/(*pp))*287*(*tvp);
                }
                else
				{
                    up = dlnp*287*(*tvp);
				}

                // Deal with where the break point is.
                b = up*dn>=0 ? 0.5 : up/(up-dn);
                up *= b;
                dn *= (1-b);

                // Now consider this layer's contribution, taking into account
                // transitions between positive and negative acceleration.
                if (up==0 && dn==0)
                    ;

                // Continuing deceleration.
                else if (up<=0 && (dn<0 || dn==0 && (*pp<*pmd || *pos==0) ) )
                    *neg -= up+dn;

                // Continuing upward acceleration.
                else if (up>=0 && (dn>0 || dn==0 && (*pp<*pmd || *neg==0) ) )
                {
                    *pos += up+dn;
                    if (*pp>*pmd && (*cap)+(*pos)<=(*cin)+(*neg))
                        ; // no net cape and below max delta
                    else if (*pp>*pmd || *cap==0)
                    { // below max delta or cape uninitialized
                        *cap += *pos;
                        *cin += *neg;
                        *neg = *pos = 0;
                    }
                    else if (*pos>=*neg)
                    { // cape initialized and net positive contribution
                        *cap += (*pos-*neg);
                        *neg = *pos = 0;
                    }
                }

                // Transition to upward acceleration.
                else if (up>0 && dn<=0)
                {
                    *neg += -dn;
                    if (*pp1<=*pmd)
                    { // above max delta, only use net pos contribution
                        *pos += up;
                        if (*pos>=*neg)
                        {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                        }
                    }
                    else if (*pp<=*pmd)
                    { // straddle max delta, force cape initialization
                        if (*cap==0)
                        {
                            *cin += *neg;
                            *cap += *pos;
                        }
                        else if (*neg>*pos)
                            *cin += *neg-*pos;
                        else
                            *cap += *pos-*neg;
                        *cap += up;
                        *neg = *pos = 0;
                    }
                    else if ((*cap)+(*pos)+up<=(*cin)+(*neg))
                    {// no net cape to this point
                        if ((*cap)+(*pos)>0) 
                        { // reinitialize if there was cape before
                            *cin -= (*cap)+(*pos);
                            *pos = *cap = 0;
                        }
                        *cin += *neg;
                        *pos += up;
                        *neg = 0;
                    }
                    else if (*cap==0)
                    { // initialize cape
                        *cap += *pos+up;
                        *cin += *neg;
                        *neg = *pos = 0;
                    }
                    else
                    { // what remains, only use net pos contribution
                        *pos += up;
                        if (*pos>=*neg)
                        {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                        }
                    }
                }

                // Transition to decceleration.
                else
                {
                    *pos += dn;
                    if (*pp1<=*pmd)
                    { // above max delta, only use net pos contribution
                        if (*pos>=*neg)
                            {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                            }
                        *neg += -up;
                    }
                    else if ((*cap)+(*pos)<=(*cin)+(*neg)-up)
                    {// no net cape to this point
                        if (*cap>0)
                            {// reinitialize if there was cape before
                            *cin -= (*cap)+(*pos);
                            *pos = *cap = 0;
                            }
                        *cin += *neg-up;
                        *pos = *neg = 0;
                    }
                    else if (*cap==0) // initialize cape
                    {
                        *cap += *pos;
                        *cin += *neg-up;
                        *neg = *pos = 0;
                    }
                    else 
                    {  // what remains, only use net pos contribution
                        if (*pos>=*neg)
                        {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                            }
                        *neg += -up;
                    }
                }
            }

            // Make current layer top next layer bottom.
            *tvp1 = *tvp;
            *pp1 = *pp;
        }
    }

    // unallocate our dynamic storage.
    free(tec_st);
    free(tvc_st);
    free(pc_st);
    free(md_st);
    free(pmd_st);
    free(tvp_st);
    free(pp1_st);
	
	return 0;
}

// In this version we stop the computation at some arbitrary upper level, ptop.
int capeFuncTop(float usetv, float ** p_dat, float ** tve_dat,
                 float * p0, float * th0, float * sh0, 
                 int mnx, int nx, int ny, int nz, 
                 float * cape_dat, float * cin_dat, float * ptop)
{
    // These pointers point to our dynamic storarge.
    float *tvp_st, *tec_st, *tvc_st, *pc_st, *pp1_st, *pmd_st, *md_st;
    tvp_st = tec_st = tvc_st = pc_st = pp1_st = pmd_st = md_st = 0;

    // Pointer to output data and end of loop pointer.
    float *eptr, *cap, *cin;

    // Working pointers inside our loops.
    float *pp, *tt, *tve, *tvp, *qq, *pc, *tec, *tvc, *pmd, *md;
    float *pp1, *tvp1, *neg, *pos, *pp0, *pfin;

    float t0, td, tdc, b, up, dn, dlnp;
    int k,nn,n2,n3,dd,i,nxm,nzm;
    float c0 = 26.66082;
    float c1 = 0.0091379024;
    float c2 = 6106.396;
    float c_1 = 223.1986;
    float c_2 = 0.0182758048;
    float kapa = 0.286;
    float kapa_1 = 3.498257;

    dd = mnx-nx;
    nn = mnx*ny-dd;
    n2 = nx*ny;
    n3 = n2*nz;
    nxm = nx-1;

    // Calculate the parcel equivalent temp, virtual temp, and press at LCL.
    // Make working copy of sfc press, use as press below current 3d pressure.
    pp0 = p0;
    tt = th0;
    qq = sh0;
    pfin = ptop;
    tec = tec_st = (float*)malloc(n2*sizeof(float));
	if (tec == NULL || tec_st == NULL)
	{
		return 1;
	}
    tvc = tvc_st = (float*)malloc(n2*sizeof(float));
	if (tvc == NULL || tvc_st == NULL)
	{
		return 1;
	}
    pc = pc_st = (float*)malloc(n2*sizeof(float));
	if (pc == NULL || pc_st == NULL)
	{
		return 1;
	}
    pp1 = pp1_st = (float*)malloc(n2*sizeof(float));
	if (pp1 == NULL || pp1_st == NULL)
	{
		return 1;
	}
    eptr = pp0+nn;
    if (dd==0)
	{
        i = 0x7FFFFFFF;
	}
    else
	{
        i = nx;
	}
    for (; pp0<eptr; pp0++,pp1++,tt++,qq++,pc++,tec++,tvc++,pfin++)
    {
        *pp1 = *pp0;
        if (--i<0)
        {
            pfin += dd;
            pp0 += dd;
            tt += dd;
            qq += dd;
            i = nxm;
        }
        if (*pp0>1e36 || *tt>1e36 || *qq>1e36 || *qq<0.0005 || *pp0<*pfin)
        {
            *tec = *tvc = *pc = 1e37;
            continue;
        }
        t0 = (*tt)*pow((*pp0)/1000,kapa);
        b = c0-log( (*pp0)/(622./(*qq) + 0.378) );
        td = (b-sqrt(b*b-c_1))/c_2;
        tdc = td-(t0-td)*(-0.37329638+41.178204/t0+0.0015945203*td);
        *pc = (*pp0)*pow(tdc/t0,kapa_1);
        *tec = adiabatic_te(&tdc,pc);
        *tvc = td*(1+usetv*0.000608*(*qq));
    }

    // Initialize md and pmd, which will be pressure of and max Te delta.
    md_st = (float*)malloc(n2*sizeof(float));
	if (md_st == NULL)
	{
		return 1;
	}
    pmd_st = (float*)malloc(n2*sizeof(float));
	if (pmd_st == NULL)
	{
		return 1;
	}
    memset(md_st, 0, n2*sizeof(float));
    memset(pmd_st, 0, n2*sizeof(float));

    // Now calculate the virtual temperature of the parcel at the pressures
    // in the input data.  Then difference it from the environmental temp,
    // which has been tweaked to not be cooler than dry adiabatic from the
    // parcel start.  Record the level of max parcel difference.
    tvp = tvp_st = (float*)malloc(n3*sizeof(float));
    nzm = 0;
    for (k=0; k<nz; k++)
    {
        pp1 = pp1_st;
        pfin = ptop;
        pp = p_dat[k];
        tve = tve_dat[k];
        tec = tec_st;
        tvc = tvc_st;
        pc = pc_st;
        md = md_st;
        pmd = pmd_st;
        eptr = pp+nn;
        if (dd==0)
		{
            i = 0x7FFFFFFF;
		}
        else
		{
            i = nx;
		}
        for (; pp<eptr; pp1++,pp++,pc++,pfin++,tec++,tvc++,
                        tvp++,tve++,md++,pmd++)
        {
            if (--i<0)
            {
                pfin += dd;
                pp += dd;
                tve += dd;
                i = nxm;
            }
            if (*pc>1e36 || *pp>1e36 || *tve>1e36 || *pp1<=*pfin)
            {
                *tvp = 1e37;
                continue;
            }
            *pp1 = *pp;
            nzm = k;
            t0 = (*tvc)*pow((*pp)/(*pc),kapa);
            if (*pp>=*pc)
			{
                *tvp = t0;
			}
            else
            {
                td = (*tec)*pow((*pp)/(*pc),kapa);
                *tvp = td = temp_of_te(&td, pp);
                if (usetv>0)
                    *tvp *= (*pp)/( *pp-exp(25.687958917-c1*td-c2/td) );
            }
            if (*tve<t0)
			{
                *tvp -= t0;
			}
            else
			{
                *tvp -= *tve;
			}
            if (*pp>*pc || *pp<*pfin || *tvp<*md) continue;
            *md = *tvp;
            *pmd = *pp;
        }
    }
    nz = nzm+1;

    // This loop performs the actual cape and cin calculation. Here we will
    // reuse storage for virt temp, equiv temp, and max delta for prev parcel
    // temp, neg and pos.  neg and pos are pending negative and positive
    // contributions we have not yet added into the cape and cin yet.
    dlnp = 0;
    tvp = tvp_st;
    for (k=0; k<nz; k++)
    {
        pp0 = p0;
        pc = pc_st;
        pmd = pmd_st;
        pp1 = pp1_st;
        pp = p_dat[k];
        pfin = ptop;
        tvp1 = tvc_st;
        neg = tec_st;
        pos = md_st;
        cap = cape_dat;
        cin = cin_dat;
        eptr = pp+nn;
        if (dd==0)
		{
            i = 0x7FFFFFFF;
		}
        else
		{
            i = nx;
		}
        for (; pp<eptr; pp0++,pc++,pmd++,pp1++,pp++,pfin++,tvp1++,tvp++,
                        cap++,cin++,pos++,neg++)
        {
            if (--i<0)
            {
                pp0 += dd;
                pp += dd;
                pfin += dd;
                cap += dd;
                cin += dd;
                i = nxm;
            }
            if (k==0)
            {
                *cin = *cap = 1e37;
                *pos = *neg = 0;
            }
            else if (*pp0>1e36)
                continue;
            else if (*pp1>1e36 || *tvp1>1e36)
                ;
            else if (*pp>=*pp1 || *tvp>1e36)
                continue;
            else if (*pp>=*pp0)
                ;
            else 
            {
                // Now we finally have the data we need for calculating
                // the cape/cin contribution for this layer.
                if (*cap>1e36) *cap = *cin = 0;
                if (*pmd==0) continue; // No parcel delta>0, we're done.

                // First deal with possibility of bottom lvl being below the
                // initial parcel and/or hitting the top of the computation.
                if (*pp<*pfin)
                {
                    b = log((*pp1)/(*pp));
                    dlnp = log((*pp1)/(*pfin));
                    *tvp = *tvp1+(dlnp/b)*(*tvp-*tvp1);
                }
                if (*pp1>*pp0)
                {
                    if (*pp<*pfin)
                        dlnp = log((*pp0)/(*pfin));
                    else
                        dlnp = log((*pp0)/(*pp));
                    dn = 0;
                }
                else
                {
                    if (*pp>=*pfin) dlnp = log((*pp1)/(*pp));
                    dn = dlnp*287*(*tvp1);
                }

                // Now deal with the fact that not allowing superadiabatic
                // layers means no cape below condensation pressure.
                if (*pp1>=*pc)
                {
                    if (dn>0) dn = 0;
                    if (*tvp<=0)
                        up = dlnp*287*(*tvp);
                    else if (*pp>=*pc)
                        up = 0;
                    else if (*pp<*pfin)
                        up = log((*pc)/(*pfin))*287*(*tvp);
                    else
                        up = log((*pc)/(*pp))*287*(*tvp);
                }
                else
				{
                    up = dlnp*287*(*tvp);
				}

                // Deal with where the break point is.
                b = up*dn>=0 ? 0.5 : up/(up-dn);
                up *= b;
                dn *= (1-b);

                // Now consider this layer's contribution, taking into account
                // transitions between positive and negative acceleration.
                if (up==0 && dn==0)
                    ;

                // Continuing deceleration.
                else if (up<=0 && (dn<0 || dn==0 && (*pp<*pmd || *pos==0) ) )
                    *neg -= up+dn;

                // Continuing upward acceleration.
                else if (up>=0 && (dn>0 || dn==0 && (*pp<*pmd || *neg==0) ) )
                {
                    *pos += up+dn;
                    if (*pp>*pmd && (*cap)+(*pos)<=(*cin)+(*neg))
                        ; // no net cape and below max delta
                    else if (*pp>*pmd || *cap==0)
                    { // below max delta or cape uninitialized
                        *cap += *pos;
                        *cin += *neg;
                        *neg = *pos = 0;
                    }
                    else if (*pos>=*neg)
                    { // cape initialized and net positive contribution
                        *cap += (*pos-*neg);
                        *neg = *pos = 0;
                    }
                }

                // Transition to upward acceleration.
                else if (up>0 && dn<=0)
                {
                    *neg += -dn;
                    if (*pp1<=*pmd)
                    { // above max delta, only use net pos contribution
                        *pos += up;
                        if (*pos>=*neg)
                        {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                        }
                    }
                    else if (*pp<=*pmd)
                    { // straddle max delta, force cape initialization
                        if (*cap==0)
                        {
                            *cin += *neg;
                            *cap += *pos;
                        }
                        else if (*neg>*pos)
						{
                            *cin += *neg-*pos;
						}
                        else
						{
                            *cap += *pos-*neg;
						}
                        *cap += up;
                        *neg = *pos = 0;
                    }
                    else if ((*cap)+(*pos)+up<=(*cin)+(*neg))
                    {// no net cape to this point
                        if ((*cap)+(*pos)>0) 
                        { // reinitialize if there was cape before
                            *cin -= (*cap)+(*pos);
                            *pos = *cap = 0;
                        }
                        *cin += *neg;
                        *pos += up;
                        *neg = 0;
                    }
                    else if (*cap==0)
                    { // initialize cape
                        *cap += *pos+up;
                        *cin += *neg;
                        *neg = *pos = 0;
                    }
                    else
                    { // what remains, only use net pos contribution
                        *pos += up;
                        if (*pos>=*neg)
                        {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                        }
                    }
                }

                // Transition to decceleration.
                else
                {
                    *pos += dn;
                    if (*pp1<=*pmd)
                    { // above max delta, only use net pos contribution
                        if (*pos>=*neg)
                        {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                        }
                        *neg += -up;
                    }
                    else if ((*cap)+(*pos)<=(*cin)+(*neg)-up)
                    {// no net cape to this point
                        if (*cap>0)
                        {// reinitialize if there was cape before
                            *cin -= (*cap)+(*pos);
                            *pos = *cap = 0;
                        }
                        *cin += *neg-up;
                        *pos = *neg = 0;
                    }
                    else if (*cap==0) // initialize cape
                    {
                        *cap += *pos;
                        *cin += *neg-up;
                        *neg = *pos = 0;
                    }
                    else 
                    {  // what remains, only use net pos contribution
                        if (*pos>=*neg)
                        {
                            *cap += (*pos-*neg);
                            *neg = *pos = 0;
                        }
                        *neg += -up;
                    }
                }
            }

            // Make current layer top next layer bottom.
            *tvp1 = *tvp;
            *pp1 = *pp;
        }
    }

    // unallocate our dynamic storage.
    free(tec_st);
    free(tvc_st);
    free(pc_st);
    free(pp1_st);
    free(md_st);
    free(pmd_st);
    free(tvp_st);

	return 0;
}

