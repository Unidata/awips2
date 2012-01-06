
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "meteoLib.h"

// We input theta and specific humidity for surface conditions because these
// are things that can be arithemitically averaged for a mixed layer.
// In order for a dcape to be calculated, there must be positive bouyancy
// somewhere in the column based on the input surface conditions, neglecting
// any cap.  Sinking parcel starts from the minimum thetaE level that is above
// the condensation pressure and below 400mb and the highest positive
// rising parcel bouyancy.  max_evap is the limit to how much water can
// be evaporated into the parcel as it decends, in Kg/Kg.  max_rh is
// the desired RH (%) as the parcel reaches the surface.  Will initially
// evaporate up to one-third of max_evap into the sinking parcel at the
// start, afterward attempting to trend the RH toward max_rh at the ground.
// If usetv=1, buoyancy is done with virtual temp, if usetv=0 with temp.
void dcapeFunc(float usetv, const float ** p_dat, const float ** t_dat, const float ** td_dat,
               float * p0, float * th0, float * sh0,
               int mnx, int nx, int ny, int nz,
               float max_evap, float max_rh, float * dcape_dat)
    {
    // These pointers point to our dynamic storarge.
    float *tvp_st, *tv0_st, *tvc_st, *tec_st, *pc_st;
    float *pm_st, *tm_st, *tdm_st, *wm_st, *rhm_st, *qm_st;
    tvp_st = tv0_st = tec_st = tvc_st = pc_st =
             pm_st = tm_st = tdm_st = wm_st = rhm_st = qm_st = 0;

    // Pointer to output data and end of loop pointer.
    float *dcape;

    // Working pointers inside our loops.
    const float *pp, *tt, *td3, *eptr;
    float *tv, *tvp, *tve, *qq, *pc, *tec, *tvc;
    float *pm, *tm, *tdm, *wm, *rhm, *pp1, *tvp1, *tve1, *pp0;

    float t0, td, b, up, dn, dlnp, qd, qw, qs, thve, eee, pb, pr, rhmx;
    int k,nn,n2,n3,dd,i,nzm,nxm;
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
    nzm = nz-1;
    nxm = nx-1;

    // Calculate the ascending parcel start equivalent temp, virtual temp,
    // and press at LCL, and the initial virtual temperature.  Initialize
    // pm and wm, which now will be pressure at and min environmetal
    // virtual theta E.
    pm = pm_st = (float*)malloc(n2*sizeof(float));
    wm = wm_st = (float*)malloc(n2*sizeof(float));
    pp0 = p0;
    tt = th0;
    qq = sh0;
    tec = tec_st = (float*)malloc(n2*sizeof(float));
    tvc = tvc_st = (float*)malloc(n2*sizeof(float));
    pc = pc_st = (float*)malloc(n2*sizeof(float));
    tv = tv0_st = (float*)malloc(n2*sizeof(float));
    eptr = pp0+nn;
    if (dd==0)
        i = 0x7FFFFFFF;
    else
        i = nx;
    for (; pp0<eptr; pp0++,tt++,qq++,pc++,tec++,tvc++,tv++,pm++,wm++)
        {
        *pm = *wm = 1e37;
        if (--i<0)
            {
            pp0 += dd;
            tt += dd;
            qq += dd;
            i = nxm;
            }
        if (*pp0>1e36 || *tt>1e36 || *qq>1e36)
            {
            *tec = *tvc = *pc = 1e37;
            continue;
            }
        t0 = (*tt)*pow((*pp0)/1000,kapa);
        *tv = t0*(1+usetv*0.000608*(*qq));
        b = c0-log( (*pp0)/(622./(*qq) + 0.378) );
        td = (b-sqrt(b*b-c_1))/c_2;
        td -= (t0-td)*(-0.37329638+41.178204/t0+0.0015945203*td);
        *pc = (*pp0)*pow(td/t0,kapa_1);
        *tec = adiabatic_te(&td,pc);
        *tvc = td*(1+usetv*0.000608*(*qq));
        }

    // Now calculate the virtual temperature of the accending parcel at the
    // pressures in the input data.
    tvp = tvp_st = (float*)malloc(n3*sizeof(float));
    for (k=0; k<nz; k++)
        {
        pp = p_dat[k];
        tec = tec_st;
        tvc = tvc_st;
        pc = pc_st;
        eptr = pp+nn;
        if (dd==0)
            i = 0x7FFFFFFF;
        else
            i = nx;
        for (; pp<eptr; pp++,pc++,tec++,tvc++,tvp++)
            {
            if (--i<0)
                {
                pp += dd;
                i = nxm;
                }
            if (*pc>1e36 || *tec>1e36 || *tvc>1e36 || *pp>1e36)
                {
                *tvp = 1e37;
                continue;
                }
            if (*pp>=*pc)
                {
                *tvp = (*tvc)*pow((*pp)/(*pc),kapa);
                continue;
                }
            t0 = (*tec)*pow((*pp)/(*pc),kapa);
            t0 = temp_of_te(&t0, pp);
            *tvp = t0*(*pp)/( *pp-usetv*exp(25.687958917-c1*t0-c2/t0) );
            }
        }

    // Calculate environment virtual temp, where we force the environment
    // to be no cooler than dry adiabatic from the ascending parcel start.
    // Find pressure of min environmetal virtual theta E above condensation
    // pressure...record temperature and dewpoint there.  Since we do not
    // need the accending parcel temps to complete the dcape calc, we 
    // will put the environmental virtual temp into the that storage.
    tm = tm_st = (float*)malloc(n2*sizeof(float));
    tdm = tdm_st = (float*)malloc(n2*sizeof(float));
    tvp = tvp_st+(n3+n2);
    for (k=nzm; k>=0; k--)
        {
        tvp -= (2*n2);
        pp = p_dat[k];
        tt = t_dat[k];
        td3 = td_dat[k];
        wm = wm_st;
        tm = tm_st;
        tdm = tdm_st;
        pm = pm_st;
        tvc = tvc_st;
        pc = pc_st;
        eptr = pp+nn;
        if (dd==0)
            i = 0x7FFFFFFF;
        else
            i = nx;
        for (; pp<eptr; pp++,tt++,td3++,tvp++,tvc++,pc++,pm++,tm++,tdm++,wm++)
            {
            if (--i<0)
                {
                pp += dd;
                tt += dd;
                td3 += dd;
                i = nxm;
                }
            if (*tvc>1e36 || *pc>1e36 || *pp>1e36 || *tvp>1e36 ||
                *tt>1e36 || *td3>1e36)
                {
                *tvp = 1e37;
                continue;
                }
            t0 = *tt;
            eee = exp(26.186004814-c1*(*td3)-c2/(*td3));
            qd = eee/(*pp-0.60771703*eee);
            eee = (1+usetv*0.608*qd);
            thve = t0*eee;
            pr = pow((*pp)/(*pc),kapa);
            if (thve<(*tvc)*pr)
                {
                thve = (*tvc)*pr;
                t0 = thve/eee;
                }
            if (*tvp<=thve && *wm>1e36 || *pp>*pc && *pm>=400)
                {
                if (*pm>1e36 && *pp<*pc) *pm = *pc;
                *tvp = thve;
                continue;
                }
            *tvp = thve;
            thve = (thve+2529*qd)*pow(1000/(*pp),kapa);
            if (thve>*wm && *pm>=400) continue;
            *wm = thve;
            *pm = *pp;
            *tm = t0;
            *tdm = *td3;
            }
        }

    // Here we will reuse our condensation level storage for
    // the level above current.  This loop performs the actual dcape
    // calculation.  
    rhm = rhm_st = (float*)malloc(n2*sizeof(float));
    qq = qm_st = (float*)malloc(n2*sizeof(float));
    tve = tvp_st+(n3+n2);
    for (k=nzm; k>=0; k--)
        {
        tve -= (2*n2);
        pp = p_dat[k];
        tvp1 = tec_st;
        tve1 = tvc_st;
        pp1 = pc_st;
        wm = wm_st;
        tm = tm_st;
        tdm = tdm_st;
        rhm = rhm_st;
        qq = qm_st;
        pm = pm_st;
        pp0 = p0;
        tv = tv0_st;
        dcape = dcape_dat;
        eptr = pp+nn;
        if (dd==0)
            i = 0x7FFFFFFF;
        else
            i = nx;
        for (; pp<eptr; pp++,pp1++,tve++,tve1++,tvp1++,pp0++,tv++,
                        pm++,wm++,tm++,tdm++,rhm++,qq++,dcape++)
            {
            if (--i<0)
                {
                pp += dd;
                pp0 += dd;
                dcape += dd;
                i = nxm;
                }
            if (k==nzm) *dcape = *pp1 = *tvp1 = *tve1 = 1e37;
            if (*pm>1e36 || *pp0>1e36 || *tv>1e36)
                continue;
            else if (*pp1>1e36)
                ;
            else if (*pp1>=*pp0)
                continue;
            else if (*tve1>1e36)
                ;
            else if (*pp>1e36 || *tve>1e36)
                continue;
            else if (*pp<=*pm)
                ;
            else if (*wm>1e36)
                *dcape = 0;
            else 
                {

                // Now we finally have the data we need for calculating
                // the dcape contribution for this layer.  If we have not
                // made any dcape calculations to this point, initialize
                // the decent parcel.
                if (*dcape>1e36)
                    {
                    *dcape = 0;
                    eee = exp(26.186004814-c1*(*tdm)-c2/(*tdm));
                    qd = eee/(*(pm)-0.60771703*eee);
                    qw = qd + max_evap/3;
                    t0 = *tm - 2529*max_evap/3;
                    eee = exp(26.186004814-c1*t0-c2/t0);
                    qs = eee/(*(pm)-0.60771703*eee);
                    if (qs>=qw)
                        {
                        *wm = max_evap-max_evap/3;
                        *tm = t0;
                        *rhm = qw/qs;
                        b = c0-log( qw*(*pm)/(0.622-0.378*qw) );
                        *tdm = (b-sqrt(b*b-c_1))/c_2;
                        }
                    else
                        {
                        *tm = *tdm = mytw(tm, tdm, pm);
                        *rhm = 1.0;
                        eee = exp(26.186004814-c1*(*tm)-c2/(*tm));
                        qw = eee/(*(pm)-0.60771703*eee);
                        *wm = max_evap-(qw-qd);
                        }
                    *qq = qw;
                    *tvp1 = *tm*(1+usetv*0.608*qw);
                    *pp1 = *pm;
                    }

                // Deal with reaching the surface, add in top of layer part.
                if (*pp>*pp0)
                    {
                    pb = *pp0;
                    dlnp = log(pb/(*pp1));
                    thve = *tv;
                    }
                else
                    {
                    pb = *pp;
                    dlnp = log(pb/(*pp1));
                    thve = *tve;
                    }
                up = -dlnp*287*0.5*(*tvp1-*tve1);
                if (up<-*dcape)
                    *dcape = 0;
                else
                    *dcape += up;

                // Deal with letting parcel fall to pb
                pr = pow(pb/(*pp1),kapa);
                if (*wm<=0)
                    *tvp1 *= pr;
                else
                    {
                    rhmx = *rhm+(pb-*pp1)*(max_rh-*rhm)/(*pp0-*pp1);
                    t0 = *tm*pr;
                    eee = exp(26.186004814-c1*t0-c2/t0);
                    qs = eee/(pb-0.60771703*eee);
                    if ((*qq)/qs>rhmx)
                        {
                        *tm = t0;
                        b = c0-log( (*qq)*pb/(0.622-0.378*(*qq)) );
                        *tdm = (b-sqrt(b*b-c_1))/c_2;
                        *tvp1 *= pr;
                        *rhm = (*qq)/qs;
                        }
                    else
                        {
                        qd = (rhmx*qs-(*qq))/sqrt(1000* (rhmx*qs+(*qq)) );
                        if (qd>*wm) qd = *wm;
                        qw = *qq + qd;
                        td = t0 - 2529*(*wm);
                        eee = exp(26.186004814-c1*td-c2/td);
                        qs = eee/(pb-0.60771703*eee);
                        if (qs>=qw)
                            {
                            *tm = td;
                            *rhm = qw/qs;
                            b = c0-log( qw*pb/(0.622-0.378*qw) );
                            *tdm = (b-sqrt(b*b-c_1))/c_2;
                            }
                        else
                            {
                            b = c0-log( (*qq)*pb/(0.622-0.378*(*qq)) );
                            *tdm = (b-sqrt(b*b-c_1))/c_2;
                            *tm = *tdm = mytw(&t0, tdm, &pb);
                            *rhm = 1.0;
                            eee = exp(26.186004814-c1*(*tm)-c2/(*tm));
                            qw = eee/(pb-0.60771703*eee);
                            qd = qw-*qq;
                            }
                        *wm -= qd;
                        *qq = qw;
                        *tvp1 = *tm*(1+usetv*0.608*qw);
                        }
                    }

                // Add contribution of bottom of layer.
                dn = -dlnp*287*0.5*(*tvp1-thve);
                if (dn<-*dcape)
                    *dcape = 0;
                else
                    *dcape += dn;

                }

            // Make current layer top next layer bottom.
            *tve1 = *tve;
            *pp1 = *pp;
            }
        }

    // unallocate our dynamic storage.
    free(tvp_st);
    free(tv0_st);
    free(tec_st);
    free(tvc_st);
    free(pc_st);
    free(pm_st);
    free(tm_st);
    free(tdm_st);
    free(wm_st);
    free(rhm_st);
    free(qm_st);

    }
