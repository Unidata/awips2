
#ifndef _meteoLib_h
#define _meteoLib_h

#include "ExtFtn.h"  

static int VortFun __UNUSED =1;
static int DivFun __UNUSED =2;
static int VadvFun __UNUSED =3;
static int ParDivFun __UNUSED =4;
static int AdvFun __UNUSED =5;
static int LaplFun __UNUSED =6;
static int GradFun __UNUSED =7;
static int GeoFun __UNUSED =8;
static int DefFun __UNUSED =9;
static int DefVecFun __UNUSED =10;
/*static int GeoDefFun __UNUSED =11;*/
/*static int GeoDefVecFun __UNUSED =12;*/
/*static int AgeoFun __UNUSED =13;*/
static int RidgeFun __UNUSED =14;
static int VcontFun __UNUSED =17;

/* Not called from Fortran or implemented in Fortran */
//extern "C" {

float adiabatic_te (const float * temp, const float * press);

float calcHeatIndex (float temp, float dewPoint);

float calcWindChill (float temp, float windSpd);

void heliComp(const float ** u, const float ** v, float * umot, float * vmot, 
        int mnx, int nx, int ny, int nz, 
        float * heli);

void defineSlice(const float ** vc3d, const float ** param3d,
        int mnx, int nx, int ny, int nz, float param, int sense,
        float * vc2d);

void createSlice(const float ** vc3d, float * vc2d, const float ** slice3d, 
        int mnx, int nx, int ny, int nz, int sense,
        float * slice);

void sampleSlice(const float ** vc3d, float * vc2d, const float ** slice3d, 
        int mnx, int nx, int ny, int nz, int sense, int hyb,
        float * slice);

void defineSlices(float * vc3d, int senseA,
        float * param3d, int senseB,
        int nx, int ny, int nz,
        float * paramC, int nc, float * vcC);

void createSlices(float * vc3d, float * param3d, int sense,
        int nx, int ny, int nz,
        float * vcC, int nc, float * paramC);

int capeFunc(float usetv, const float ** p_dat, const float ** tve_dat,
        float * p0, float * th0, float * sh0, 
        int mnx, int nx, int ny, int nz, 
        float * cape_dat, float * cin_dat);

int dcapeFunc(float usetv, const float ** p_dat, const float ** t_dat, const float **
        td_dat, float * p0, float * th0, float * sh0, 
        int mnx, int nx, int ny, int nz, 
        float max_evap, float max_rh, float * dcape_dat);
//}
EXT_FTN (void, g2gkinematics, (float * Udx, float * Vdy, float * Par,
            float * SpaX, float * SpaY,
            int * mnx, int * mny, int * nx, int * ny,
            int * choice, float * Scalar))

EXT_FTN (void, alt2press, (float * alt, float * z,
            int * mni, int * ni, int * nj, float * p))

EXT_FTN (void, hgt2pres, (float * z, float * p,
            int * mni, int * ni, int * nj))

EXT_FTN (void, add_by_cnst, (float * a, float * cnst, float * result,
            int * mni, int * ni, int * nj))

EXT_FTN (void, mult_by_cnst, (float * a, float * cnst, float * result,
            int * mni, int * ni, int * nj))

EXT_FTN (void, max_min, (float * a, float * b, float * result,
            int * mni, int * ni, int * nj, int * mode))

EXT_FTN (void, add_aray, (float * a, float * b, float * result,
            int * mni, int * ni, int * nj, int * mode))

EXT_FTN (void, sub_aray, (float * inp, float * b, float * result,
            int * mni, int * ni, int * nj))

EXT_FTN (void, ver_pts, (float * a, float * count, int * init,
            int * mni, int * ni, int * nj))

EXT_FTN (void, lintrans, (float * a, float * mult, float * add, float * result,
            int * mni, int * ni, int * nj))

EXT_FTN (void, replinrange, (float * a, int * tsttyp, float * lo, float * hi,
            float * repl,
            float * result, int * mni, int * ni, int * nj))

EXT_FTN (void, dist_filter, (float * a, float * npts, float * result,
            int * mni, int * ni, int * nj))

EXT_FTN (void, calccondpr, (float * p, float * t, float * rh,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, calccondprdef, (float * p, float * t, float * rh,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, calcdpd, (float * t, float * rh,
            int * mni, int * ni, int * nj, float * dpd))

EXT_FTN (void, calctd, (float * t, float * rh,
            int * mni, int * ni, int * nj, float * td))

EXT_FTN (void, calctd2, (float * p, float * t, float * q,
            int * mni, int * ni, int * nj, float * td))

EXT_FTN (void, calctw, (float * p, float * t, float * rh,
            int * mni, int * ni, int * nj, float * tw))

EXT_FTN (float, mytw, (float * t, float * td, float * p));

EXT_FTN (void, spechum, (float * p, float * t, float * rh,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, mixrat, (float * p, float * t, float * rh,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, spechum2, (float * p, float * td,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, calcthetae, (float * p, float * t, float * rh,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, calcthetae2, (float * p, float * t, float * td,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, calcrh, (float * t, float * td,
            int * mni, int * ni, int * nj, float * rh))

EXT_FTN (void, calcrh2, (float * p, float * t, float * q,
            int * mni, int * ni, int * nj, float * rh))

EXT_FTN (void, windspeed, (float * u, float * v, float * ff,
            int * mni, int * ni, int * nj))

EXT_FTN (void, winddir, (float * u, float * v, float * ff,
            int * mni, int * ni, int * nj))

EXT_FTN (void, temp2theta, (float * p, int * aflgp, float * t, int * aflgt, 
            float * theta, int * mni, int * ni, int * nj))

EXT_FTN (void, theta2temp, (float * p, int * aflgp, float * theta,
            int * aflgth, 
            float * t, int * mni, int * ni, int * nj))

EXT_FTN (void, tv2temp, (float * tv, float * q,
            int * mni, int * ni, int * nj, float * t))

EXT_FTN (void, calctv, (float * p, float * t, float * rh,
            int * mni, int * ni, int * nj, float * tv))

EXT_FTN (void, calctv2, (float * t, float * q,
            int * mni, int * ni, int * nj, float * tv))

EXT_FTN (void, calcpv, (float * p_up, float * p_low,
            float * o_up, float * o_low, float * pvort,
            int * mni, int * ni, int * nj,
            float * u_up, float * v_up, float * u_low,float *v_low,
            float * avort1, float * avort2,
            float * dx, float * dy, float * coriolis))

EXT_FTN (void, pvpres, (float * t_up, float * t_low, float * p_up,float* p_low,
            float * pvort, int * mni, int * ni, int * nj,
            float * u_up, float * v_up, float * u_low,
            float * v_low,
            float * avort1, float * avort2,
            float * dtdx1, float * dtdy1, float * dtdx2,
            float * dtdy2,
            float * dx, float * dy, float * coriolis))

EXT_FTN (void, lapserate, (float * tlo, float * pzlo, float * thi,
            float * pzhi,
            int * vc, int * mnx, int * nx, int * ny,
            float * lapse))

EXT_FTN (void, calcli, (float * p, float * t, float * rh,
            float * t5, float * p5,
            int * mni, int * ni, int * nj, float * li))

EXT_FTN (void, sweatidx, (float * tt, float * td8,
            float * u8, float * v8, float * u5, float * v5,
            int * mni, int * ni, int * nj, float * q))

EXT_FTN (void, derivative, (float * a1, float * a2, float * b1, float * b2,
            float * result, int * mni, int * ni, int * nj))

EXT_FTN (void, div_aray, (float * a, float * b,
            float * result, int * mni, int * ni, int * nj))

EXT_FTN (void, mult_aray, (float * a, float * b,
            float * result, int * mni, int * ni, int * nj))

EXT_FTN (void, dotvectors, (float * aX, float * aY, float * bX, float * bY,
            float * result, int * mni, int * ni, int * nj))

EXT_FTN (void, crossvectors, (float * aX, float * aY, float * bX, float * bY,
            float * result, int * mni, int * ni, int * nj))

EXT_FTN (void, rotvectors, (float * aX, float * aY,
            float * angle, float * bX, float * bY,
            int * mni, int * ni, int * nj))

EXT_FTN (void, exp_aray, (float * a, float * b,
            int * mni, int * ni, int * nj))

EXT_FTN (void, natlog, (float * a, float * b,
            int * mni, int * ni, int * nj))

EXT_FTN (void, powercalc, (float * a, float * b,
            float * result, int * mni, int * ni, int * nj))

EXT_FTN (void, mslp2thkns, (float * mslp, float * hgt,
            float * thkns, int * mni, int * ni, int * nj))

EXT_FTN (void, nadgdt, (float * u, float * v, float * a,
            int * mni, int * ni, int * nj, 
            float * dx, float * dy,
            float * dadxdt, float * dadydt))

EXT_FTN (void, comp_by, (float * u, float * v, float * uu, float * vv,
            int * mni, int * ni, int * nj, float * control,
            float * comp, float * comp2))

EXT_FTN (void, setqsmooth, (int * npass, float * smthwgt))

EXT_FTN (void, slqvect, (float * z, float * t, float * p,
            float * dx, float * dy, float * coriolis,
            int * mni, int * ni, int * nj,
            float * slqx, float * slqy,
            float * dugdx, float * dugdy, float * dvgdx,
            float * dvgdy,
            float * dtdx, float * dtdy))

EXT_FTN (void, qvector, (float * zmid, float * ztop, float * zbot,
            float * ptop, float * pbot,
            int * mni, int * ni, int * nj,
            float * dx, float * dy, float * f,
            float * dugdx, float * dvgdx, float * dugdy,
            float * dvgdy,
            float * dtdx, float * dtdy, float * qx, float * qy))

EXT_FTN (void, slfront, (float * z, float * t, float * p,
            float * dx, float * dy, float * coriolis,
            int * mni, int * ni, int * nj, float * fgen,
            float * slqx, float * slqy,
            float * w1, float * w2, float * w3,
            float * dtdx, float * dtdy))

EXT_FTN (void, frontogen, (float * zmid, float * ztop, float * zbot,
            float * ptop, float * pbot,
            int * mni, int * ni, int * nj,
            float * dx, float * dy, float * f,
            float * w1, float * w2, float * w3,
            float * dtdx, float * dtdy, float * qx, float * qy,
            float * fgen))

EXT_FTN (void, fndiverg, (float * zmid, float * ztop, float * zbot,
            float * ptop, float * pbot,
            int * mni, int * ni, int * nj,
            float * dx, float * dy, float * f,
            float * fnx, float * fny, float * w1,
            float * dtdx, float * dtdy, float * qx, float * qy,
            float * fndiv))

EXT_FTN (void, fsdiverg, (float * zmid, float * ztop, float * zbot,
            float * ptop, float * pbot,
            int * mni, int * ni, int * nj,
            float * dx, float * dy, float * f,
            float * fsx, float * fsy, float * w1,
            float * dtdx, float * dtdy, float * qx, float * qy,
            float * fsdiv))

EXT_FTN (float, ptozsa, (float *))

EXT_FTN (float, ztopsa, (float *))

EXT_FTN (void, interp, ( float p1, float p2, float temp1, float temp2,  
            float td1, float td2, float levelP,
            float * interT, float * interTd ))

EXT_FTN (void, calckidx, (float * press, float * temp, float * td,
            int numOfLevel, float * K ))

EXT_FTN (void, calctotidx, (float * press, float * temp, float * td,
            int numOfLevel, float * total ))

EXT_FTN (void, radiation, (float * lat, float * lng, float * lsm, int * jd,
            float * hr, float * bext, float * od,
            float * solrad ))

EXT_FTN (void, richno, (float * HT, float * HW, float * UW, float * VW,
            float * RHO,
            int * NLVLS, int * NW, float * BUOY, float * RICHNUM))

EXT_FTN (void, wndrho, (float * RHO, float * HT, int * NLVLS,
            float * HW, int * NW, float * RHOW))

EXT_FTN (void, density, (float * P, float * TVIR, int * NLVLS, float * RHO))

EXT_FTN (float, interp1, (float *, float *, float *, float *, float *))

EXT_FTN (float, dzdlnp, (float *, float *, float *))

EXT_FTN (float, rang2d, (const float * data, int * mnx, int * nx, int * ny,
            float * minData, float * maxData))

EXT_FTN (void, avwind, (float * ELEV, float * TOP, float * BOT, float * HW, 
            float * PW, float * TW, float * UW, float * VW,
            int * NW,
            float * UAVG, float * VAVG, float * AVDIR,
            float * AVSPD))

EXT_FTN (void, ctop, (float * P, float * HT, float * VV,
            float * PEQLEV, int * NPAR, float * CLDTOP))

EXT_FTN (void, eqlev, (float * P, float * HT, float * TP, float * TE,
            float * PLFC, 
            float * EPTPAR, int * NPAR, float * PEQLEV,
            float * HEQLEV, float * TEQLEV))

EXT_FTN (void, hailsiz, (float * VVMAX, float * HSIZE))

EXT_FTN (void, mxtp, (float * ANSOL, float * DELTAP,
            float * SFCP, float * P2, float * TL,
            float * DELTAZ, int * LVL, float * CTMAX))

EXT_FTN (void, liftedp, (float * P, float * T, float * HT,
            float * TVIR, int * NLVLS,
            int * NPAR, float * PCB, float * HCB,
            float * TCB, float * WCB, 
            float * THDPAR, float * EPTPAR,
            float * PL, float * TL, 
            float * PP, float * HTP, float * TP,
            float * TVIRP, float * TE, 
            float * TVIRE, int * NPARCEL))

EXT_FTN (void, sweat, (float * P, float * T, float * TD,
            int * NLVLS, float * PW, float * UW,
            float * VW, int * NW, float * SWIDX))

EXT_FTN (void, uvcomp, (float * DIR, float * SPD,
            float * U, float * V, int * NLVLS))

EXT_FTN (void, cclpar, (float * MIX, float * P,
            float * HT, float * T, int * NLVLS,
            float * PCCL, float * TCCL, float * HCCL))

EXT_FTN (void, lclpar, (float * MIX, float * TS,
            float * P, float * HT, float * T, float * TD,
            int * NLVLS, float * PLCL, float * TLCL, float * HLCL))

EXT_FTN (void, lfcpar, (float * EPTPAR, float * PCB,
            float * TCB, float * HCB, float * T1, float * T2,
            float * P1, float * HT1, int * NPAR,
            float * PLFC1, float * HLFC1, float * TLFC1,
            float * PLFC2, float * HLFC2, float * TLFC2))

EXT_FTN (void, ddff, (float * U, float * V, float * DIR,
            float * SPD, int * NLVLS))

EXT_FTN (void, frzlev, (float * ELEV, float * P,
            float * HT, float * T, int * NLVLS,
            float * PFRZ, float * HFRZ))

EXT_FTN (void, intpos, (float * VDIF, float * HT,
            float * P, float * T, int * NLVLS))

EXT_FTN (void, negarea, (float * PCB, float * TCB, float * HCB, float * PLFC, 
            float * HLFC, float * TLFC,
            float * THDPAR, float * EPTPAR,
            float * P, float * HT, float * TE,
            float * TP, int * NPAR, 
            float * CINFRMCAPE, float * NEGBUOY))

EXT_FTN (void, posarea, (float * PLFC, float * PEQLEV,
            float * TLFC, float * TEQLEV,
            float * HLFC, float * HEQLEV,
            float * EPTPAR, float * P, 
            float * HT, float * TE,
            float * TP, int * NPAR, float * BUOY, float * CIN))

EXT_FTN (void, totals, (float * P, float * T, float * TD,
            int * NLVLS, float * TOTIDX,
            float * CRSTOT, float * VERTOT))

EXT_FTN (void, vvel, (float * pcb, float * PEQLEV,
            float * P, float * HT, float * TP,
            float * TVE, float * TVP, float * WLCL, int * NPAR, 
            float * VV, float * VVMAX))

EXT_FTN (float, esat, (float * T))

EXT_FTN (void, temp_mixratio, ( float * press, float * mixratio,
            float * tempmr ))

EXT_FTN (void, solax, (int * JULDAY, int * MONTH, float * SLAT, int * TYMINC,
            int * TSTART, int * TSTOP, float * TSRAD))

EXT_FTN (void, eqp, (float * DELTAP, float * P, float * HT,
            float * T, float * TD, 
            int * N, float * PP, float * HTT,
            float * TT, float * TTD, int * NN))

EXT_FTN (void, rhbar, (float * ENDLVL, int * MRH,
            int * NCLYR, float * SFCP, 
            float * P, float * TL, float * TDL))

EXT_FTN (void, forecast, (int * yr, int * mon, int * day,
            int * hour, int * min,
            char * stnid, int * snow,
            float * slat, float * slon, 
            float * p, float * ht,
            float * t, float * td, int * nlvls, 
            float * ftmax, int * status))

EXT_FTN (void, cv_date2jul, (int * YR, int * MON,
            int * DAY, int * JD, int * ISTATUS))

EXT_FTN (float, virttemp, (float * T, float * Td, float * P))

EXT_FTN (void, virtualt, (float * T, float * TD, float * P,
            int * NLVLS, float * TVIR))

EXT_FTN (void, wbzero, (float * ELEV, float * P, float *HT,
            float *Tt, float * TD,
            int * NLVLS, float * PWBZ,
            float * HWBZ, float * TWBZ))

EXT_FTN (void, tsoar, (float * elev, float * p, float * z,
            float * t, float * theta, int * nl, 
            float * Tpmax, float * PTLXEC,
            float * zlnec, float * tlnec,  
            float * zlxec, float * tlxec,
            float * soarindx, float * Trigtemp))

EXT_FTN (void, gusts, (float * p, float * t, float * td,
            int * np, int * gstpot))

EXT_FTN (void, deftrk, (float * tcb, float * pcb,
            float * thdpar, float * eptpar))

EXT_FTN (void, pvalue, (float * pres, float * p,
            int * np, float * param, float * value))

EXT_FTN (void, cvgust, (float * dd7, float * ui, int * gstpot))

EXT_FTN (float, thetawa, (float * temp, float * dwpt,
            float * pres, int * iw, int * ier))

EXT_FTN (int, cgp, (float * tempip, float * dwptip,
            float * presip, float * thetawip,
            float * sfcpres, float * toppres,
            int * iw, float * deltap))

EXT_FTN (void, tpzlcl, (float * tk, float * tdk, float * pinit, int * iw,
            float * tl, float * pl, float * zl, int * ier))

EXT_FTN (float, pottemp, (float * temp, float * dwpt, float * pres, int * iw))

EXT_FTN (float, dmixr, (float * temp, float * pres, int * iw))

EXT_FTN (void, pseudolift, (int * n, float * pstart,
            float * pfinish, float * soln))

EXT_FTN (float, vp, (float * tk, int * iw))

EXT_FTN (void, calchelicity, (float * HW, float * PW,
            float * UW, float * VW, int * NW,
            float * elev, float * ztop,
            float * ghx, float * ghy,
            float * diravg, float * spdavg,
            float * stmdir, float * stmspd,
            float * helicity, float * SRHel))

EXT_FTN (void, tplcl, (float * tk,float * td,
            float * pinit,float * tl,float * pl,int * ier))

EXT_FTN (float, temp_of_te, ( const float * te, const float * press))

EXT_FTN (float, ept, (float * t, float * td, float * p))

EXT_FTN (float, tsa, (float * os, float * pres))

EXT_FTN (void, fortconbuf, (float * Array, int * Work,
            int * mnx, int * nx, int * ny,
            float * scale, float * offset,
            int * mode, float * seed,
            float * xpoints, float * ypoints, int * npoints,
            float * badlo, float * badhi, int * status))

EXT_FTN (void, strmpak, (const float * U, const float * V, int * work, const int * mnx,
        const int * nx, const int * ny, const float * asize, const float *
        xpoints, const float * ypoints, const int * npoints, const float *
        minspc, const float * maxspc, const float * badlo, const float *
        badhi));

EXT_FTN (void, strmsmth, (float * smoothness, int * npass));



EXT_FTN (void, matsln, (float * Array, float * yVector,
            int * work, float * soln, 
            int * mn, int * n, int * ok))

#if 1
EXT_FTN (int, scaleless_analysis, (float * xind, float * yind, float * values, 
            int * nv, int * nx, int * ny, float * grid))
#endif

#endif
