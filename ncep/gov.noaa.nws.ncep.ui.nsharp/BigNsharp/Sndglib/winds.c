/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Wind/Vector Manipulation Library                           */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <math.h>
#include "winds.h"
#include "sndglib.h"

float ucomp(float wdir, float wspd)
	/*************************************************************/
	/*  UCOMP                                                    */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a u-component of the wind (kt), given         */
	/*  a direction and speed.                                   */
	/*                                                           */
	/*  wdir             - Wind Direction (deg)                  */
	/*  wspd             - Wind Speed (kt)                       */
	/*************************************************************/
{
	if (!qc(wdir) || !qc(wspd))
	  return RMISSD;

	while (wdir > 360.0)
	  wdir -= 360;
	wdir *= (PI / 180.0);
	return (-wspd * sin(wdir));
}

float vcomp(float wdir, float wspd)
	/*************************************************************/
	/*  VCOMP                                                    */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a v-component of the wind (kt), given         */
	/*  a direction and speed.                                   */
	/*                                                           */
	/*  wdir             - Wind Direction (deg)                  */
	/*  wspd             - Wind Speed (kt)                       */
	/*************************************************************/
{
	if (!qc(wdir) || !qc(wspd))
	  return RMISSD;

	while (wdir >= 360.0)
	  wdir -= 360;
	wdir *= (PI / 180.0);
	return (-wspd * cos(wdir));
}

float angle(float u, float v)
	/*************************************************************/
	/*  ANGLE                                                    */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates an angle (deg) of the wind (u,v).             */
	/*                                                           */
	/*  u                - U-Component (kt)                      */
	/*  v                - V-Component (kt)                      */
	/*************************************************************/
{

	double sc, t1;

	if (!qc(u) || !qc(v))
	  return RMISSD;

/*
	t1 = atan2(-u, -v) * 180.0 / PI;
	if (t1 <= 0.0)
	  t1 += 360.0;
	return t1;
*/

        sc = PI/180;
        if ((u == 0) && (v == 0)) return 0;

	/* modified 29 March 2011 RLT */
 	if ((u == 0) && (v < 0)) return 360;
        if ((u == 0) && (v > 0)) return 180;
	      t1 = atan(-v / -u) / sc;
        if (u <= 0)
           {return (float)(90 - t1);}
        else
           {return (float)(270 - t1);}

}

float speed(float u, float v)
	/*************************************************************/
	/*  SPEED                                                    */
	/*  Mike Kay   SPC                                           */
	/*                                                           */
	/*  Calculates the speed of the wind (u,v).                  */
	/*                                                           */
	/*  u                - U-Component                           */
	/*  v                - V-Component                           */
	/*************************************************************/
{
	if (!qc(u) || !qc(v))
	  return RMISSD;
	return sqrt((u * u) + (v * v));
}

void mean_wind(float pbot, float ptop, float *mnu, float *mnv,
	       float *wdir, float *wspd)
	/*************************************************************/
	/*  MEAN_WIND                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a pressure-weighted mean wind thru the        */
	/*  layer (pbot-ptop).  Default layer is LFC-EL.             */
	/*                                                           */
	/*  pbot             - Bottom level of layer (mb)            */
	/*  ptop             - Top level of layer (mb)               */
	/*  mnu              - U-Component of mean wind (kt)         */
	/*  mnv              - V-Component of mean wind (kt)         */
	/*************************************************************/
{
	float  pinc, usum, vsum, wgt, w1, num, i, ix1, p;
	float  sfctemp, sfcdwpt, sfcpres, lower, upper;
	Parcel pcl;

	*wdir = RMISSD;
	*wspd = RMISSD;
	*mnu  = RMISSD;
	*mnv  = RMISSD;

	if (!sndg)
	  return;

	if (lplvals.flag == (int)RMISSD)
	  return;

	/* ----- Check for Default Values ----- */
	if ((pbot == -1) || (ptop == -1)) {
	   sfctemp = lplvals.temp;
	   sfcdwpt = lplvals.dwpt;
	   sfcpres = lplvals.pres;
	   ix1 = parcel(-1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	   lower = pcl.lfcpres;
	   upper = pcl.elpres;
	   if (!qc(lower))
	     lower = 850.0;
	   if (!qc(upper))
	     upper = 200.0;
	}

	if (pbot == -1) { pbot = lower; }
	if (ptop == -1) { ptop = upper; }

	pinc = (pbot - ptop) / 20.0;

	if (pinc < 1.0) {
	   usum = (i_wndu(pbot, I_PRES)*pbot) + (i_wndu(ptop, I_PRES)*ptop);
	   vsum = (i_wndv(pbot, I_PRES)*pbot) + (i_wndv(ptop, I_PRES)*ptop);
	   wgt = pbot + ptop;
	}
	else {
	   num = wgt = usum = vsum = 0;
	   p = pbot;
	   for (i=0;i<=20;i++) {
	      w1 = p;
	      usum = usum + (i_wndu(p, I_PRES) * w1);
	      vsum = vsum + (i_wndv(p, I_PRES) * w1);
	      p -= pinc;
	      wgt = wgt + w1;
	      num++;
	   }
	}

	*mnu = (usum / wgt);
	*mnv = (vsum / wgt);

	if (qc(*mnu) && qc(*mnv)) {
	  *wdir = angle(*mnu, *mnv);
	  *wspd = speed(*mnu, *mnv);
	}
}

void mean_wind_npw(float pbot, float ptop, float *mnu, float *mnv,
	       float *wdir, float *wspd)
	/*************************************************************/
	/*  MEAN_WIND_NPW                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a mean wind thru the layer (pbot-ptop).       */
	/*  Does not use a pressure weighting function!!!            */
	/*                      Default layer is LFC-EL.             */
	/*                                                           */
	/*  pbot             - Bottom level of layer (mb)            */
	/*  ptop             - Top level of layer (mb)               */
	/*  mnu              - U-Component of mean wind (kt)         */
	/*  mnv              - V-Component of mean wind (kt)         */
	/*************************************************************/
{
	float  pinc, usum, vsum, wgt, w1, num, i, ix1;
	float  sfctemp, sfcdwpt, sfcpres, lower, upper;
	Parcel pcl;

	*wdir = RMISSD;
	*wspd = RMISSD;
	*mnu  = RMISSD;
	*mnv  = RMISSD;

	if (!sndg)
	  return;

	if (lplvals.flag == (int)RMISSD)
	  return;

	/* ----- Check for Default Values ----- */
	if ((pbot == -1) || (ptop == -1)) {
	   sfctemp = lplvals.temp;
	   sfcdwpt = lplvals.dwpt;
	   sfcpres = lplvals.pres;
	   ix1 = parcel(-1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	   lower = pcl.lfcpres;
	   upper = pcl.elpres;
	   if (!qc(lower))
	     lower = 850.0;
	   if (!qc(upper))
	     upper = 200.0;
	}

	if (pbot == -1) { pbot = lower; }
	if (ptop == -1) { ptop = upper; }

	pinc = (pbot - ptop) / 20.0;

	if (pinc < 1.0) {
	   usum = i_wndu(pbot, I_PRES) + i_wndu(ptop, I_PRES);
	   vsum = i_wndv(pbot, I_PRES) + i_wndv(ptop, I_PRES);
	   wgt = pbot + ptop;
	}
	else {
	   num = wgt = usum = vsum = 0;
	   for (i = pbot; i >= ptop; i -= pinc) {
	      w1 = 1;
	      usum = usum + i_wndu(i, I_PRES);
	      vsum = vsum + i_wndv(i, I_PRES);
	      //wgt = wgt + w1;
	      num++;
	   }
	}

	*mnu = (usum / num);
	*mnv = (vsum / num);

	if (qc(*mnu) && qc(*mnv)) {
	  *wdir = angle(*mnu, *mnv);
	  *wspd = speed(*mnu, *mnv);
	}
}

void sr_wind(float pbot, float ptop, float stdir, float stspd,
	     float *mnu, float *mnv, float *wdir, float *wspd)
	/*************************************************************/
	/*  SR_WIND                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a pressure-weighted SR mean wind thru the     */
	/*  layer (pbot-ptop).  Default layer is SFC-3KM.            */
	/*                                                           */
	/*  pbot             - Bottom level of layer (mb)            */
	/*  ptop             - Top level of layer (mb)               */
	/*  stdir            - Storm motion dirction (deg)           */
	/*  stspd            - Storm motion speed (kt)               */
	/*  mnu              - U-Component of mean wind (kt)         */
	/*  mnv              - V-Component of mean wind (kt)         */
	/*************************************************************/
{
	float pinc, usum, vsum, wgt, w1, num, i, ix1, stu, stv;
	short idx;

	/* ----- Calculate Storm motion vectors ----- */
	stu = ucomp(stdir, stspd);
	stv = vcomp(stdir, stspd);

	*wdir = RMISSD;
	*wspd = RMISSD;
	*mnu  = RMISSD;
	*mnv  = RMISSD;

	idx = getParmIndex("PRES");

	if (!sndg || idx == -1)
	  return;

	/* ----- Check for Default Values ----- */
	if (pbot == -1) { pbot = sndg[sfc()][idx]; }
	if (ptop == -1) { ptop = i_pres(msl(3000.0)); }

	pinc = (pbot - ptop) / 20.0;

	if (pinc < 1.0) {
	   usum = ((i_wndu(pbot, I_PRES) - stu) * pbot) + 
	          ((i_wndu(ptop, I_PRES) - stu) * ptop);
	   vsum = ((i_wndv(pbot, I_PRES) - stv) * pbot) +
	          ((i_wndv(ptop, I_PRES) - stv) * ptop);
	   /*usum = ((ucomp(i_wdir(pbot, I_PRES), i_wspd(pbot, I_PRES)) - stu) * pbot) + 
                  ((ucomp(i_wdir(ptop, I_PRES), i_wspd(ptop, I_PRES)) - stu) * ptop); 
           vsum = ((vcomp(i_wdir(pbot, I_PRES), i_wspd(pbot, I_PRES)) - stv) * pbot) +        
                  ((vcomp(i_wdir(ptop, I_PRES), i_wspd(ptop, I_PRES)) - stv) * ptop);*/
	   wgt = pbot + ptop;
	}
	else {
	   num = wgt = usum = vsum = 0;
	   for (i = pbot; i >= ptop; i -= pinc) {
	      w1 = i;
	      usum = usum + ((i_wndu(i, I_PRES) - stu) * w1);
	      vsum = vsum + ((i_wndv(i, I_PRES) - stv) * w1);
	      /*usum = usum + ((ucomp(i_wdir(i, I_PRES), i_wspd(i, I_PRES)) - stu) * w1);
              vsum = vsum + ((vcomp(i_wdir(i, I_PRES), i_wspd(i, I_PRES)) - stv) * w1);*/
	      wgt = wgt + w1;
	      num++;
	   }
	}

	*mnu = (usum / wgt);
	*mnv = (vsum / wgt);

	if (qc(*mnu) && qc(*mnv)) {
	  *wdir = angle(*mnu, *mnv);
	  *wspd = speed(*mnu, *mnv);
	}
}

void wind_shear(float pbot, float ptop, float *shu, float *shv,
		float *sdir, float *smag)
	/*************************************************************/
	/*  WIND_SHEAR                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the shear between the wind at (pbot) and      */
	/*  (ptop).  Default lower wind is a 1km mean wind, while    */
	/*  the default upper layer is 3km.                          */
	/*                                                           */
	/*  pbot             - Bottom level of layer (mb)            */
	/*  ptop             - Top level of layer (mb)               */
	/*  mnu              - U-Component of shear (m/s)            */
	/*  mnv              - V-Component of shear (m/s)            */
	/*  sdir             - Direction of shear vector (degrees)   */
	/*  smag             - Magnitude of shear vector (m/s)       */
	/*************************************************************/
{
	float udif, vdif, num, i, ubot, vbot, ix1, ix2, ix3, ix4;
	short idx;

	*shu  = RMISSD;
	*shv  = RMISSD;
	*sdir = RMISSD;
	*smag = RMISSD;

	idx = getParmIndex("PRES");

	if (!sndg || idx == -1)
	  return;

	/* ----- Check for Default Values ----- */
	if (pbot == -1) {
	   pbot = sndg[sfc()][idx];
	   mean_wind(sndg[sfc()][idx], i_pres(msl(1000.0)), &ubot, &vbot,
	             &ix3, &ix4); 
	}
	else {
	   /* 10/9/07 RLT edit */
	   ubot = i_wndu(pbot, I_PRES);
	   vbot = i_wndv(pbot, I_PRES);
	   /*ubot = ucomp((i_wdir(pbot, I_PRES)), (i_wspd(pbot, I_PRES)));
           vbot = vcomp((i_wdir(pbot, I_PRES)), (i_wspd(pbot, I_PRES)));
	   printf("\n ubot = %7.1f   vbot = %7.1f\n", ubot, vbot);*/

	}

	if (ptop == -1) { ptop = i_pres(agl(3000.0)); }

	/* ----- Make sure winds were observed through layer ----- */
	if (qc(i_wndu(ptop, I_PRES)) && qc(i_wndu(pbot, I_PRES))) {

	   /* ----- Calculate Vector Difference ----- */
	   *shu = i_wndu(ptop, I_PRES) - ubot;
	   *shv = i_wndv(ptop, I_PRES) - vbot;
	   /* *shu = ucomp((i_wdir(ptop, I_PRES)), (i_wspd(ptop, I_PRES))) - ubot;
	   *shv = vcomp((i_wdir(ptop, I_PRES)), (i_wspd(ptop, I_PRES))) - vbot; */

	  /*printf( "TOP:  %7.1f  %7.1f  %7.1f %7.1f %7.1f\nBOT:  %7.1f  %7.1f  %7.1f %7.1f %7.1f\nANS:  %7.1f  %7.1f %7.1f %7.1f\n",
		    ptop, i_wdir(ptop, I_PRES), i_wspd(ptop, I_PRES), i_wndu(ptop, I_PRES), i_wndv(ptop, I_PRES), pbot, 
		    angle(ubot, vbot), speed(ubot, vbot), i_wndu(pbot, I_PRES), i_wndv(pbot, I_PRES), angle(*shu, *shv), 
		    speed(*shu, *shv), *shu, *shv);*/  
	

	   if (qc(*shu) && qc(*shv)) {
	     *sdir = angle(*shu, *shv);
	     *smag = speed(*shu, *shv);
	   }
	}
}


float helicity(float lower, float upper, float sdir, float sspd,
	       float *phel, float *nhel)
	/*************************************************************/
	/*  HELICITY                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the storm-relative helicity (m2/s2) of a      */
	/*  layer from LOWER(m, agl) to UPPER(m, agl).  Uses the     */
	/*  storm motion vector (sdir, sspd).                        */
	/*                                                           */
	/*  lower            - Bottom level of layer (m, AGL)[-1=LPL]*/
	/*  upper            - Top level of layer (m, AGL)   [-1=LFC]*/
	/*  sdir             - Storm motion direction (degrees)      */
	/*  sspd             - Storm motion speed (kt)               */
	/*  phel             - Positive helicity in layer (m2/s2)    */
	/*  nhel             - Negative helicity in layer (m2/s2)    */
	/*  RETURN VALUE     - Total helicity (m2/s2)                */
	/*************************************************************/
{
	float  cx, cy, sru1, srv1, sru2, srv2, lyrh;
	float  sfctemp, sfcdwpt, sfcpres, ix1;
	short  i, lptr, uptr, ok, idxz, idxs, idxd;
	Parcel pcl;

	*phel = RMISSD;
	*nhel = RMISSD;

	if (!sndg || numlvl < 1)
	  return RMISSD;

	idxz = getParmIndex("HGHT");
	idxs = getParmIndex("SPED");
	idxd = getParmIndex("DRCT");

	if (idxz == -1 || idxs == -1 || idxd == -1)
	  return RMISSD;

	if (!qc(sspd) || !qc(sdir))
	  return RMISSD;

	/* ----- Check for Default Values ----- */
	if ((upper == -1) || (lower == -1)) {
	   sfctemp = lplvals.temp;
	   sfcdwpt = lplvals.dwpt;
	   sfcpres = lplvals.pres;
	   ix1 = parcel(-1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	   /* Should check for missing values */
	   if (upper == -1 || lower == -1) {
	     ix1 = i_hght(esfc(50.0), I_PRES);
	     if (!qc(ix1)) {
	       //fprintf(stderr,
	        // "helicity: effective surface not found in sounding.\n");
	       return RMISSD;
	     }
	   }
	
	   if (upper == -1) { 
	     upper = agl(ix1 + 3000.0);
	   }
	   if (lower == -1) {
	     lower = agl(ix1);
	   }
	   if (!qc(upper)) {
	     upper = 3000.0;
	   }
	}

	/* ----- See if this is a valid layer ----- */
	ok = 1;
	if (!qc(i_wndu(i_pres(msl(lower)), I_PRES)) ||
	    !qc(i_wndu(i_pres(msl(upper)), I_PRES)))
	  ok = 0;

	/* ----- Make sure winds were observed through layer ----- */
	if (ok) {
	   /* ----- Calculate Storm Motion x,y (kt) ----- */
	   cx = ucomp(sdir, sspd);
	   cy = vcomp(sdir, sspd);

	   /* ----- Find lowest observation in layer ----- */
	   i = 0;
	   while (agl(sndg[i][idxz]) < lower)
	     i++;
	   lptr = i;
	   if (agl(sndg[i][idxz]) == lower)
	     lptr++;

	   /* ----- Find highest observation in layer ----- */
	   i = numlvl-1;
	   while (agl(sndg[i][idxz]) > upper)
	     i--;
	   uptr = i;
	   if (agl(sndg[i][idxz]) == upper)
	     uptr--;

	   /* ----- Start with interpolated bottom layer ----- */
	   sru1 = kt_to_mps(i_wndu(i_pres(msl(lower)), I_PRES) - cx);
	   srv1 = kt_to_mps(i_wndv(i_pres(msl(lower)), I_PRES) - cy);

	   *phel = *nhel = 0.0;

	   for (i = lptr; i <= uptr; i++) {
	      if (qc(sndg[i][idxs]) && qc(sndg[i][idxd])) {
		 sru2 = kt_to_mps(ucomp(sndg[i][idxd], sndg[i][idxs]) - cx);
		 srv2 = kt_to_mps(vcomp(sndg[i][idxd], sndg[i][idxs]) - cy);

		 lyrh = (sru2 * srv1) - (sru1 * srv2);
		 if (lyrh > 0.0)
		   *phel += lyrh;
		 else
		   *nhel += lyrh;
		 sru1 = sru2;
		 srv1 = srv2;
	      }
	   }

	   /* ----- Finish with interpolated top layer ----- */
	   sru2 = kt_to_mps(i_wndu(i_pres(msl(upper)), I_PRES) - cx);
	   srv2 = kt_to_mps(i_wndv(i_pres(msl(upper)), I_PRES) - cy);

	   lyrh = (sru2 * srv1) - (sru1 * srv2);
	   if (lyrh > 0.0)
	     *phel += lyrh;
	   else
	     *nhel += lyrh;

/*
fprintf(stderr, "helicity: layer: %.1f:%.1f phel: %.1f nhel: %.1f\n",
lower, upper, *phel, *nhel);
*/
	   return (*phel + *nhel);
	}

	return RMISSD;
}

void corfidi_MCS_motion(float *fpu, float *fpv, float *fp_dir, float *fp_spd, float *bpu, float *bpv, float *bp_dir, float *bp_spd)
        /*************************************************************/
        /*  CORFIDI_FPMCS 	                                     */
        /*  John Hart  SPC OUN                                       */
	/*							     */
	/*  Calculates the Corfidi forward-propagating mcs motion.   */
        /*************************************************************/
	{
        float u_mw, v_mw, u_mwl, v_mwl, ix1, ix2;
        short pIndex;

        *fpu    = RMISSD;
        *fpv    = RMISSD;
        *fp_dir = RMISSD;
        *fp_spd = RMISSD;

        *bpu    = RMISSD;
        *bpv    = RMISSD;
        *bp_dir = RMISSD;
        *bp_spd = RMISSD;

        pIndex = getParmIndex("PRES");

        if (!sndg || pIndex == -1) return;

        /* 850-300mb mean wind */
        mean_wind_npw(850, 300, &u_mw, &v_mw, &ix1, &ix2);
/*	printf("850-300mb mean wind=%.1f/%.1f    %.1f, %.1f\n", ix1, ix2, u_mw, v_mw); */

        /* Sfc-1500m mean wind */
        mean_wind_npw(sndg[sfc()][pIndex], i_pres(msl(1500.0)), &u_mwl, &v_mwl, &ix1, &ix2);
/*	printf("Sfc-1.5km mean wind=%.1f/%.1f    %.1f, %.1f\n", ix1, ix2, u_mwl, v_mwl); */

	/* Calculate the Upwind-Propagating MCS motion vector */
	*bpu = u_mw - u_mwl;
	*bpv = v_mw - v_mwl;
	*bp_dir = angle(*bpu, *bpv);
	*bp_spd = speed(*bpu, *bpv);

	/* Calculate the Downwind_Propagating MCS motion vector */
	*fpu = u_mw + *bpu;
	*fpv = v_mw + *bpv;
	*fp_dir = angle(*fpu, *fpv);
	*fp_spd = speed(*fpu, *fpv);
	}

float max_wind(float *lvl, float *dir, float *spd, float lower, float upper)
        /*************************************************************/
        /*  MAX_WIND                                                 */
        /*  John Hart/Rich Thompson  SPC OUN                         */
        /*                                                           */
        /*  Finds the maximum wind speed between levels upper and    */
        /*  lower.                                                   */
        /*                                                           */
        /*  *param      =  Returned max wind level (mb)              */
        /*  lower       =  Bottom level of layer (mb) [ -1=SFC]      */
        /*  upper       =  Top level of layer (mb)    [ -1=low 300mb]*/
        /*************************************************************/
{
        short i, okl, oku, lptr, uptr, idxp, idxd, idxs;
        float s1, p_maxspd, pres, maxspd;

        *lvl = RMISSD;
	*dir = RMISSD;
	*spd = RMISSD;

        if (!sndg)
          return RMISSD;

        idxp  = getParmIndex("PRES");
        idxd  = getParmIndex("DRCT");
        idxs = getParmIndex("SPED");
        if (idxp == -1 || idxd == -1 || idxs == -1)
          return RMISSD;

        /* ----- See if default layer is specified ----- */
        if (lower == -1) { lower = sndg[sfc()][idxp];    }
        if (upper == -1) { upper = sndg[sfc()][idxp] - 300.0; }

        /* ----- Make sure this is a valid layer ----- */
        while (!qc(i_wspd(upper, I_PRES))) { upper += 50.0; }
        if (!qc(i_wdir(lower, I_PRES))) { lower = i_pres(sfc()); }

        /* Find lowest observation in layer ----- */
        i = 0;
        while(sndg[i][idxp] > lower)  { i++; }
        while (!qc(sndg[i][idxs])) { i++; }
        lptr = i;
        if( sndg[i][idxp] == lower ) { lptr++; }

        /* ----- Find highest observation in layer ----- */
        i=numlvl-1;
        while( sndg[i][idxp] < upper) { i--;}
        uptr = i;
        if( sndg[i][idxp] == upper ) { uptr--; }

        /* ----- Start with interpolated bottom layer ----- */
        maxspd = i_wspd(lower, I_PRES);
        p_maxspd = lower;

        for ( i = lptr; i <= uptr; i++) {
          if (qc(sndg[i][idxp])) {
            /* ----- Calculate every level that reports a wind speed ----- */
            pres = sndg[i][idxp];
            s1 = i_wspd(pres, I_PRES);
            if (s1 > maxspd) {
               maxspd = s1;
               p_maxspd = pres;
            }
          }
        }

        /* ----- Finish with interpolated top layer ----- */
        s1 = i_wspd(upper, I_PRES);
        if (s1 > maxspd)
          p_maxspd = sndg[i][idxp];

        *lvl = p_maxspd;
	*spd = maxspd;
	*dir = i_wdir(p_maxspd, I_PRES);

        return *spd;
}

