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
#include <sharp95.h>

	/*NP*/
	float ucomp( float wdir, float wspd )
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
	while( wdir > 360) { wdir = wdir - 360; }
	wdir = wdir * PI / 180;
	return wspd * sin(wdir);
	}

	/*NP*/
	float vcomp( float wdir, float wspd )
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
	while( wdir > 360) { wdir = wdir - 360; }
	wdir = wdir * PI/180.0;
	return wspd * cos(wdir);
	}

	/*NP*/
	float angle( float u, float v )
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
	sc = PI/180;
	if ((u == 0) && (v == 0)) return 0;
	if ((u == 0) && (v > 0)) return 360;
	if ((u == 0) && (v < 0)) return 180;
	t1 = atan(v / u) / sc;
	if (u >= 0)
	   {return (float)(90 - t1);}
	else
	   {return (float)(270 - t1);}
	}

	/*NP*/
	void mean_wind( float pbot, float ptop, float *mnu, float *mnv,
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
	float pinc, usum, vsum, wgt, w1, i, ix1;
	float sfctemp, sfcdwpt, sfcpres, lower, upper;
        int num;
	struct _parcel pcl;

        *wspd = -999; *wdir = -999; *mnu = -999; *mnv = -999;

	/* ----- Check for Default Values ----- */
	if((pbot == -1) || (ptop == -1))
	   {
	   sfctemp = lplvals.temp;
	   sfcdwpt = lplvals.dwpt;
	   sfcpres = lplvals.pres;
	   ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	   lower = pcl.lfcpres;
	   upper = pcl.elpres;
	   if(!qc(lower)) { lower = 850; }
	   if(!qc(upper)) { upper = 200; }
	   }

	if( pbot == -1 ) { pbot = lower; }
	if( ptop == -1 ) { ptop = upper; }

        if((!qc(ptop))||(!qc(pbot)))
           return;
        if(!qc(i_hght(pbot)))
           return;
        if(!qc(i_hght(ptop)))
           return;

	pinc = (pbot - ptop) / 20;

	if (pinc < 1)
	   {
	   usum = (i_wndu( pbot ) * pbot) + (i_wndu( ptop ) * ptop);
	   vsum = (i_wndv( pbot ) * pbot) + (i_wndv( ptop ) * ptop);
	   wgt = pbot + ptop;
	   num = 1;
	   }
	else
	   {
	   num = 0;
	   wgt = 0;
	   usum = 0;
	   vsum = 0;
	   for(i = pbot; i >= ptop; i = i - pinc)
	      {
              if((qc(i_wndu( i )))&&(qc(i_wndv( i ))))
                 {
	         w1 = i;
	         usum = usum + (i_wndu( i ) * w1);
	         vsum = vsum + (i_wndv( i ) * w1);
	         wgt = wgt + w1;
	         num++;
	         }
	      }
	   }
        if(num < 1)
           {
           *mnu = -999.; *mnv = -999.; *wdir = -999.; *wspd = -999.;
           }
        else
           {
	   *mnu = (usum / wgt);
	   *mnv = (vsum / wgt);
	   *wdir = angle( *mnu, *mnv);
	   *wspd = (float)(sqrt((*mnu * *mnu) + (*mnv * *mnv)));
           }
	}

	/*NP*/
	void sr_wind( float pbot, float ptop, float stdir, float stspd,
		      float *mnu, float *mnv, float *wdir, float *wspd)
	/*************************************************************/
	/*  SR_WIND                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a pressure-weighted SR mean wind thru the     */
	/*  layer (pbot-ptop).  Default layer is LFC-EL.             */
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

	/* ----- Calculate Storm motion vectors ----- */
	stu = ucomp( stdir, stspd);
	stv = vcomp( stdir, stspd);

        if((!qc(pbot))||(!qc(ptop))||(!qc(stdir))||(!qc(stspd)))
           {
           *mnu = -999; *mnv = -999; *wdir = -999; *wspd = -999;
           return;
           }

	/* ----- Check for Default Values ----- */
	if(pbot == -1) { pbot = sndg[sfc()][1]; }
	if(ptop == -1) { ptop = i_pres(msl(3000)); }

	pinc = (pbot - ptop) / 20;

	if (pinc < 1)
	   {
	   usum = ((i_wndu( pbot ) - stu) * pbot) + ((i_wndu( ptop ) - stu) * ptop);
	   vsum = ((i_wndv( pbot ) - stv) * pbot) + ((i_wndv( ptop ) - stv) * ptop);
	   wgt = pbot + ptop;
	   }
	else
	   {
	   num = 0;
	   wgt = 0;
	   usum = 0;
	   vsum = 0;
	   for(i = pbot; i >= ptop; i = i - pinc)
	      {
              if((qc(i_wndu( i )))&&(qc(i_wndv( i ))))
                 {
	         w1 = i;
	         usum = usum + ((i_wndu( i ) - stu) * w1);
	         vsum = vsum + ((i_wndv( i ) - stv) * w1);
	         wgt = wgt + w1;
	         num++;
	         }
	      }
	   }
	*mnu = (usum / wgt);
	*mnv = (vsum / wgt);
	*wdir = angle( *mnu, *mnv);
	*wspd = (float)(sqrt((*mnu * *mnu) + (*mnv * *mnv)));
	}

	/*NP*/
	void wind_shear( float pbot, float ptop, float *shu, float *shv,
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

	/* ----- Check for Default Values ----- */
	if( pbot == -1 )
	   {
	   pbot = sndg[sfc()][1];
	   mean_wind( sndg[sfc()][1], i_pres(msl(1000)), &ubot, &vbot, &ix3, &ix4);
	   }
	else
	   {
	   ubot = i_wndu( pbot );
	   vbot = i_wndv( pbot );
	   }

	if( ptop == -1 ) { ptop = i_pres(agl(3000)); }

	/* ----- Make sure winds were observed through layer ----- */
	if (qc(i_wndu( ptop )) && qc(i_wndu( pbot )))
	   {

	   /* ----- Calculate Vector Difference ----- */
	   *shu = i_wndu( ptop ) - ubot;
	   *shv = i_wndv( ptop ) - vbot;

	   *sdir = angle( *shu, *shv);
	   *smag = (float)(sqrt((*shu * *shu) + (*shv * *shv)));
	   }
	else
	   {
	   *shu = -999;
	   *shv = -999;
	   *sdir = -999;
	   *smag = -999;
	   }
	}


	/*NP*/
	float helicity( float lower, float upper, float sdir, float sspd,
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
	float cx, cy, sru1, srv1, sru2, srv2, lyrh;
	short i, lptr, uptr, ok;
	float sfctemp, sfcdwpt, sfcpres, ix1;
	struct _parcel pcl;

	/* ----- Check for Default Values ----- */
	if((upper == -1) || (lower == -1))
	   {
	   sfctemp = lplvals.temp;
	   sfcdwpt = lplvals.dwpt;
	   sfcpres = lplvals.pres;
	   ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
           if(upper == -1) { upper = agl(i_hght(esfc(50)) + 3000); }
           if(lower == -1) { lower = agl(i_hght(esfc(50))); }
/*
	   if(upper == -1) { upper = agl(i_hght(pcl.lfcpres)); }
	   if(lower == -1) { lower = agl(i_hght(lplvals.pres)); }
*/
	   if(!qc(upper)) { upper = 3000; }
	   }

	/* ----- See if this is a valid layer ----- */
	ok = 1;
	if(!qc(i_wndu( i_pres(msl(lower))))) { ok = 0; }
	if(!qc(i_wndu( i_pres(msl(upper))))) { ok = 0; }
        if(!qc(sdir)) { ok = 0; }
        if(!qc(sspd)) { ok = 0; }

	/* ----- Make sure winds were observed through layer ----- */
	if(ok)
	   {
	   /* ----- Calculate Storm Motion x,y (kt) ----- */
	   cx = ucomp(sdir, sspd);
	   cy = vcomp(sdir, sspd);

	   /* ----- Find lowest observation in layer ----- */
	   i = 0;
	   while( agl(sndg[i][2]) < lower)  { i++; }
	   lptr = i;
	   if( agl(sndg[i][2]) == lower ) { lptr++; }

	   /* ----- Find highest observation in layer ----- */
	   i = numlvl-1;
	   while( agl(sndg[i][2]) > upper)
	      { i--; }
	   uptr = i;
	   if( agl(sndg[i][2]) == upper ) { uptr--; }

	   /* ----- Start with interpolated bottom layer ----- */
	   sru1 = (i_wndu(i_pres(msl(lower))) - cx) * .51479F;
	   srv1 = (i_wndv(i_pres(msl(lower))) - cy) * .51479F;

	   *phel = 0;
	   *nhel = 0;
	   for( i = lptr; i <= uptr; i++)
	      {
	      if( qc(sndg[i][6]) )
		 {
		 sru2 = (ucomp(sndg[i][5], sndg[i][6]) - cx) * .51479F;
		 srv2 = (vcomp(sndg[i][5], sndg[i][6]) - cy) * .51479F;

		 lyrh = (sru2 * srv1) - (sru1 * srv2);
		 if(lyrh > 0)
		    { *phel += lyrh; }
		 else
		    { *nhel += lyrh; }
		 sru1 = sru2;
		 srv1 = srv2;
		 }
	      }

	   /* ----- Finish with interpolated top layer ----- */
	   sru2 = (i_wndu(i_pres(msl(upper))) - cx) * .51479F;
	   srv2 = (i_wndv(i_pres(msl(upper))) - cy) * .51479F;

	   lyrh = (sru2 * srv1) - (sru1 * srv2);
	   if(lyrh > 0)
	      { *phel += lyrh; }
	   else
	      { *nhel += lyrh; }

	   return *phel + *nhel;
	   }
	else
	   {
	   *phel = -999;
	   *nhel = -999;
	   return -999;
	   }
	}

