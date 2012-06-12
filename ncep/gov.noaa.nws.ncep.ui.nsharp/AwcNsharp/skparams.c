/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Thermodynamic Parameter Calculations                       */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sharp95.h>

	/*NP*/
	float k_index( float *param )
	/*************************************************************/
	/*  K_INDEX                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the K-Index from data in SNDG array.          */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*************************************************************/
	{
	float t8, t5, t7, td7, td8;
        *param = 0;

	t8 = i_temp( 850 ); if( !qc( t8 ) ) {*param = -999;}
	t7 = i_temp( 700 ); if( !qc( t7 ) ) {*param = -999;}
	t5 = i_temp( 500 ); if( !qc( t5 ) ) {*param = -999;}
	td7 = i_dwpt( 700 ); if( !qc( td7 ) ) {*param = -999;}
	td8 = i_dwpt( 850 ); if( !qc( td8 ) ) {*param = -999;}

	if( *param != -999)
	   { *param = t8 - t5 + td8 - (t7 - td7); }

	return *param;
	}

	/*NP*/
	float t_totals( float *param, float *ct, float *vt )
	/*************************************************************/
	/*  T_TOTALS                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Total Totals index from data in           */
	/*  SNDG array.  Value is returned both as (param) and as    */
	/*  a RETURN;.  Cross Totals (ct) and Vertical Totals(vt)    */
	/*  are also returned.                                       */
	/*************************************************************/
	{
	float t8, t5, td8;

        *param = 0;

	t8 = i_temp( 850 );  if( !qc( t8  ) ) {*param = -999;}
	t5 = i_temp( 500 );  if( !qc( t5  ) ) {*param = -999;}
	td8 = i_dwpt( 850 ); if( !qc( td8 ) ) {*param = -999;}

	if( *param != -999)
	   {
	   *vt = t8 - t5;
	   *ct = td8 - t5;
	   *param = *vt + *ct;
	   }

	return *param;
	}


	/*NP*/
	float precip_water( float *param, float lower, float upper )
	/*************************************************************/
	/*  PRECIP_WATER                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Precipitable Water from data in           */
	/*  SNDG array within specified layer.                       */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*                                                           */
	/*  *param      =  Returned precipitable water value (in)    */
	/*  lower       =  Bottom level of layer (mb) [ -1=SFC]      */
	/*  upper       =  Top level of layer (mb)    [ -1=400]      */
	/*************************************************************/
	{
	short i, okl, oku, lptr, uptr;
	float d1, p1, d2, p2, tot, w1, w2, wbar;

        *param = 0;

	/* ----- See if default layer is specified ----- */
	if( lower == -1) { lower = sndg[sfc()][1]; }
	if( upper == -1) { upper = 400.0; }

	/* ----- Make sure this is a valid layer ----- */
	while( !qc( i_dwpt( upper ))) { upper += 50; if(upper > lower) return(-999.);}
	if( !qc( i_temp( lower ))) { lower = i_pres( sfc() ); }

	if( qc(*param))
	   {
	   /* ----- Find lowest observation in layer ----- */
	   i = 0;
	   while( sndg[i][1] > lower)  { i++; }
	   while ( !qc(sndg[i][4]) ) { i++; }
	   lptr = i;
	   if( sndg[i][1] == lower ) { lptr++; }

	   /* ----- Find highest observation in layer ----- */
	   i=numlvl;
	   while( sndg[i][1] < upper) { i--;}
	   uptr = i;
	   if( sndg[i][1] == upper ) { uptr--; }

	   /* ----- Start with interpolated bottom layer ----- */
	   d1 = i_dwpt( lower );
	   p1 = lower;

	   tot = 0;
	   for( i = lptr; i <= uptr; i++)
	      {
	      if( qc(sndg[i][4]) )
		 {
		 /* ----- Calculate every level that reports a dwpt ----- */
		 d2 = sndg[i][4];
		 p2 = sndg[i][1];
		 w1 = mixratio( p1, d1 );
		 w2 = mixratio( p2, d2 );
		 wbar = (w1 + w2) / 2;
		 tot = tot + wbar * (p1 - p2);
		 d1 = d2;
		 p1 = p2;
		 }
	      }

	   /* ----- Finish with interpolated top layer ----- */
	   d2 = i_dwpt( upper );
	   p2 = upper;
	   w1 = mixratio( p1, d1 );
	   w2 = mixratio( p2, d2 );
	   wbar = (w1 + w2) / 2;
	   tot = tot + wbar * (p1 - p2);

	   /* ----- Convert to inches (from g*mb/kg) ----- */
	   *param = tot * .00040173;
	   }
	return *param;
	}

	/*NP*/
	float parcel( float lower, float upper, float pres, float temp,
		      float dwpt, struct _parcel *pcl)
	/*************************************************************/
	/*  PARCEL                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts specified parcel, calculating various levels and   */
	/*  parameters from data in SNDG array.  B+/B- are           */
	/*  calculated based on layer (lower-upper).  Bplus is       */
	/*  returned value and in structure.                         */
	/*                                                           */
	/*  All calculations use the virtual temperature correction. */
	/*                                                           */
	/*  lower       =  Bottom level of layer (mb)  [ -1=SFC]     */
	/*  upper       =  Top level of layer (mb)     [ -1=TOP]     */
	/*  pres        =  LPL pressure (mb)                         */
	/*  temp        =  LPL temperature (c)                       */
	/*  dwpt        =  LPL dew point (c)                         */
	/*  pcl         =  returned _parcel structure                */
	/*************************************************************/
	{
	short i;
	short lptr, uptr;
	float te1, pe1, te2, pe2, h1, h2, lyre, tdef1, tdef2, totp, totp3000, 
	      totn;
	float te3, pe3, h3, tp1, tp2, tp3, tdef3, lyrf, lyrlast, pelast;
	float tote, totx, ls1, cap_strength, li_max, li_maxpres;
	float cap_strengthpres, tpx, ix1;
        float pp, pp1, pp2, dz, dpt, det;
        float blupper, bltheta, blmr, cinh_old;
        float theta_parcel, tv_env_bot, tv_env_top, lclhght;
        
        if (! get_g_TvParcelCor()) {
          return parcelx(lower,upper,pres,temp,dwpt,pcl);          
        }

	/* ----- Initialize PCL values ----- */
	pcl->lplpres = pres;
	pcl->lpltemp = temp;
	pcl->lpldwpt = dwpt;
	pcl->blayer = lower;
	pcl->tlayer = upper;
	pcl->entrain = 0;
	pcl->lclpres = -999;
	pcl->lfcpres = -999;
	pcl->elpres = -999;
	pcl->mplpres = -999;
	pcl->bplus = -999;
	pcl->bplus3000 = -999;
	pcl->bminus = -999;
	pcl->bfzl = -999;
	pcl->li5 = -999;
	pcl->li3 = -999;
	pcl->brn = -999;
	pcl->limax = -999;
	pcl->limaxpres = -999;
	pcl->cap = -999;
	pcl->cappres = -999;
	lyre = -1;
	cap_strength = -999;
	cap_strengthpres = -999;
	li_max = -999;
	li_maxpres = -999;
	totp = 0;
	totn = 0;
	tote = 0;

        if( !qc( dwpt ) ) { return -999; }

	/* ----- See if default layer is specified ----- */
	if( lower == -1)
	   {
	   lower = sndg[sfc()][1];
	   pcl->blayer = lower;
	   }
	if( upper == -1)
	   {
	   upper = sndg[numlvl-1][1];
	   pcl->tlayer = upper;
	   }

	/* ----- Make sure this is a valid layer ----- */
	if( lower > pres )
	   {
	   lower = pres;
	   pcl->blayer = lower;
	   }
	if( !qc( i_vtmp( upper ))) { return -999; }
	if( !qc( i_vtmp( lower ))) { return -999; }

	/* ----- Begin with Mixing Layer (LPL-LCL) ----- */
	te1 = i_vtmp( pres );
	pe1 = lower;
	h1 =  i_hght( pe1 );
	tp1 = virtemp( pres, temp, dwpt);

	drylift(pres, temp, dwpt, &pe2, &tp2);
        /* Define top of layer as LCL pres */
        blupper = pe2;
	h2 =  i_hght( pe2 );
	te2 = i_vtmp( pe2 );
	pcl->lclpres = pe2;
        /********************************************************/
        /* calculate lifted parcel theta for use in iterative CINH loop below */
	/* recall that lifted parcel theta is CONSTANT from LPL to LCL */
	theta_parcel = theta(pe2, tp2, 1000.0);
	
	/* environmental theta and mixing ratio at LPL */
	bltheta = theta(pres, i_temp(pres), 1000.0);
	blmr = mixratio(pres, dwpt);
        
        /* ----- Accumulate CINH in mixing layer below the LCL ----- */
	/* This will be done in 10mb increments, and will use the    */
	/* virtual temperature correction where possible.            */

	/* initialize negative area to zero and iterate from LPL to LCL in 10mb increments */
	totn=0;
	for (pp=lower; pp > blupper; pp-=10.0) {
	   pp1 = pp; 
	   pp2 = pp-10.0;
	   if (pp2 < blupper)
	     pp2=blupper;

	   dz = i_hght(pp2) - i_hght(pp1);


	   /* calculate difference between Tv_parcel and Tv_environment at top and bottom of 10mb layers */	
	   /* make use of constant lifted parcel theta and mixr from LPL to LCL */
	   tv_env_bot = virtemp(pp1, theta(pp1, i_temp(pp1), 1000.0), i_dwpt(pp1));
           tdef1 = ((virtemp(pp1, theta_parcel, temp_at_mixrat(blmr, pp1))) - tv_env_bot) /
	           (tv_env_bot + 273.15);

/*	   tdef2 = (virtemp(pp2, theta_parcel, temp_at_mixrat(blmr, pp2)) - 
	     i_vtmp(pp2, I_PRES)) / (i_vtmp(pp2, I_PRES) + 273.15);
*/
	   tv_env_top = virtemp(pp2, theta(pp2, i_temp(pp2), 1000.0), i_dwpt(pp2));
           tdef2 = ((virtemp(pp2, theta_parcel, temp_at_mixrat(blmr, pp2))) - tv_env_top) /
                   (tv_env_top + 273.15);

           lyre = 9.8 * (tdef1 + tdef2) / 2.0 * dz;
	   if (lyre < 0)
	     totn+=lyre;
	/* printf("\nlayer energy below LCL = %.1f\n", lyre);
	printf("\ntotal CAPE below LCL = %.1f\n", totp); */
	}
        
        /*********************************************************/
        
	if( lower > pe2 )
	   {
	   lower = pe2;
	   pcl->blayer = lower;
	   }

	/* ----- Find lowest observation in layer ----- */
	i = 0;
	while( sndg[i][1] > lower)  { i++; }
	while ( !qc(sndg[i][4]) ) { i++; }
	lptr = i;
	if( sndg[i][1] == lower ) { lptr++; }

	/* ----- Find highest observation in layer ----- */
	i=numlvl-1;
	while(sndg[i][1] < upper) { i--; }
	uptr = i;
	if( sndg[i][1] == upper ) { uptr--; }

	/* ----- Start with interpolated bottom layer ----- */
	pe1 = lower;
	h1 =  i_hght( pe1 );
	te1 = i_vtmp( pe1 );
	tp1 = wetlift(pe2, tp2, pe1);

	totp = 0;
	totp3000 = 0;
        lyre = 0;
        
	for( i = lptr; i < numlvl; i++)
	   {
	   if( qc(sndg[i][3]) )
	      {
	      /* ----- Calculate every level that reports a temp ----- */
	      pe2 = sndg[i][1];
	      h2 =  sndg[i][2];
	      te2 = i_vtmp( pe2 );
	      tp2 = wetlift(pe1, tp1, pe2);
	      tdef1 = (virtemp(pe1, tp1, tp1) - te1) / (te1 + 273.15);
	      tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15);
	      lyrlast = lyre;
	      lyre = 9.8F * (tdef1 + tdef2) / 2.0F * (h2 - h1);

	      /* ----- Check for Max LI ----- */
	      if ((virtemp(pe2, tp2, tp2) - te2) > li_max)
		 {
		 li_max = virtemp(pe2, tp2, tp2) - te2;
		 li_maxpres = pe2;
		 }

	      /* ----- Check for Max Cap Strength ----- */
	      if ((te2 - virtemp(pe2, tp2, tp2)) > cap_strength)
		 {
		 cap_strength = te2 - virtemp(pe2, tp2, tp2);
		 cap_strengthpres = pe2;
		 }

	      if( lyre > 0 ) {
		 totp += lyre;
		 if (agl(sndg[i][2]) <= 3000)
		   totp3000 += lyre;
              }
	      else
		 if(pe2 > 500) totn += lyre;

	      tote += lyre;
	      pelast = pe1;
	      pe1 = pe2;
	      h1 = h2;
	      te1 = te2;
	      tp1 = tp2;

	      /* ----- Is this the top of given layer ----- */
	      if((i >= uptr) && ( !qc(pcl->bplus)))
		 {
		 pe3 = pe1;
		 h3 = h1;
		 te3 = te1;
		 tp3 = tp1;
		 lyrf = lyre;

		 if( lyrf > 0 )
		    {
		    pcl->bplus = totp - lyrf;
		    pcl->bminus = totn;
		    }
		 else
		    {
		    pcl->bplus = totp;
		    if(pe2 > 500)
		       pcl->bminus = totn - lyrf;
		    else
		       pcl->bminus = totn;
		    }

		 pe2 = upper;
		 h2 = i_hght( pe2 );
		 te2 = i_vtmp( pe2 );
		 tp2 = wetlift(pe3, tp3, pe2);
		 tdef3 = (virtemp(pe3, tp3, tp3) - te3) / (te3 + 273.15);
		 tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15);
		 lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		 if( lyrf > 0 )
		    pcl->bplus += lyrf;
		 else
		    if(pe2 > 500) pcl->bminus += lyrf;

		 if( pcl->bplus == 0 ) { pcl->bminus = 0; }
		 pcl->bplus3000 = totp3000;
		 }

	      /* ----- Is this the freezing level ----- */
	      if((te2 <= 0) && ( !qc(pcl->bfzl)))
		 {
		 pe3 = pelast;
		 h3 = i_hght( pe3 );
		 te3 = i_vtmp( pe3 );
		 tp3 = wetlift(pe1, tp1, pe3);
		 lyrf = lyre;

		 if( lyrf > 0 )
		    { pcl->bfzl = totp - lyrf; }
		 else
		    { pcl->bfzl = totp; }

		 if ( qc (pe2 = temp_lvl( 0, &pe2) ) )
		   {
		   h2 = i_hght( pe2 );
		   te2 = i_vtmp( pe2 );
		   tp2 = wetlift(pe3, tp3, pe2);
		   tdef3 = (virtemp(pe3, tp3, tp3) - te3) / 
						(te3 + 273.15);
		   tdef2 = (virtemp(pe2, tp2, tp2) - te2) / 
						(te2 + 273.15);
		   lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		   if( lyrf > 0 )
		      { pcl->bfzl += lyrf; }
		   }
		 }

	      /* ----- LFC Possibility ----- */
	      if(( lyre >= 0 ) && ( lyrlast <= 0 ))
		 {
		 tp3 = tp1;
		 te3 = te1;
		 pe2 = pe1;
		 pe3 = pelast;
		 while( i_vtmp( pe3 ) > virtemp( pe3, wetlift(pe2, tp3, pe3), wetlift(pe2, tp3, pe3)) ) { pe3 -= 5; }
		 pcl->lfcpres = pe3;
                 cinh_old = totn;

		 tote = 0;
		 pcl->elpres = -999;

		 li_max = -999;

		 if(cap_strength < 0) { cap_strength = 0; }
		 pcl->cap = cap_strength;
		 pcl->cappres = cap_strengthpres;
		 }

	      /* ----- EL Possibility ----- */
	      if(( lyre <= 0.0 ) && ( lyrlast >= 0.0 ))
		 {
		 tp3 = tp1;
		 te3 = te1;
		 pe2 = pe1;
		 pe3 = pelast;
		 while ( i_vtmp( pe3 ) < virtemp( pe3, wetlift(pe2, tp3, pe3), wetlift(pe2, tp3, pe3)) ) { pe3 -= 5; }
		 pcl->elpres = pe3;
		 pcl->mplpres = -999;
		 pcl->limax = -li_max;
		 pcl->limaxpres = li_maxpres;
		 }

	      /* ----- MPL Possibility ----- */
	      if(( (tote <= 0) && ( !qc( pcl->mplpres ) ) && ( qc( pcl->elpres ))))
		 {
		 pe3 = pelast;
		 h3 = i_hght( pe3 );
		 te3 = i_vtmp( pe3 );
		 tp3 = wetlift(pe1, tp1, pe3);
		 totx = tote - lyre;

		 pe2 = pelast;
		 while( totx > 0 )
		    {
		    pe2 -= 1;
		    te2 = i_vtmp( pe2 );
		    tp2 = wetlift(pe3, tp3, pe2);
		    h2 = i_hght( pe2 );
		    tdef3 = (virtemp(pe3, tp3, tp3) - te3) / (te3 + 273.15);
		    tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15);
		    lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		    totx += lyrf;

		    tp3 = tp2;
		    te3 = te2;
		    pe3 = pe2;
		    }
		 pcl->mplpres = pe2;
		 }

	      /* ----- 500mb Lifted Index ----- */
	      if((sndg[i][1] <= 500) && (pcl->li5 == -999))
		 {
		 /* pcl->li5 = sndg[i][3] - virtemp(500, tp1, tp1); */
                 pcl->li5 = i_vtmp(500) - virtemp(500, wetlift(pe1, tp1, 500), wetlift(pe1, tp1, 500));
		 }

	      /* ----- 300mb Lifted Index ----- */
	      if((sndg[i][1] == 300) && (pcl->li3 == -999))
		 {
		 /* pcl->li3 = sndg[i][3] - virtemp(300, tp1, tp1); */
                 pcl->li3 = i_vtmp(300) - virtemp(300, wetlift(pe1, tp1, 300), wetlift(pe1, tp1, 300));
		 }
	      }
	   }
	/* ----- Calculate BRN if available ----- */
	pcl->brn = bulk_rich( *pcl, &ix1 );
        pcl->bminus = cinh_old;
        if (pcl->bplus == 0.0) pcl->bminus = 0;
	return pcl->bplus;
	}

	/*NP*/
	float parcelx( float lower, float upper, float pres, float temp,
		       float dwpt, struct _parcel *pcl)
	/*************************************************************/
	/*  PARCELX                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts specified parcel, calculating various levels and   */
	/*  parameters from data in SNDG array.  B+/B- are           */
	/*  calculated based on layer (lower-upper).  Bplus is       */
	/*  returned value and in structure.                         */
	/*                                                           */
	/*  No virtual corrections are used.                         */
	/*                                                           */
	/*  lower       =  Bottom level of layer (mb)  [ -1=SFC]     */
	/*  upper       =  Top level of layer (mb)     [ -1=TOP]     */
	/*  pres        =  LPL pressure (mb)                         */
	/*  temp        =  LPL temperature (c)                       */
	/*  dwpt        =  LPL dew point (c)                         */
	/*  pcl         =  returned _parcel structure                */
	/*************************************************************/
	{
	short i, lptr, uptr;
	float te1, pe1, te2, pe2, h1, h2, lyre, tdef1, tdef2, totp, totn;
	float te3, pe3, h3, tp1, tp2, tp3, tdef3, lyrf, lyrlast, pelast;
	float tote, totx, ls1, cap_strength, li_max, li_maxpres;
	float cap_strengthpres, tpx, ix1;
        float pp, pp1, pp2, dz, dpt, det;
        float blupper, bltheta, blmr, cinh_old;
        float theta_parcel, t_env_bot, t_env_top, lclhght;

	/* ----- Initialize PCL values ----- */
	pcl->lplpres = pres;
	pcl->lpltemp = temp;
	pcl->lpldwpt = dwpt;
	pcl->blayer = lower;
	pcl->tlayer = upper;
	pcl->entrain = 0;
	pcl->lclpres = -999;
	pcl->lfcpres = -999;
	pcl->elpres = -999;
	pcl->mplpres = -999;
	pcl->bplus = -999;
	pcl->bminus = -999;
	pcl->bfzl = -999;
	pcl->li5 = -999;
	pcl->li3 = -999;
	pcl->brn = -999;
	pcl->limax = -999;
	pcl->limaxpres = -999;
	pcl->cap = -999;
	pcl->cappres = -999;

	lyre = -1;
	cap_strength = -999;
	cap_strengthpres = -999;
	li_max = -999;
	li_maxpres = -999;
	totp = 0;
	totn = 0;

	/* ----- See if default layer is specified ----- */
	if( lower == -1)
	   {
	   lower = sndg[sfc()][1];
	   pcl->blayer = lower;
	   }
	if( upper == -1)
	   {
	   upper = sndg[numlvl-1][1];
	   pcl->tlayer = upper;
	   }

	/* ----- Make sure this is a valid layer ----- */
	if( lower > pres )
	   {
	   lower = pres;
	   pcl->blayer = lower;
	   }
	if( !qc( i_temp( upper ))) { return -999; }
	if( !qc( i_temp( lower ))) { return -999; }

	/* ----- Begin with Mixing Layer (LPL-LCL) ----- */
	te1 = i_temp( pres );
	pe1 = lower;
	h1 =  i_hght( pe1 );
	tp1 = temp;

	drylift(pres, temp, dwpt, &pe2, &tp2);
        /* define top of layer as LCL pres */
	blupper = pe2;
	h2 =  i_hght( pe2 );
	te2 = i_temp( pe2 );
	pcl->lclpres = pe2;
        /**********************************************************************/
        /* calculate lifted parcel theta for use in iterative CINH loop below */
	/* recall that lifted parcel theta is CONSTANT from LPL to LCL */
	theta_parcel = theta(pe2, tp2, 1000.0);
	
	/* environmental theta and mixing ratio at LPL */
	bltheta = theta(pres, i_temp(pres), 1000.0);
	blmr = mixratio(pres, dwpt);
        
        /* ----- Accumulate CINH in mixing layer below the LCL ----- */
	/* This will be done in 10mb increments, and will use the    */
	/* virtual temperature correction where possible.            */
       
	/* initialize negative area to zero and iterate from LPL to LCL in 10mb increments */
	totn=0;
	for (pp=lower; pp > blupper; pp-=10.0) {
	   pp1 = pp; 
	   pp2 = pp-10.0;
	   if (pp2 < blupper)
	     pp2=blupper;

	   dz = i_hght(pp2) - i_hght(pp1);

	   /* calculate difference between T_parcel and T_environment at top and bottom of 10mb layers */	
	   /* make use of constant lifted parcel theta from LPL to LCL */
           t_env_bot = theta(pp1, i_temp(pp1), 1000.0);
           
           tdef1 = (theta_parcel - t_env_bot) / (t_env_bot + 273.15);
           t_env_top = theta(pp2, i_temp(pp2), 1000.0);
           tdef2 = (theta_parcel - t_env_top)/(t_env_top + 273.15);

           lyre = 9.8 * (tdef1 + tdef2) / 2.0 * dz;
	   if (lyre < 0)
	     totn+=lyre;
	   /* printf("\nlayer energy below LCL = %.1f\n", lyre);
	   printf("\ntotal CAPE below LCL = %.1f\n", totp); */
	}
       
        /**********************************************************************/
	if( lower > pe2 )
	   {
	   lower = pe2;
	   pcl->blayer = lower;
	   }

	/* ----- Find lowest observation in layer ----- */
	i = 0;
	while( sndg[i][1] > lower)  { i++; }
	while ( !qc(sndg[i][4]) ) { i++; }
	lptr = i;
	if( sndg[i][1] == lower ) { lptr++; }

	/* ----- Find highest observation in layer ----- */
	i=numlvl-1;
	while(sndg[i][1] < upper) { i--; }
	uptr = i;
	if( sndg[i][1] == upper ) { uptr--; }

	/* ----- Start with interpolated bottom layer ----- */
	pe1 = lower;
	h1 =  i_hght( pe1 );
	te1 = i_temp( pe1 );
	tp1 = wetlift(pe2, tp2, pe1);

	totp = 0;
        lyre = 0;
	
	for( i = lptr; i < numlvl; i++)
	   {
	   if( qc(sndg[i][3]) )
	      {
	      /* ----- Calculate every level that reports a temp ----- */
	      pe2 = sndg[i][1];
	      h2 =  sndg[i][2];
	      te2 = i_temp( pe2 );
	      tp2 = wetlift(pe1, tp1, pe2);
	      tdef1 = (tp1 - te1) / (te1 + 273.15);
	      tdef2 = (tp2 - te2) / (te2 + 273.15);
	      lyrlast = lyre;
	      lyre = 9.8F * (tdef1 + tdef2) / 2.0F * (h2 - h1);

	      /* ----- Check for Max LI ----- */
	      if ((tp2 - te2) > li_max)
		 {
		 li_max = tp2 - te2;
		 li_maxpres = pe2;
		 }

	      /* ----- Check for Max Cap Strength ----- */
	      if ((te2 - tp2) > cap_strength)
		 {
		 cap_strength = te2 - tp2;
		 cap_strengthpres = pe2;
		 }

	      if( lyre > 0 )
		 { totp += lyre; }
	      else
		 {
		 if(pe2 > 500) { totn += lyre; }
		 }

	      tote += lyre;
	      pelast = pe1;
	      pe1 = pe2;
	      h1 = h2;
	      te1 = te2;
	      tp1 = tp2;

	      /* ----- Is this the top of given layer ----- */
	      if((i >= uptr) && ( !qc(pcl->bplus)))
		 {
		 pe3 = pe1;
		 h3 = h1;
		 te3 = te1;
		 tp3 = tp1;
		 lyrf = lyre;

		 if( lyrf > 0 )
		    {
		    pcl->bplus = totp - lyrf;
		    pcl->bminus = totn;
		    }
		 else
		    {
		    pcl->bplus = totp;
		    if(pe2 > 500)
		       { pcl->bminus = totn - lyrf; }
		    else
		       { pcl->bminus = totn; }
		    }

		 pe2 = upper;
		 h2 = i_hght( pe2 );
		 te2 = i_temp( pe2 );
		 tp2 = wetlift(pe3, tp3, pe2);
		 tdef3 = (tp3 - te3) / (te3 + 273.15);
		 tdef2 = (tp2 - te2) / (te2 + 273.15);
		 lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		 if( lyrf > 0 )
		    { pcl->bplus += lyrf; }
		 else
		    { if(pe2 > 500) { pcl->bminus += lyrf; } }

		 if( pcl->bplus == 0 ) { pcl->bminus = 0; }
		 }

	      /* ----- Is this the freezing level ----- */
	      if((te2 <= 0) && ( !qc(pcl->bfzl)))
		 {
		 pe3 = pelast;
		 h3 = i_hght( pe3 );
		 te3 = i_temp( pe3 );
		 tp3 = wetlift(pe1, tp1, pe3);
		 lyrf = lyre;

		 if( lyrf > 0 )
		    { pcl->bfzl = totp - lyrf; }
		 else
		    { pcl->bfzl = totp; }

		 pe2 = temp_lvl( 0, &pe2);
		 h2 = i_hght( pe2 );
		 te2 = i_temp( pe2 );
		 tp2 = wetlift(pe3, tp3, pe2);
		 tdef3 = (tp3 - te3) / (te3 + 273.15);
		 tdef2 = (tp2 - te2) / (te2 + 273.15);
		 lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		 if( lyrf > 0 )
		    { pcl->bfzl += lyrf; }
		 }

	      /* ----- LFC Possibility ----- */
	      if(( lyre >= 0 ) && ( lyrlast <= 0 ))
		 {
		 tp3 = tp1;
		 te3 = te1;
		 pe2 = pe1;
		 pe3 = pelast;
		 while( i_temp( pe3 ) > wetlift(pe2, tp3, pe3) ) { pe3 -= 5; }
		 pcl->lfcpres = pe3;
                 cinh_old = totn;

		 tote = 0;
		 pcl->elpres = -999;

		 li_max = -999;
                 
                 if (cap_strength < 0.0) cap_strength = 0.0;

		 pcl->cap = cap_strength;
		 pcl->cappres = cap_strengthpres;
		 }

	      /* ----- EL Possibility ----- */
	      if(( lyre <= 0.0 ) && ( lyrlast >= 0.0 ))
		 {
		 tp3 = tp1;
		 te3 = te1;
		 pe2 = pe1;
		 pe3 = pelast;
		 while ( i_temp( pe3 ) < wetlift(pe2, tp3, pe3) ) { pe3 -= 5; }
		 pcl->elpres = pe3;
		 pcl->mplpres = -999;
		 pcl->limax = -li_max;
		 pcl->limaxpres = li_maxpres;
		 }

	      /* ----- MPL Possibility ----- */
	      if(( (tote <= 0) && ( !qc( pcl->mplpres ) ) && ( qc( pcl->elpres ))))
		 {
		 pe3 = pelast;
		 h3 = i_hght( pe3 );
		 te3 = i_temp( pe3 );
		 tp3 = wetlift(pe1, tp1, pe3);
		 totx = tote - lyre;

		 pe2 = pelast;
		 while( totx > 0 )
		    {
		    pe2 -= 1;
		    te2 = i_temp( pe2 );
		    tp2 = wetlift(pe3, tp3, pe2);
		    h2 = i_hght( pe2 );
		    tdef3 = (tp3 - te3) / (te3 + 273.15);
		    tdef2 = (tp2 - te2) / (te2 + 273.15);
		    lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
		    totx += lyrf;

		    tp3 = tp2;
		    te3 = te2;
		    pe3 = pe2;
		    }
		 pcl->mplpres = pe2;
		 }

	      /* ----- 500mb Lifted Index ----- */
	      if((sndg[i][1] <= 500) && (pcl->li5 == -999))
		 {
                 pcl->li5 = i_temp(500) - wetlift(pe1,tp1,500);
		 }

	      /* ----- 300mb Lifted Index ----- */
	      if((sndg[i][1] <= 300) && (pcl->li3 == -999))
		 {
                 pcl->li3 = i_temp(300) - wetlift(pe1, tp1, 300);
		 }
	      }
	   }
	/* ----- Calculate BRN if available ----- */
	pcl->brn = bulk_rich( *pcl, &ix1 );
        pcl->bminus = cinh_old;
        if (pcl->bplus == 0.0) pcl->bminus = 0;
	return pcl->bplus;
	}

	/*NP*/
	float temp_lvl( float temp, float *param )
	/*************************************************************/
	/*  TEMP_LVL                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the level (mb) of the first occurrence of     */
	/*  (temp) in the environment.                               */
	/*                                                           */
	/*  *param      =  Returned level (mb).                      */
	/*  temp        =  Temperature (c) to search for.            */
	/*************************************************************/
	{
	short i;
	float p0;
	double nm1, nm2, nm3;

	for( i = 0; i < numlvl; i++)
	   {
	   if(( qc(sndg[i][3]) ) && ( sndg[i][3] <= temp ))
	      {
	      if(i == 0) { return -999; }
	      if( sndg[i][3] == temp ) { return sndg[i][1]; }

	      p0 = sndg[i-1][1];
	      nm1 = temp - i_temp(p0);
	      nm2 = sndg[i][3] - i_temp(p0);
	      nm3 = log( sndg[i][1] / p0 );
	      *param = (float)(p0 * exp(( nm1 / nm2) * nm3));
	      return *param;
	      }
	   }
	   return -999;
	}
	
	int mult_temp_lvl(float temp,float params[2],int *number)
	/*************************************************************/
	/*  MULT_TEMP_LVL                                            */
	/*  Larry J. Hinson, AWC                                     */
	/*                                                           */
	/*  Calculates the levels (mb) of occurrence(s) of temp      */
	/*  at the lowest and highest levels in the environment.     */
	/*                                                           */
	/*  temp        =  Temperature (c) to search for.            */
	/*  params[2]   =  Returned levels (mb).                     */
	/*  number      =  Number of levels found  0,1, or 2         */
	/*                                                           */
	/*Log:                                                       */
	/* L.Hinson, AWC  3/03  Initialized firsttemp                */
	/* L.Hinson, AWC  10/03 Corrected sndg[i][3] to sndg[i][1]   */
	/*                        compare to params [height (m)]     */
	/*************************************************************/
	{
	short i;
	float p0,firsttemp;
	double nm1, nm2, nm3;
	int colder=-1;
	int firsttime=-1;
	*number=0;
	params[0]=-999.0;
	params[1]=-999.0;
	firsttemp=-999.0;
	for (i=0;i<numlvl;i++) 
	  if (qc(sndg[i][3])) {
	    if (firsttime) {
	      firsttemp=sndg[i][3];
	      firsttime=0;
	    }
    	    if (sndg[i][3]>=temp) {
	      colder=0;
	      break;
	    }

	  }
	;
	if (colder)
	  return -1;
	
        if (firsttemp>=temp) {
	  for( i = 0; i < numlvl; i++)
	  {
	     if(( qc(sndg[i][3]) ) && ( sndg[i][3] <= temp )) {
	        if( sndg[i][3] == temp ) { 
	          params[0]=sndg[i][1];
       	          (*number)++;
	          break;
                }
                if (i>0) {
     	          p0 = sndg[i-1][1];
	          nm1 = temp - i_temp(p0);
	          nm2 = sndg[i][3] - i_temp(p0);
	          nm3 = log( sndg[i][1] / p0 );
	          params[0] = (float)(p0 * exp(( nm1 / nm2) * nm3));
	          (*number)++;
	          break;
	        }
             }
	  }
        } else if (firsttemp < temp ) {
          for( i = 0; i < numlvl; i++)
	  {
	     if(( qc(sndg[i][3]) ) && ( sndg[i][3] >= temp )) {
	        if( sndg[i][3] == temp ) { 
	          params[0]=sndg[i][1];
       	          (*number)++;
	          break;
                }
                if (i>0) {
     	          p0 = sndg[i-1][1];
	          nm1 = temp - i_temp(p0);
	          nm2 = sndg[i][3] - i_temp(p0);
	          nm3 = log( sndg[i][1] / p0 );
	          params[0] = (float)(p0 * exp(( nm1 / nm2) * nm3));
	          (*number)++;
	          break;
	        }
             }
	  }
        }
        /* Find Upper Freezing Level */
        for ( i = numlvl-1; i > 0; i--)
        {
	  if((qc(sndg[i][3])) && (sndg[i][3] >= temp)) {
	     /* Change subscript 3 to 1 on sndg compare to params */
	     if ( sndg[i][3] == temp && sndg[i][1] != params[0]) {
	        params[1]=sndg[i][1];
       	        (*number)++;
       	        break;
             }
	     if (i > 0 && i<numlvl-1 && sndg[i][1] != params[0]) {
	       p0=sndg[i][1];
               nm1 = temp - i_temp(p0);
	       nm2 = sndg[i+1][3] - i_temp(p0);
	       nm3 = log( sndg[i+1][1] / p0 );
               params[1] = (float)(p0 * exp(( nm1 / nm2) * nm3));
       	       if (fabs(params[0]-params[1])>.01) {
                 (*number)++;
               }
               break;
             }
          }
        }
        return 0;
        }
        


	/*NP*/
	float wb_lvl( float temp, float *param )
	/*************************************************************/
	/*  WB_LVL                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the level (mb) of the given Wet-bulb          */
	/*  temperature.                                             */
	/*                                                           */
	/*  *param      =  Returned level (mb).                      */
	/*  temp        =  Wet-bulb temperature (c) to search for.   */
	/*************************************************************/
	{
	short i;
	float p0;
	double nm1, nm2, nm3, wb1, wb0;

	for( i = 0; i < numlvl; i++)
	   {
	   if(( qc(sndg[i][3]) ) && ( qc(sndg[i][4])) )
	      {
	      wb1 = wetbulb( sndg[i][1], sndg[i][3], sndg[i][4] );
	      if( wb1 < temp )
		 {
		 if(i == 0) { return -999; }
		 if( wb1 == temp ) { return sndg[i][1]; }

		 p0 = sndg[i-1][1];
		 wb0 = wetbulb( p0, i_temp( p0 ), i_dwpt( p0 ));
		 nm1 = temp - wb0;
		 nm2 = wb1 - wb0;
		 nm3 = log( sndg[i][1] / p0 );

		 *param = (float)(p0 * exp(( nm1 / nm2) * nm3));
		 return *param;
		 }
	      }
	   }
	   return -999;
	}



	/*NP*/
	float mean_mixratio( float *param, float lower, float upper )
	/*************************************************************/
	/*  MEAN_MIXRATIO                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Mean Mixing Ratio from data in            */
	/*  SNDG array within specified layer.                       */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*                                                           */
	/*  *param      =  Returned mean mixing ratio (g/kg)         */
	/*  lower       =  Bottom level of layer (mb) [ -1=SFC]      */
	/*  upper       =  Top level of layer (mb)    [ -1=SFC-100mb]*/
	/*************************************************************/
	{
	short i, okl, oku, lptr, uptr;
	float num, dp1, dp2, totd, dbar, p1, p2, pbar, totp;

        *param = 0;

	/* ----- See if default layer is specified ----- */
	if( lower == -1) { lower = sndg[sfc()][1]; }
	if( upper == -1) { upper = sndg[sfc()][1] - 100; }

	/* ----- Make sure this is a valid layer ----- */
	if( !qc( i_temp( upper ))) { *param = -999; }
	if( !qc( i_temp( lower ))) { lower = i_pres( sfc() ); }

	if( qc(*param))
	   {
	   /* ----- Find lowest observation in layer ----- */
	   i = 0;
	   while( sndg[i][1] > lower)  { i++; }
	   while ( (i < numlvl) && (!qc(sndg[i][4])) ) { i++; }
           if(i == numlvl)
              {
              *param = -999.;
              return(*param);
              }
	   lptr = i;
	   if( sndg[i][1] == lower ) { lptr++; }

	   /* ----- Find highest observation in layer ----- */
	   i=numlvl-1;
	   while( sndg[i][1] < upper) { i--;}
	   uptr = i;
	   if( sndg[i][1] == upper ) { uptr--; }

	   /* ----- Start with interpolated bottom layer ----- */
	   dp1 = i_dwpt( lower );
	   p1 = lower;
	   num = 1;

	   totd = 0;
	   totp = 0;
	   for( i = lptr; i <= uptr; i++)
	      {
	      if( qc(sndg[i][4]) )
		 {
		 /* ----- Calculate every level that reports a dwpt ----- */
		 dp2 = sndg[i][4];
		 p2 = sndg[i][1];
		 dbar = (dp1 + dp2) / 2;
		 pbar = (p1 + p2)/2;
		 totd = totd + dbar;
		 totp = totp + pbar;
		 dp1 = dp2;
		 p1 = p2;
		 num += 1;
		 }
	      }

	   /* ----- Finish with interpolated top layer ----- */
	   dp2 = i_dwpt( upper );
	   p2 = upper;
	   dbar = (dp1 + dp2) / 2;
	   pbar = (p1 + p2) / 2;
	   totd = totd + dbar;
	   totp = totp + pbar;
	   *param = mixratio( totp/num, totd/num);
	   }
	return *param;
	}

	/*NP*/
	float mean_relhum( float *param, float lower, float upper )
	/*************************************************************/
	/*  MEAN_RELHUM                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Mean Relative Humidity from data in       */
	/*  SNDG array within specified layer.                       */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*                                                           */
	/*  *param      =  Returned mean relative Humidity (%)       */
	/*  lower       =  Bottom level of layer (mb) [ -1=SFC]      */
	/*  upper       =  Top level of layer (mb)    [ -1=TOP]      */
	/*************************************************************/
	{
	short i, okl, oku, lptr, uptr;
	float num, dp1, dp2, totd, dbar, p1, p2, pbar, totp;
	float t1, t2, tbar, tott, dum;

	/* ----- See if default layer is specified ----- */
	if( lower == -1) { lower = sndg[sfc()][1]; }
	if( upper == -1) { upper = sndg[numlvl - 1][1]; }

	/* ----- Make sure this is a valid layer ----- */
	while( !qc( i_dwpt( upper ))) { upper += 50; if(upper > lower) return(-999.);}
	if( !qc( i_temp( lower ))) { lower = i_pres( sfc() ); }

	if( qc(*param))
	   {
	   /* ----- Find lowest observation in layer ----- */
	   i = 0;
	   while( sndg[i][1] > lower)  { i++; }
	   while ( !qc(sndg[i][4]) ) { i++; }
	   lptr = i;
	   if( sndg[i][1] == lower ) { lptr++; }

	   /* ----- Find highest observation in layer ----- */
	   i=numlvl;
	   while( sndg[i][1] < upper) { i--;}
	   uptr = i;
	   if( sndg[i][1] == upper ) { uptr--; }

	   /* ----- Start with interpolated bottom layer ----- */
	   t1  = i_temp( lower );
	   dp1 = i_dwpt( lower );
	   p1 = lower;
	   num = 1;

	   totd = 0;
	   totp = 0;
	   tott = 0;
	   for( i = lptr; i <= uptr; i++)
	      {
	      if( qc(sndg[i][4]) )
		 {
		 /* ----- Calculate every level that reports a dwpt ----- */
		 dp2 = sndg[i][4];
		 t2  = sndg[i][3];
		 p2 = sndg[i][1];
		 tbar = (t1 + t2) / 2;
		 dbar = (dp1 + dp2) / 2;
		 pbar = (p1 + p2)/2;
		 totd += dbar;
		 totp += pbar;
		 tott += tbar;
		 dp1 = dp2;
		 p1 = p2;
		 t1 = t2;
		 num += 1;
		 }
	      }

	   /* ----- Finish with interpolated top layer ----- */
	   if( qc( i_dwpt( upper )))
	      {
	      dp2 = i_dwpt( upper );
	      t2  = i_temp( upper );
	      p2 = upper;
	      tbar = (t1 + t2) / 2;
	      dbar = (dp1 + dp2) / 2;
	      pbar = (p1 + p2) / 2;
	      tott += tbar;
	      totd += dbar;
	      totp += pbar;
	      }

	   *param = 100 * mixratio( totp/num, totd/num) / mixratio( totp/num, tott/num);
	   }
	return *param;
	}

	/*NP*/
	float mean_dwpt( float *param, float lower, float upper )
	/*************************************************************/
	/*  MEAN_DWPT                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Mean Dew Point from data in               */
	/*  SNDG array within specified layer.                       */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*                                                           */
	/*  *param      =  Returned mean dew point (c)               */
	/*  lower       =  Bottom level of layer (mb) [ -1=SFC]      */
	/*  upper       =  Top level of layer (mb)    [ -1=SFC-100mb]*/
	/*************************************************************/
	{
	short i, okl, oku, lptr, uptr;
	float num, dp1, dp2, totd, dbar, p1, p2, pbar, totp;

	/* ----- See if default layer is specified ----- */
	if( lower == -1) { lower = sndg[sfc()][1]; }
	if( upper == -1) { upper = sndg[sfc()][1] - 100; }

	/* ----- Make sure this is a valid layer ----- */
	if( !qc( i_temp( upper ))) { *param = -999; }
	if( !qc( i_temp( lower ))) { lower = i_pres( sfc() ); }

	if( qc(*param))
	   {
	   /* ----- Find lowest observation in layer ----- */
	   i = 0;
	   while( sndg[i][1] > lower)  { i++; }
	   while ( !qc(sndg[i][4]) ) { i++; }
	   lptr = i;
	   if( sndg[i][1] == lower ) { lptr++; }

	   /* ----- Find highest observation in layer ----- */
	   i=numlvl;
	   while( sndg[i][1] < upper) { i--;}
	   uptr = i;
	   if( sndg[i][1] == upper ) { uptr--; }

	   /* ----- Start with interpolated bottom layer ----- */
	   dp1 = i_dwpt( lower );
	   p1 = lower;
	   num = 1;

	   totd = 0;
	   totp = 0;
	   for( i = lptr; i <= uptr; i++)
	      {
	      if( qc(sndg[i][4]) )
		 {
		 /* ----- Calculate every level that reports a dwpt ----- */
		 dp2 = sndg[i][4];
		 p2 = sndg[i][1];
		 dbar = (dp1 + dp2) / 2;
		 pbar = (p1 + p2)/2;
		 totd = totd + dbar;
		 totp = totp + pbar;
		 dp1 = dp2;
		 p1 = p2;
		 num += 1;
		 }
	      }

	   /* ----- Finish with interpolated top layer ----- */
	   dp2 = i_dwpt( upper );
	   p2 = upper;
	   dbar = (dp1 + dp2) / 2;
	   pbar = (p1 + p2) / 2;
	   totd = totd + dbar;
	   totp = totp + pbar;
	   *param = totd/num;
	   }
	return *param;
	}

	/*NP*/
	float top_moistlyr( float *param )
	/*************************************************************/
	/*  TOP_MOISTLYR                                             */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Determines the top of the moist layer of a given         */
	/*  environment.  Criteria:  50% dcrs in q in 50mb.          */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*                                                           */
	/*  *param      =  Returned top of moist layer (mb)          */
	/*************************************************************/
	{
	short i;
	float num=0, p1, p2, q1, q2, mq1, mq2, mh1, mh2, dqdz, dz;

	q1  = mixratio( sndg[sfc()][1], sndg[sfc()][4] );
	p1  = sndg[sfc()][1];
	mq1 = q1;
	mh1 = i_hght( p1 );
	for( i = sfc() + 1; i < numlvl; i++)
	   {
	   if( qc(sndg[i][4]) && qc(sndg[i][3]) )
	      {
	      /* ----- Calculate every level that reports a dwpt ----- */
	      p2 = sndg[i][1];
	      q2 = mixratio( p2, sndg[i][4] );

	      mq2 = (q1 + q2) / 2;
	      mh2 = (i_hght(p2) + i_hght(p1)) / 2;
	      dz = mh2 - mh1;
	      if( dz == 0 ) { dz = .001F; }
	      dqdz = (mq2 - mq1) / dz;
	      /* printf( "dqdz = %f,   mq2 = %f   %f\n", dqdz, mq2, p1); */
	      if(( dqdz < -.01F ) && ( i > sfc() + 1 ) && (sndg[i][1] >= 500))
		 {
		 *param = p1;
		 return *param;
		 }
	      q1  = q2;
	      p1  = p2;
	      mq1 = mq2;
	      mh1 = mh2;
	      num += 1;
	      }
	   *param = -999.0F;
	   }
	return *param;
	}

	/*NP*/
	float max_temp( float *param, float mixlyr)
	/*************************************************************/
	/*  MAX_TEMP                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a maximum temperature forecast based on       */
	/*  depth of mixing level and low level temperatures.        */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*                                                           */
	/*  *param      =  Returned max temp (c)                     */
	/*  mixlyr      =  Assumed depth of mixing layer [-1=100mb]  */
	/*************************************************************/
	{
	float t1, temp, sfcpres;

	/* ----- See if default layer is specified ----- */
	if( mixlyr == -1) { mixlyr = sndg[sfc()][1] - 100; }


	temp = i_temp( mixlyr ) + 273.15 + 2.0;
	sfcpres = sndg[sfc()][1];
	*param = (temp * pow( sfcpres / mixlyr , ROCP)) - 273.15;
	return *param;
	}

	/*NP*/
	float delta_t( float *param )
	/*************************************************************/
	/*  DELTA-T                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the delta-t index from data in SNDG array.    */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*************************************************************/
	{
	if( !qc( i_temp( 700 )) || !qc( i_temp( 500 )))
	   { *param = -999;}
	else
	   { *param = i_temp( 700 ) - i_temp( 500 ); }
	return *param;
	}

	/*NP*/
	float vert_tot( float *param )
	/*************************************************************/
	/*  VERT_TOT                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the vertical totals from data in SNDG array.  */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*************************************************************/
	{
	if( !qc( i_vtmp( 850 )) || !qc( i_vtmp( 500 )))
	   { *param = -999;}
	else
	   { *param = i_vtmp( 850 ) - i_vtmp( 500 ); }
	return *param;
	}

	/*NP*/
	float lapse_rate( float *param, float lower, float upper )
	/*************************************************************/
	/*  LAPSE_RATE                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the lapse rate (C/km) from SNDG array.        */
	/*  Value is returned both as (param) and as a RETURN.       */
	/*************************************************************/
	{
	float dt, dz, lr;

	if( !qc( i_vtmp( lower )) || !qc( i_vtmp( upper )))
	   {
	   *param = -999;
	   }
	else
	   {
	   dt = i_vtmp( upper ) - i_vtmp( lower );
	   dz = i_hght( upper ) - i_hght( lower );
	   *param = (dt / dz) * -1000;
	   }
	return *param;
	}

	/*NP*/
	float bulk_rich( struct _parcel pcl, float *brnshear)
	/*************************************************************/
	/*  BRN                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Bulk Richardson Number for given parcel.  */
	/*  Value is returned both as (param) and as a RETURN.       */
	/*************************************************************/
	{
	float ptop, pbot, x1, y1, x2, y2, z1, z2, dx, dy;

        if(lplvals.flag < 4)
           {
	   ptop = i_pres(msl(6000));
	   pbot = sndg[sfc()][1];
           }
        else
           {
           pbot = i_pres(i_hght(pcl.lplpres) - 500);
           if (!qc(pbot)) pbot = sndg[sfc()][1];
           ptop = i_pres(i_hght(pbot) + 6000);
           }

	/* ----- First, calculate 0-500m mean wind ----- */
	/* mean_wind( sndg[sfc()][1], pbot, &x1, &y1, &z1, &z2); */
        mean_wind( pbot, i_pres(i_hght(pbot)+500), &x1, &y1, &z1, &z2);

	/* ----- Next, calculate 0-6000m mean wind ----- */
	/* mean_wind( sndg[sfc()][1], ptop, &x2, &y2, &z1, &z2); */
        mean_wind( pbot, ptop, &x2, &y2, &z1, &z2);

	/* ----- Check to make sure CAPE and SHEAR are avbl ----- */
	if (!qc(pcl.bplus)) {return -999;}
	if (!qc(x1)) {return -999;}
	if (!qc(x2)) {return -999;}

	/* ----- Calculate shear between winds ----- */
	dx = x2 - x1;
	dy = y2 - y1;
	*brnshear = (float)(sqrt((dx * dx) + (dy * dy))) * .51479;
	*brnshear = *brnshear * *brnshear / 2;

	/* ----- Calculate and return BRN ----- */
	return pcl.bplus / *brnshear;
	}

	/*NP*/
	float cnvtv_temp( float *param, float mincinh )
	/*************************************************************/
	/*  CNVTV_TEMP                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Computes convective temperature, assuming no change in   */
	/*  moisture profile.  Parcels are iteratively lifted until  */
	/*  only (mincinh) j/kg are left as a cap.  The first guess  */
	/*  is the observed surface temperature.                     */
	/*************************************************************/
	{
	float mmr, ix1=0., pres, te, tm, sfcpres, sfctemp, sfcdwpt, hct, hcd;
	struct _parcel pcl;

	mmr = mean_mixratio( &ix1, -1, -1 );
	sfcpres = sndg[sfc()][1];
	sfctemp = sndg[sfc()][3];
	sfcdwpt = temp_at_mixrat( mmr, sfcpres);

	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	while(( pcl.bminus < mincinh ) && (ix1 != -999))
	   {
	   if((mincinh - pcl.bminus) > 100)
	      {
	      sfctemp += 2;
	      }
	   else
	      {
	      sfctemp += 1;
	      }
	   ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	   }

	if(ix1 == -999)
	   {
	   *param = -999;
	   }
	else
	   {
	   *param = sfctemp;
	   }
	return *param;
	}

	/*NP*/
	float sweat_index( float *param )
	/*************************************************************/
	/*  SWEAT_INDEX                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the SWEAT-Index from data in SNDG array.      */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*************************************************************/
	{
	float ix1, ix2, ix3, d8, tt, wsp8, wsp5, sw, wd8, wd5, sinw, angl;

        *param = 0;
	sw=0.0;

	d8 = i_dwpt( 850 ); if( !qc( d8 ) ) {*param = -999;}
	tt = t_totals( &ix1, &ix2, &ix3 ); if( !qc( tt ) ) {*param = -999;}
	wsp8 = i_wspd( 850 ); if( !qc( wsp8 ) ) {*param = -999;}
	wsp5 = i_wspd( 500 ); if( !qc( wsp5 ) ) {*param = -999;}
	wd8 = i_wdir( 850 ); if( !qc( wd8 ) ) {*param = -999;}
	wd5 = i_wdir( 500 ); if( !qc( wd5 ) ) {*param = -999;}

	if( *param != -999)
	   {
	   sinw = 0;

	   if( d8 > 0 ) { sw = 12 * d8; }
	   if( tt > 49) { sw = sw + (20 * (tt - 49)); }
	   sw = sw + (2 * wsp8) + wsp5;
	   if(( wd8 >= 130) && (wd8 <= 250))
	      {
	      if((wd5 >= 210) && (wd5 <= 310))
		 {
		 if(wd5 > wd8)
		    {
		    angl = (wd5 - wd8) * (PI / 180);
		    sinw = (float)sin(angl);
		    }
		 }
	      }
	   if(sinw > 0) { sw = sw + (125 * (sinw + .2)); }
	   *param = sw;
	   }
	return *param;
	}

	/*NP*/
	float old_cnvtv_temp( float *param )
	/*************************************************************/
	/*  OLD_CNVTV_TEMP                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Computes convective temperature, assuming no change in   */
	/*  moisture profile.  Intersection of mixing ratio line and */
	/*  temperature profile is found, then the dry adiabat is    */
	/*  followed back to the surface pressure.                   */
	/*************************************************************/
	{
	float mmr, ix1=0., pres, te, tm;

	/* ----- Compute Historical Convective Temperature ----- */
	mmr = mean_mixratio( &ix1, -1, -1 );
	for(pres = sndg[sfc()][1]; pres > sndg[numlvl-1][1]; pres -= 5 )
	   {
	   te = i_temp( pres );
	   tm = temp_at_mixrat( mmr, pres );
	   if(tm >= te)
	      {
	      te += 273.15;
	      *param = (te * pow( sndg[sfc()][1] / pres , ROCP)) - 273.15;
	      return *param;
	      }
	   }
	*param = -999;
	return -999;
	}

	/*NP*/
	void define_parcel( short flag, float pres )
	/*************************************************************/
	/*  DEFINE_PARCEL                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Chooses LPL, and assigns values to LPLVALS struct.       */
	/*                                                           */
	/*  flag   -   Parcel selection.                             */
	/*            -1 = Use Previous Selection                    */
	/*             1 = Observed sfc parcel                       */
	/*             2 = Fcst sfc parcel                           */
	/*             3 = Mean mixlyr parcel                        */
	/*             4 = Most unstable parcel                      */
    /*             5 = Smallest CINH                             */
	/*             6 = User defined parcel                       */
	/*                                                           */
	/*  pres   -   Pressure(mb) of user defined parcel.          */
	/*************************************************************/
	{
	float mmr, mtha, ix1 = 0;

	if(flag == -1) { flag = lplvals.flag; }

	switch( flag )
	      {
	      case 2:
		 strcpy( lplvals.desc, "FCST SFC PARCEL");
		 lplvals.temp = max_temp( &ix1, -1);
		 mmr = mean_mixratio( &ix1, -1, -1 );
		 lplvals.dwpt = temp_at_mixrat( mmr, sndg[sfc()][1]);
		 lplvals.pres = sndg[sfc()][1];
		 break;

	      case 1:
		 strcpy( lplvals.desc, "SFC PARCEL");
		 lplvals.temp = sndg[sfc()][3];
		 lplvals.dwpt = sndg[sfc()][4];
		 lplvals.pres = sndg[sfc()][1];
		 break;

	      case 3:
		 strcpy( lplvals.desc, "MEAN MIXING LAYER PARCEL");
		 mtha = mean_theta( &ix1, -1, -1 );
		 lplvals.temp = theta( 1000, mtha, sndg[sfc()][1]);
		 mmr = mean_mixratio( &ix1, -1, -1 );
		 lplvals.dwpt = temp_at_mixrat( mmr, sndg[sfc()][1]);
		 lplvals.pres = sndg[sfc()][1];
		 break;

	      case 4:
		 ix1 = unstbl_lvl( &ix1, -1, -1 );
		 sprintf( lplvals.desc, "MOST UNSTABLE PARCEL", (short)ix1);
		 lplvals.pres = ix1;
		 lplvals.temp = i_temp(ix1);
		 lplvals.dwpt = i_dwpt(ix1);
		 break;
                 
              case 5:
                 ix1 = min_cinh_lvl( &ix1, -1, -1);
                 sprintf( lplvals.desc, "SMALLEST CINH", (short) ix1);
                 lplvals.pres = ix1;
		 lplvals.temp = i_temp(ix1);
		 lplvals.dwpt = i_dwpt(ix1);
                 break;
                 
	      case 6:
		 sprintf( lplvals.desc, "%d mb %s", (short)pres, "PARCEL");
		 lplvals.pres = pres;
		 lplvals.temp = i_temp(pres);
		 lplvals.dwpt = i_dwpt(pres);
		 break;
	     }
	lplvals.flag = flag;
	}

	/*NP*/
	float mean_theta( float *param, float lower, float upper )
	/*************************************************************/
	/*  MEAN_THETA                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Mean Potential temperature of the         */
	/*  SNDG array within specified layer.                       */
	/*  Value is returned both as (param) and as a RETURN;.      */
	/*                                                           */
	/*  *param      =  Returned mean theta (c)                   */
	/*  lower       =  Bottom level of layer (mb) [ -1=SFC]      */
	/*  upper       =  Top level of layer (mb)    [ -1=SFC-100mb]*/
	/*************************************************************/
	{
	short i, okl, oku, lptr, uptr;
	float num, dp1, dp2, totd, dbar, p1, p2, pbar, totp;

	/* ----- See if default layer is specified ----- */
	if( lower == -1) { lower = sndg[sfc()][1]; }
	if( upper == -1) { upper = sndg[sfc()][1] - 100; }
	/* ----- Make sure this is a valid layer ----- */
	if( !qc( i_temp( upper ))) { *param = -999; }
	if( !qc( i_temp( lower ))) { lower = i_pres( sfc() ); }

	if( qc(*param))
	   {
	   /* ----- Find lowest observation in layer ----- */
	   i = 0;
	   while( sndg[i][1] > lower)  { i++; }
	   while ( !qc(sndg[i][3]) ) { i++; }
	   lptr = i;
	   if( sndg[i][1] == lower ) { lptr++; }

	   /* ----- Find highest observation in layer ----- */
	   i=numlvl;
	   while( sndg[i][1] < upper) { i--;}
	   uptr = i;
	   if( sndg[i][1] == upper ) { uptr--; }

	   /* ----- Start with interpolated bottom layer ----- */
	   dp1 = theta( lower, i_temp( lower ), 1000);
	   p1 = lower;
	   num = 1;

	   totd = 0;
	   totp = 0;
	   for( i = lptr; i <= uptr; i++)
	      {
	      if( qc(sndg[i][3]) )
		 {
		 /* ----- Calculate every level that reports a temp ----- */
		 dp2 = theta( sndg[i][1], sndg[i][3], 1000);
		 p2 = sndg[i][1];
		 dbar = (dp1 + dp2) / 2;
		 pbar = (p1 + p2)/2;
		 totd = totd + dbar;
		 totp = totp + pbar;
		 dp1 = dp2;
		 p1 = p2;
		 num += 1;
		 }
	      }

	   /* ----- Finish with interpolated top layer ----- */
	   dp2 = theta( upper, i_temp( upper ), 1000);
	   p2 = upper;
	   dbar = (dp1 + dp2) / 2;
	   pbar = (p1 + p2) / 2;
	   totd = totd + dbar;
	   totp = totp + pbar;
	   *param = totd/num;
	   }
	return *param;
	}

        float min_cinh_lvl(float *param, float lower, float upper ) {
        /**************************************************************/
        /* MIN_CINH_LVL                                               */
        /* Larry J. Hinson AWC                                        */
        /* Purpose: Determine the level where the smallest CIN and    */
        /* minimal CAPE (> 10 J/Kg) exists for identifying elevated   */
        /* convection.  This algorithm works from the top of the      */
        /* atmosphere downward.                                       */
        /*                                                            */
        /* *param = Returned least CINH level (mb)                    */
        /* lower  = Bottom level of layer (mb) [-1=SFC]               */
        /* upper  = Top level of layer (mb)    [-1=low 300 mb]        */                                                             
        /**************************************************************/
        
          int i, lptr,uptr;
          float ix1 = 0,pres,presstart,temp,dwpt,mincin=-999,mincinpres=-999;
          struct _parcel pcl;
          int firsttime=-1;
          if ( lower == -1 ) { lower = sndg[sfc()][1]; }
          if ( upper == -1 ) { upper = sndg[sfc()][1] - 300; }
          while( !qc( i_dwpt( upper ))) 
            { upper += 50; if(upper > lower) return(-999.);}
	  if( !qc( i_temp( lower ))) { lower = i_pres( sfc() ); }
          /* ---- Start with interpolated bottom layer ---- */
	  if( qc(*param)) {
	    /* Find lowest observation in layer ----- */
	    i = 0;
	    while( sndg[i][1] > lower)  { i++; }
	    while ( !qc(sndg[i][3]) ) { i++; }
	    lptr = i;
	    if( sndg[i][1] == lower ) { lptr++; }

	    /* ----- Find highest observation in layer ----- */
	    i=numlvl;
	    while( sndg[i][1] < upper) { i--;}
	    uptr = i;
	    if( sndg[i][1] == upper ) { uptr--; }
            /* Opted for working from the top of the atmosphere downward instead
            since the intent is to catch elevated convection */
            for (i = uptr; i>=lptr;i--) {
              if (qc (sndg[i][4])) {
                pres=sndg[i][1];
                temp=sndg[i][3];
                dwpt=sndg[i][4];
                if (firsttime) {
                  presstart=pres;
                  firsttime=0;
                }
                ix1 = parcel( -1, -1, pres, temp, dwpt, &pcl);
                if (ix1 != -999) {
                  if (pcl.bminus > mincin && pcl.bplus > 10.0) {
                    mincin=pcl.bminus;
                    mincinpres=pres;
                    if (fabs(mincin)<.01) {
                      break;
                    }
                  }
                }
              }
            }
          }
          if (mincinpres== -999.) {
            *param=presstart;
          } else {
            *param=mincinpres;
          }
          return *param;
        }

	/*NP*/
	float unstbl_lvl( float *param, float lower, float upper )
	/*************************************************************/
	/*  UNSTBL_LVL                                               */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Finds the most unstable level between levels upper and   */
	/*  lower.                                                   */
	/*                                                           */
	/*  *param      =  Returned most unstable level (mb)         */
	/*  lower       =  Bottom level of layer (mb) [ -1=SFC]      */
	/*  upper       =  Top level of layer (mb)    [ -1=low 300mb]*/
	/*************************************************************/
	{
	short i, okl, oku, lptr, uptr;
	float t1, p2, t2, tmax, pmax, pres, temp, dwpt;

	/* ----- See if default layer is specified ----- */
	if( lower == -1) { lower = sndg[sfc()][1];    }
	if( upper == -1) { upper = sndg[sfc()][1] - 300; }

	/* ----- Make sure this is a valid layer ----- */
	while( !qc( i_dwpt( upper ))) 
           { upper += 50; if(upper > lower) return(-999.);}
	if( !qc( i_temp( lower ))) { lower = i_pres( sfc() ); }

	if( qc(*param))
	   {
	   /* Find lowest observation in layer ----- */
	   i = 0;
	   while( sndg[i][1] > lower)  { i++; }
	   while ( !qc(sndg[i][3]) ) { i++; }
	   lptr = i;
	   if( sndg[i][1] == lower ) { lptr++; }

	   /* ----- Find highest observation in layer ----- */
	   i=numlvl;
	   while( sndg[i][1] < upper) { i--;}
	   uptr = i;
	   if( sndg[i][1] == upper ) { uptr--; }

	   /* ----- Start with interpolated bottom layer ----- */
	   drylift( lower, i_temp( lower ), i_dwpt( lower), &p2, &t2);
	   tmax = wetlift( p2, t2, 1000);
	   pmax = lower;

	   for( i = lptr; i <= uptr; i++)
	      {
	      if( qc(sndg[i][4]) )
		 {
		 /* ----- Calculate every level that reports a dwpt ----- */
		 pres = sndg[i][1];
		 temp = sndg[i][3];
		 dwpt = sndg[i][4];
		 drylift( pres, temp, dwpt, &p2, &t2);
		 t1 = wetlift( p2, t2, 1000);
		 if( t1 > tmax )
		    {
		    tmax = t1;
		    pmax = pres;
		    }
		 }
	      }

	   /* ----- Finish with interpolated top layer ----- */
	   drylift( upper, i_temp( upper ), i_dwpt( upper), &p2, &t2);
	   t1 = wetlift( p2, t2, 1000);
	   if( t1 > tmax ) { pmax = sndg[i][1]; }

	   *param = pmax;

	   }
	return *param;
	}

	/*NP*/
	float ehi( float cape, float hel )
	/*************************************************************/
	/*  EHI                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the Energy-Helicity Index.                    */
	/*************************************************************/
	{
	float param=0;

	if( !qc( cape  ) ) {param = -999;}
	if( !qc( hel  ) ) {param = -999;}

	if( param != -999) { param = (cape * hel) / 160000.; }

	return param;
	}

	/*NP*/
	float ThetaE_diff( float *param)
	/*************************************************************/
	/*  THETAE_DIFF                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Finds the maximum and minimum theta-e values in the      */
	/*  sounding below 500mb, and returns the difference.        */
	/*                                                           */
	/*  lower       =  Bottom level of layer (mb)  [ -1=SFC]     */
	/*  upper       =  Top level of layer (mb)     [ -1=TOP]     */
	/*************************************************************/
	{
	short i;
	float maxe = -999, mine = 999, the;

	for( i = sfc() + 1; i < numlvl; i++)
	   {
	   if( qc(sndg[i][4]) && qc(sndg[i][3]) )
	      {
	      the = thetae(sndg[i][1], sndg[i][3], sndg[i][4]);
	      if( the > maxe ) { maxe = the; }
	      if( the < mine ) { mine = the; }
	      if( sndg[i][1] < 500 ) { break; }
	      }
	   }
	return (maxe - mine);
	}



	/*NP*/
	float Mean_WBtemp( float *param, float lower, float upper)
	/*************************************************************/
	/*  MEAN_WBTEMP                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Computes the average Wetbulb temperature between the     */
	/*  two given levels (lower,upper)			     */
	/*                                                           */
	/*  lower       =  Bottom level of layer (mb)  [ -1=SFC]     */
	/*  upper       =  Top level of layer (mb)     [ -1=WBZ]     */
	/*************************************************************/
        {
        short i, okl, oku, lptr, uptr;
        float num, dp1, dp2, totd, dbar, p1, p2, pbar, totp, ix1;

        /* ----- See if default layer is specified ----- */
        if( lower == -1) { lower = sndg[sfc()][1]; }
        if( upper == -1) { upper = wb_lvl(0, &ix1); }

        /* ----- Make sure this is a valid layer ----- */
        if( !qc( i_dwpt( upper ))) { *param = -999; }
        if( !qc( i_dwpt( lower ))) { lower = i_pres( sfc() ); }

        if( qc(*param))
           {
           /* ----- Find lowest observation in layer ----- */
           i = 0;
           while( sndg[i][1] > lower)  { i++; }
           while ( !qc(sndg[i][3]) ) { i++; }
           lptr = i;
           if( sndg[i][1] == lower ) { lptr++; }

           /* ----- Find highest observation in layer ----- */
           i=numlvl;
           while( sndg[i][1] < upper) { i--;}
           uptr = i;
           if( sndg[i][1] == upper ) { uptr--; }

           /* ----- Start with interpolated bottom layer ----- */
           dp1 = wetbulb( lower, i_temp(lower), i_dwpt(lower));
           p1 = lower;
           num = 1;

           totd = 0;
           totp = 0;
           for( i = lptr; i <= uptr; i++)
              {
              if( qc(sndg[i][4]) )
                 {
                 /* ----- Calculate every level that reports a temp ----- */
                 dp2 = wetbulb( sndg[i][1], sndg[i][3], sndg[i][4]);
                 p2 = sndg[i][1];
                 dbar = (dp1 + dp2) / 2;
                 pbar = (p1 + p2)/2;
                 totd = totd + dbar;
                 totp = totp + pbar;
                 dp1 = dp2;
                 p1 = p2;
                 num += 1;
                 }
              }

           /* ----- Finish with interpolated top layer ----- */
           dp2 = wetbulb( upper, i_temp(upper), i_dwpt(upper));
           p2 = upper;
           dbar = (dp1 + dp2) / 2;
           pbar = (p1 + p2) / 2;
           totd = totd + dbar;
           totp = totp + pbar;
           *param = totd/num;
           }
        return *param;
        }


	/*NP*/
	float Rogash_QPF( float *param)
	/*************************************************************/
	/*  Rogash_QPF                                               */
	/*  John Hart  SPC Norman OK                                 */
	/*                                                           */
	/*  Computes an approximate 6 hour QPF.                      */
	/*  Based on research by Joe Rogash.			     */
	/*                                                           */
	/*  *param      =  6 Hour QPF				     */
	/*************************************************************/
	{
	float q1, q2, q3, q4, q5, x1=0., qpf, M;
	float sfcpres, sfctemp, sfcdwpt;
	float rog_li, rog_omeg;
	struct _parcel pcl;

	/* Lift a MOST UNSTABLE Parcel */	
	sfcpres = unstbl_lvl( &x1, -1, -1 );
        if(sfcpres < 0) return (-999.);
	sfctemp = i_temp(sfcpres);
        if(sfctemp == -999.) return (-999.);
	sfcdwpt = i_dwpt(sfcpres);
        if(sfcdwpt == -999.) return (-999.);
	x1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	rog_li = pcl.li5;
	if(rog_li > -2) rog_li = -2;
	if (!qc(rog_li)) rog_li = -2;
	
	rog_omeg = i_omeg(700);
	/*printf( "rog_omeg = %f\n", rog_omeg);*/
	if (rog_omeg > -2) rog_omeg = -2;
	if (!qc(rog_omeg)) rog_omeg = -2;
	rog_omeg *= -10;

	q1 = mean_relhum( &x1, 700, 300 ) / 70.0;
	q2 = rog_li / -10.0;
	q3 = precip_water( &x1, -1, -1 ) / 1.75;
        q4 = rog_omeg / 12.0;
	q5 = 3.0 * (q1 * q2 * q3 * q4);

	M = 1;
	
	if((i_dwpt(850) >= 8) && (i_dwpt(850) <= 10) && (i_wspd(850) >= 25)) 
		M=1.35;
		
	if((i_dwpt(850) >= 11) && (i_wspd(850) >= 20) && (i_wspd(850) < 30))
		M=1.35;
		
	if((i_dwpt(850) >= 11) && (i_wspd(850) >= 30)) M=1.85;
	
	*param = M * q5;
	
	/*printf( "QPF = %4.2f  %4.2f  %4.2f  %4.2f  %4.2f %4.2f\n", 
		q1, q2, q3, q4, q5, M);*/
	
	return *param;
	}

