/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Thermodynamic Library                                      */
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
	float lifted(float pres, float temp, float dwpt, float lvl2)
	/*************************************************************/
	/*  LIFTED                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates a lifted index for a parcel(pres,temp,dwpt)   */
	/*  at level (lvl2).                                         */
	/*                                                           */
	/*  pres             - Pressure of initial parcel(mb)        */
	/*  temp             - Temperature of initial parcel (c)     */
	/*  dwpt             - Dew Point of initial parcel (c)       */
	/*  lvl2             - Pressure to lift parcel (mb)          */
	/*************************************************************/
	{
	float p2, t2;
	printf( "Parcel = %f   %f   %f\n", pres, temp, dwpt);
	drylift(pres, temp, dwpt, &p2, &t2);
	printf( "LCL = %f, %f\n", p2, t2);
	return wetlift(p2, t2, lvl2);
	}

	/*NP*/
	void drylift(float p1, float t1, float td1, float *p2, float *t2)
	/*************************************************************/
	/*  DRYLIFT                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts a parcel(p1,t1,td1) to the LCL and returns its     */
	/*  new level(p2) and temperature(t2).                       */
	/*                                                           */
	/*  p1               - Pressure of initial parcel(mb)        */
	/*  t1               - Temperature of initial parcel (c)     */
	/*  td1              - Dew Point of initial parcel (c)       */
	/*  p2               - Pressure at LCL (mb)                  */
	/*  t2               - Temperature at LCL (c)                */
	/*************************************************************/
	{
        *t2 = -999;
        *p2 = -999;
        if((t1 != -999)&&(td1 != -999))
	   *t2 = lcltemp( t1, td1 );

        if((p1 != -999)&&(*t2 != -999))
	   *p2 = thalvl( theta(p1, t1, 1000), *t2);

	}

	/*NP*/
	float lcltemp(float temp, float dwpt)
	/*************************************************************/
	/*  LCLTEMP                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the temperature(c) of a parcel (temp,dwpt)       */
	/*  when raised to its LCL.                                  */
	/*                                                           */
	/*  temp             - Temperature of initial parcel (c)     */
	/*  dwpt             - Dew Point of initial parcel (c)       */
	/*************************************************************/
	{
	float s,t, dlt;
	s = temp - dwpt;
	dlt = s * (1.2185 + .001278 * temp + s * (-.00219 + 1.173E-05 *
	      s - .0000052 * temp));
	return temp - dlt;
	}

	/*NP*/
	float thalvl(float thta, float temp)
	/*************************************************************/
	/*  THALVL                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the level(mb) of a parcel( thta, temp).          */
	/*                                                           */
	/*  thta             - Potential Temperature of parcel(c)    */
	/*  temp             - Temperature of parcel (c)             */
	/*************************************************************/
	{
        float level,term1,term2;
	temp = temp + 273.15;
	thta = thta + 273.15;
        term1 = thta/temp;
        term2 = 1/ROCP;
        level = 1000.0 / pow(term1,term2);
        return (level);
	}

	/*NP*/
	float theta(float pres, float temp, float pres2 )
	/*************************************************************/
	/*  THETA                                                    */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the potential temperature(c) of the              */
	/*  parcel(pres, temp).                                      */
	/*                                                           */
	/*  pres             - Pressure of parcel(mb)                */
	/*  temp             - Temperature of parcel (c)             */
	/*  pres2            - Reference Level (Usually 1000mb)      */
	/*************************************************************/
	{
        float pot;
	temp = temp + 273.15;


        pot =  (temp * pow((pres2 / pres), ROCP)) - 273.15;
        return (pot);
	}

	/*NP*/
	float mixratio(float pres, float temp)
	/*************************************************************/
	/*  MIXRATIO                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the mixing ratio (g/kg) of the                   */
	/*  parcel(pres, temp).                                      */
	/*                                                           */
	/*  pres             - Pressure of parcel(mb)                */
	/*  temp             - Temperature of parcel (c)             */
	/*************************************************************/
	{
	float x, wfw, fwesw;
	x = .02 * (temp - 12.5 + 7500.0 / pres);
	wfw = 1 + .0000045 * pres + .0014 * x * x;
	fwesw = wfw * vappres( temp );
	return 621.97 * (fwesw / (pres - fwesw));
	}


	/*NP*/
	float temp_at_mixrat( float mr, float pres)
	/*************************************************************/
	/*  TEMP_AT_MIXRAT                                           */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the temperature(c) of air at the given           */
	/*  mixing ratio (mr, g/kg) and pressure (pres, mb).         */
	/*                                                           */
	/*************************************************************/
	{
	double c1, c2, c3, c4, c5, c6, x, tmrk;

	c1 = .0498646455;
	c2 = 2.4082965;
	c3 = 7.07475;
	c4 = 38.9114;
	c5 = .0915;
	c6 = 1.2035;

	x = log10( mr * pres / (622 + mr));
	tmrk = pow(10., c1 * x + c2) - c3 + c4 * pow( pow(10., c5 * x) - c6, 2.);
	return (float)(tmrk - 273.15);
	}


	/*NP*/
	float vappres( float temp )
	/*************************************************************/
	/*  VAPPRES                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the vapor pressure of dry air at                 */
	/*  temperature (temp).                                      */
	/*                                                           */
	/*  temp             - Temperature of parcel (c)             */
	/*************************************************************/
	{
	double pol;

	pol = temp * (1.1112018e-17 + temp * (-3.0994571e-20));
	pol = temp * (2.1874425e-13 + temp * (-1.789232e-15 + pol));
	pol = temp * (4.3884180e-09 + temp * (-2.988388e-11 + pol));
	pol = temp * (7.8736169e-05 + temp * (-6.111796e-07 + pol));
	pol = .99999683e-00 + temp * (-9.082695e-03 + pol);
	pol = (pol * pol);
	pol = (pol * pol);
	return 6.1078F / (pol * pol);
	}

	/*NP*/
	float wetlift(float pres, float temp, float pres2)
	/*************************************************************/
	/*  WETLIFT                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts a parcel(pres,temp) moist adiabatically to its     */
	/*  new level(pres2), and returns the temperature(c).        */
	/*                                                           */
	/*  pres             - Pressure of initial parcel(mb)        */
	/*  temp             - Temperature of initial parcel (c)     */
	/*  pres2            - Final Pressure (mb)                   */
	/*************************************************************/
	{
	float tha, thm, woth, wott;
	tha = theta(pres, temp, 1000);
	woth = wobf(tha);
	wott = wobf(temp);
	thm = tha - woth + wott;
	return satlft(pres2, thm);
	}

	/*NP*/
	float satlft( float pres, float thm )
	/*************************************************************/
	/*  SATLFT                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the temperature (c) of a parcel (thm),           */
	/*  when lifted to level (pres).                             */
	/*                                                           */
	/*  pres             - Pressure to raise parcel (mb)         */
	/*  thm              - Sat. Pot. Temperature of parcel (c)   */
	/*************************************************************/
	{
	float pwrp, t1, t2, woto, wotm, wot2, woe2, e1, e2, rate, eor;

	if ((fabs(pres - 1000) -.001) <= 0) return thm;

	eor = 999;
	while (fabs( eor ) - .1 > 0)
	   {
	   if (eor == 999)                      /* First Pass */
	      {
	      pwrp = (float)(pow( pres / 1000.0, ROCP));
	      t1 = (thm + 273.15) * pwrp - 273.15;
	      woto = wobf( t1 );
	      wotm = wobf( thm );
	      e1 = woto - wotm;
	      rate = 1;
	      }
	   else                                 /* Successive Passes */
	      {
	      rate = (t2 - t1) / (e2 - e1);
	      t1 = t2;
	      e1 = e2;
	      }
	   t2 = t1 - e1 * rate;
	   e2 = (t2 + 273.15) / pwrp - 273.15;
	   wot2 = wobf( t2 );
	   woe2 = wobf( e2 );
	   e2 = e2 + wot2 - woe2 - thm;
	   eor = e2 * rate;
	   }
	return t2 - eor;
	}

	/*NP*/
	float wobf(float temp)
	/*************************************************************/
	/*  WOBF                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  pres             - Pressure to raise parcel (mb)         */
	/*  thm              - Sat. Pot. Temperature of parcel (c)   */
	/*************************************************************/
	{
	float x;
	double pol;

	x = temp - 20;
	if (x <= 0)
	   {
	   pol = 1 + x * (-8.841660499999999e-03 + x * ( 1.4714143e-04
		   + x * (-9.671989000000001e-07 + x * (-3.2607217e-08
		   + x * (-3.8598073e-10)))));
	   pol = pol * pol;
	   return 15.13F / (pol * pol);
	   }
	else
	   {
	   pol = x * (4.9618922e-07 + x * (-6.1059365e-09 +
		 x * (3.9401551e-11 + x * (-1.2588129e-13 +
		 x * (1.6688280e-16)))));
	   pol = 1 + x * (3.6182989e-03 + x * (-1.3603273e-05 + pol));
	   pol = pol * pol;
	   return 29.93 / (pol * pol) + .96 * x - 14.8;
	   }
	}

	/*NP*/
	float wetbulb( float pres, float temp, float dwpt )
	/*************************************************************/
	/*  WETBULB                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the wetbulb temperature (c) for given         */
	/*  parcel (pres, temp, dwpt).                               */
	/*                                                           */
	/*  pres        =  Pressure of parcel (mb).                  */
	/*  temp        =  Temperature of parcel (c)                 */
	/*  dwpt        =  Dew point of parcel (c)                   */
	/*************************************************************/
	{
	float p2, t2;

	if( !qc(pres)) {return -999;}
	if( !qc(temp)) {return -999;}
	if( !qc(dwpt)) {return -999;}

	drylift( pres, temp, dwpt, &p2, &t2);
	return wetlift(p2, t2, pres);
	}

	/*NP*/
	float thetaw( float pres, float temp, float dwpt )
	/*************************************************************/
	/*  THETAW                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the wet-bulb potential temperature for given  */
	/*  parcel (pres, temp, dwpt).                               */
	/*                                                           */
	/*  pres        =  Pressure of parcel (mb).                  */
	/*  temp        =  Temperature of parcel (c)                 */
	/*  dwpt        =  Dew point of parcel (c)                   */
	/*************************************************************/
	{
	float p2, t2;

	if( !qc(pres)) {return -999;}
	if( !qc(temp)) {return -999;}
	if( !qc(dwpt)) {return -999;}

	drylift( pres, temp, dwpt, &p2, &t2);
	return wetlift(p2, t2, 1000);
	}

	/*NP*/
	float thetae( float pres, float temp, float dwpt )
	/*************************************************************/
	/*  THETAE                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the equivalent potential temperature          */
	/*  for given parcel (pres, temp, dwpt).                     */
	/*                                                           */
	/*  pres        =  Pressure of parcel (mb).                  */
	/*  temp        =  Temperature of parcel (c)                 */
	/*  dwpt        =  Dew point of parcel (c)                   */
	/*************************************************************/
	{
	float p2, t2;

	if( !qc(pres)) {return -999;}
	if( !qc(temp)) {return -999;}
	if( !qc(dwpt)) {return -999;}

	drylift( pres, temp, dwpt, &p2, &t2);
	return theta(100, wetlift(p2, t2, 100), 1000 );
	}


        /*NP*/
        float esfc( float val )
        /*************************************************************/
        /*  ESFC                                                     */
        /*  John Hart  SPC NORMAN OK                                 */
        /*                                                           */
        /*  Calculate effective surface for elevated convection.     */
        /*  Assumes that lowest layer with CAPE >= val is "sfc".     */
        /*  Returns level (mb) of effective surface.                 */
        /*                                                           */
        /*************************************************************/
        {
        short i;
        float ix1;
        struct _parcel pcl;
        
        /* ----- Begin at surface and search upward for instability ----- */
        for(i=sfc();i<numlvl;i++)
           {
           ix1 = parcel( -1, -1, sndg[i][1], sndg[i][3], sndg[i][4], &pcl);
           if(pcl.bplus >= val) return sndg[i][1];
           }
           
        return -999.0;
        }
