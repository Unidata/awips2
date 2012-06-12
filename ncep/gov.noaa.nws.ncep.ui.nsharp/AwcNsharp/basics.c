/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Basic Interpolation and Conversion Routines                */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  I_TEMP                                                     */
/*  I_DWPT                                                     */
/*  I_PRES                                                     */
/*  I_HGHT                                                     */
/*  I_VTMP                                                     */
/*  I_WDIR                                                     */
/*  I_WSPD                                                     */
/*  I_WNDU                                                     */
/*  I_WNDV                                                     */
/*  I_OMEG                                                     */
/*  VIRTEMP                                                    */
/*  SFC                                                        */
/*  TOP_PRES                                                   */
/*  QC                                                         */
/*  QC2                                                        */
/*  MTOF                                                       */
/*  FTOM                                                       */
/*  FTOC                                                       */
/*  CTOF                                                       */
/*  AGL                                                        */
/*  MSL                                                        */
/*  KT_TO_MPS                                                  */
/***************************************************************/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <sharp95.h>

	/*NP*/
	float i_temp( float pres )
	/*************************************************************/
	/*  I_TEMP                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a temperature.  */
	/*  at pressure level (pres).                                */
	/*                                                           */
	/*  pres             - Level(mb) to compute a temperature    */
	/*************************************************************/
	{
	short below, above, i, ok;
	double nm1, nm2, nm4;

	below=0;
	above=0;

	/* ----- Find Temperature Immediately Above level ----- */
	ok=0;
	for (i = 0; i < numlvl; i++)
	   {
	   if((sndg[i][1] == pres) && ( qc(sndg[i][3]) ) )
	      {
              /* if pressure level exists, and temp exists, no need to interpolate */
              return sndg[i][3];
              }
           if((sndg[i][1] < pres) && ( qc(sndg[i][3]) ) )
              {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Temperature Immediately Below level ----- */
	ok = 0;
	for (i = numlvl-1; i > -1; i--)
	   {
	   if((sndg[i][1] >= pres) && (sndg[i][3] != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndg[above][3];
	   }

	/* ----- Now we need to interpolate to get the temperature ----- */

	nm1 = sndg[above][3] - sndg[below][3];
	nm2 = log( sndg[below][1] / sndg[above][1] );
	nm4 = log( sndg[below][1] / pres );
	return (float)(sndg[below][3] + (( nm4 / nm2) * nm1));
	}

	/*NP*/
	float i_dwpt( float pres )
	/*************************************************************/
	/*  I_DWPT                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a dew point     */
	/*  at pressure level (pres).                                */
	/*                                                           */
	/*  pres             - Level(mb) to compute a Dew Point      */
	/*************************************************************/
	{
	short below, above, i, ok;
	double nm1, nm2, nm4;

	below=0;
	above=0;

	/* ----- Find Dew Point Immediately Above level ----- */
	ok=0;
	for (i = 0; i < numlvl; i++)
	   {
           if((sndg[i][1] == pres) && (sndg[i][4] != -999) && (sndg[i][1] != -999))
	      {
              /* no need to interpolate if we have the level and dewpoint exists */
              return sndg[i][4];
              }
           if((sndg[i][1] < pres) && (sndg[i][4] != -999.0F))
              {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Dew Point Immediately Below level ----- */
	ok=0;
	for (i = numlvl - 1; i > -1; i--)
	   {
	   if((sndg[i][1] >= pres) && (sndg[i][4] != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndg[above][4];
	   }

	/* ----- Now we need to interpolate to get the dew point ----- */
	nm1 = sndg[above][4] - sndg[below][4];
	nm2 = log( sndg[below][1] / sndg[above][1] );
	nm4 = log( sndg[below][1] / pres );
	return (float)(sndg[below][4] + (( nm4 / nm2) * nm1));
	}

	/*NP*/
	float i_hght( float pres )
	/*************************************************************/
	/*  I_HGHT                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a height        */
	/*  at pressure level (pres).                                */
	/*                                                           */
	/*  pres             - Level(mb) to compute a Height         */
	/*************************************************************/
	{
	short below, above, i, ok, itophght;
	double nm1, nm2, nm3, nm4;

	below=0;
	above=0;

	/* ----- Find Height Immediately Above level ----- */
	ok=0;
	for (i = 0; i < numlvl; i++)
	   {
	   if((sndg[i][1] == pres) && (sndg[i][2] != -999) && (sndg[i][1] != -999))
	      {
             /* no need to interpolate if we have the level!!! */
             return sndg[i][2];
             }
           if((sndg[i][1] < pres) && (sndg[i][2] != -999) && (sndg[i][1] != -999))
              {
	      itophght = i;
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Height Immediately Below level ----- */
	ok=0;
	for (i = numlvl - 1; i > -1; i--)
	   {
	   if((sndg[i][1] >= pres) && (sndg[i][2] != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndg[above][2];
	   }

	/* ----- Now we need to interpolate to get the height ----- */
	nm1 = sndg[above][2] - sndg[below][2];
	nm2 = log( sndg[below][1] / sndg[above][1] );
	nm4 = log( sndg[below][1] / pres );
	return (float)(sndg[below][2] + (( nm4 / nm2) * nm1));
	}

	/*NP*/
	float i_pres( float hght )
	/*************************************************************/
	/*  I_PRES                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a pressure(mb)  */
	/*  at height (hght).                                        */
	/*                                                           */
	/*  hght             - Height(m) of level                    */
	/*************************************************************/
	{
	short below, above, i, ok;
	double nm1, nm2, nm3, nm4;

	below = 0;
	above = 0;

	/* ----- Find Pressure Immediately Above level ----- */
	ok = 0;
	for (i = 0; i < numlvl; i++)
	   {
           /* if we have the level, no need to interpolate pressure */
           if((sndg[i][2] == hght) && (sndg[i][1] != -999.0F))
              return(sndg[i][1]);
	   if((sndg[i][2] > hght) && (sndg[i][1] != -999.0F))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Pressure Below level ----- */
	ok = 0;
	for (i = numlvl - 1; i > -1; i--)
	   {
	   if((sndg[i][2] <= hght) && (sndg[i][1] != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndg[above][1];
	   }

	/* ----- Now we need to interpolate to get the Pressure ----- */
	nm1 = hght - sndg[below][2];
	nm2 = sndg[above][2] - sndg[below][2];
	nm3 = log( sndg[above][1] / sndg[below][1] );
	return (float)(sndg[below][1] * exp(( nm1 / nm2) * nm3));
	}

	/*NP*/
	float i_vtmp( float pres )
	/*************************************************************/
	/*  I_VTMP                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the virtual temperature (C) at pressure       */
	/*  level (pres).                                            */
	/*                                                           */
	/*  pres             - Level(mb) to compute a virtual temp.  */
	/*************************************************************/
	{
	double cta, eps, tk, w;

	if( !qc(i_dwpt( pres )))
	   {
	   if( !qc( i_temp( pres ))) { return -999.0F; }
	   return i_temp( pres );
	   }

	cta = 273.15;
	eps = .62197;

	tk = (double)i_temp(pres) + cta;
	w = .001 * (double) mixratio( pres, i_dwpt(pres));
	return (float)(tk * (1 + w / eps) / (1 + w) - cta);
	}

	/*NP*/
	float virtemp( float pres, float temp, float dwpt )
	/*************************************************************/
	/*  VIRTEMP                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Calculates the virtual temperature (C) of parcel         */
	/*  (pres, temp, dwpt).                                      */
	/*                                                           */
	/*  pres             - Level(mb) to compute a virtual temp.  */
	/*  temp             - Temperature(c).                       */
	/*  dwpt             - Dew point(c).                         */
	/*************************************************************/
	{
	double cta, eps, tk, w;

	if( !qc(dwpt) ) { return temp; }

	cta = 273.15;
	eps = .62197;

	tk = temp + cta;
	w = .001 * mixratio( pres, dwpt);
	return (float)(tk * (1 + w / eps) / (1 + w) - cta);
	}

	/*NP*/
	float i_wdir( float pres )
	/*************************************************************/
	/*  I_WDIR                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a wind          */
	/*  direction (deg) at pressure level (pres).                */
	/*                                                           */
	/*  pres             - Level(mb) to compute a Wind           */
	/*************************************************************/
	{
	float u, v;

	if( !qc(i_wndu( pres )) || !qc(i_wndv( pres ))) { return -999.0F; }

	u = i_wndu( pres );
	v = i_wndv( pres );
	return angle( u, v );
	}


	/*NP*/
	float i_wspd( float pres )
	/*************************************************************/
	/*  I_WSPD                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a wind          */
	/*  magnitude (kt) at pressure level (pres).                 */
	/*                                                           */
	/*  pres             - Level(mb) to compute a Wind           */
	/*************************************************************/
	{
	float u, v;

	if( !qc(i_wndu( pres )) || !qc(i_wndv( pres ))) { return -999.0F; }

	u = i_wndu( pres );
	v = i_wndv( pres );
	return (float)(sqrt((u * u) + (v * v)));
	}


	/*NP*/
	float i_wndu( float pres )
	/*************************************************************/
	/*  I_WNDU                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a               */
	/*  u-component to the wind at pressure level (pres).        */
	/*                                                           */
	/*  pres             - Level(mb) to compute a U-Component    */
	/*************************************************************/
	{
	short below, above, i, ok;
	float utop, ubot, nm1;

	below=0;
	above=0;

	/* ----- Find Wind Immediately Above level ----- */
	ok = 0;
	for (i = 0;i < numlvl; i++)
	   {
           if((sndg[i][1] == pres) && (sndg[i][5] != -999.0F) &&
              (sndg[i][6] != -999.0F))
              return ucomp( sndg[i][5], sndg[i][6] );
	   if((sndg[i][1] < pres) && (sndg[i][5] != -999.0F))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Wind Immediately Below level ----- */
	ok = 0;
	for (i = numlvl - 1; i > -1; i--)
	   {
	   if((sndg[i][1] >= pres) && (sndg[i][5] != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return ucomp( sndg[above][5], sndg[above][6] );
	   }

	/* ----- Now we need to interpolate to get the Wind ----- */
	nm1 = sndg[below][1] - pres;
	ubot = ucomp( sndg[below][5], sndg[below][6] );
	utop = ucomp( sndg[above][5], sndg[above][6] );
	return ubot - (nm1 / (sndg[below][1] - sndg[above][1]))
	* (ubot - utop);
	}


	/*NP*/
	float i_wndv( float pres )
	/*************************************************************/
	/*  I_WNDV                                                   */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Interpolates the given data to calculate a               */
	/*  v-component to the wind at pressure level (pres).        */
	/*                                                           */
	/*  pres             - Level(mb) to compute a V-Component    */
	/*************************************************************/
	{
	short below, above, i, ok;
	float vtop, vbot, nm1;

	below=0;
	above=0;

	/* ----- Find Wind Immediately Above level ----- */
	ok = 0;
	for (i = 0; i < numlvl; i++)
	   {
           if((sndg[i][1] == pres) && (sndg[i][5] != -999.0F) &&
              (sndg[i][6] != -999.0F))
              return vcomp( sndg[i][5], sndg[i][6] );
	   if((sndg[i][1] < pres) && (sndg[i][5] != -999.0F))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Wind Immediately Below level ----- */
	ok = 0;
	for (i = numlvl - 1; i > -1; i--)
	   {
	   if((sndg[i][1] >= pres) && (sndg[i][5] != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return vcomp( sndg[above][5], sndg[above][6] );
	   }

	/* ----- Now we need to interpolate to get the Wind ----- */
	nm1 = sndg[below][1] - pres;
	vbot = vcomp( sndg[below][5], sndg[below][6] );
	vtop = vcomp( sndg[above][5], sndg[above][6] );
	return vbot - ((nm1 / (sndg[below][1] - sndg[above][1]))
	* (vbot - vtop));
	}


	/*NP*/
        float i_omeg( float pres )
        /*************************************************************/
        /*  I_OMEG                                                   */
        /*  John Hart  NSSFC KCMO                                    */
        /*                                                           */
        /*  Interpolates the given data to calculate a vert motion   */
        /*  at pressure level (pres).                                */
        /*                                                           */
        /*  pres             - Level(mb) to compute a UVV            */
        /*************************************************************/
        {
        short below, above, i, ok;
        double nm1, nm2, nm4;

        below=0;
        above=0;

        /* ----- Find UVV Immediately Above level ----- */
        ok=0;
        for (i = 0; i < numlvl; i++)
           {
           if((sndg[i][1] <= pres) && (qc(sndg[i][0])))
              {
              above = i; ok=1; break;
              }
           }
           if( ok == 0 ) return -999.0F;

        /* ----- Find Temperature Immediately Below level ----- */
        ok = 0;
        for (i = numlvl-1; i > -1; i--)
           {
           if((sndg[i][1] >= pres) && (qc(sndg[i][0])))
              {
              below = i; ok=1; break;
              }
           }
           if( ok == 0 ) return -999.0F;

        /* ----- If both levels are the same, return them ---- */
        if( above == below)
           {
           return sndg[above][0];
           }

        /* ----- Now we need to interpolate to get the temperature ----- */

        nm1 = sndg[above][0] - sndg[below][0];
        nm2 = log( sndg[below][1] / sndg[above][1] );
        nm4 = log( sndg[below][1] / pres );
        return (float)(sndg[below][0] + (( nm4 / nm2) * nm1));
        }

        /*NP*/
	short sfc( void )
	/*************************************************************/
	/*  SFC                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the array pointer to the Surface Level.          */
	/*************************************************************/
	{
	short i;
	for( i = 0; i < numlvl; i++)
	   { if( qc( sndg[i][3] )) { return i; } }
	return 0;
	}


	/*NP*/
	float top_pres( void )
	/*************************************************************/
	/*  TOP_PRES                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the pressure(mb) of highest level in sounding.   */
	/*************************************************************/
	{
	short i;

	for( i=numlvl-1; i>0; i--)
	   {
	   if( qc(sndg[i][1])) { return sndg[i][1]; }
	   }
	return -999;
	}


	/*NP*/
	short qc( float value )
	/*************************************************************/
	/*  QC                                                       */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Quality control of sndg data.  Searches for missing      */
	/*  data (-999) and returns (1 = OK), (0 = missing)          */
	/*************************************************************/
	{
	if( value < -998.0F ) { return 0; }
	if( value > 2.0E+05F ) { return 0; }
	return 1;
	}

	/*NP*/
	char *qc2( float value, char *label, short prec )
	/*************************************************************/
	/*  QC2                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Quality control of sndg data.  Searches for missing      */
	/*  data (-999) and returns (char = OK), (M = missing)       */
	/*  Returns real numbers with (prec) decimal points.         */
	/*************************************************************/
	{
	char fmt[10], st2[10];
	static char st1[40];


	if( !qc(value) )
	   {
	   return "M";
	   }
	else
	   {
	   itoa( prec, st2, 10 );
	   strcpy( fmt, "%." );
	   strcat( fmt, st2 );
	   strcat( fmt, "f%s" );
	   sprintf( st1, fmt, value, label);
	   return st1;
	   }
	}

	/*NP*/
	float ctof( float value )
	/*************************************************************/
	/*  CTOF                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given temperature (c) to (f).                   */
	/*************************************************************/
	{
	if( value <= -998.0F )
	   { return -999; }
	else
	   { return ( 1.8F * value) + 32; }
	}

	/*NP*/
	float ftoc( float value )
	/*************************************************************/
	/*  FTOC                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given temperature (f) to (c).                   */
	/*************************************************************/
	{
	if( value <= -998.0F )
	   { return -999; }
	else
	   { return (float)(5/9) * (value - 32); }
	}

	/*NP*/
	float mtof( float value )
	/*************************************************************/
	/*  MTOF                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given distance (m) to (ft).                     */
	/*************************************************************/
	{
	if( value <= -998.0F )
	   { return -999; }
	else
	   { return value / .3049F; }
	}

	/*NP*/
	float ftom( float value )
	/*************************************************************/
	/*  FTOM                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given distance (ft) to (m).                     */
	/*************************************************************/
	{
	if( value <= -998.0F )
	   { return -999; }
	else
	   { return value * .3049F; }
	}


	/*NP*/
	float agl( float height )
	/*************************************************************/
	/*  AGL                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts height (meters) MSL to AGL.                     */
	/*************************************************************/
	{
	if( height <= -998.0F )
	   { return -999; }
	else
	   { return height - sndg[sfc()][2]; }
	}


	/*NP*/
	float msl( float height )
	/*************************************************************/
	/*  MSL                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts height (meters) AGL to MSL.                     */
	/*************************************************************/
	{
	if( height <= -998.0F )
	   { return -999; }
	else
	   { return sndg[sfc()][2] + height; }
	}


	/*NP*/
	float kt_to_mps( float spd )
	/*************************************************************/
	/*  KT_TO_MPS                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts speed (knots) to meters per second.             */
	/*************************************************************/
	{
	if( spd <= -998.0F )
	   { return -999; }
	else
	   { return spd * .51479F; }
	}


