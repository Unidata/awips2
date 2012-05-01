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
/*  I_OMEG						       */
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
#include "gui.h"
#include "sharp95.h"

float i_temp ( float pres )
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

	if ( sndgp == NULL ) return ( -999.0F );

	below=0;
	above=0;

	/* ----- Find Temperature Immediately Above level ----- */
	ok=0;
	for (i = 0; i < sndgp->numlev; i++)
	   {
           if((sndgp->sndg[i].pres == pres) && ( qc(sndgp->sndg[i].temp)))
              {
              /* if pressure level exists, and temp exists, no need to interpolate */
              return sndgp->sndg[i].temp;
              }
	   if((sndgp->sndg[i].pres < pres) && ( qc(sndgp->sndg[i].temp) ) )
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Temperature Immediately Below level ----- */
	ok = 0;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].pres >= pres) && (sndgp->sndg[i].temp != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndgp->sndg[above].temp;
	   }

	/* ----- Now we need to interpolate to get the temperature ----- */

	nm1 = sndgp->sndg[above].temp - sndgp->sndg[below].temp;
	nm2 = log( sndgp->sndg[below].pres / sndgp->sndg[above].pres );
	nm4 = log( sndgp->sndg[below].pres / pres );
	return (float)(sndgp->sndg[below].temp + (( nm4 / nm2) * nm1));
	}

/*=============================================================================*/

float i_dwpt ( float pres )
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

	if ( sndgp == NULL ) return ( -999.0F );

	below=0;
	above=0;

	/* ----- Find Dew Point Immediately Above level ----- */
	ok=0;
	for (i = 0; i < sndgp->numlev; i++)
	   {
           if((sndgp->sndg[i].pres == pres) && (sndgp->sndg[i].dwpt != -999) && (sndgp->sndg[i].pres != -999))
              {
              /* no need to interpolate if we have the level and dewpoint exists */
              return sndgp->sndg[i].dwpt;
              }
	   if((sndgp->sndg[i].pres < pres) && (sndgp->sndg[i].dwpt != -999.0F))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Dew Point Immediately Below level ----- */
	ok=0;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].pres >= pres) && (sndgp->sndg[i].dwpt != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndgp->sndg[above].dwpt;
	   }

	/* ----- Now we need to interpolate to get the dew point ----- */
	nm1 = sndgp->sndg[above].dwpt - sndgp->sndg[below].dwpt;
	nm2 = log( sndgp->sndg[below].pres / sndgp->sndg[above].pres );
	nm4 = log( sndgp->sndg[below].pres / pres );
	return (float)(sndgp->sndg[below].dwpt + (( nm4 / nm2) * nm1));
	}

/*===========================================================================*/

float i_hght ( float pres )
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
	short below, above, i, ok;
	double nm1, nm2, nm4;

	if ( sndgp == NULL ) return ( -999.0F );

	below=0;
	above=0;

	/* ----- Find Height Immediately Above level ----- */
	ok=0;
	for (i = 0; i < sndgp->numlev; i++)
	   {
	   if((sndgp->sndg[i].pres == pres) && (sndgp->sndg[i].hght != -999) && (sndgp->sndg[i].pres != -999))
             {
             /* no need to interpolate if we have the level!!! */
             return sndgp->sndg[i].hght;
             }
	   if((sndgp->sndg[i].pres < pres) && (sndgp->sndg[i].hght != -999) && (sndgp->sndg[i].pres != -999))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Height Immediately Below level ----- */
	ok=0;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].pres >= pres) && (sndgp->sndg[i].hght != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndgp->sndg[above].hght;
	   }

	/* ----- Now we need to interpolate to get the height ----- */
	nm1 = sndgp->sndg[above].hght - sndgp->sndg[below].hght;
	nm2 = log( sndgp->sndg[below].pres / sndgp->sndg[above].pres );
	nm4 = log( sndgp->sndg[below].pres / pres );
	return (float)(sndgp->sndg[below].hght + (( nm4 / nm2) * nm1));
	}

/*==========================================================================*/

float i_pres ( float hght )
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
	double nm1, nm2, nm3;

	if ( sndgp == NULL ) return ( -999.0F );

	below = 0;
	above = 0;

	/* ----- Find Pressure Immediately Above level ----- */
	ok = 0;
	for (i = 0; i < sndgp->numlev; i++)
	   {
           /* if we have the level, no need to interpolate pressure */
           if((sndgp->sndg[i].hght == hght) && (sndgp->sndg[i].pres != -999.0F))
              return(sndgp->sndg[i].pres);
	   if((sndgp->sndg[i].hght > hght) && (sndgp->sndg[i].pres != -999.0F))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Pressure Below level ----- */
	ok = 0;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].hght <= hght) && (sndgp->sndg[i].pres != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndgp->sndg[above].pres;
	   }

	/* ----- Now we need to interpolate to get the Pressure ----- */
	nm1 = hght - sndgp->sndg[below].hght;
	nm2 = sndgp->sndg[above].hght - sndgp->sndg[below].hght;
	nm3 = log( sndgp->sndg[above].pres / sndgp->sndg[below].pres );
	return (float)(sndgp->sndg[below].pres * exp(( nm1 / nm2) * nm3));
	}

/*============================================================================*/

float i_vtmp ( float pres )
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

/*============================================================================*/

float virtemp ( float pres, float temp, float dwpt )
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

/*=============================================================================*/

float i_wdir ( float pres )
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

/*=============================================================================*/

float i_wspd ( float pres )
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

/*=============================================================================*/

float i_wndu ( float pres )
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

	if ( sndgp == NULL ) return ( -999.0F );

	below=0;
	above=0;

	/* ----- Find Wind Immediately Above level ----- */
	ok = 0;
	for (i = 0;i < sndgp->numlev; i++)
	   {
	   if((sndgp->sndg[i].pres == pres) && (sndgp->sndg[i].drct != -999.0F) &&
              (sndgp->sndg[i].sped != -999.0F))
              return ucomp( sndgp->sndg[i].drct, sndgp->sndg[i].sped );
	   if((sndgp->sndg[i].pres < pres) && (sndgp->sndg[i].drct != -999.0F))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Wind Immediately Below level ----- */
	ok = 0;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].pres >= pres) && (sndgp->sndg[i].drct != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return ucomp( sndgp->sndg[above].drct, sndgp->sndg[above].sped );
	   }

	/* ----- Now we need to interpolate to get the Wind ----- */
	nm1 = sndgp->sndg[below].pres - pres;
	ubot = ucomp( sndgp->sndg[below].drct, sndgp->sndg[below].sped );
	utop = ucomp( sndgp->sndg[above].drct, sndgp->sndg[above].sped );
	return ubot - (nm1 / (sndgp->sndg[below].pres - sndgp->sndg[above].pres))
	* (ubot - utop);
	}

/*========================================================================*/

float i_wndv ( float pres )
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

	if ( sndgp == NULL ) return ( -999.0F );

	below=0;
	above=0;

	/* ----- Find Wind Immediately Above level ----- */
	ok = 0;
	for (i = 0; i < sndgp->numlev; i++)
	   {
	   if((sndgp->sndg[i].pres == pres) && (sndgp->sndg[i].drct != -999.0F) &&
              (sndgp->sndg[i].sped != -999.0F))
              return vcomp( sndgp->sndg[i].drct, sndgp->sndg[i].sped );
	   if((sndgp->sndg[i].pres < pres) && (sndgp->sndg[i].drct != -999.0F))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Wind Immediately Below level ----- */
	ok = 0;
	for (i = sndgp->numlev - 1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].pres >= pres) && (sndgp->sndg[i].drct != -999.0F))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return vcomp( sndgp->sndg[above].drct, sndgp->sndg[above].sped );
	   }

	/* ----- Now we need to interpolate to get the Wind ----- */
	nm1 = sndgp->sndg[below].pres - pres;
	vbot = vcomp( sndgp->sndg[below].drct, sndgp->sndg[below].sped );
	vtop = vcomp( sndgp->sndg[above].drct, sndgp->sndg[above].sped );
	return vbot - ((nm1 / (sndgp->sndg[below].pres - sndgp->sndg[above].pres))
	* (vbot - vtop));
	}

/*=============================================================================*/

float i_omeg ( float pres )
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

	if ( sndgp == NULL ) return ( -999.0F );

	below=0;
	above=0;

	/* ----- Find UVV Immediately Above level ----- */
	ok=0;
	for (i = 0; i < sndgp->numlev; i++)
	   {
	   if((sndgp->sndg[i].pres <= pres) && (qc(sndgp->sndg[i].omega)))
	      {
	      above = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- Find Temperature Immediately Below level ----- */
	ok = 0;
	for (i = sndgp->numlev-1; i > -1; i--)
	   {
	   if((sndgp->sndg[i].pres >= pres) && (qc(sndgp->sndg[i].omega)))
	      {
	      below = i; ok=1; break;
	      }
	   }
	   if( ok == 0 ) return -999.0F;

	/* ----- If both levels are the same, return them ---- */
	if( above == below)
	   {
	   return sndgp->sndg[above].omega;
	   }

	/* ----- Now we need to interpolate to get the temperature ----- */

	nm1 = sndgp->sndg[above].omega - sndgp->sndg[below].omega;
	nm2 = log( sndgp->sndg[below].pres / sndgp->sndg[above].pres );
	nm4 = log( sndgp->sndg[below].pres / pres );
	return (float)(sndgp->sndg[below].omega + (( nm4 / nm2) * nm1));
	}

/*=============================================================================*/

short sfc ( void )
/*************************************************************/
/*  SFC                                                      */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Returns the array pointer to the Surface Level.          */
/*************************************************************/
{
	short i;

	if ( sndgp == NULL ) return ( 0 );

	for( i = 0; i < sndgp->numlev; i++)
	   if( qc( sndgp->sndg[i].temp )) return i;

	return 0;
	}

/*=============================================================================*/

float top_pres ( void )
/*************************************************************/
/*  TOP_PRES                                                 */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Returns the pressure(mb) of highest level in sounding.   */
/*************************************************************/
{
	short i;

	if ( sndgp == NULL ) return ( -999.0F );

	for( i=sndgp->numlev-1; i>0; i--)
	   {
	   if( qc(sndgp->sndg[i].pres)) { return sndgp->sndg[i].pres; }
	   }
	return -999;
	}

/*=============================================================================*/

short qc ( float value )
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

/*=============================================================================*/

char *qc2 ( float value, char *label, short prec )
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

/*=============================================================================*/

float ctof ( float value )
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

/*=============================================================================*/

float ftoc ( float value )
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

/*=============================================================================*/

float mtof ( float value )
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

/*=============================================================================*/

float ftom ( float value )
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

/*=============================================================================*/

float agl ( float height )
/*************************************************************/
/*  AGL                                                      */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Converts height (meters) MSL to AGL.                     */
/*************************************************************/
{
	if ( sndgp == NULL ) return ( -999.0F );

	if( height <= -998.0F )
	   { return -999; }
	else
	   { return height - sndgp->sndg[sfc()].hght; }
	}

/*=============================================================================*/

float msl ( float height )
/*************************************************************/
/*  MSL                                                      */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Converts height (meters) AGL to MSL.                     */
/*************************************************************/
{
	if ( sndgp == NULL ) return ( -999.0F );

	if( height <= -998.0F )
	   { return -999; }
	else
	   { return sndgp->sndg[sfc()].hght + height; }
	}

/*=============================================================================*/

float kt_to_mps ( float spd )
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

