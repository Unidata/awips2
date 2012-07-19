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

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "basics.h"
#include "sndglib.h"

/* 
 * General interpolation routine
 *
 * var is a variable name (TEMP, DWPT, etc.) in your data set
 * var may also be SPED for wind speed or DRCT for wind direction
 *
 * Right now no other interpolation is done for derived parameters
 *
 * level is the level you wish to interpolate to
 * itype is the interpolation type. Valid types are
 * I_PRES for pressure
 * I_HGHT for height
 *
 */
float i_var(char *var, float level, short itype)
{
	short idx1, idx2, idx3, idxabove1, idxbelow1, idxabove2, idxbelow2, levidx;
	float u, v, dir, spd;
	int debugx;

	debugx=0;
	if (!strcasecmp(var, "U"))
		{ 
		debugx = 0;
		/* printf( "Interpolating %s\n", var);  */
		}

	/* Make sure we don't work on NULL pointers */
	if (!sndg)
	  return RMISSD;

	/* Catch stupid stuff */
	if ((!strcasecmp(var, "PRES") && itype == I_PRES) || (!strcasecmp(var, "HGHT") && itype == I_HGHT))
	  return level;

	levidx = -1;
	if (itype == I_PRES)
		{ levidx = getParmIndex("PRES"); }
	else if (itype == I_HGHT) 
		{ levidx = getParmIndex("HGHT"); }

	if (levidx == -1) return RMISSD;

	/* Handle wind speed and direction separately from the basic
	   parameters such as temp, dwpt, hght, etc. */
	if (!strcasecmp(var, "DRCT") || !strcasecmp(var, "SPED")) 
		{
	  	idx1 = getParmIndex("DRCT");
	  	idx2 = getParmIndex("SPED");
	  	if (idx1 == -1 || idx2 == -1) return RMISSD;

	  	/* Get the array indices in the vertical for this data */
		  idxabove1 = getlevelindex(idx1, level, ABOVE, itype);
		  idxabove2 = getlevelindex(idx2, level, ABOVE, itype);
		  idxbelow1 = getlevelindex(idx1, level, BELOW, itype);
		  idxbelow2 = getlevelindex(idx2, level, BELOW, itype);

		  if (idxabove1 == -1 || idxbelow1 == -1) return RMISSD;

		  /* if they are the same return the data at that level */
		  if (idxabove1 == idxbelow1) 
			{
			u = ucomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]);
		    	v = vcomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]);
	 		}
	  	  else 
			{
			/* begin 12/04/06 edit by RLT to fix interpolation bug */
	 	        /*dir = interp_gen(sndg[idxabove1][idx1], sndg[idxbelow1][idx1],
                             level, sndg[idxabove1][levidx], 
                             sndg[idxbelow1][levidx], itype);
	    		spd = interp_gen(sndg[idxabove2][idx2], sndg[idxbelow2][idx2],
                             level, sndg[idxabove2][levidx], 
                             sndg[idxbelow2][levidx], itype);
	    		u = ucomp(dir, spd);
	    		v = vcomp(dir, spd);*/
			
			u = interp_gen(ucomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]), 
			    ucomp(sndg[idxbelow1][idx1], sndg[idxbelow1][idx2]),
			    level, sndg[idxabove1][levidx],
 			    sndg[idxbelow1][levidx], itype);
 
			v = interp_gen(vcomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]),
                            vcomp(sndg[idxbelow1][idx1], sndg[idxbelow1][idx2]),
                            level, sndg[idxabove1][levidx],
                            sndg[idxbelow1][levidx], itype);

			/* end 12/04/06 edit */
	  		}

	 	 if (!strcasecmp(var, "DRCT")) 
			{ return angle(u, v); }
		  else
		    	{ return sqrt((u * u) + (v * v)); }
		}
	else if (!strcasecmp(var, "PRES") && itype == I_HGHT) 
		{ return i_pres(level); }
	else if (!strcasecmp(var, "U") || !strcasecmp(var, "V")) 
		{

		idx1 = getParmIndex("DRCT");
	  	idx2 = getParmIndex("SPED");
	  	idx3 = getParmIndex("PRES");
	  	if (idx1 == -1 || idx2 == -1) return RMISSD;

	  	/* Get the array indices in the vertical for this data */
	  	idxabove1 = getlevelindex(idx1, level, ABOVE, itype);
	  	idxabove2 = getlevelindex(idx2, level, ABOVE, itype);
		idxbelow1 = getlevelindex(idx1, level, BELOW, itype);
	  	idxbelow2 = getlevelindex(idx2, level, BELOW, itype);

	  	if (idxabove1 == -1 || idxbelow1 == -1) return RMISSD;

	  	/* if they are the same return the data at that level */
	  	if (idxabove1 == idxbelow1) 
			{
	    		if (!strcasecmp(var, "U"))
	      			{ return ucomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]); }
	    		else
	      			{ return vcomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]); }
	  		}
		else 
			{
	    		dir = interp_gen(sndg[idxabove1][idx1], sndg[idxbelow1][idx1],
                             level, sndg[idxabove1][levidx], 
                             sndg[idxbelow1][levidx], itype);
	    		spd = interp_gen(sndg[idxabove2][idx2], sndg[idxbelow2][idx2],
                             level, sndg[idxabove2][levidx], 
                             sndg[idxbelow2][levidx], itype);
	    
		if (debugx)
			{ printf ("      ABOVE: %7.1f %7.1f %7.1f\n      ANSWER:%7.1f %7.1f %7.1f\n      BELOW: %7.1f %7.1f %7.1f\n",
			  sndg[idxabove1][idx3], sndg[idxabove1][idx1], sndg[idxabove1][idx2],
			  level                , dir                  , spd,
                          sndg[idxbelow1][idx3], sndg[idxbelow1][idx1], sndg[idxbelow1][idx2]); 
			}

			if (!strcasecmp(var, "U"))
	      			{ return ucomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]); }
	    		else
	      			{ return vcomp(sndg[idxabove1][idx1], sndg[idxabove1][idx2]); }
	  		}
		}
	else 
		{
	  	idx1 = getParmIndex(var);
	  	if (idx1 == -1) return RMISSD;

	  	/* Get the array indices in the vertical for this data */
	  	idxabove1 = getlevelindex(idx1, level, ABOVE, itype);
	  	idxbelow1 = getlevelindex(idx1, level, BELOW, itype);

	  	if (idxabove1 == -1 || idxbelow1 == -1) return RMISSD;

	  	if (idxabove1 == idxbelow1) return sndg[idxabove1][idx1];

	  	return interp_gen(sndg[idxabove1][idx1], sndg[idxbelow1][idx1], 
	                    level, sndg[idxabove1][levidx], 
	                    sndg[idxbelow1][levidx], itype);
		}
	return RMISSD;
}

/*
 * getlevelindex
 *
 * Helper routine for i_var() which finds the first level above
 * or below the specified level
 *
 *
 * Inputs:
 * parmIndex - the index in the sndg array for the parameter of
 * interest
 * level - the baseline level you're interested in
 * direction -above (1) or below (1) the baseline level. valid values
 * are ABOVE and BELOW
 * itype - Is this pressure or height you're talking about. values are
 * I_PRES or I_HGHT
 */

short getlevelindex(short parmIndex, float level, short direction, short itype)
{
	short i, idx, ok;

	/* Make sure we don't work on NULL pointers */
	if (!sndg)
	  return -1;

	ok = 0;
	if (itype == I_PRES) {
	  idx = getParmIndex("PRES");
	  if (idx == -1) return -1;
	  if (direction == ABOVE) {
	    /* Above the level */
	    for (i=0; i < numlvl; i++) {
	      if ((sndg[i][idx] <= level) && (qc(sndg[i][parmIndex])))
	        return i;
	    }
	  }
	  else {
	    /* Below the level */
	    for (i=numlvl-1; i >= 0; i--) {
	      if ((sndg[i][idx] >= level) && (qc(sndg[i][parmIndex])))
	        return i;
	    }
	  }
	}
	else if (itype == I_HGHT) {
	  idx = getParmIndex("HGHT");
	  if (idx == -1) return -1;
	  if (direction == ABOVE) {
	    /* Above the level */
	    for (i=0; i < numlvl; i++) {
	      if ((sndg[i][idx] >= level) && (qc(sndg[i][parmIndex])))
	        return i;
	    }
	  }
	  else {
	    /* Below the level */
	    for (i=numlvl-1; i >= 0; i--) {
	      if ((sndg[i][idx] <= level) && (qc(sndg[i][parmIndex])))
	        return i;
	    }
	  }
	}

	return -1;
}

/*
 * General interpolation routine
 */
float interp_gen(float valueabove, float valuebelow, float level,
	         float levelabove, float levelbelow, short itype)
{
	float nm1, nm2, nm3, val;

	if (!qc(valueabove) || !qc(valuebelow) || !qc(levelabove) ||
	    !qc(levelbelow))
	  return RMISSD;

	val = RMISSD;

	if (itype == I_PRES) {
	  nm1 = valueabove - valuebelow;
	  nm2 = log(levelbelow / levelabove);
	  nm3 = log(levelbelow / level);
	  val = valuebelow + ((nm3 / nm2) * nm1);
	}
	else if (itype == I_HGHT) {
	  nm1 = level - levelbelow;
	  nm2 = levelabove - levelbelow;
	  val = valuebelow + ((nm1 / nm2) * (valueabove - valuebelow));
	}

	return val;
}


float i_pres(float hght)
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
	short below, above, i, idxp, idxz, ok;
	double nm1, nm2, nm3;

	below = 0;
	above = 0;

	/* Make sure we don't work on NULL pointers */
	if (!sndg)
	  return RMISSD;

	idxp = getParmIndex("PRES");
	idxz = getParmIndex("HGHT");

	if (idxz == -1 || idxp == -1)
	  return RMISSD;

	/* ----- Find Pressure Immediately Above level ----- */
	ok = 0;
	for (i=0; i < numlvl; i++) {
	   if ((sndg[i][idxz] >= hght) && qc(sndg[i][idxp])) {
	      above = i;
	      ok=1;
	      break;
	   }
	}
	if (!ok)
	  return RMISSD;

	/* ----- Find Pressure Below level ----- */
	ok = 0;
	for (i=numlvl-1; i > -1; i--) {
	   if ((sndg[i][idxz] <= hght) && qc(sndg[i][idxp])) {
	      below = i;
	      ok=1;
	      break;
	   }
	}
	if (!ok)
	  return RMISSD;

	/* ----- If both levels are the same, return them ---- */
	if (above == below)
	   return sndg[above][idxp];

	/* ----- Now we need to interpolate to get the Pressure ----- */
	nm1 = hght - sndg[below][idxz];
	nm2 = sndg[above][idxz] - sndg[below][idxz];
	nm3 = log(sndg[above][idxp] / sndg[below][idxp]);
	return (float)(sndg[below][idxp] * exp((nm1 / nm2) * nm3));
}


short sfc(void)
	/*************************************************************/
	/*  SFC                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the array pointer to the Surface Level.          */
	/*************************************************************/
{
	short i, j;

	/* Make sure we don't work on NULL pointers */
	if (!sndg)
	  return RMISSD;

	/* Note: we qc on U and not on other fields. */

	j = getParmIndex("TEMP");
	if (j == -1)
	  return 0;

	for (i=0; i < numlvl; i++)
	  if (qc(sndg[i][j])) 
	    return i;

	return 0;
}


	/*NP*/
float top_pres(void)
	/*************************************************************/
	/*  TOP_PRES                                                 */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Returns the pressure(mb) of highest level in sounding.   */
	/*************************************************************/
{
	short i, idx;

	/* Make sure we don't work on NULL pointers */
	if (!sndg)
	  return RMISSD;

	idx = getParmIndex("PRES");
	if (idx == -1)
	  return RMISSD;

	for( i=numlvl-1; i>0; i--) {
	   if (qc(sndg[i][idx]))
	     return sndg[i][idx];
	}
	return RMISSD;
}


	/*NP*/
short qc(float value)
	/*************************************************************/
	/*  QC                                                       */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Quality control of sndg data.  Searches for missing      */
	/*  data (-999) and returns (1 = OK), (0 = missing)          */
	/*************************************************************/
{
	if (value < -998.0 || value > 2.0E+05)
	  return 0;

	return 1;
}

	
char *qc2(float value, char *label, short prec)
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

	if (!qc(value)) {
	   strcpy(st1, "M");
	}
	else {
	   itoa(prec, st2, 10);
	   strcpy(fmt, "%.");
	   strcat(fmt, st2);
	   strcat(fmt, "f%s");
	   sprintf(st1, fmt, value, label);
	}
	return st1;
}


float ctof(float value)
	/*************************************************************/
	/*  CTOF                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given temperature (c) to (f).                   */
	/*************************************************************/
{
	if (!qc(value))
	   return RMISSD;
	else
	   return ((1.8 * value) + 32.0);
}


float ftoc(float value)
	/*************************************************************/
	/*  FTOC                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given temperature (f) to (c).                   */
	/*************************************************************/
{
	if (!qc(value))
	  return RMISSD;
	else
	  return ((5.0/9.0) * (value - 32.0)); 
}


float mtof(float value)
	/*************************************************************/
	/*  MTOF                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given distance (m) to (ft).                     */
	/*************************************************************/
{
	if (!qc(value))
	  return RMISSD;
	else
	  return (value / 0.3049);
}


float ftom(float value)
	/*************************************************************/
	/*  FTOM                                                     */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts given distance (ft) to (m).                     */
	/*************************************************************/
{
	if (!qc(value))
	  return RMISSD;
	else
	  return (value * .3049);
}


	
float agl(float height)
	/*************************************************************/
	/*  AGL                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts height (meters) MSL to AGL.                     */
	/*************************************************************/
{
	short idx;

	if (!sndg)
	  return RMISSD;

	if (!qc(height))
	  return RMISSD;
	else {
	  idx = getParmIndex("HGHT");
	  if (idx == -1)
	    return RMISSD;
	  else
	    return (height - sndg[sfc()][idx]);
	}
}


float msl(float height)
	/*************************************************************/
	/*  MSL                                                      */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts height (meters) AGL to MSL.                     */
	/*************************************************************/
{
	short idx;

	if (!sndg)
	  return RMISSD;

	if (!qc(height))
	  return RMISSD;
	else {
	  idx = getParmIndex("HGHT");
	  if (idx == -1)
	    return RMISSD;
	  else
	    return (sndg[sfc()][idx] + height);
	}
}


float kt_to_mps(float spd)
	/*************************************************************/
	/*  KT_TO_MPS                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Converts speed (knots) to meters per second.             */
	/*************************************************************/
{
	if (!qc(spd))
	  return RMISSD;
	else
	  return (spd * 0.51479);
}


char *itoa(int value, char *st, int radx)
       /*************************************************************/
       /* ITOA                                                      */
       /*************************************************************/
{
        sprintf(st, "%d", value);
        return st;
}


void xtnd_sndg(void)
	/*************************************************************/
	/*  XTND_SNDG                                                */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine will extend a sounding to 50mb, if the      */
	/*  sounding made it at least to 150mb.                      */
	/*                                                           */
	/*  An isothermal/hydrostatic layer is assumed above the     */
	/*  last observed level.                                     */
	/*************************************************************/
{
	short i, p, above, below, idxp, idxz;
	double nm1, nm2, nm4;

	/* Make sure we don't work on NULL pointers */
	if (!sndg || numlvl < 1)
	  return;

	return;

	/* 
	 * If we don't have pressure data in the sounding then it 
	 * makes no sense to extend it in this manner!
	 */
	idxp = getParmIndex("PRES");
	idxz = getParmIndex("HGHT");
	if (idxp == -1 || idxz == -1)
	  return;

	/* Is the top level between 150 and 100 mb? */
	if (sndg[numlvl-1][idxp] <= 150.0 && sndg[numlvl-1][idxp] >= 100.0) {

	   for (p=75; p >= 50; p-=25) {

	     sndg[numlvl][idxp] = (float)p;

	     /* Interpolate everything but pressure and height */
	     for (i=0; i< ndsetparms; i++) {
	       if (!strcasecmp(dsetparms[i], "PRES") || 
	           !strcasecmp(dsetparms[i], "HGHT"))
  	         continue;
  	       sndg[numlvl][i] = i_var(dsetparms[i], 150.0, I_PRES);
	     }

	     /* 
	      * Compute height
	      *
	      * We have to do this separately as an extrapolation. 
	      * That's why it's not in the loop above with the 
	      * other parameters
	      */

	     idxz = getParmIndex("HGHT");
	     if (idxz != -1) {
	       above = numlvl-1;
	       below = numlvl-2;

	       sndg[numlvl][idxz] = interp_gen(sndg[above][idxz], 
	         sndg[below][idxz], sndg[numlvl][idxp],
	         sndg[above][idxp], sndg[below][idxp], I_PRES);
	     }
	     numlvl++;
	   }
	}
}


void interp_sndg(void)
	/*************************************************************/
	/*  INTERP_SNDG                                              */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  This routine take the current sounding array and         */
	/*  interpolate it to 25mb increments.                       */
	/*************************************************************/
{

/*	printf( "\nCurrent Parcel begin interp_sndg (Sndglib/basics.c):  %d\n", lplvals.flag);
*/	float **newsndg = NULL;
	float nsndg[200][7], sfclvl, pres, cint, p1;
	short newnum, i, j, k, idx;

	/* Make sure we don't work on NULL pointers */
	if (!sndg || numlvl < 1)
	  return;

	idx = getParmIndex("PRES");
	if (idx == -1)
	  return;

	/*
	 * Allocate space for new sounding. Let's assume the top of
	 * the sounding will be 25mb at most, and the bottom is 1050mb
	 * Let's add 2 as a bufr as well
	 */
	cint = 25.0;
	i = (short)((1050.0 - 25.0) / cint) + 2;

	newsndg = (float **)malloc(i * sizeof(float *));
	if (!newsndg)
	  return;

	newnum = 0;
	newsndg[newnum] = (float *)malloc(ndsetparms * sizeof(float));
	if (!newsndg[newnum])
	  return;

	/* ----- Copy surface conditions to new array ----- */
	sfclvl = sndg[sfc()][idx];

	newsndg[newnum][idx] = sfclvl;

	/* Copy everything but pressure */
	for (j=0; j < ndsetparms; j++) {
	  if (strcasecmp(dsetparms[j], "PRES")) {
	    newsndg[newnum][j] = i_var(dsetparms[j], sfclvl, I_PRES);
	  }
	}

	/* ----- Determine first interpolated level above surface ----- */
	pres = (float)((short)((sfclvl / cint) + 0) * cint);
	if (pres == sfclvl)
	  pres -= cint;

	/* ----- Interpolate NSNDG array to prescribed increments ----- */
	for (p1=pres; p1 >= sndg[numlvl-1][idx]; p1-=cint) {
	  newnum++;
	  newsndg[newnum] = (float *)malloc(ndsetparms * sizeof(float));
	  if (!newsndg[newnum])
	    break;

	  newsndg[newnum][idx] = p1;
	  for (j=0; j < ndsetparms; j++) {
	    if (strcasecmp(dsetparms[j], "PRES")) {
	      newsndg[newnum][j] = i_var(dsetparms[j], p1, I_PRES);
	    }
	  }
	}

	/* ----- Copy NSNDG to SNDG array ----- */
	for (i=0;i<globalsndg->nlev;i++)
  	  free(globalsndg->data[i]);
	free(globalsndg->data);

        numlvl           = newnum;
	globalsndg->nlev = numlvl;
	globalsndg->data = newsndg;
	sndg             = globalsndg->data;
}

void check_data()
	/***********************************************************/
	/* CHECK_DATA						   */
	/* Routine to check the data array for "out of order" data */
	/* or other items that might lead to a crash.              */
	/***********************************************************/
	{
	int idx_p, idx_h, idx_t, idx_td, idx_ws, idx_wd, idx_uv;
	int i, j, i0, i1, ok;
	float tmp, p0, p1;

	printf ("Beginning CHECK_DATA\n");

	idx_p = getParmIndex("PRES");
	idx_h = getParmIndex("HGHT");
	idx_t = getParmIndex("TEMP");
	idx_td = getParmIndex("DWPT");
	idx_ws = getParmIndex("SPED");
	idx_wd = getParmIndex("DRCT");
	idx_uv = getParmIndex("OMEG");

	/* Fix bad height at 1000mb */
	for(i=1; i<numlvl; i++)
		{
		if (sndg[i][idx_p] == 1000)
			{
			if ((sndg[i][idx_h] > 5000)) sndg[i][idx_h] = -9999.0;
			break;
			}
		}

	/* Check to make sure array is in ascending order */
	/* Continue to loop through array until no swaps are required. */
	ok=1;
	while(ok==1)
		{
		p0 = sndg[0][idx_p];
		i0 = 0;
		ok = 0;
		for(i=1; i<numlvl; i++)
			{
			p1 = sndg[i][idx_p];
			i1 = i;
			if (p1 > p0) 
				{
				ok = 1;
				printf("Order error between item %d and item %d.  Items swapped.\n", i0, i1);
				printf("I0 = %d  %7.2f %7.2f\n", i0, sndg[i0][idx_p], sndg[i0][idx_h]);
				printf("I1 = %d  %7.2f %7.2f\n", i1, sndg[i1][idx_p], sndg[i1][idx_h]);
				for(j=0;j<7;j++)
					{
					tmp = sndg[i0][j];
					sndg[i0][j] = sndg[i1][j];
					sndg[i1][j] = tmp;
					}
				break;
				}
			p0=p1;
			i0=i1;
			}
		printf ("Finished one loop\n");
		}

	/* Next, fill all levels that have missing data */
	for(i=1; i<numlvl; i++)
		{
		if (!qc(sndg[i][idx_h]))  sndg[i][idx_h] =  i_hght(sndg[i][idx_p], I_PRES);
		if (!qc(sndg[i][idx_t]))  sndg[i][idx_t] =  i_temp(sndg[i][idx_p], I_PRES);
		if (!qc(sndg[i][idx_td])) sndg[i][idx_td] = i_dwpt(sndg[i][idx_p], I_PRES);
		/* if (!qc(sndg[i][idx_wd])) sndg[i][idx_wd] = i_wdir(sndg[i][idx_p], I_PRES); */
		/* if (!qc(sndg[i][idx_ws])) sndg[i][idx_ws] = i_wspd(sndg[i][idx_p], I_PRES); */
		}

	printf ("Finished CHECK_DATA\n");

	/* ----- Copy to GLOBALSNDG array ----- */
	globalsndg->nlev = numlvl;
	globalsndg->data = sndg;
	}



float i_temp(float level, short itype)
{
	return i_var("TEMP", level, itype);
}

float i_dwpt(float level, short itype)
{
	return i_var("DWPT", level, itype);
}

float i_hght(float level, short itype)
{
	return i_var("HGHT", level, itype);
}

float i_wndu(float level, short itype)
{
	return i_var("U", level, itype);
}

float i_wndv(float level, short itype)
{
	return i_var("V", level, itype);
}

float i_omeg(float level, short itype)
{
	return i_var("OMEG", level, itype);
}

float i_wdir(float level, short itype)
{
	return i_var("DRCT", level, itype);
}

float i_wspd(float level, short itype)
{
	return i_var("SPED", level, itype);
}

float i_vtmp(float level, short itype)
{
	double cta, eps, tk, w;
	float  t, td, pres;

	pres = level;
	t  = i_temp(level, itype);
	td = i_dwpt(level, itype);

	/* 
	 * Return temperature in those cases when we can't get the
	 * dewpoint temp
	 */
	if (!qc(td)) 
		{ 
		if (!qc(t)) 
			{ return RMISSD; }
		else 
			{ return t; }
		}

	cta = 273.15;
	eps = 0.62197;

	/* 
	 * Note that if we're working with height data we need to
	 * compute the pressure at this height for use in the mixing
	 * ratio routine.
	 */

	/* if (itype == I_HGHT) pres = i_pres(level); */

	if (!qc(pres)) return RMISSD;

	tk = t + cta;
	w = 0.001 * (double)mixratio(pres, td);
	return (float)(tk * (1.0 + w / eps) / (1.0 + w) - cta);
}
