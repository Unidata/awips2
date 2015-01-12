/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Winter Parameter Calculations                              */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sndglib.h"

/*NP*/
float ptype1( void )
/*************************************************************/
/*  PTYPE1                                                   */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Determines precipitation type using same method as       */
/*  MESOETA model.                                           */
/*                                                           */
/*************************************************************/
{
	short i, IWX;
	float pmid, T[50], P[50], Q[50], PINT[50], LMH;
	float TD[50], TWET[50], LM, PPT;
	short pIndex;

	pIndex = getParmIndex("PRES");

	if (!sndg || pIndex == -1)
	  return RMISSD;

	for (i=0;i<numlvl-1;i++) {
	  pmid = (sndg[i][pIndex] + sndg[i+1][pIndex]) / 2;
	  P[i] = pmid;
	  T[i] = i_temp(pmid, I_PRES);
	  Q[i] = mixratio(pmid, i_dwpt(pmid, I_PRES)); 
	  PINT[i] = sndg[i][pIndex] - sndg[i+1][pIndex];
	}

	LMH = numlvl;


/* Call M.Baldwin FORTRAN routine to actually compute this stuff */
/* CALWXT(T, Q, P, PINT, TD, TWET, LMH, LM, PPT, IWX); */
	printf( "IWX = %d\n", IWX );
}


/*NP*/
char *init_phase(float *plevel, short *phase)
/*************************************************************/
/*  INIT_PHASE                                               */
/*************************************************************/
{
	short       i, ok, avail;
	short       pIndex, zIndex, tIndex, tdIndex, oIndex;
	float       rh, p1, pbegin, pos, neg, p, w1;
	float       p_pres, p_phase, ptop, pbot;
	static char st[80];
	char        pt[80];

	*plevel = 0;
	*phase = -1;

	pIndex  = getParmIndex("PRES");
	zIndex  = getParmIndex("HGHT");
	tIndex  = getParmIndex("TEMP");
	tdIndex = getParmIndex("DWPT");
	oIndex  = getParmIndex("OMEG");

	if (!sndg || pIndex == -1 || zIndex == -1 || tIndex == -1 || 
	    tdIndex == -1) {
	  strcpy(st, "N/A");
	  return st;
	}

	/* First, determine whether VVELS are available.  If they are,   */
	/* use them to determine level where precipitation will develop. */
	avail=0;
	if (oIndex != -1) {
          for( i = 0; i < numlvl; i++) {
	    if (qc(sndg[i][oIndex]) && (sndg[i][oIndex] < 1)) avail++;
	  }
	}

	if (avail< 5) {
	   /* No VVELS...must look for saturated level */

	   /* ----- Find the highest near-saturated 50mb 
	      layer blo 5km agl ---- */
	   for(i=numlvl-1;i>0;i--) {
	      ok = 0;
	      pbegin = -999;
	      if (agl(sndg[i][zIndex]) < 5000.0) {
		  rh = mixratio(sndg[i][pIndex], sndg[i][tdIndex]) / 
	               mixratio(sndg[i][pIndex], sndg[i][tIndex]);
		  if (rh > 0.8) {
		    p1 = sndg[i][pIndex]+50;
		     if ((mixratio(p1, i_dwpt(p1, I_PRES)) / 
	                 mixratio(p1, i_temp(p1, I_PRES))) > 0.8) {
				ok = 1;
				pbegin = p1-25.0;
				break;
		     }
		}
	      }
	   }
	}
	else {
		/* ----- Find the highest near-saturated layer with UVV in 
	           the lowest 5km ----- */
	        for(i=numlvl-1;i>0;i--) {
			ok=0;
			pbegin=-999;
			if ((agl(sndg[i][zIndex])<5000) && 
	                    (sndg[i][oIndex] <= 0)) {
		  		rh = mixratio(sndg[i][pIndex], 
	                                      sndg[i][tdIndex]) / 
	                             mixratio(sndg[i][pIndex], sndg[i][tIndex]);
		  		if (rh > 0.8) {
					p1 = sndg[i][pIndex]+50;
				if ((mixratio(p1, i_dwpt(p1, I_PRES)) / 
	                             mixratio(p1, i_temp(p1, I_PRES))) > 0.8) {
						ok = 1;
						pbegin = p1-25;
						break;
				}
				}
				}
			}
	      }	

	if (!ok) {
	   *plevel =  0;
	   *phase  = -1;
	   strcpy(st, "N/A");
	   return st;
	}

	p1 = i_temp(pbegin, I_PRES);
	if(p1>0) {p_phase=0; strcpy(pt, "Rain"); }
	if((p1<=0) && (p1 > -5)) {p_phase=1; strcpy(pt, "Freezing Rain"); }
	if((p1<=-5) && (p1 > -9)) {p_phase=1; strcpy(pt, "ZR/S Mix" ); }
	if(p1 <= -9) {p_phase=3; strcpy(pt, "Snow"); }
	*plevel = pbegin;
	*phase = p_phase;
	return pt;
}


/*NP*/
void posneg_wetbulb(float start, float *pos, float *neg, float *top, float *bot)
/***********************************************************************/
/* POSNEG                                                              */
/* Calculates positive and negative areas as related to winter weather */
/* forecasting.  Search begins at 500mb, but only returns results if   */
/* a positive area is found, overlaying a negative area.               */
/* START is the upper limit of search.(default=init_phase)             */
/***********************************************************************/
{
	float upper, lower, pe1, h1, te1, tp1, totp, totn, pe2, h2, te2, 
	      tp2, tdef1, tdef2;
	float lyrlast, lyre, tote, pelast, ptop, pbot, lvl;
	short i, lptr, uptr, warmlayer=0, coldlayer=0, phase;
	short pIndex, zIndex, tdIndex;
	char  st[80];

	*pos = 0;
	*neg = 0;
	*top = 0;
	*bot = 0;

	/* ----- If there is no sounding, do not compute ----- */
	if (!qc(i_temp(500, I_PRES)) && !qc(i_temp(850, I_PRES))) 
	  return;

	pIndex  = getParmIndex("PRES");
	zIndex  = getParmIndex("HGHT");
	tdIndex = getParmIndex("DWPT");

	if (!sndg || pIndex == -1 || zIndex == -1 || tdIndex == -1)
	  return;
        
	/* ----- Find lowest observation in layer ----- */
	lower = sndg[sfc()][pIndex];
        lptr  = sfc();

        /* ----- Find highest observation in layer ----- */
	if (start=-1) {
		strcpy( st, init_phase( &lvl, &phase ));
		if (lvl> 0)
	   	   { upper = lvl; }
		else
	   	   { upper=500; }
	}
	else { 
	  upper = start;
	}

        i=numlvl-1;
        while (sndg[i][pIndex] < upper) {
	  i--;
	  if (i < 0) {
	    fprintf(stderr, 
      "Warning: posneg_wetbulb: Could not find a pressure greater than %.2f\n",
	      upper);
	    fprintf(stderr, "Using %.2f as the upper level.\n", 
	      sndg[0][pIndex]);
	    i = 0;
	    break;
	  }
	}
        uptr = i;

        if (sndg[i][pIndex] == upper)
	  uptr--;

        /* ----- Start with top layer ----- */
        pe1 = upper;
        h1 =  i_hght(pe1 , I_PRES);
        te1 = wetbulb(pe1, i_temp(pe1, I_PRES), i_dwpt(pe1, I_PRES));
        tp1 = 0;

        totp = totn = tote = ptop = pbot = 0;

        for( i = uptr; i >= lptr; i-- ) {
           if (qc(sndg[i][tdIndex])) {
              /* ----- Calculate every level that reports a temp ----- */
              pe2 = sndg[i][pIndex];
              h2 =  sndg[i][zIndex];
              te2 = wetbulb(pe2, i_temp(pe2, I_PRES), i_dwpt(pe2, I_PRES));
              tp2 = 0;
              tdef1 = (0 - te1) / (te1 + 273.15);
              tdef2 = (0 - te2) / (te2 + 273.15);
              lyrlast = lyre;
              lyre = 9.8F * (tdef1 + tdef2) / 2.0F * (h2 - h1);

	      /* Has a warm layer been found yet? */
	      if (te2>0) 
		if (warmlayer==0) {
			warmlayer=1;
			ptop=pe2;
		}

	      /* Has a cold layer been found yet? */
	      if (te2<0) 
		if ((warmlayer==1) && (coldlayer==0)) {
			coldlayer=1;
			pbot=pe2;
		}

	      if (warmlayer>0) {
	         if (lyre>0) { 
	           totp += lyre;
	         }
	         else {
	           totn += lyre;
	         }
	         tote += lyre;
 
	        // printf("%4.0f - %4.0f E=%6.0f TOT=%6.0f Top=%6.0f Bot=%6.0f\n",
	       //  pe1, pe2, lyre, tote, ptop, pbot);
	      }

              pelast = pe1;
              pe1 = pe2;
              h1 = h2;
              te1 = te2;
              tp1 = tp2;
	      }
 	}

	if ((warmlayer==1) && (coldlayer==1)) {
		*pos = totp;
		*neg = totn;
		*top = ptop;
		*bot = pbot;
		//printf("Tot= %.0f J/kg   Pos= %.0f J/kg   Neg= %.0f J/kg\n",
	     //   tote, totp, totn);
		//printf("Top= %.0f        Bot= %.0f\n", ptop, pbot);
	}
	else {
		//printf("Warm/Cold Layers not found.\n" );
		*pos = 0;
		*neg = 0;
		*top = 0;
		*bot = 0;
	}
}




/*NP*/
void posneg_temperature(float start, float *pos, float *neg, float *top, 
	                float *bot)
/***********************************************************************/
/* POSNEG                                                              */
/* Calculates positive and negative areas as related to winter weather */
/* forecasting.  Search begins at 500mb, but only returns results if   */
/* a positive area is found, overlaying a negative area.               */
/* START is the upper limit of search.(default=init_phase)             */
/***********************************************************************/
{
	float upper, lower, pe1, h1, te1, tp1, totp, totn, pe2, h2, te2, 
	      tp2, tdef1, tdef2;
	float lyrlast, lyre, tote, pelast, ptop, pbot, lvl;
	short i, lptr, uptr, warmlayer=0, coldlayer=0, phase;
	short pIndex, zIndex, tdIndex;

	/* ----- If there is no sounding, do not compute ----- */
	if (!qc(i_temp(500, I_PRES)) && !qc(i_temp(850, I_PRES))) return;

	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tdIndex = getParmIndex("DWPT");

	if (!sndg || pIndex == -1 || zIndex == -1 || tdIndex == -1)
	  return;
        
	/* ----- Find lowest observation in layer ----- */
	lower=sndg[sfc()][pIndex];
        lptr = sfc();

        /* ----- Find highest observation in layer ----- */
	if (start=-1) {
	  (void)init_phase(&lvl, &phase);
/*
	  strcpy( st, init_phase( &lvl, &phase ));
*/
	  if (lvl > 0.0)
   	   upper = lvl;
	  else
   	   upper = 500.0;
	}
	else { 
	  upper = start;
	}
        
	i=numlvl-1;
        while(sndg[i][pIndex] < upper) {
	  i--;
	  if (i < 0) {
	    fprintf(stderr, 
      "Warning: posneg_temp: Could not find a pressure greater than %.2f\n",
	      upper);
	    fprintf(stderr, "Using %.2f as the upper level.\n", 
	      sndg[0][pIndex]);
	    i = 0;
	    break;
	  }
	}
        uptr = i;
        if (sndg[i][pIndex] == upper)
	  uptr--;

        /* ----- Start with top layer ----- */
        pe1 = upper;
        h1 =  i_hght(pe1, I_PRES);
        te1 = i_temp(pe1, I_PRES);
        tp1 = 0;

        totp = totn = tote = ptop = pbot = 0;

        for( i = uptr; i >= lptr; i--) {
           if (qc(sndg[i][tdIndex])) {
              /* ----- Calculate every level that reports a temp ----- */
              pe2 = sndg[i][pIndex];
              h2  =  sndg[i][zIndex];
              te2 = i_temp(pe2, I_PRES);
              tp2 = 0;
              tdef1 = (0 - te1) / (te1 + 273.15);
              tdef2 = (0 - te2) / (te2 + 273.15);
              lyrlast = lyre;
              lyre = 9.8F * (tdef1 + tdef2) / 2.0F * (h2 - h1);

	      /* Has a warm layer been found yet? */
	      if (te2>0) 
		if (warmlayer==0) {
		  warmlayer=1;
		  ptop=pe2;
		}

	      /* Has a cold layer been found yet? */
	      if (te2<0) 
		if ((warmlayer==1) && (coldlayer==0)) {
		  coldlayer=1;
		  pbot=pe2;
		}

	      if (warmlayer>0) {
	        if (lyre>0)
	 	  totp += lyre;
	        else
		  totn += lyre;

	        tote += lyre;
 
               // printf("%4.0f - %4.0f E=%6.0f TOT=%6.0f Top=%6.0f Bot=%6.0f\n",
	          //pe1, pe2, lyre, tote, ptop, pbot);
	      }

              pelast = pe1;
              pe1 = pe2;
              h1  = h2;
              te1 = te2;
              tp1 = tp2;
	   }
	}

	if ((warmlayer==1) && (coldlayer==1)) {
	  *pos = totp;
	  *neg = totn;
	  *top = ptop;
	  *bot = pbot;
	 // printf("Tot= %.0f J/kg   Pos= %.0f J/kg   Neg= %.0f J/kg\n",
       //     tote, totp, totn);
	  //printf("Top= %.0f        Bot= %.0f\n", ptop, pbot);
	}
	else {
	  //printf ("Warm/Cold Layers not found.\n" );
	  *pos = 0;
	  *neg = 0;
	  *top = 0;
	  *bot = 0;
	}
}


/*NP*/
char *best_guess(struct _ptype p)
/***********************************************************************/
/* BEST_GUESS                                                          */
/***********************************************************************/
{
	float       x1, y1, x2, y2;
	static char st[80];

	/* Case:  no precip */
	if (p.init_phase<0) {
		strcpy(st, "None.");
		return st;
	}

	/* Case:  always too warm - Rain */
	if ((p.init_phase==0) && (p.tneg>=0) && (p.tsfc>0)) {
		strcpy(st, "Rain.");
		return st;
	}

	/* Case:  always too cold - Snow */
	if ((p.init_phase==3) && (p.tpos<=0) && (p.tsfc<=0)) {
		strcpy(st, "Snow.");
		return st;
	}

	/* Case:  ZR too warm at sfc - Rain */
	if ((p.init_phase==1) && (p.tpos<=0) && (p.tsfc>0)) {
		strcpy(st, "Rain.");
		return st;
	}

	/* Case:  non-snow init...always too cold - Initphase&sleet  */
	if ((p.init_phase==1) && (p.tpos<=0) && (p.tsfc<=0)) {
		if (agl(i_hght(p.init_lvl, I_PRES))>=3000) {
			if (p.init_temp <= -4) {
				strcpy(st, "Sleet and Snow." );
				return st;
			}
			else {
				strcpy(st, "Sleet." );
				return st;
			}
		}
		else {
			strcpy(st, "Freezing Rain/Drizzle.");
			return st;
		}
	}

	/* Case:  Snow...but warm at sfc */
	if ((p.init_phase==3) && (p.tpos<=0) && (p.tsfc>0)) {
		if (p.tsfc>4) 
	          strcpy(st, "Rain.");
	        else
	          strcpy(st, "Snow.");
		return st;
	}


	/* Case:  Warm layer. */
	if (p.tpos>0) {
		x1 = p.tpos;
		y1 = -p.tneg;
		y2 = (.62 * x1) + 60;
		if (y1 > y2) 
			{ strcpy(st, "Sleet." ); }
		else
			{
			if (p.tsfc<=0) 
	                  strcpy(st, "Freezing Rain." );
	                else
	                  strcpy( st, "Rain." );
			}

		return st;
	}

	strcpy(st, "Unknown.");
	return st;
}
