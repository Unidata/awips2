/***************************************************************/
/*  NSHARP                                                     */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  Routines for Pino and Moore Hail Algorithm                 */
/*                                                             */
/*  John A. Hart                                               */
/*  NCEP/SPC Norman, OK                                        */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "pinomoore.h"
#include "sndglib.h"

float pinomoore(void)
/***************************************************************/
/*  Routine to compute the pino&moore hail algorithm           */
/*  Method assumes max hail is calculated with W at -10c.      */
/*							       */
/*  Returns hail size (cm.)				       */
/***************************************************************/
{
	float asize, sfc_size;

	/* ----- First, calculate maximum hail size ----- */
	hailgrowth(&asize);
	printf( "Initial Hail Diameter (aloft)  = %.1f cm.\n", asize);
	
	/* ----- Next, account for melting in sub-cloud layer ----- */
	hailreduction(asize, &sfc_size);

	return sfc_size;
}




float hailgrowth(float *max_diameter)
/***************************************************************/
/*  							       */
/***************************************************************/
{
        float ix1, sfcpres, sfctemp, sfcdwpt, ix2;
	float cd, rd, denh, g, dena, hfl, wmax, diam;
        Parcel pcl;

        sfcpres = lplvals.pres;
        sfctemp = lplvals.temp;
        sfcdwpt = lplvals.dwpt;
        ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);	
	
	/* ----- Calculate maximum hail size ----- */

	wmax = sqrt(2*pcl.wm10c);	/* UVV at -10c */
	hfl = -10;			/* hail formation temperature */
	cd = 0.60;			/* coefficient of drag */ 
	rd = 287.04;			/* Ideal gas constant */
	g = 9.806;			/* gravity */
	denh = 900;			/* density of hail */
	dena = (temp_lvl(-10, &ix1)*100) / 
		(rd*(hfl + 273.15)); 	/* density of air */

	*max_diameter = 100 * (3.0*dena*cd*(wmax*wmax))/(2.0*denh*g); 
}



float hailreduction(float max_diameter, float *sfc_diameter)
/***************************************************************/
/*  							       */
/***************************************************************/
{
	float ix1, ix2, drtemp, dd_thw, p1, p_init, p_final;
	float tavg, z1, z2, term_vel, deltaz, lsubf, ksubw;
	float res_time, curradius, ksuba, lsubv, d, bb, deltaa;
	float rhoice, beta, a, b, c, rd, mu, diam, tsubs;
	float dena, renum;

        lsubf = 80;
        ksubw = 1.47e-3;
        ksuba = 6.e-5;
        lsubv = 600.0;
        d = 0.24;
        rhoice = 0.90;
        beta = 4.8e-7;
        c = 1.6;
        rd = 287.04;
        mu = 0.0001775;

	*sfc_diameter = RMISSD;

	if (!sndg)
	  return RMISSD;
	
	
	/* This routine assumes that the hail will be in the
	   core of the downdraft.  The DCAPE routine is used to
	   find the properties of that downdraft.               */

	dcape(&ix1, &drtemp);
	dd_thw = thetaw(sndg[sfc()][1], drtemp, drtemp);

	ix1=2;
	for(p1=sndg[sfc()][1]; ix1>0; p1-=2) ix1 = satlft(p1, dd_thw);
	p_init=p1;
	p_final=sndg[sfc()][1];

	/* Now, the hailstone will be followed down at 100m
	   increments.  Through each level, a new diameter will
	   be computed based on melting rates and terminal vel.  */

	deltaz = 100;
	z1 = i_hght(p_init, I_PRES);
	z2 = z1-deltaz;
	diam = max_diameter;
	a = (diam/2);
	b = a + .1;

	while (z2 >= sndg[sfc()][2])
		{
		p1 = (i_pres(z1) + i_pres(z2))/2;
		term_vel = hailspeed(diam, i_pres(z1), dd_thw);
		res_time = deltaz / term_vel;
		tavg = (satlft(i_pres(z1),dd_thw) + satlft(i_pres(z2),dd_thw))/2;

		dena = ((p1*100)/(rd * (tavg+273.16)))/1000;
		renum = (2 * a * term_vel * 100 * dena) / mu;
		tsubs=tavg/(1.+(ksubw/(c*(ksuba+lsubv*d*beta)))*(a/(b-a)));
		bb = 0.0001118*(tavg-tsubs);
		deltaa=(((0.76*sqrt(renum)*bb)/(2.*0.9*lsubf))/a)*res_time;
	
		diam = diam - (2*deltaa);

		printf( "%.0fmb   %.0f m/s.  %.3f cm   %.1f cm\n", i_pres(z1), term_vel, deltaa, diam);
		z1=z2;
		z2=z1-deltaz;
		a = a - deltaa;
		b = a + 0.1;
		}

	*sfc_diameter = diam;
	return diam;
}




float hailspeed(float diam, float pres, float dd_thw)
/***************************************************************/
/*  							       */
/*  Computes terminal velocity of hailstones (m/s)	       */
/*  							       */
/***************************************************************/
{
	float cd, rd, g, denh, dena, vel;
	
	cd = 0.60;				/* coefficient of drag */ 
	rd = 287.04;				/* Ideal gas constant */
	g = 9.806;				/* gravity */
	denh = 900;				/* density of hail */
	dena = (pres*100) / (rd*(-10+273.15));	/* density of air */
	printf( "Air density= %.2f\n", dena);

	vel = sqrt((2 * denh * g * diam) / (3 * dena * cd * 100));
	return vel;
}
