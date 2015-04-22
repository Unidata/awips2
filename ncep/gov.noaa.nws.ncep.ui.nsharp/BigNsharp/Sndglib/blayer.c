#include <math.h>
#include <stdlib.h>
#include "basics.h"
#include "thermo.h"
#include "blayer.h"
#include "sndglib.h"

/*
 *
 * Function to adjust the boundary layer temperature profile 
 * in the sounding such that the thermal profile is dry adiabatic from 
 * the surface temperature up to the point at which the surface 
 * theta is equal to the theta of the environment
 *
 * Currently nothing is done to adjust the moisture profile
 *
 * -mkay
 * 10/15/99
 *
 */

void adjbndrylayer(float sfct, float sfctd)
{
	int   i, j;
	short pIndex, tIndex;
	float pres, thta, thtatemp, temp;

	if (!sndg)
	  return;

	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");
	if (pIndex == -1 || tIndex == -1)
	  return;

	/* Find theta for the surface parcel */
	i = sfc();
	thta = theta(sndg[i][pIndex], sfct, 1000.0);

	/*
	 * For all levels up to where Theta(new) = Theta(sounding)
	 * Change T to be T for Theta(new) at that pressure level
	 *
	 */

	for (j=i; j<numlvl; j++) {
	  if (qc(sndg[j][tIndex])) {
	    thtatemp = theta(sndg[j][pIndex], sndg[j][tIndex], 1000.0);
	    if (thtatemp < thta) {
	      temp = ((thta + 273.15) / pow((1000.0/sndg[j][pIndex]), 0.286)) - 
	             273.15;
	      sndg[j][tIndex] = temp;
	    }
	  }
	}
}
