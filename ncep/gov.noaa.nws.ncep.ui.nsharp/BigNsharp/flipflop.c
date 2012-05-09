#include <stdio.h>
#include "sndglib.h"

/*
 *  Temporary routine written as we move to my new code base which doesn't
 *  store omega in index 0
 *
 *  Takes a sounding from getsndg() which has OMEG stored in index 6 and
 *  moves it to index 0 and slide the others over one index
 */
void flipflopsounding(float **inoutsndg, int nlev)
{
	float tmpsndg[MAXLEV][NPARM];
	int i, j;

	if (nlev >= MAXLEV) {
	  fprintf(stderr, " flipflopsounding. nlev >= MAXLEV\n");
	}

	/* First copy stuff into temporary array */
	for (i=0;i<nlev;i++) 
	  for (j=0;j<NPARM;j++) 
	    tmpsndg[i][j] = inoutsndg[i][j];

	/* Now put omega into index 0 */
	for (i=0;i<nlev;i++) inoutsndg[i][0] = tmpsndg[i][6];

	/* Move the first 6 parms over one slot (OMEG is in 0) */
	for (i=0;i<nlev;i++) 
	  for (j=0;j<NPARM-1;j++) inoutsndg[i][j+1] = tmpsndg[i][j];
}
