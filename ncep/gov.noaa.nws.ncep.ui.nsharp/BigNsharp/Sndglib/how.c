#include <stdio.h>
#include <string.h>
#include "profile.h"
#include "sndglib.h"
#include "parms.h"

#ifdef UNDERSCORE
#define getsndg getsndg_
#endif /* UNDERSCORE */

#undef DOHISTORY
#define DOHISTORY

#ifdef USEMAIN
History hist;
#endif

void resetSoundingData(void);
int load_sounding2(char *fileptr, char *timeptr, char *stnptr, int stype);

void in_bdta(int *);
void gg_init(int *, int *);

void getinputinfo(char **fileptr, char **timeptr, char *stns[12], int *nstns);
void stress();

void dump_history(History *h);

int load_sounding2(char *fileptr, char *timeptr, char *stnptr, int stype)
{
	short     nparms = 0;
	char    **parmlist = NULL, parms[128];
	Sounding *s = NULL, *tmp = NULL;
	int       newlev, ier, i, j, k, ind, sv;
	float     sndg[8192], pres, sfct, sfctd, sfch, sfcwd, sfcws, slat, slon;
	float     **tmpa, sdir, sspd;
	short     pIndex, zIndex, tIndex, tdIndex, wdIndex, wsIndex, oIndex;

#ifdef DEBUG_JL
	fprintf(stderr, "load_sounding2: fileptr = %s\n", fileptr);
	fprintf(stderr, "load_sounding2: timeptr = %s\n", timeptr);
	fprintf(stderr, "load_sounding2: stnptr = %s\n", stnptr);
	fprintf(stderr, "load_sounding2: stype = %d\n", stype);
#endif /* DEBUG_JL */

	/* Make sure we've got all that we need to go */
	if (!fileptr || !timeptr || !stnptr)
	  return -1;

	/* Figure out parm list based on data type */
	switch (stype) {
	  case SNDG_OBS:
	    strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED");
	  break;
	  case SNDG_MDL:
	    strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED;OMEG");
	  break;
	  case SNDG_PFC:
	    strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED;OMEG;TKEL");
	  break;
	  case SNDG_ACARS:
	  case SNDG_ARCH:
	  default:
	    return -1;
	  break;
	}

	newlev = 0;
	/* Call FORTRAN subroutine */
	getsndg(fileptr, parms, timeptr, stnptr, &stype, sndg,
                &newlev, &pres, &sfct, &sfctd, &sfch, &sdir, &sspd, 
		&slat, &slon, &ier, strlen(fileptr),
                strlen(parms), strlen(timeptr), strlen(stnptr));
	if (ier != 0) {
	  fprintf(stderr, "getsndg returned an error code of %d\n", ier);
	  return ier;
	}

	parmlist = defineParms(parms, &nparms);	

	s = newSounding(nparms, newlev);

	if (!s) {
	  return -1;
	}

	s->parms    = parmlist;
	s->nparms   = nparms;
	s->datatype = stype;
	s->nlev     = newlev;
	s->noriglev = s->nlev;
	strcpy(s->stid, stnptr);
	strcpy(s->dattim, timeptr);
	s->lat     = slat;
	s->lon     = slon;

	/* Populate 
	 * Start at index=1, just in case we need to
	 * add a level for sfc pressures above 1000mb */
	sv = 0;
	/* if ((stype == SNDG_MDL) && (pres >= 1000)) {sv=1;} */

	for (i=sv;i<newlev;i++) {
	  ind = i * nparms;
	  for (j=0;j<nparms;j++) {
	    s->data[i][j]     = sndg[ind + j];
	    s->origdata[i][j] = sndg[ind + j];
	  }
	  /* Initialize 0th level as missing. */
	  if (sv==1) { for (j=0;j<nparms;j++) { s->data[0][j] = -999; } }
	}

	/* Set global sounding to point to this one now. Need to do this
	   here so that the interpolations and such see the data */

	changeGlobalSounding(s);

	/* Extend sounding to 100mb. Do this after we've changed soundings
	   b/c the routine operates on global sndg */
	xtnd_sndg();

	/* Reset levels in sounding since the global var numlvl is updated in xtnd_sndg() */
	s->nlev     = numlvl;
	s->noriglev = numlvl;

	/* Handle any conversions necessary for the different data types */
	if (stype == SNDG_MDL) {

	  	/* Get indices */
	  	/* All of these indices will be valid for model data */
	  	pIndex  = getParmIndex("PRES");
	  	zIndex  = getParmIndex("HGHT");
	  	tIndex  = getParmIndex("TEMP");
	  	tdIndex = getParmIndex("DWPT");
	  	wdIndex = getParmIndex("DRCT");
	  	wsIndex = getParmIndex("SPED");

          	i = sfc();

	  	printf("GRID SFC = %.1f %.1fC\n", s->data[i][pIndex], s->data[i][tIndex]);
          	/* Correct surface level and data */
          	sfcwd = sdir;
          	sfcws = sspd;
	  	printf("MDL  SFC = %.1f %.1fC\n", pres, sfct);


	  	/* if (pIndex != -1 && pres < s->data[i][pIndex]) { i=0; } else { i=1; } */

	  	if (pIndex != -1) {
            		s->data[i][pIndex] = pres;
            		s->origdata[i][pIndex] = pres;
	  	}
	  	if (zIndex != -1) {
            		s->data[i][zIndex] = sfch;
            		s->origdata[i][zIndex] = sfch;
	  	}
	  	if (tIndex != -1) {
            		s->data[i][tIndex] = sfct;
            		s->origdata[i][tIndex] = sfct;
	  	}
	  	if (tdIndex != -1) {
            		s->data[i][tdIndex] = sfctd;
            		s->origdata[i][tdIndex] = sfctd;
	  	}
	  	if (wdIndex != -1) {
            		s->data[i][wdIndex] = sfcwd;
            		s->origdata[i][wdIndex] = sfcwd;
	  	}
	  	if (wsIndex != -1) {
            		s->data[i][wsIndex] = sfcws;
            		s->origdata[i][wsIndex] = sfcws;
	  	}
	}

        /* PFC vvel's get scaled by 100. Why? */
	oIndex  = getParmIndex("OMEG");
        if (stype == SNDG_PFC && oIndex != -1) {
          	for (j=0;j<s->nlev;j++) {
	    		if (s->data[j][oIndex] > RMISSD) {
	      			s->data[j][oIndex] /= 100.0;
	      			s->origdata[j][oIndex] = s->data[j][oIndex];
	    		}
		}
	}

        /* Put winds in knots rather than m/s */
	wsIndex = getParmIndex("SPED");
	if (wsIndex != -1) {
          	for (j=0;j<s->nlev;j++)
	    	if (s->data[j][wsIndex] > 0.0) {
	      		s->data[j][wsIndex] /= 0.51479;
	     		s->origdata[j][wsIndex] /= 0.51479;
	    	}
	}

#ifdef DOHISTORY
	history_add(&hist, s);
#endif

	return ier;
}

/*
 * This routine should take a sounding as input and return an int which
 * tells the result
 */
void resetSoundingData(void)
{
	short i, j;
	float **temp=NULL;

	/* Make sure sounding exists */
	if (!globalsndg)
	  return;

	if (globalsndg->nlev > globalsndg->noriglev) {
	  /* Downsize */
	  temp = (float **)realloc(globalsndg->data, 
	                           (globalsndg->noriglev * sizeof(float *)));
	  globalsndg->data = temp;
	}
	else if (globalsndg->nlev < globalsndg->noriglev) {
	  /* Need more space */
	  temp = (float **)realloc(globalsndg->data, 
	                           (globalsndg->noriglev * sizeof(float *)));
	  globalsndg->data = temp;

  	  i = globalsndg->noriglev - globalsndg->nlev;

  	  for (j=globalsndg->nlev; j < (globalsndg->nlev + i); j++) {
	    globalsndg->data[j] = (float *)malloc(globalsndg->nparms * 
	                                          sizeof(float));
	    /* Need to handle NULL pointer here */
	  }
	}

	/* Copy our data over */
        for(j=0;j<globalsndg->noriglev;j++) {
	  memcpy(globalsndg->data[j], globalsndg->origdata[j],
	    (globalsndg->nparms * sizeof(float)));
        }

	/* Reinitialize global vars */
	/* This one is a must BEFORE calling changeGlobalSounding */
	globalsndg->nlev = globalsndg->noriglev;

	/* Reset the global vars here that have changed */
        numlvl           = globalsndg->nlev;
        sndg             = globalsndg->data;
}

#ifdef USEMAIN
int main(int argc, char *argv[])
{
	int i, j, mode=1, ret, nstns=0;
	float ix1, ix2;
	Sounding *s = NULL;
	char *fileptr=NULL, *timeptr=NULL, *stns[12];
	/* OUN is the full sounding  AMA is an abbreviated version of OUN */
	char *stnptr;

	/* Initialize GEMPAK */
	in_bdta(&ret);
	gg_init(&mode, &ret);

	history_init(&hist, 4, NULL);

	getinputinfo(&fileptr, &timeptr, stns, &nstns);

	for (j=0;j<nstns;j++) {

	  stnptr = stns[j];

	  printf ("Currently loading %s\n", stns[j]);
	  i = load_sounding2(fileptr, timeptr, stnptr, SNDG_PFC);
/*
	  stress();
*/
	}

	dump_history(&hist);

	return 0;
}

// Stress test sounding
void stress(void)
{
	int i, j;
	float ix1, ix2;
	Parcel pcl;

	printf("Stressing %s\n", globalsndg->stid);

	define_parcel(1, 1000.0);

	/* Compute CAPE/CIN */
	(void)parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	printf("Parcel CAPE: %.2f J/kg\n", pcl.bplus);
	printf("Parcel CINH: %.2f J/kg\n", pcl.bminus);
	printf("DCAPE = %.2f J/kg\n", dcape(&ix1, &ix2));
}

void getinputinfo(char **fileptr, char **timeptr, char *stns[12], int *nstns)
{
	FILE *fp;
	char bufr[80], *ptr;
	int  count=0, len=0;

	fp = fopen("config", "r");
	if (!fp)
	  return;

	while (fgets(bufr, sizeof(bufr), fp)) {
	  bufr[strlen(bufr)-1] = '\0';
	  if (*bufr == '#')
	    continue;
	  switch (count) {
	    case 0:
	      *fileptr = strdup(bufr);
	    break;
	    case 1:
	      *timeptr = strdup(bufr);
	    break;
	    case 2:
	      ptr = strtok(bufr, ";");
	      while (ptr) {
	        stns[len] = strdup(ptr);
	        ptr = strtok(NULL, ";");
	        len++;
	      }
	      *nstns = len;
	    break;
	  }
	  count++;
	}
	fclose(fp);
}
#endif

void dump_history(History *h)
{
	HistElmt *he;
	Sounding *tmps;

	he = history_first(h);
	while (he) {
	  tmps = (Sounding *)history_data(he);
	  if (tmps) {
	    printSounding(tmps);
	  }
	  else {
	    printf("Sounding data is NULL.\n");
	  }
	  he = history_next(he);
	}
}
