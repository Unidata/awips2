#include <stdio.h>
#include <stdlib.h>
#include "profile.h"
#include "sndglib.h"

Sounding *newSounding(short nparms, short nlev)
{
	Sounding *new=NULL;
	int       i, j;

	if (nparms == 0 || nlev == 0)
	  return NULL;

	if (!(new = (Sounding *)calloc(1, sizeof(struct Sounding)))) 
	  return NULL;

	/* */
	new->nparms = 0;
	new->parms = (char **)calloc(1, nparms*sizeof(char *));
	if (!new->parms)
	  return NULL;
	new->nparms = nparms;

	/* Allocate space for the data */
	new->data = (float **)calloc(1, nlev*sizeof(float *));
	if (!new->data)
	  return NULL;
	for(j=0;j<nlev;j++) {
	  new->data[j] = (float *)calloc(1, nparms*sizeof(float));
	  if (!new->data[j])
	    return NULL;
	}

	/* Allocate space for a copy of the data */
	new->origdata = (float **)calloc(1, nlev*sizeof(float *));
	if (!new->origdata)
	  return NULL;
	for(j=0;j<nlev;j++) {
	  new->origdata[j] = (float *)calloc(1, nparms*sizeof(float));
	  if (!new->origdata[j])
	    return NULL;
	}

	/*
	 * Initialize everything
	 */

	strcpy(new->stid,"UNKNOWN");
	new->datatype = -1;
	new->nlev     =  nlev;
	new->noriglev =  nlev;
	new->sfct     = RMISSD;
	new->sfctd    = RMISSD;
	new->sfcspd   = RMISSD;
	new->sfcdir   = RMISSD;

	for(j=0;j<nlev;j++) {
	  for(i=0;i<nparms;i++) {
	    new->data[j][i]     = RMISSD;
	    new->origdata[j][i] = RMISSD;
	  }
	}

	return new;
}

void printSounding(Sounding *s)
{
	int i, j;

	printf("Station ID: %s\n",s->stid);
	printf("Sounding time: %s\n",s->dattim);
	printf("Sounding Data Type: %d\n",s->datatype);
	printf("Number of Parameters: %d\n",s->nparms);
	printf("Number of Levels: %d\n",s->nlev);

        for(j=0;j<s->nparms;j++) {
          printf("%9s", s->parms[j]);
        }
        putchar('\n');

	for(j=0;j<s->nlev;j++) {
	  for(i=0;i<s->nparms;i++)
	    printf(" %8.2f", s->data[j][i]);
	  putchar('\n');
	}
}

void freeSounding(Sounding *s)
{
	int i;

	if (s) {
	  /* Free sounding data */
	  if (s->data) {
	    for (i=0;i<s->nlev;i++) {
	      safefree(s->data[i]);
	    }
	    safefree(s->data);
	  }

	  if (s->origdata) {
	    for (i=0;i<s->nlev;i++) {
	      safefree(s->origdata[i]);
	    }
	    safefree(s->origdata);
	  }

	  /* Free parameters */
	  if (s->parms) {
	    for (i=0;i<s->nparms;i++)
	      safefree(s->parms[i]);
	    safefree(s->parms);
	  }

	  /* Finally finish up */
	  safefree(s);
	}
}

void safefree(void *ptr)
{
	if (ptr)
	  free(ptr);
}
