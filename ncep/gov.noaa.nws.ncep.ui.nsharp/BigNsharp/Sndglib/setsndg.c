#include <stdio.h>
#include <string.h>
#include "sndglib.h"

/* define global sndg data here */
float   **sndg = NULL, **origsndg = NULL, **sndg2 = NULL;
Sounding *globalsndg = NULL;
short     numlvl  = 0;
short     origlvl = 0;

/* Lifted parcel level information */
LPLvalues lplvals;

/* Function to make this sounding the current sounding */
void changeGlobalSounding(Sounding *s)
{
        if (!s)
          return;

        /* Make current sounding the previous sounding */

        /* Assign global vars to point to this data */
        sndg       = s->data;
        if (!sndg)
          fprintf(stderr, "changeGlobalSounding: sndg is NULL\n");
        numlvl     = s->nlev;
        origsndg   = s->origdata;
        globalsndg = s;

        /* Change our global parameters */
        setGlobalParms(s->parms, s->nparms);
}

Sounding *getGlobalSounding(void)
{
	return globalsndg;
}
