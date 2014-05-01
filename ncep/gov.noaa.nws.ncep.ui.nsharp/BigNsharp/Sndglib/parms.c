#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parms.h"
#include "sndglib.h"

short  ndsetparms = 0;
char **dsetparms = NULL;

char **defineParms(char *parmlist, short *nparms)
{
	char   tempbufr[1024], *ptr=NULL;
	short  np=0;
	short  maxsize=0;
	char **parms=NULL;
	char **newparms=NULL;

	*nparms = 0;

	strcpy(tempbufr, parmlist);

	if (!parmlist) return parms;

        ptr = strtok(tempbufr, ";");
        while (ptr != NULL) {
          parms = addParm(parms, &maxsize, ptr, np);
          np++;
          ptr = strtok(NULL, ";");
        }

	/* Reduce size here */
	if (maxsize > np) {
	  newparms = (char **)realloc(parms, np * sizeof(char *));
	  if (newparms) {
	    parms = newparms;
	  }
	}

	*nparms = np;

	return parms;
}

char **addParm(char **parms, short *size, char *name, short index)
{
	char **newparms=NULL;
	char  *ptr;

        if (index >= *size) {
          newparms = (char **)realloc(parms, ((*size)+8)*sizeof(char *));
          if (newparms) {
            parms = newparms;
            (*size) += 8;
          }
          else {
            fprintf(stderr,
              "Could not increase size of parms. This is a Bad Thing.\n");
            return parms;
          }
        }

	/* Trim leading/trailing whitespace from name here */
	while(isspace(*name)) name++;
	if ((ptr = strchr(name, ' '))) *ptr = '\0';

	parms[index] = strdup(name);

	return parms;
}

short getParmIndex(char *name)
{
	int i;
	for (i=0; i<ndsetparms; i++) {
	  if (!strcasecmp(dsetparms[i], name)) return i;
	}
	return -1;
}

void freeParms(char **parms, short nparms)
{
	int i;
	for (i=0; i<nparms; i++) {
	  free(parms[i]);
	}
	free(parms);
}

void setGlobalParms(char **parms, short nparms)
{
	dsetparms  = parms;
	ndsetparms = nparms;
}
