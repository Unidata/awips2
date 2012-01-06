#ifndef _PARMS_H
#define _PARMS_H

/* functions in parms.c */
void   freeParms(char **parms, short nparms);
short  getParmIndex(char *name);
char **addParm(char **parms, short *maxsize, char *name, short index);
char **defineParms(char *parmlist, short *nparms);
void   setGlobalParms(char **parms, short nparms);

/* global vars for parms.c */
extern char **dsetparms;
extern short  ndsetparms;

#endif /* _PARMS_H */
