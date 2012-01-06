#ifndef _PROFILE_H
#define _PROFILE_H

typedef struct Sounding {
	/* FIRST KEEP TRACK OF THE PARAMETERS IN THIS SOUNDING */	
	char  **parms;
	short   nparms;
        char    stid[8];                /* Station ID */
        short   datatype;               /* Data type */
        char    dattim[16];             /* The dattim string */
        short   nlev;                   /* Number of levels of data */
        float **data;                   /* The data */
        short   noriglev;               /* Number of levels of data */
        float **origdata;               /* The unmodified data */
	float   sfct;	                /* Surface temp */
	float   sfctd;	                /* Surface dewpoint temp */
        float   sfcspd;                 /* Surface wind speed */
        float   sfcdir;                 /* Surface wind direction */
	float   lat;
	float   lon;
} Sounding;

#ifndef RMISSD
#define RMISSD -9999.0
#endif

/* prototypes */
Sounding *newSounding(short nparms, short nlev);
void printSounding(Sounding *s);
void freeSounding(Sounding *s);
void safefree(void *ptr);

#endif  /* _PROFILE_H */
