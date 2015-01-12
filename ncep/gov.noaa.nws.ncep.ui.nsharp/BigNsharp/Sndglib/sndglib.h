#ifndef _SNDGLIB_H
#define _SNDGLIB_H

#define MAXLEV   200

#ifndef PI
#define PI 3.14159265
#endif

#define SNDG_SFC   0
#define SNDG_OBS   1
#define SNDG_MDL   2
#define SNDG_PFC   3
#define SNDG_ACARS 4
#define SNDG_ARCH  5

#define ROCP 0.28571428    /* Rd over Cp */

#ifndef _PROFILE_H
#include "profile.h"
#endif

struct _parcel {
	float lplpres;
	float lpltemp;
	float lpldwpt;
	float blayer;
	float tlayer;
	float entrain;
	float lclpres;
	float lfcpres;
	float elpres;
	float mplpres;
	float bplus;
	float bminus;
	float bfzl;
	float cape3km;
	float cape6km;
	float wm10c;
	float wm20c;
	float wm30c;
	float li5;
	float li3;
	float brn;
	float limax;
	float limaxpres;
	float cap;
	float cappres;
};
typedef struct _parcel Parcel;

struct _lplvalues
{
	char  desc[40];
	float pres;
	float temp;
	float dwpt;
	float presval;
	short flag;
};
typedef struct _lplvalues LPLvalues;

struct _ptype {
	float init_lvl;
	float init_temp;
	short init_phase;
	float tpos;
	float tneg;
	float tsfc;
};
typedef struct _ptype Ptype;

extern LPLvalues lplvals;

/* Global vars used by the sounding library */
/* These are all initialized in setsndg.c   */
extern float   **sndg, **origsndg, **sndg2;
extern Sounding *globalsndg;

extern short     numlvl;
extern short     origlvl;

/* Function includes */

#include "basics.h"
#include "blayer.h"
#include "pinomoore.h"
#include "setsndg.h"
#include "skparams.h"
#include "thermo.h"
#include "winds.h"
#include "computeparms.h"
#include "parms.h"
#include "winter.h"
#include "newhistory.h"
/*
#include "dmalloc.h"
*/
#include "list.h"

extern History hist;

#endif /* _SNDGLIB_H */
