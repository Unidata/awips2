/* Make sure we import stuff from sounding library */

#ifndef _SNDGLIB_H
#include "sndglib.h"
#endif

/* 
 * Header file for global variables not directly related any 
 * X windows routines
 *
 * mkay 8/19/99
 */

#define MAXSTNS 2000

/* 
 * Data types that we can display
 *
 * If you add a type here then you should also update the config table
 * in config.c as well
 */
#define NSHARP_OBS   SNDG_OBS     /* Observed Soundings */
#define NSHARP_MDL   SNDG_MDL     /* Model soundings */
#define NSHARP_PFC   SNDG_PFC     /* Point forecast soundings */
#define NSHARP_ACARS SNDG_ACARS   /* ACARS soundings */
#define NSHARP_ARCH  SNDG_ARCH    /* Archived soundings */
#define NSHARP_SFC   SNDG_SFC     /* Surface obs for lowest level */

extern int curdatatype;    /* What type of data are we displaying */
extern int curHodoMode;    /* Hodograph Mode */

/* Storm motion */
extern float st_spd, st_dir;
extern float bd_spd, bd_dir;

extern float mucape, mucin; /* used for effective layer check */
extern float p_top, p_bot; /* effective inflow layer */

extern float user_level; /* user-defined level */
extern float mml_layer;  /* mean-mixed layer */
extern float mu_layer;   /* most-unstable layer */

/* Titling stuff */
extern char raobtitle[80], stn_abbrev[4], raob_type[80], mdl_type[80];
extern char raobtitle2[80], stn_abbrev2[4], raob_type2[80], mdl_type2[80];
extern char raobsavefilename[256], sars_filename[256], sup_filename[256]; //CHIN

extern struct _configure config;
extern struct _pagemode pages;
extern struct _startup autostart;

extern History hist;
