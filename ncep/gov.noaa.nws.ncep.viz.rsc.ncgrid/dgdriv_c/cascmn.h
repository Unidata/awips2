
/************************************************************************
 * cascmn.h                                                             *
 *									*
 * Contains the structure for elements found on a SigWx chart. 		*
 *									*
 **									*
 *Log:									*
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC        02/01   Added MAXCH				*
 * A. Hardy/NCEP        04/02   Increased MAXOFF 300 -> 5000		*
 * M. Li/SAIC		04/04	Added levabv, and levblw		*
 * M. Li/SAIC		08/04	Added mcloud_t				*
 * M. Li/SAIC		09/04	Added HI_BASE, HI_TOP, MID_BASE, MID_TOP*
 * H. Zeng/SAIC		03/06	removed pr_hgfm & pr_knms		*
 ***********************************************************************/

#define MAXCH           128
#define	MAXMCLD		15	
#define MAXOFF          5000
#define SIGIMSS        -9999999
#define SIGRLMS        (-9999999.0F)

#define HI_BASE   	7620.0   /* high level base in meters (25,000 ft) */
#define HI_TOP    	19200.0  /* high level top in meters (62,992 ft)  */
#define MID_BASE  	3050.0   /* mid level base in meters (10,007 ft)  */
#define MID_TOP   	13720.0  /* mid level top in meters (45,013 ft)   */

typedef struct  VolRad_T {
       char	name[40]; 	/* volcano name or location of radiation*/
       float	lat;            /* latitude value */
       float	lon;            /* longitude value */
       int 	year;		/* eruption time - year */
       int 	month;		/* eruption time - month */
       int 	day;		/* eruption time - day */
       int 	hour;		/* eruption time - hour */
       int 	minute;		/* eruption time - minute */
       struct VolRad_T	*next;  /* link to next volrad LABEL group */
   } volrad_t;

typedef struct  Storm_T {
       char	name[40]; 	/* name of storm */
       float	lat;            /* latitude value */
       float	lon;            /* longitude value */
       int	stmtyp;		/* storm type */
       struct Storm_T	*next;  /* link to next storm LABEL group */
   } storm_t;

typedef struct  TropHi_T {
    	float   lat;		/* latitude value */
    	float   lon;		/* longitude value */
    	float   level;		/* height of tropopause (meters) */
        struct  TropHi_T  *next;  /* link to next HIGH trop value */
   } trophi_t;

typedef struct  TropLo_T {
    	float   lat;		/* latitude value */
    	float   lon;		/* longitude value */
    	float   level;		/* height of tropopause (meters) */
        struct  TropLo_T  *next;  /* link to next LOW trop value */
   } troplo_t;

typedef struct  Trop_T {
    	float   lat;		/* latitude value */
    	float   lon;		/* longitude value */
    	float   level;		/* height of tropopause (meters) */
        struct  Trop_T  *next;  /* link to next SPOT trop value */
   } trop_t;

typedef struct  Front_T {
       int	ftype;		/* type of front */
       int	npt;		/* number of points in FRONT line */
       float	lat[MAXPTS];    /* latitude values */
       float	lon[MAXPTS];    /* longitude values */
       float	fntspd[MAXPTS];	/* speed of the front (meters/second) */
       float	fntdir[MAXPTS];	/* movement direction of front (deg) */
       struct 	Front_T	*next;  /* link to next turbulence group */
   } front_t;

typedef struct  Jets_T {
       int	npt;		/* number of points in JETS line */
       float	lat[MAXPTS];    /* latitude values */
       float	lon[MAXPTS];    /* longitude values */
       float	level[MAXPTS];	/* jet height (meters) */
       float	speed[MAXPTS];	/* speed of the jet (meters/second) */
       float	levabv[MAXPTS];	/* height above jet (meters) */
       float	levblw[MAXPTS];	/* height below jet (neters) */
       struct 	Jets_T	*next;  /* link to next turbulence group */
   } jets_t;

typedef struct  Turb_T {
       float	level1;		/* base of CAT area (meters) */
       float	level2;		/* top of CAT area (meters) */
       int	npt;		/* number of points in CAT area */
       float	lat[MAXPTS];    /* latitude values */
       float	lon[MAXPTS];    /* longitude values */
       int	tdeg;		/* degree of turbulence */
       struct 	Turb_T	*next;  /* link to next turbulence group */
   } turb_t;

typedef struct  Cloud_T {
       float	level1;		/* base of cloud area (meters) */
       float	level2;		/* top of cloud area (meters) */
       int	npt;		/* number of points in CLOUD area */
       float	lat[MAXPTS];    /* latitude values */
       float	lon[MAXPTS];    /* longitude values */
       int	clddist;	/* cloud distribution */
       int 	cldtyp;		/* cloud type */
       struct Cloud_T	*next;  /* link to next cloud group */
   } cloud_t;

typedef struct  Mcloud_T {
       int	npt;		/* number of points in MCLOUD area */
       float	lat[MAXPTS];	/* latitide values */
       float	lon[MAXPTS];	/* longitude values */
       int	ncld;		/* number of non-Cb cloud distributions */
       int	ncdis[MAXMCLD];	/* non-Cb cloud distribution values */
       int 	ntyp;		/* number of non-Cb cloud types */
       int	nctyp[MAXMCLD];	/* non-Cb cloud type values */
       int	turb;		/* turbulence flag */
       float	tbase;		/* base of turbulence (meters) */
       float    ttop;		/* top of turbulence (meters) */
       int	tdeg;		/* degree of turbulence */
       int	icing;		/* icing flag */
       float	icbase;		/* base of icing (meters) */
       float	ictop;		/* top of icing (meters) */		
       int	dic;		/* degree of icing */
       int	fcb;		/* Cb flag */
       float	cbbase;		/* base of Cb (meters) */
       float    cbtop;		/* top of Cb (meters) */
       int	cbdis;		/* Cb cloud distribution */
       int      cbtyp;		/* Cb cloud type */
       struct	Mcloud_T *next;	/* link to next mcloud group */
    } mcloud_t;

/*---------------------------------------------------------------------*/

#include "proto_cas.h"
