#ifndef _CTBCMN_H
#define _CTBCMN_H
/************************************************************************
 * CTBCMN.H								*
 *									*
 * This header file defines the structures needed to read in tables.	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94						*
 * C. Lin/EAI	 	 2/97	increase the array size of map_list	*
 *				name (13 -> 40), garea(31->80),         *
 *				proj (25 ->80)				*
 * D. Plummer/NCEP	 3/98	Added information from prmlst.tbl	*
 * C. Lin/EAI	 	 8/98	Add font size table			*
 * S. Jacobs/NCEP	 9/98	Changed the length of the path string	*
 * S. Jacobs/NCEP	 8/99	Changed the contents of dtinfo		*
 * D.W.Plummer/NCEP	 1/01	Add clustered counties structures	*
 * T. Lee/SAIC		 2/02	Added layer table structure		*
 * T. Lee/SAIC		 2/02	Added group type to layer structure	*
 * T. Lee/SAIC		 2/02	Added display mode to layer structure	*
 * H. Zeng/EAI          08/02   Added preference table structures.      *
 * H. Zeng/EAI          08/02   Modified preference table structures.   *
 * m.gamazaychikov/SAIC  5/03	add grib2 table G2VARS_TBL		*
 * m.gamazaychikov/SAIC  5/03	add pdtnmbr to G2VARS_TBL structure	*
 * H. Zeng/XTRIA	10/03   modified station_list structure		*
 * E. Safford/SAIC	11/03	removed MAX_LAYER definition -- use	*
 *				 MAX_LAYERS defined in gemprm.h		*
 * T. Lee/SAIC		11/03	increased val (25 -> 128) in prefs.tbl	*
 * E. Safford/SAIC	11/03	add outfile to layer_t struct		*
 * A. Hardy/NCEP	 3/04	Added marine zone names structures	*
 * M. Li/SAIC		 4/04	Added hzremap and direction to G2Vinfo	*
 * T. Lee/SAIC		 9/04	added time binning to datatype structure*
 * B. Yin/SAIC		10/04	added GFA gui info structures		*
 * A. Hardy/NCEP	10/04   Added permanent cluster structure	*
 * B. Yin/SAIC          11/04   Added hazard category into GFA structure*
 * m.gamazaychikov/SAIC 12/04   Added ionoff flag to Dtinfo structure   *
 * S. Gilbert/NCEP      12/04   Added various GRIB2 table structures    *
 *                              Added _CTBCMN_H check to see if prev    *
 *                                 included to avoid redefinition errs  *
 * m.gamazaychikov/SAIC 01/06   Increased template string length to 	*
 *				MXTMPL.					*
 * m.gamazaychikov/SAIC 04/06   Added domtmmtch to Dtinfo		*
 * B. Yin/SAIC          06/06   Added hazard id into GFA structure      *
 * S. Jacobs/NCEP	10/07	Added Hershey font structure		*
 * F. J. Yen/NCEP	 4/08	Add bin mins &most recent flag to DTinfo*
 ***********************************************************************/

typedef struct station_list {
	int	stnm;
				/* Station WMO number */
	int	prior;
				/* Station priority  */
	float	rlat;
				/* Station latitude */
	float	rlon;
				/* Station longitude */
	float	elev;
				/* Station elevation */
	char	stid[9];
				/* Station ID */
	char	name[33];
				/* Station name */
	char	state[3];
				/* State of station */
	char	coun[3];
				/* Country of station */
	char	misc_info[30];
				/* Misc Station Info */
} StnLst;

struct bulletin_list {
	float	rlat;
				/* Station latitude */
	float	rlon;
				/* Station longitude */
	float	elev;
				/* Station elevation */
	char	bullid[7];
				/* Bulletin ID */
	char	stid[9];
				/* Originating station ID */
	char	name[33];
				/* Station name */
	char	state[3];
				/* State of station */
	char	coun[3];
				/* Country of station */
};

struct datatype_list {
	char	datatyp[13];
				/* Data type */
	char	loctbl[13];
				/* Location table */
	char	bsflag[2];
				/* Bulletin/Station flag */
	char	datadir[41];
				/* Path to the data files */
	char	filext[9];
				/* File extension */
};

struct maptype_list {
	char	name[40];
				/* Map attributes */
	char	proj[80];
				/* Map projection */
	char	garea[80];
				/* Map graphics area */
};

#define	PRMLST_TBL	"prmlst.tbl"

typedef struct parmele
{
	char	name[20];
	char	value[128];
} ParmEle;

typedef	struct	plinfo {
	int	npe;		/* Alias number of parameters	*/
	char	*alias;		/* Alias name			*/
	char	*dtype;		/* Alias datatype 		*/
	char	colcod;		/* Alias color code flag	*/
	ParmEle	*parmele;	/* Alias parameter elements	*/
} PLinfo;

typedef	struct prmlst_t {
	int	nalias; 	/* Number of display aliases	*/
	PLinfo	*info;		/* Alias information		*/
} Prmlst_t;


#define	DATA_TBL	"datatype.tbl"

typedef	struct	dtinfo {
	int	catgry;		/* Template category number	*/
	int	subcat;		/* Template subcategory number	*/
	int	nframe;		/* Template def number of frames */
	int	range;		/* Template def time range (min) */
	int	intrvl;		/* Template def time interval (min)*/
        int	ionoff;         /* Bin hours turned on or off */
	int	hrsbfr;		/* no of hrs to bin before current time */
	int	mnsbfr;		/* minutes portion of bin time before curtime */
	int	hraftr;		/* no of hrs to bin after current time */
	int	mnaftr;		/* minutes portion of bin time after curtime */
        int	mstrct;         /* most recent only flag for time binning */
	int	domtmmtch;	/* time matching scheme if dominant source */
	char	alias[13];	/* Template alias		*/
	char	path[26];	/* Template path 		*/
	char	template[MXTMPL];	/* Template 			*/
} DTinfo;

typedef	struct data_t {
	int	numtmpl; 	/* Number of templates		*/
	DTinfo	*info;		/* Template information		*/
} Data_t;


#define	FONTSZ_TBL	"fontsz.tbl"

typedef	struct	{
	char	name[12];	/* font size name	*/
	float	value;		/* font size value	*/
	int	xval;		/* X window font size   */
} FSZinfo;

typedef	struct {
	int	nfsz; 		/* Number of font sizes		*/
	FSZinfo	*info;		/* Font size information	*/
} Fontsz_t;


#define G2VARS_TBL	"g2vars.tbl"

typedef struct {
        int     discpln;
        int     categry;
        int     paramtr;
        int     pdtnmbr;
        int     scale;
	int	hzremap;
	int	direction;
        float   missing;
        char    name[33];
        char    units[21];
        char    gemname[13];
} G2Vinfo;

typedef struct {
        int     nlines;         /* number of lines         */
        G2Vinfo *info;          /* grib2 table information        */
} G2vars_t;

typedef struct {
        int     id;             /* WMO Center id number       */
        char    name[65];       /* WMO Center name            */
        char    abbrev[9];      /* WMO Center abbreviation    */
} G2wmocenter;

typedef struct {
        int     nlines;         /* number of lines            */
        G2wmocenter *info;      /* GRIB Orig Center Tbl info  */
} G2wmocntrs;

typedef struct {
        int     id1;            /* GRIB2 1st level number     */
        int     id2;            /* GRIB2 2nd level number     */
        int     scale;
        char    name[34];       /* name of level/layer        */
        char    unit[21];       /* name of level/layer        */
        char    abbrev[5];      /* level/layer abbreviation   */
} G2level;

typedef struct {
        int     nlines;         /* number of lines            */
        G2level *info;          /* GRIB2 level/layer tbl info */
} G2lvls;

#define	CNTYCLUST_TBL	"countyclust.tbl"

typedef	struct	{
	char	ccwfo[4];	/* Name of cluster WFO			*/
	char	*ccname;	/* Name of cluster			*/
	int	ncc;		/* Number of counties in cluster	*/
	int	*cc;		/* Clustered county list (FIPS codes)	*/
} CCinfo;

typedef	struct {
	int	nclust;		/* Number of clustered county combos	*/
	CCinfo	*clust;		/* Clustered county information		*/
} Clustcnty_t;

#define	PERMCLUST_TBL	"permclust.tbl"

typedef	struct	{
	char	pcwfo[4];	/* Name of permanent cluster WFO			*/
	char	*pcname;	/* Name of permanent cluster			*/
	int	npc;		/* Number of counties in cluster	*/
	int	*pc;		/* Permanent clustered county list (FIPS codes)	*/
} PCinfo;

typedef	struct {
	int	nclust;		/* Number of permanent clustered county combos	*/
	PCinfo	*clust;		/* Permanent clustered county information		*/
} Permclust_t;


typedef struct layer_t {
	int	cid;			/* Color id			*/
	char	name[10];		/* Name of the class		*/
	char	vgfile[MXFLSZ];		/* VG file name			*/
	char	outfile[MXFLSZ];	/* Name of output VG file	*/
	char	cmode[5];		/* Color mode			*/
	char	fmode[4];		/* Fill mode			*/
	char	gtype[10];		/* Group type			*/
	char	dsply[4];		/* Display mode			*/
} Layer_t;


#define	MZNAMES_TBL	"marinenames.tbl"
typedef struct {
    	char	mzid[7];	/* Marine zone id		*/
    	char	name[257];	/* Full marine zone name	*/
} MZinfo;

typedef struct {
    	int	nummz;		/* number of marine zones	*/
    	MZinfo	*mzones;        /* Full marine zone information */
} Marzon_t;

#define PREFS_TBL       "prefs.tbl"
#define MAX_PREF_STR	25
#define MAX_PREF_VAL	128

/*
 *  Preference table entry structure.
 */
typedef struct {
    char    tag[MAX_PREF_STR+1];           /* preference tag name */
    char    val[MAX_PREF_VAL+1];  	   /* preference value    */
}pref_ent_t; 

/*
 *  Preference table structure.
 */
typedef struct {
    int           npref;          /* total # of preference entry    */
    pref_ent_t    *prefs;         /* preference tag_value pair list */
}prefTbl_t; 

/*
 * GFA gui info 
 */
typedef struct haz_desc_t {
    char	*label;
    char	*type;    /* pull-down or user input */
    int        	nchoices; /* number of pull-down choices or columns */
    char        **choice; /* NULL if type is user input */
} Haz_Desc_t;

typedef struct hazard_t {
    char        *hazard;
    char	cat[ 32 ];       /* Sierra/Tango/Zulu */
    char	id[ 8 ];	 /* hazard identifier */
    int         ndesc;
    Haz_Desc_t  *desc;
} Hazard_t;

typedef struct GFA_haz_t {
    int         nhaz;
    Hazard_t    *haz;
} GFA_Haz_t;


/*
 * Hershey Font info
 */
typedef struct hfpoint_t {
    int		x;
    int		y;
} HF_point_t;

typedef struct hfchar_t {
    int		ascii_val;
    int		npts;
    int		xmin;
    int		xmax;
    HF_point_t	*point;
    char	*point_code;
} HF_char_t;

typedef struct hffont_t {
    int		font_code;
    int		numchr;
    HF_char_t	*character;
} HF_font_t;


#ifdef CTB_PREFERENCE
	prefTbl_t               _prefTbl;
#else
	extern	prefTbl_t	_prefTbl;
#endif

#ifdef CTB_HERSHEY
	HF_font_t		*_hfontTbl;
	int			_nhfont;
#else
	extern HF_font_t	*_hfontTbl;
	extern int		_nhfont;
#endif


#include "proto_ctb.h"

#endif   /* _CTBCMN_H */
