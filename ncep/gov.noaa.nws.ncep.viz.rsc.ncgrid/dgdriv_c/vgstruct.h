/************************************************************************
 * vgstruct.h								*
 * Contains the structure for a drawing element that is being stored	*
 * or retrieved from a Vector Graphics File.  Elements are stored	*
 * in this file in a geographic coordinate fashion.  This lets GEMPAK	*
 * handle all of the device to world coordinate transformations.	*
 *									*
 **									*
 * E. Wehner/EAi	03/97	Created                                 *
 * D. Keiser/GSC	04/97	Added contour, symbols, line		*
 * D. Keiser/GSC	04/97	Added special line			*
 * E. Wehner/EAi	05/97	Added padding and a class to header	*
 * E. Safford/GSC	06/97	Added special text			*
 * E. Wehner/EAi	06/97	Added file header and group info	*
 * E. Safford/GSC	07/97	Mofified SpTextInfo to include offsets  *
 * D.W.Plummer/NCEP	09/97	Remove LatLonPt and IntPt structures	*
 * D.W.Plummer/NCEP	02/98	Updated Watch Box structures		*
 * S. Law/GSC		04/98	Added comment for unused1 usage		*
 * I. Durham/GSC	04/98	Added DARR_ELM and HASH_ELM		*
 * D.W.Plummer/NCEP	 6/98	Added w_shape to watches		*
 * E. Safford/GSC	08/98	added MAXGHOST				*
 * E. Safford/GSC	09/98	Removed MAXGHOST			*
 * D.W.Plummer/NCEP	 9/98	Modified watch info definitions		*
 * A. Hardy/GSC         10/98   Added CMBSY                             *
 * E. Safford/GSC	11/98	bumped MAX_RECTYPES to 26		*
 * A. Hardy/GSC         11/98   Added circle structures                 *
 * S. Law/GSC		05/99	Added track structure and defines	*
 * G. Krueger/EAI	05/99	Modified circles for latlon array	*
 * D.W.Plummer/NCEP	 6/99	Added sigmet structure			*
 * D.W.Plummer/NCEP	 8/99	Added parmeters for SIGMETs		*
 * S. Law/GSC		 8/99	Increased MAX_RECTYPES to 35		*
 * S. Law/GSC		 8/99	Made changes to SIGMETs			*
 * S. Law/GSC		 9/99	Made changes to SIGMETs			*
 * S. Law/GSC		 9/99	Cleaned up SIGMET string lengths	*
 * D.W.Plummer/NCEP	12/99	Added WATCH STATUS MESSAGE element type	*
 * M. Li/GSC		12/99	Added SWPALL, SWPHDR and SWPINF		*
 * D.W.Plummer/NCEP	 2/00	Added CCFP element			*
 * S. Law/GSC		02/00	More CCF work				*
 * S. Law/GSC		02/00	increased MAX_SIGMETS, CCFLVL changes	*
 * H. Zeng/EAI		08/00	added skip for TrackInfo		*
 * F. J. Yen/NCEP	08/00   Added more SIGMET info			*
 * D.W.Plummer/NCEP	10/00	Added anchor info to watch element	*
 * M. Li/GSC		10/00	Added itxfn, ithw and sztext to track	*
 * J. Wu/GSC		11/00	Changed SymData & WindData size to 1 	*
 * J. Wu/GSC		11/00	Removed CONTOUR_ELM type & struct       *
 * J. Wu/GSC		01/01	Added definitions for latest VG version	*
 * J. Wu/GSC		02/01	Renamed 'unused1' & 'unused2' to 	*
 *				meaningful 'smooth'  & 'version'	*
 * D.W.Plummer/NCEP	 8/01	Chg watch element to include FIPS codes	*
 * D.W.Plummer/NCEP	 9/01	Chg watch element to incr states string	*
 * E. Safford/SAIC	05/02	add WatchBoxInfo string lengths		*
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * J. Wu/SAIC		09/02	add a new structure - list		*
 * J. Wu/SAIC		09/02	add loctyp/lat/lon into List structure	*
 * S. Jacobs/NCEP	10/02	Reduced MAXLISTITEMS from 3000 to 10	*
 * J. Wu/SAIC		09/02	revise List struct, MAXLISTITEMS to 1600*
 * H. Zeng/XTRIA        01/03   added new info for WatchBoxInfo         *
 * D.W.Plummer/NCEP	 6/03	Add new ash cloud and volcano element	*
 * H. Zeng/XTRIA        08/03   Added new fields fpr volcano element	*
 * J. Wu/SAIC	 	 8/03	add Jet element & related definitions	*
 * H. Zeng/XTRIA	09/03   added ash cloud line types		*
 * H. Zeng/XTRIA	11/03   added addlsorc for volcano element	*
 * J. Wu/SAIC	 	01/04	add GFA element & related definitions	*
 * H. Zeng/XTRIA	01/04   added more Ash Cloud types		*
 * B. Yin/SAIC		02/04	added tca element & related definitions *
 * J. Wu/SAIC           02/04   add lat/lon into GfaInfo                *
 * J. Wu/SAIC           03/04   add GFA subtype definitions		*
 * B. Yin/SAIC          03/04   added storm type in tca element         *
 * A. Hardy/NCEP	03/04	added list type  LISTYP_MZCNTY		*
 * B. Yin/SAIC          04/04   added type TcaWw_T		        *
 * J. Wu/SAIC           05/04   add GFA_GFA subtype & description	*
 * H. Zeng/SAIC         07/04   removed NOTAVBL&ENDVAA                  *
 * B. Yin/SAIC          07/04   changed TCA break point array to pointer*
 *				changed the order of basins		*
 * J. Wu/SAIC		08/04	reduce MAXLISTITEMS to MAXPTS		*
 * E. Safford/SAIC      08/04   change gfa top & bottom to char arrays  *
 * J. Wu/SAIC		09/04	remove definition for GFA subtypes 	*
 * J. Wu/SAIC		10/04	redesign GFA structure 			*
 * H. Zeng/SAIC		11/04	removed CCFLVL_MEDIUM			*
 * B. Yin/SAIC          12/04   added timezone into TCA			*
 * H. Zeng/SAIC		02/05	added back CCFLVL_MEDIUM		*
 * H. Zeng/SAIC		03/05	added ASHCLD_OTHERS			*
 * B. Yin/SAIC          04/05   added issue status into TCA             *
 *                              removed year from TCA                   *
 * T. Piper/SAIC	10/05	Moved MAXPTS to gemprm.h		*
 * S. Gilbert/NCEP	11/05	Added text_lat, text_lon, text_font,    *
 *                              text_size, and text_width to TcaInfo    *
 * S. Gilbert/NCEP	01/06	Changed advisoryNum from int to char    *
 * F. J. Yen/NCEP       03/07   Moved MAX_CNTY to gemprm.h              *
 * m.gamazaychikov/SAIC	04/07	Add new TC error, TC track and 		*
 *				TC breakpoint line elements		*
 * m.gamazaychikov/SAIC	10/08	Add stSrc to TCT element 		*
 ***********************************************************************/
#ifndef _vgstruct_include
#define _vgstruct_include

#define MAX_TEXT	255
#define MAX_TRACKS	50
#define MAX_SIGMET	100
#define MAX_ASH		50      /* Max number of points per ash cloud	*/
#define MAX_ATSUNIT	12      /* Maximum of ATS unit	*/
#define MAX_ATTR	100     /* Max number of area attributes */
#define STD_STRLEN	1024	/* Max length of attribute string */
#define MAX_BREAK_PT_STR 256	/* Max length of break points string */
#define MAX_STORM_STR	128	/* Max length of storm name string */
#define MAX_TCAWW	50	/* Max number of TCA warning/watches */
#define MAX_DSCRPTN_LEN	50	/* Max length of GFA description string */
#define MAX_TCT_STRING	50	/* Max length of TCTrack entry string */

#define MAX_RECTYPES	42

/*	Values for vg_type from struct vg_hdrstruct		*/
#define LINE_ELM 	1	/* line 			*/
#define FRONT_ELM 	2	/* front 			*/
#define CIRCLE_ELM 	4	/* circle symbol 		*/
#define WXSYM_ELM 	5	/* weather symbol 		*/
#define WBOX_ELM	6	/* watch box 			*/
#define WCNTY_ELM	7	/* watch county 		*/
#define BARB_ELM	8	/* wind barb 			*/
#define ARROW_ELM	9	/* wind arrow 			*/
#define CTSYM_ELM	10	/* cloud symbol 		*/
#define ICSYM_ELM	11	/* icing symbol 		*/
#define PTSYM_ELM	12	/* pressure tendency symbol 	*/
#define PWSYM_ELM	13	/* past weather symbol 		*/
#define SKSYM_ELM	14	/* sky cover 			*/
#define SPSYM_ELM	15	/* special symbol 		*/
#define TBSYM_ELM	16	/* turbulence symbol 		*/
#define TEXT_ELM	17	/* text 			*/
#define TEXTC_ELM	18	/* justified text 		*/
#define MARK_ELM	19	/* marker 			*/
#define	SPLN_ELM	20	/* special line 		*/
#define	SPTX_ELM	21	/* special text 		*/
#define FILEHEAD_ELM	22	/* FILE Header element 		*/
#define DARR_ELM	23	/* directional arrow 		*/
#define HASH_ELM	24	/* hash mark 			*/
#define CMBSY_ELM 	25	/* combination weather symbol 	*/
#define TRKSTORM_ELM	26	/* storm track 			*/
#define SIGINTL_ELM	27	/* international SIGMET 	*/
#define SIGNCON_ELM	28	/* non-convective SIGMET 	*/
#define SIGCONV_ELM	29	/* convective SIGMET 		*/
#define SIGOUTL_ELM	30	/* convective outlook 		*/
#define SIGAIRM_ELM	31	/* AIRMET 			*/
#define SIGCCF_ELM	32	/* CCF 				*/
#define WSM_ELM		33	/* watch status 		*/
#define LIST_ELM	34	/* list 			*/
#define VOLC_ELM        35      /* volcano element              */
#define ASHCLD_ELM      36      /* ash cloud element            */
#define JET_ELM		37      /* jet element            	*/
#define GFA_ELM		38      /* gfa element            	*/
#define TCA_ELM		39      /* tca element            	*/
#define TCERR_ELM	40      /* tc error-cone element       	*/
#define TCTRK_ELM	41      /* tc track element       	*/
#define TCBKL_ELM	42      /* tc break point line element	*/


#define VGFHEAD_COMMENT "NAWIPS Vector Graphic Format \0"

#define SWPALL		0
#define SWPHDR		1
#define SWPINF		2

/* 
 * WATCH INFORMATION
 * watch types (doubles as display color)
 */
#define UNDWTCH 	7
#define TRWWTCH 	6
#define TORWTCH 	2
/* 
 * watch styles
 */
#define WBC		6
#define PGRAM 		4
/* 
 * watch style PGRAM shapes
 */
#define NONE		0
#define NS		1
#define EW		2
#define ESOL		3

/*
 * Ash cloud subtypes
 */
#define ASHCLD_AREA     0
#define ASHCLD_LINE     1
#define ASHCLD_NOTSEEN  2
#define ASHCLD_OTHERS	3

/* 
 * CCF defines
 */
#define CCFLVL_HIGH	0
#define CCFLVL_MEDIUM	1
#define CCFLVL_LOW	2

#define CCFGRW_FAST	0
#define CCFGRW_POS	1
#define CCFGRW_NC	2
#define CCFGRW_NEG	3

/* 
 * SIGMET subtypes
 */
#define SIGTYP_AREA	0
#define SIGTYP_LINE	1
#define SIGTYP_ISOL	2

/*
 * maximum SIGMET string sizes
 */
#define MAX_AREASTR	  8
#define MAX_DIRSTR	  4
#define MAX_FIRSTR	 32
#define MAX_SIGSTR	768
#define MAX_MIDSTR	 12
#define MAX_PHENSTR	 32
#define MAX_PHENMSTR  	 35
#define MAX_LATSTR  	  8
#define MAX_LONSTR  	  8
#define MAX_MOVESTR	  8
#define MAX_SPDSTR	  4
#define MAX_TRENDSTR	  8
#define MAX_REMSTR	 80
#define MAX_FTEXTSTR	256
#define MAX_TOPSSTR	 80
#define MAX_FCSTRSTR	 16
#define MAX_TIMESTR	 20

/*
 * SIGMET side of line options
 */
#define SIGLINE_ESOL	0
#define SIGLINE_NOF	1
#define SIGLINE_SOF	2
#define SIGLINE_EOF	3
#define SIGLINE_WOF	4

/*
 *  Latest version of VG elements in use. Only SIGMET, WATCH
 *  BOX have more than one versions now. Other VG elements 
 *  are still using version 0. 
 */
#define CUR_SIG_VER	1	
#define CUR_WBX_VER	6	

/*
 *  String length limits for WatchBoxInfo
 */
#define MAX_FCSTR_LEN	( 64 )
#define MAX_REPLW_LEN	( 24 )

/*
 *  Maximum list items allowed for ListType
 */
#define MAXLISTITEMS	MAXPTS

/*
 *  List subtypes
 */
#define LISTYP_COUNTY	1	/* county list 			*/
#define LISTYP_ZONE	2	/* zone list 			*/
#define LISTYP_WFO	3	/* wfo list 			*/
#define LISTYP_STATE	4	/* state list 			*/
#define LISTYP_MZCNTY	5	/* Marine zone/county  list 	*/

/*
 *  Maximum points along a Jet line
 */
#define MAX_JETPTS	50

/*
 *  TCA issue status
 */
#define TCA_OPERATIONAL		'O'
#define TCA_EXPERIMENTAL	'E'
#define TCA_TEST		'T'
#define TCA_EXPERIMENTAL_OPNL	'X'	

typedef struct frontinfo
{
    int		numpts;
    int		fcode;
    int		fpipsz;
    int		fpipst;
    int		fpipdr;
    int		fwidth;
    char	frtlbl[4];
} FrontInfo;
			    
typedef struct watchboxinfo
{
    int 	numpts;
    /* watch definition information					*/
    int		w_style;	/* watch style (WBC or PGRAM)		*/
    int		w_shape;	/* PGRAM watch shape (EW, NS or ESOL)	*/
    int		w_mrktyp;	/* marker type to identify county       */
    float	w_mrksiz;	/* marker size  for county		*/
    int		w_mrkwid;	/* marker width for county		*/
    char	w_a0id[8];	/* watch anchor point #0 station id	*/
    float	w_a0lt;		/* watch anchor point #0 latitude	*/
    float	w_a0ln;		/* watch anchor point #0 longitude	*/
    int		w_a0dis;	/* watch anchor point #0 distance (sm)	*/
    char	w_a0dir[4];	/* watch anchor point #0 dir (16-pt)	*/
    char	w_a1id[8];	/* watch anchor point #1 station id	*/
    float	w_a1lt;		/* watch anchor point #1 latitude	*/
    float	w_a1ln;		/* watch anchor point #1 longitude	*/
    int 	w_a1dis;	/* watch anchor point #1 distance (sm)	*/
    char	w_a1dir[4];	/* watch anchor point #1 dir (16-pt)	*/
    /* watch formatting and issuance relevant information		*/
    int		w_istat;	/* watch issuing status			*/
    int 	w_number;	/* watch number				*/
    char	w_iss_t[20];	/* watch issue time			*/
    char	w_exp_t[20];	/* watch expiration time		*/
    int		w_type;		/* watch type (TORNADO or SVR T-STM)	*/
    int		w_severity;	/* watch severity			*/
    char	w_timezone[4];	/* watch primary time zone		*/
    char	w_hailsz[8];	/* watch max hail size			*/
    char	w_windg[8];	/* watch max wind gust			*/
    char	w_tops[8];	/* watch max tops     			*/
    char	w_msmv_d[8];	/* watch mean storm motion vector (dir)	*/
    char	w_msmv_s[8];	/* watch mean storm motion vector (spd)	*/
    char	w_states[80];	/* watch states included		*/
    char	w_adjarea[80];	/* watch adjacent areas included	*/
    char	w_replw[MAX_REPLW_LEN];	/* watch replacement watch numbers */
    char	w_fcstr[MAX_FCSTR_LEN];	/* watch issuing forecaster name(s)*/
    char 	w_file[128];	/* watch filename			*/
    int		w_issued;	/* watch flag for issuance: =0 N, =1 Y	*/
    /* watch status message formatting and issuance relevant info	*/
    char	wsm_iss_t[20];	/* wsm issue time			*/
    char	wsm_exp_t[20];	/* wsm expiration time			*/
    char	wsm_ref[32];	/* wsm reference direction		*/
    char	wsm_from[128];	/* wsm most recent "from" line		*/
    char	wsm_meso[8];	/* wsm mesoscale discussion number	*/
    char	wsm_fcstr[MAX_FCSTR_LEN];/* wsm issuing forecaster name(s)*/
    /* county information						*/
    int 	numcnty;		/* number of counties		*/
    int 	cn_flag;		/* county plot flag		*/
    int 	cn_fips[MAX_CNTY];	/* county FIPS code		*/
    float	cn_ltln[MAX_CNTY*2];	/* county locations		*/
} WatchBoxInfo;

typedef struct syminfo
{
    int		numsym;
    int		width;
    float	size;
    int		ityp;
} SymInfo;

typedef struct windinfo
{
    int		numwnd;
    int		width;
    float	size;
    int		wndtyp;
    float	hdsiz;
} WindInfo;

typedef struct lineinfo
{
    int		numpts;
    int		lintyp;
    int		lthw;
    int		width;
    int		lwhw;
} LineInfo;

typedef struct splninfo
{
    int		numpts;
    int		spltyp;
    int		splstr;
    int		spldir;
    float	splsiz;
    int		splwid;
} SpLineInfo;

typedef struct textinfo
{
    float	rotn;
    float	sztext;
    int		itxfn;
    int		ithw;
    int		iwidth;
    int		ialign;
    float	lat;
    float	lon;
    int		offset_x;
    int		offset_y;
} TextInfo;

typedef struct spltextinfo
{
    float	rotn;
    float	sztext;
    int		sptxtyp;
    int		turbsym;
    int		itxfn;
    int		ithw;
    int		iwidth;
    int		txtcol;
    int		lincol;
    int		filcol;
    int		ialign;
    float	lat;
    float	lon;
    int		offset_x;
    int		offset_y;
} SpTextInfo;

typedef struct cntrinfo
{
    LineInfo	line;
    TextInfo	txt;
} CntrInfo;

typedef struct volinfo {
    char        name[64];         /* volcano name                         */
    float       code;             /* symbol code (volcano)                */
    float       size;             /* symbol size                          */
    int         width;            /* symbol width                         */
    char        number[17];       /* volcano Smithsonian number           */
    char        location[17];     /* volcano location (e.g., N1901W09837) */
    char        area[33];         /* volcano area (e.g., Mexico)          */
    char        origstn[17];      /* originating station                  */
    char        vaac[33];         /* VAAC                                 */
    char        wmoid[8];         /* WMO ID                               */
    char        hdrnum[9];        /* header number                        */
    char        elev[9];          /* volcano elevation (m)                */
    char        year[9];          /* year                                 */
    char        advnum[9];        /* advisory number                      */
    char	corr[4];	  /* correction flag			  */
    char        infosorc[256];    /* information source(s)                */
    char	addlsorc[256];    /* additional source(s)		  */
    char	avcc[16];	  /* aviation color code		  */
    char        details[256];     /* eruption details                     */
    char        obsdate[16];      /* obs date                             */
    char        obstime[16];      /* obs time                             */
    char        obsashcld[1024];  /* obs (analyzed) ash cloud "from line" */
    char        fcst_06[1024];    /* 06hr fcst ash cloud "from line"      */
    char        fcst_12[1024];    /* 12hr fcst ash cloud "from line"      */
    char        fcst_18[1024];    /* 18hr fcst ash cloud "from line"      */
    char        remarks[512];     /* remarks                              */
    char        nextadv[128];     /* next advisory                        */
    char        fcstrs[64];       /* forecaster(s)                        */
} VolInfo;

typedef struct ashinfo {
    int         subtype;        /* type of ash cloud (isol,line,area)   */
    int         npts;           /* number of points for ash cloud       */
    float       distance;       /* distance (nautical miles)            */
    int         fhr;            /* forecast hour                        */
    int         lintyp;         /* line type                            */
    int         linwid;         /* line width                           */
    int		sol;		/* side of line				*/
    float       spd;            /* speed (kts), (will be removed later) */
    char	spds[16];	/* speed (kts)				*/
    char        dir[4];         /* direction (16-pt compass)            */
    char        flvl1[16];      /* flight level 1 (100s of ft)          */
    char        flvl2[16];      /* flight level 2 (100s of ft)          */
} AshInfo;

typedef struct ccfinfo {
    int		subtype;	/* type of CCF ( 0-line or 1-area)	*/
    int		npts;		/* number of points			*/
    int		cover;		/* coverage (defines the color)		*/
    int		tops;		/* tops					*/
    int		prob;		/* prob					*/
    int		growth;		/* growth				*/
    float	spd;		/* speed				*/
    float	dir;		/* direction				*/
} CCFInfo;

typedef struct sigmetinfo {
    int		subtype;	/* type of sigmet area (isol,line,area) */
    int		npts;		/* number of points for sigmet area 	*/
    int		lintyp;		/* line type 				*/
    int		linwid;		/* line width 				*/
    int		sol;		/* side of line				*/
    char	area[MAX_AREASTR];   /* area(MWO) indicator of unit 	*/
    char	fir[MAX_FIRSTR];   /* location indicator of FIR unit(s) */
    int		status;		/* 0 = new, 1 = amend, 2 = cancel 	*/
    float	distance;	/* distance (nautical miles) 		*/
    char	msgid[MAX_MIDSTR];	/* message id (alfa,bravo,etc.)	*/
    int		seqnum;			/* sequence number (1,2,3,...)	*/
    char	stime[MAX_TIMESTR];	/* start valid time (ddhhmm)	*/
    char	etime[MAX_TIMESTR];	/* end valid time (ddhhmm)	*/
    char	remarks[MAX_REMSTR];	/* descriptive remarks		*/
    int		sonic;			/* supersonic indicator (0,1)	*/
    char	phenom[MAX_PHENSTR];	/* phenomenon 			*/
    char	phenom2[MAX_PHENSTR];	/* second phenomenon		*/
    char	phennam[MAX_PHENMSTR];  /* phenomenon name		*/
    char	phenlat[MAX_LATSTR];	/* phenomenon latitude		*/
    char	phenlon[MAX_LONSTR];	/* phenomenon longitude		*/
    int		pres;			/* pressure			*/
    int		maxwind;		/* max wind			*/
    char	freetext[MAX_FTEXTSTR]; /* free text			*/
    char	trend[MAX_TRENDSTR];	/* trend			*/
    char	move[MAX_MOVESTR];	/* movement 			*/
    int		obsfcst;	/* observed/forecast indicator (0,1,2) 	*/
    char	obstime[MAX_TIMESTR];	/* observed/forecast time (ddhhmm, UTC) */
    int		fl;		/* flight level (100s ft) 		*/
    int		spd;		/* speed of phenomenon (kts)		*/
    char	dir[MAX_DIRSTR];/* direction of phenomenon (compass)	*/
    char	tops[MAX_TOPSSTR];      /* tops				*/
    char	fcstr[MAX_FCSTRSTR]; 	/* forecaster name		*/
} SigmetInfo;

typedef struct trackinfo {
    int		subtype;	/* just in case */
    int		npts;		/* total number of points */
    int		nipts;		/* number of initial points */
    int		ltype1;		/* type of initial line */
    int		ltype2;		/* type of extrapolated line */
    int		mtype1;		/* type of initial mark */
    int		mtype2;		/* type of extrapolated mark */
    int		width;		/* width of lines */
    float	speed;		/* speed between last two initial points */
    float	dir;		/* direction between last two initial points */
    int		incr;		/* increment between extrapolated points */
    int		skip;		/* skip factor for extrapolated time labels */
    int		itxfn;		/* text font number	*/	
    int		ithw;		/* font hw/sw flag	*/
    float	sztext;		/* font size		*/
    fdttms_t	times[MAX_TRACKS]; /* times for each point */
} TrackInfo;

typedef struct listinfo {
    int		subtyp; /* list type: 
        			COUNTY = 1
        			ZONE   = 2
        			WFO    = 3
        			STATE  = 4
        			MARINE = 5 */
    int		mrktyp;	/* marker type */			    
    float	mrksiz;	/* marker size */			    
    int		mrkwid;	/* marker width */
 } ListInfo;



/*
 * 	structs and enums for TCA element
 */

enum basin_t {
    ATLANTIC_BASIN	= 0,
    E_PACIFIC_BASIN	= 1,
    C_PACIFIC_BASIN	= 2,
    W_PACIFIC_BASIN	= 3
};

enum tca_sev_t {
    TROPICAL_STORM_S	= 0,
    HURRICANE_S		= 1
};

enum storm_type_t {
    HURRICANE      		= 0,
    TROPICAL_STORM 		= 1,
    TROPICAL_DEPRESSION 	= 2,
    SUBTROPICAL_STORM		= 3,
    SUBTROPICAL_DEPRESSION 	= 4
};

enum tca_adv_t {
    WATCH	= 0,
    WARNING	= 1
};

enum tca_sp_geog_t {
    NO_TYPE	= 0,
    ISLANDS	= 1,
    WATER	= 2
};

typedef struct breakpt_t {			/* break point structure */
    float	lat;
    float 	lon;
    char	breakPtName[ MAX_BREAK_PT_STR ];
} Breakpt_T;

typedef struct tcaww_t {			/* tca watches/warning */
    enum tca_sev_t	severity;		/* hurricane or tropical storm */
    enum tca_adv_t	advisoryType;		/* watch or warning */
    enum tca_sp_geog_t 	specialGeog;		/* special geography type */
    int			numBreakPts;		/* number of break points */
    struct breakpt_t	*breakPnt;		/* break points */
} TcaWw_T;

struct tcawwfile_t {				/* file version of tca */
    char	tcawwStr[ STD_STRLEN ];		/* watches/warnings */
    char	breakPts[ STD_STRLEN ];
};

typedef struct tcainfo {
    int		stormNum;			/* storn number */
    char 	issueStatus;			/* issue status */
    enum basin_t	basin;			/* basin */
    char		advisoryNum[5];		/* advisory number */
    char	stormName[ MAX_STORM_STR ];	/* storm name */
    enum storm_type_t   stormType;		/* storm type */
    char		validTime[ DTTMSZ ];	/* valid time in GEMPAK format */
    int			wwNum;			/* number of watches/warnings */
    struct tcaww_t	tcaww[ MAX_TCAWW ];	/* array of watches/warnings */
    char		timezone[ 4 ];		/* time zone */
    float		text_lat;               /* Lat of Zero WW box  */
    float		text_lon;               /* Lon of Zero WW box  */
    int                 text_font;        /*  Font used in zero W/W text box  */
    float               text_size;    /*  Text size used in zero W/W text box */
    int                 text_width;  /*  Text width used in zero W/W text box */
} TcaInfo;

typedef struct tcafileinfo {			/* file version of tca */
    char	attrib1[ STD_STRLEN ];
    char	attrib2[ STD_STRLEN ];
} TcaFileInfo;

typedef struct tcatype {		
    TcaInfo	info;
} TcaType;

/*
 * TC Info structure is the same for TCT and TCE elements
 */
typedef struct tcinfo {
    char	stormNum [ 5 ];			/* storm number */
    char 	issueStatus [ 2 ];		/* issue status */
    char 	basin [ 5 ];			/* basin */
    char	advisoryNum[5];			/* advisory number */
    char	stormName[ MAX_STORM_STR ];	/* storm name */
    char	stormType [5 ];			/* storm type */
    char	validTime[ DTTMSZ ];		/* valid time in GEMPAK format */
    char	timezone[ 4 ];			/* time zone */
    char	fcstpd[ 5 ];			/* forecast period */
} TcInfo;

/*
 * TCE element structure
 */
typedef struct tcecone {
    int 	lincol;				/* line color  */
    int 	lintyp;				/* line type   */
    int		filcol;				/* fill color  */
    int		filtyp;				/* fill pattern*/
    int		npts;				/* number of points */
    float	latlon[MAXPTS*2];             
} TceCone;
    
typedef struct tcerrtype {		
    TcInfo	info;				/* tropical cyclone info*/	
    TceCone	cone;				/* cone of uncertanty */
} TcerrType;

/*
 * TCT element structure
 */
typedef struct tctrack {
    float	lat;				/* point lat */
    float 	lon;				/* point lon */
    char	advDate [MAX_TCT_STRING];  	/* advisory date */
    char	tau     [MAX_TCT_STRING];  	/* forecast period */
    char	mxWnd   [MAX_TCT_STRING];  	/* max wind in knots */
    char	wGust   [MAX_TCT_STRING];  	/* wind gusts in knots */
    char	mslp    [MAX_TCT_STRING];  	/* minimum sea level pressure */
    char	tcDv    [MAX_TCT_STRING];  	/* level of tc development */
    char	tcDvLbl [MAX_TCT_STRING];  	/* label for tc development */
    char	tcDir   [MAX_TCT_STRING];  	/* direction of tc movement */
    char	tcSpd   [MAX_TCT_STRING]; 	/* speed of tc movement */
    char	dtLbl   [MAX_TCT_STRING];  	/* advisory date */
    char	stSrc   [MAX_TCT_STRING];  	/* storm source (tropical or extratropical) */
} TcTrack;
    
typedef struct tctrktype {
    TcInfo       	info;                   /* tropical cyclone info*/
    int         	lincol;                 /* line color  */
    int         	lintyp;                 /* line type   */
    int			numTrackPts;		/* number of track points */
    struct tctrack     *trackPnt;               /* tropical cyclone track */
} TctrkType;

/*
 * TCB element structure
 */
typedef struct tcbkltype {
    TcInfo       	info;                   /* tropical cyclone info*/
    int                 lincol;                 /* line color  */
    int                 linwid;                 /* line width   */
    int			tcww;			/* TC Watch-Warning level:
                                                   HU warning = 0
                                                   HU watch   = 1
                                                   TS warning = 2
                                                   TS watch   = 3              */
    int                 numBkPts;               /* number of break points in segment*/
    struct breakpt_t    *bkPntLn;               /* break points */
} TcbklType;
/*===============================================================================*/

typedef struct symdata
{
    float	code[1];
    float	latlon[2];
    int		offset_xy[2];
} SymData;

typedef struct winddata
{
    float	spddir[2];
    float	latlon[2];
} WindData;

typedef struct circdata
{
    float	latlon[4];
} CircData;

typedef struct listdata
{
    int		nitems;  		/* number of list members */
    char	item[MAXLISTITEMS][9];  /* list of members */
    float	lat[MAXLISTITEMS];     
    float	lon[MAXLISTITEMS];      /* Lat and lon are centroids
                                           for bounds */
} ListData;

typedef struct fileheadtype
{
    char 	version[128];
    char	notes[256];
} FileHeadType;


typedef struct fronttype
{
    FrontInfo	info;
    float	latlon[MAXPTS*2];
} FrontType;

typedef struct watchboxtype
{
    WatchBoxInfo info;
    float	latlon[MAXPTS*2];
} WatchBoxType;

typedef struct watchsmtype
{
    SpLineInfo	info;
    float	latlon[MAXPTS*2];
} WatchSMType;

typedef struct symtype
{
    SymInfo	info;
    SymData	data;
} SymType;

typedef struct windtype
{
    WindInfo	info;
    WindData	data;
} WindType;

typedef struct linetype
{
    LineInfo	info;
    float	latlon[MAXPTS*2];
} LineType;

typedef struct splntype
{
    SpLineInfo	info;
    float	latlon[MAXPTS*2];
} SpLineType;

typedef struct texttype
{
    TextInfo	info;
    char	text[MAX_TEXT];
} TextType;

typedef struct sptxtype
{
    SpTextInfo	info;
    char     	text[MAX_TEXT];
} SptxType;

typedef struct circtype
{
    LineInfo	info;
    CircData	data;
} CircType;

typedef struct sigmettype {
    SigmetInfo	info;
    float	latlon[MAX_SIGMET*2];
} SigmetType;

typedef struct ccftype {
    CCFInfo	info;
    float	latlon[MAX_SIGMET*2];
} CCFType;

typedef struct tracktype {
    TrackInfo	info;
    float	latlon[MAX_TRACKS*2];
} TrackType;

typedef struct listtype {
    ListInfo	info;
    ListData	data;
} ListType;

typedef struct ashtype {
    AshInfo	info;
    SptxType	spt;			/* "NOT SEEN" type attrib. info */
    float	latlon[MAX_ASH*2];
} AshType;

typedef struct voltype {
    VolInfo	info;
    float	latlon[2];
    int		offset_xy[2];
} VolType;

typedef struct lineattr
{
    int		splcol;			/* Line color */
    SpLineType	spl;			/* Line attribute info */
} LineAttr;

typedef struct barbattr
{
    int		wndcol;			/* Wind barb color */
    WindType	wnd;			/* Wind barb attribute info */
    int		sptcol;			/* Flight level color */
    SptxType	spt;			/* Flight level attribute info */
} BarbAttr;

typedef struct hashattr
{
    int		wndcol;			/* Hash mark color */
    WindType	wnd;			/* Hash mark attribute info */
} HashAttr;

typedef struct jettype {
    LineAttr	line;			/* Line attribute info */
    int		nbarb;			/* Number of barbs */
    BarbAttr	barb[MAX_JETPTS];	/* Wind barb attribute info */
    int		nhash;			/* Number of hashs */
    HashAttr	hash[MAX_JETPTS];	/* Hash mark attribute info */
} JetType;

/*
 *  Structs for GFA element
 */
#define MAX_GFA_BLOCKS	10

typedef char gfaBlock_t[STD_STRLEN];

typedef struct gfainfo {
    int		nblocks;
    int		npts;
    gfaBlock_t	*blockPtr[MAX_GFA_BLOCKS];
} GfaInfo;

typedef struct gfatype {
    GfaInfo	info;
    float	latlon[MAXPTS*2];
} GfaType;


typedef struct vg_hdrstruct
{
    char	delete;
    char	vg_type;
    char	vg_class;
    signed char	filled;
    char	closed;
    char	smooth;  /* smooth level of VG elements */
    char	version; /* version number of VG elements */
    char	grptyp;
    int		grpnum;
    int		maj_col;
    int		min_col;
    int		recsz;
    float	range_min_lat;
    float	range_min_lon;
    float	range_max_lat;
    float	range_max_lon;

} VG_HdrStruct;


typedef struct vg_dbstruct
{
    VG_HdrStruct hdr;
    union
    {
	FileHeadType	fhed;
        FrontType 	frt;
        LineType 	lin;
	SpLineType 	spl;
	WatchBoxType 	wbx;
        WatchSMType 	wsm;
	SymType		sym;
	WindType	wnd;
	TextType	txt;
	SptxType	spt;
	CircType	cir;
	TrackType	trk;
	SigmetType	sig;
	CCFType		ccf;
	ListType	lst;
	AshType		ash;
	VolType		vol;
	JetType		jet;
	GfaType		gfa;
	TcaType		tca;
	TcerrType	tce;
	TctrkType	tct;
	TcbklType	tcb;
    } elem; 
} VG_DBStruct;

#include "proto_vg.h"

#endif
