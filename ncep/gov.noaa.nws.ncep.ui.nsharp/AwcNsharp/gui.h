/************************************************************************
 * gui.h								*
 *									*
 * This header file includes the necessary GEMPAK, X11, Motif and NXM	*
 * header files, defines structures and macros, and declares variables.	*
 ** 									*
 * Log:									*
 * S. Jacobs/NMC	 8/94						*
 * C. Lin/EAI	 	 9/95						*
 * D.W.Plummer/NCEP	 9/96	added another element in smethod_t (OBS)*
 * D.W.Plummer/NCEP	11/96	added ddttm to usrslct_t structure	*
 *                              also increased MAX_PLOT_STN to 3800	*
 * D. Kidwell/NCEP	 8/98	Added watch box information		*
 * D. Kidwell/NCEP	 4/99	Deleted watch box cancel flag		*
 * D. Kidwell/NCEP	 5/99	Replaced MAX_PLOT_STN with LLSTFL       *
 ***********************************************************************/

#include <sys/utsname.h>
#include "geminc.h"
#include "gemprm.h"   	/* MAXTYP, LLSTFL */
#include "ctbcmn.h"
#include "Nxm.h"
#include "nwxcmn.h"
#include "guid.h" 

/*---------------------------------------------------------------------*/

#define PRNMAX		( 90000 )

#define ALL_COL         2
#define ALL_MRK         6
#define SEL_COL         3
#define SEL_MRK         1
#define RES_COL         32
#define MIN_HR          00
#define MAX_PTS		100
#define MAX_CONTOURS	100
#define MAX_REPORTS     300
#define MAX_WATCHES	200
#define MAX_WPTS	7
#define REPMAX          ( 150000 ) /* Maximum text report size */

/*---------------------------------------------------------------------*/

#define SSTR_LEN 128         /* short string length     */
#define LSTR_LEN 256         /* long  string length     */

#define SRCHSTR_LEN  25     /* search string length     */


/*
 * structure to hold nwx master table and map table
 */
typedef struct {
        int ndtyp;                              /* # of data types */
        int nmap;                               /* # of maps  */
        struct datatype_list  dtyp_info[MAXTYP];/* data type info of each product*/
        struct maptype_list   map_info[MAXTYP]; /* info of maps */
}nwxtbl_t;

/*
 * structure to hold the info of possible forecast stations
 */
typedef struct {
        int   nstn;                     	/* # of stations */
        char  stnName[LLSTFL][33];      	/* station local names  */
        char  stateId[LLSTFL][3];       	/* state id names  */
        char  counId[LLSTFL][3];        	/* country id names  */
        float lat[LLSTFL];              	/* latitude of the station */
        float lon[LLSTFL];              	/* longitude of the station */
        float elv[LLSTFL];              	/* elevation of the station */
	char  srchstr[LLSTFL][SRCHSTR_LEN];   	/* record search strings */
	char  bulstr[SRCHSTR_LEN];	      	/* bulletion head string */
	int   rptstn[MAX_REPORTS]; 		/* indices of the stations issuing reports */
	float rptstnlat[MAX_REPORTS]; 		/* lat of the stations issuing reports */
	float rptstnlon[MAX_REPORTS]; 		/* lon of the stations issuing reports */
						/* lat, lon info for stations issuing 
						   special reports duplicated to facilitate 
						   easier marker plotting, gtrans, etc.,
						   function calls                     */
	int   nrptstn;		    		/* number of unique stations issueing report(s) */
	int   nreports;		    		/* total number of individual reports */
}stnlist_t; 

/*
 * lat/lon of the map boundaries
 */
typedef struct {
        float  x[2];    /* x-coord of lower left and upper right */
        float  y[2];    /* y-coord of lower left and upper right */
}mapbnd_t; 

/*
 * structure to hold search related information
 */

typedef enum {
	STANDARD, WATCHWARN, OBS
}smethod_t;

typedef struct {
        int     idtyp;                    /* index to data type info */
        struct  directory_info  dir_info; /* info about the data directory */ 
        struct  data_file_info  file_info;/* info about the selected data file */
        char    srchstr[SRCHSTR_LEN];     /* string to search for */
        char    start_of_text[2][9];      /* start mark(s) of a record */
        char    end_of_text[2][9];	  /* end mark(s) of a record */
        struct  date_time_info startd;    /* starting date/time */
        struct  date_time_info endd;      /* endding date/time */
        int     sflag;                    /* -1 = backward */
                                          /*  0 = start over */
                                          /*  1 = foreward */
	smethod_t     smethod;		  /* flag for search method */
	int	current;                  /* pointer to current WW file */
}srchinfo_t;

/*
 * structure to hold user selections
 */

typedef enum {
	STATION, STATE
} selectby_t ;
typedef enum {
	NO_ZOOM, ZOOM
} zoom_t ;

typedef struct {
	int  mapindx;		/* map background index into nwxtbl map_info*/
	selectby_t  selct_by;  	/* 0 -- select by station, 1 -- state */
	zoom_t  zoomflg;	/* 0 -- no zoom, 1 -- zoom */  
	int  ndttm;     	/* date/time preference */
	int  ddttm;     	/* date/time default */
	struct guid_grp *group; /* address of the selected group info */
	int  prod;      	/* index of the selected product in the group */ 
	int  prvnxt;    	/* flag for previous/next button in text window */
}usrslct_t;

/*
 * structures to hold the plot information
 */

typedef enum {
	EMPTY, STNSELECT, VALUE, GRAPHIC, WATCHBOX 
}plotmode_t;

struct mrkv {
	int          nstn;
	float        lat[LLSTFL];
	float        lon[LLSTFL];
        float        elev[LLSTFL];
	int          dvalues[LLSTFL];
	int          breaks[LLSTFL];
	int          ncolor;
	int          icolrs[LLSTFL];
	int          marktype; 
	float        marksize;
	int          markwdth;
	int          pltflag;
	int          iposn;
}; /* for plotting values as marker */

struct cntline {
	char  label[7]; /* label for the line */
	int   npt;      /* # of points for the contour*/
	float lat[MAX_PTS];  /* lat for each point */
	float lon[MAX_PTS];  /* lon for each point */
	int   color;	/* color for the contour */
}; /* for each contour line */

struct contour {
	int            nc;	 /* # of contours */
	struct cntline line[MAX_CONTOURS];  
}; /* for collection of contour lines */

struct 	watchinfo {
	char	wtchnum[5];	/* watch number */
	char	valid[20];  	/* watch valid GEMPAK dattim */
	char	expire[20];	/* watch expires GEMPAK dattim */
	int   	npt;      	/* # of watch area vertices */
	float 	lat[MAX_WPTS];  /* lat for each point */
	float   lon[MAX_WPTS];  /* lon for each point */
	int     color;		/* color for the watch box */
}; /* for each watch box */

struct	watchbox {
	int	nb;		/* # of watchboxes */
	struct	watchinfo winfo[MAX_WATCHES];
}; /* for collection of watch boxes */

typedef struct {
	plotmode_t     mode;            /* plot mode */
	union  {
		struct mrkv    markv;
		struct contour cnt;
		struct watchbox wbox;
	}data;				/* plot data */

/*
 * convenient for accessing data
 */
#define plt_mark    data.markv	        /* marker for plot values  */
#define plt_cnt     data.cnt	        /* contours */
#define plt_wbox    data.wbox	        /* watch box */

}plotdata_t;
/*---------------------------------------------------------------------*/

/*
 *	Declare the global variables.
 */

extern int	  _idtyp_save;
extern nwxtbl_t   *nwxTable; 	/* nwx master/map table */
extern stnlist_t  stnList;	/* related station info */
extern usrslct_t  usrSelect;	/* user selections */ 
extern mapbnd_t	  mapBnd;	/* map boundaries */
extern srchinfo_t srchInfo;	/* search information */
extern plotdata_t plotData;	/* data for plotting */

extern char	reportText[];   /* text report */
extern char	printText[];	/* text for print */
extern char	wtchText[MAX_WATCHES][REPMAX]; /* Text for watch boxes */

extern Widget	mapCanvW;	/* widget for drawing map */
extern Widget   textW; 		/* widget for displaying text */
extern Widget   dataSelectW;	/* popup window for data selection */
extern Widget 	prevBtnW, nextBtnW; /* previous/next buttons in text window */
extern Widget   prntBtnW;	    /* print button in text window */
extern Widget   dttmtxtW; 	    /* date/time info widget in text window */
extern Widget   prdgrptxtW;	    /* product/group info widget in text window */

/*---------------------------------------------------------------------*/

/*
 *	Declare the function prototypes.
 */

#include "proto_nwx.h"

/*---------------------------------------------------------------------*/

struct report_info{
        char            stnid[25];
        int             position;
        char            fname[133];
        int             stnindex;
};

extern struct report_info    reportInfo[];

