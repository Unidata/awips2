/************************************************************************
 * SHPPRM.H								*
 *									*
 * This header file defines the parameters and structures used in the	*
 * Shapefile routines.							*
 *									*
 **									*
 * Log:									*
 * R. Tian/SAIC  	 8/04						*
 * R. Tian/SAIC  	 2/05		Added MAP_FIRE/RFCR/CWAB	*
 * R. Tian/SAIC  	 4/05		Added MAP_RFCB			*
 * R. Tian/SAIC  	 6/05		Added MAP_TMZN and MAP_USST	*
 * R. Tian/SAIC  	10/05		Added MAP_STMX			*
 * m.gamazaychikov/SAIC 06/08		Added MAP_OFSH, _CNPR, _UNDF	*
 ***********************************************************************/

/*---------------------------------------------------------------------*/

#ifndef SHPPRM_H
#define SHPPRM_H

/*
 * Shapefile type definition.
 */
#define MAP_CNTY ((unsigned long)0x01)	/* County */
#define MAP_MARI (MAP_CNTY<<1)		/* Marine */
#define MAP_MZCN (MAP_CNTY|MAP_MARI)	/* Combined County & Marine */
#define MAP_ZONE (MAP_CNTY<<2)		/* Public Forcast Zone */
#define MAP_IEDG (MAP_CNTY<<3)		/* Ice Edge */
#define MAP_FIRE (MAP_CNTY<<4)		/* Fire Weather Zone Boundaries */
#define MAP_RFCR (MAP_CNTY<<5)		/* River Forecast Center Regions */
#define MAP_CWAB (MAP_CNTY<<6)		/* County Warning Area Boundaries */
#define MAP_RFCB (MAP_CNTY<<7)		/* River Forecast Center Basin */
#define MAP_TMZN (MAP_CNTY<<8)		/* Time Zone */
#define MAP_USST (MAP_CNTY<<9)		/* US State and Territories */
#define MAP_MXST (MAP_CNTY<<10)		/* Mexico State */
#define MAP_CNPR (MAP_CNTY<<11)         /* Canadian Provinces */
#define MAP_OFSH (MAP_CNTY<<12)         /* Offshore Marine Zones */
#define MAP_UNDF (MAP_CNTY<<13)         /* Maps other that defined */

/*
 * Output bound, bound info, and station table names.
 */
#define COUNTYBNDS	"countybnds.tbl"
#define COUNTYBNDS_INFO	"countybnds.tbl.info"
#define COUNTYTBL	"county.tbl"
#define COUNTYNAMTBL	"countynam.tbl"
#define MZBNDS		"mzbnds.tbl"
#define MZBNDS_INFO	"mzbnds.tbl.info"
#define MARINETBL	"marinezones.tbl"
#define MZCNTYBNDS	"mzcntybnds.tbl"
#define MZCNTYBNDS_INFO	"mzcntybnds.tbl.info"
#define MZCNTYTBL	"mzcntys.tbl"
#define MZCNTYNAMTBL	"marinenames.tbl"
#define PFZBNDS		"pfzbnds.tbl"
#define PFZBNDS_INFO	"pfzbnds.tbl.info"
#define PFZTBL		"landzones.tbl"
#define ICEEDGEBND	"IceEdgeBnd.tbl"
#define ICEEDGEBND_INFO	"IceEdgeBnd.tbl.info"
#define FIREBND		"firebnds.tbl"
#define FIREBND_INFO	"firebnds.tbl.info"
#define RFCBND		"rfcbnds.tbl"
#define RFCBND_INFO	"rfcbnds.tbl.info"
#define CWABND		"cwabnds.tbl"
#define CWABND_INFO	"cwabnds.tbl.info"
#define RIVERBASINTBL	"riverbas.tbl"
#define TZBNDS		"tzbnds.tbl"
#define	TZBNDS_INFO	"tzbnds.tbl.info"
#define STBNDS		"statebnds.tbl"
#define STBNDS_INFO	"statebnds.tbl.info"
#define STATETBL	"state.tbl"
#define CNPRBNDS        "cnprbnds.tbl"
#define CNPRBNDS_INFO   "cnprbnds.tbl.info"
#define OSMZBNDS        "osmzbnds.tbl" 
#define OSMZBNDS_INFO   "osmzbnds.tbl.info"
#define UNDFBNDS        "undfbnds.tbl" 
#define UNDFBNDS_INFO   "undfbnds.tbl.info"

/*
 * Do not change anything below for new type of shapefile.
 */
#define MAXFLD 16	/* Maximum number of fields per record */
#define MAXOUT 75000	/* Maximum numer of points per part */
#define MAXRECLEN 500	/* Maximum record length */
#define MAXSHP 3	/* Max shape file to process */
#define ROUNDUP(x) ( (x) < 0.0 ? (x-0.005) : (x+0.005) )
typedef enum { LITTLE, BIG } endian_t;

/*
 * The following structures are related the dBASE III file *.dbf.
 */
#define DBF_VALID       0x20
#define DBF_DELETED     0x2A
#define FLD_NAMELEN	11	   	   /* maximum fieldname-length */

typedef struct {
    unsigned char dbf_vers;	   	   /* dBASE III version number */
    unsigned char dbf_date[3];     	   /* last modification-date */
    unsigned char dbf_nrec[4];	   	   /* number of records */
    unsigned char dbf_hlen[2];	   	   /* length of header structure */
    unsigned char dbf_rlen[2];	   	   /* length of the record */
    unsigned char dbf_stub[20];	  	   /* reserved bytes */
} dbf_hdrmap;
#define DBF_HEADER_SIZE	sizeof(dbf_hdrmap) /* size of header */

typedef struct {
    unsigned char fld_name[FLD_NAMELEN];   /* field-name */
    unsigned char fld_type;                /* field-type */
    unsigned char fld_addr[4];             /* some reserved stuff */
    unsigned char fld_lens;                /* field-length */
    unsigned char fld_decs;                /* field decimal count */
    unsigned char fld_stub[14]; 	   /* reserved bytes */
} dbf_fldmap;
#define DBF_FIELD_SIZE sizeof(dbf_fldmap)  /* size of field descriptor */

typedef struct {
    char name[FLD_NAMELEN];       	   /* field name */
    int  type;                    	   /* field type */
    int  lens;                    	   /* field length */
    int  decs;                    	   /* field decimal count */
    char data[MAXRECLEN];         	   /* field data */
} dbf_field;

typedef struct {
    int nrec;        		           /* number of records */
    int hlen;                              /* length of header structure */
    int rlen;                              /* length of the record */
    int nfld;	                           /* number of fields */
    dbf_field dbflds[MAXFLD];              /* fields array */
} dbf_header;

/*
 * The following structures are related to the index file *.shx.
 */
#define SHX_HEADER_LEN 		100
#define SHX_RECORD_LEN		8

typedef struct {
    unsigned char  rec_offs[4];		   /* record offset in big endian */
    unsigned char  rec_lens[4];            /* content length in big endian */
} shx_recmap;

typedef struct {
    int offs;		                   /* record offset */
    int lens;                              /* content length */
} shx_record;

/*
 * The following are related to shape file *.shp.
 */
#define SHP_HEADER_LEN 		100
#define SHP_RECHDR_LEN		8
#define BOX_SIZE                32
#define POINT_SIZE              16
#define INTEGER_SIZE            4

typedef struct part_t {
    int prtflg;				   /* part status flag */
    int numpts;				   /* number of points */
    float maxlat;			   /* maximum latitude */
    float minlat;			   /* minimum latitude */
    float maxlon;			   /* maximum longitude */
    float minlon;			   /* minimum longitude */
    float *ptx;				   /* points x array */
    float *pty;				   /* points y array */
    struct part_t *prvprt;		   /* previous part */
    struct part_t *nxtprt;		   /* next part */
} shp_part;
#define SHP_PRTSZ sizeof(shp_part)
#define PRT_OUT		0x01
#define PRT_DEL		0x02

typedef struct record_t {
    int numfld;				   /* number of fields */
    int numprt;			           /* number of parts */
    float cenlat;			   /* computed center latitude */
    float cenlon;			   /* computed center longitude */
    float maxlat;			   /* maximum latitude */
    float maxlon;			   /* maximum longitude */
    float minlat;			   /* minimum latitude */
    float minlon;			   /* minimum longitude */
    dbf_field fields[MAXFLD];		   /* fields */
    shp_part *shpart;			   /* data parts */
    struct record_t *prvrec;		   /* previous record */
    struct record_t *nxtrec;		   /* next record */
} shp_record;
#define SHP_RECSZ sizeof(shp_record)

/*
 * The following defines program globals.
 */
#define NUMABBR 153			   /* Number of abbreviations */
#define NUMSTNO 75			   /* Number of states */

#ifdef SHP_GLOBAL

endian_t	mch_endian;
int		maptyp;

char stateno[NUMSTNO][3] = { 
     /* State Abbrev */ /* State Num */ /* State Name */
	"--",		/* 0 */		/* No name */
	"AL",		/* 1 */		/* Alabama */
	"AK",		/* 2 */		/* Alaska */
	"AZ",		/* 3 */		/* Arizona */
	"AR",		/* 4 */		/* Arkansas */
	"CA",		/* 5 */		/* California */
	"CO",		/* 6 */		/* Colorado */
	"CT",		/* 7 */		/* Connecticut */
	"DE",		/* 8 */		/* Delaware */
	"FL",		/* 9 */		/* Florida */
	"GA",		/* 10 */	/* Georgia */
	"HI",		/* 11 */	/* Hawaii */
	"ID",		/* 12 */	/* Idaho */
	"IL",		/* 13 */	/* Illinois */
	"IN",		/* 14 */	/* Indiana */
	"IA",		/* 15 */	/* Iowa */
	"KS",		/* 16 */	/* Kansas */
	"KY",		/* 17 */	/* Kentucky */
	"LA",		/* 18 */	/* Lousiana */
	"ME",		/* 19 */	/* Maine */
	"MD",		/* 20 */	/* Maryland */
	"MA",		/* 21 */	/* Massachusetts */
	"MI",		/* 22 */	/* Michigan */
	"MN",		/* 23 */	/* Minnesota */
	"MS",		/* 24 */	/* Mississippi */
	"MO",		/* 25 */	/* Missouri */
	"MT",		/* 26 */	/* Montana */
	"NE",		/* 27 */	/* Nebraska */
	"NV",		/* 28 */	/* Nevada */
	"NH",		/* 29 */	/* New Hampsire */
	"NJ",		/* 30 */	/* New Jersey */
	"NM",		/* 31 */	/* New Mexico */
	"NY",		/* 32 */	/* New York */
	"NC",		/* 33 */	/* North Carolina */
	"ND",		/* 34 */	/* North Dakota */
	"OH",		/* 35 */	/* Ohio */
	"OK",		/* 36 */	/* Oklahoma */
	"OR",		/* 37 */	/* Oregon */
	"PA",		/* 38 */	/* Pennsylvania */
	"RI",		/* 39 */	/* Rhode Island */
	"SC",		/* 40 */	/* South Carolina */
	"SD",		/* 41 */	/* South Dakota */
	"TN",		/* 42 */	/* Tennessee */
	"TX",		/* 43 */	/* Texas */
	"UT",		/* 44 */	/* Utah */
	"VT",		/* 45 */	/* Vermont */
	"VA",		/* 46 */	/* Virginia */
	"WA",		/* 47 */	/* Washington */
	"WV",		/* 48 */	/* West Virginia */
	"WI",		/* 49 */	/* Wisconsin */
	"WY",		/* 50 */	/* Wyoming */
	"DC",		/* 51 */	/* District of Columbia */
	"VI",		/* 52 */	/* Virgin Islands */
	"PR",		/* 53 */	/* Puerto Rico */
	"GU",		/* 54 */	/* Guam */
	"MP",		/* 55 */	/* Marianas Islands */
	"UM",		/* 56 */	/* Midway Islands */
	"--",		/* 57 */	/* No name */
	"--",		/* 58 */	/* No name */
	"--",		/* 59 */	/* No name */
	"LC",		/* 60 */	/* Lake St. Clair */
	"PZ",		/* 61 */	/* Pacific Coast */
	"LO",		/* 62 */	/* Lake Ontario */
	"LE",		/* 63 */	/* Lake Erie */
	"LM",		/* 64 */	/* Lake Michigan */
	"LS",		/* 65 */	/* Lake Superior */
	"AM",		/* 66 */	/* Mid Atlantic */
	"AN",		/* 67 */	/* North Atlantic */
	"GM",		/* 68 */	/* Gulf of Mexico */
	"PK",		/* 69 */	/* Pacific Coast of Alaska */
	"PH",		/* 70 */	/* Pacific Coasts of Hawaii */
	"PM",		/* 71 */	/* Pacific Coasts of Marianas Islands */
	"PS",		/* 72 */	/* Pacific Coasts of Tututila and Manua */
	"SL",		/* 73 */	/* Saint Lawrence Seaway */
	"LH"		/* 74 */	/* Lake Huron */
};

char abbrev_tbl[NUMABBR*2][32] = {
     /* abbrev */  /* full name */
	"/",       " /",
	"/",   	   "/ ",
	"-",       " -",
	"-",       "- ",
	"Entr",    "Entrance",
	"Mtn",     "Mountain",
	"30",      "Thirty",
	"2",       "Two",
	"Dist",    "District",
	"Grn",     "Green",
	"NWrn",    "Northwestern",
	"SWrn",    "Southwestern",
	"NErn",    "Northeastern",
	"SErn",    "Southeastern",
	"NW",      "Northwest",
	"SW",      "Southwest",
	"NE",      "Northeast",
	"SE",      "Southeast",
	"NMi",     "Nautical Mile",
	"Mi",      "Mile",
	"Naut",    "Nautical",
	"Nrn",     "Northern",
	"Nrn",     "northern",
	"Srn",     "Southern",
	"Srn",     "southern",
	"Ern",     "Eastern",
	"Ern",     "eastern",
	"Wrn",     "Western",
	"Wrn",     "western",
	"Ctrl",    "Central",
	"Ctrl",    "central",
	"N",       "North",
	"N",       "north",
	"Nrn",     "Nthrn",
	"S",       "South",
	"S",       "south",
	"Srn",     "Sthrn",
	"E",       "East",
	"E",       "east",
	"Ern",     "Eern",
	"W",       "West",
	"W",       "west",
	"Wrn",     "Wern",
	"Excl",    "Excluding",
	"Incl",    "Including",
	"Ext",     "Extending",
	"Hi",      "High",
	"Pr",      "Prince",
	"Wx",      "Weather",
	"Vic",     "Vicinity",
	"MI",      "Michigan",
	"MS",      "Mississippi",
	"HI",      "Hawaii",
	"NV",      "Nevada",
	"CO",      "Colorado",
	"AK",      "Alaska",
	"DE",      "Delaware",
	"AR",      "Arkansas",
	"Jct",     "Junction",
	"Atl",     "Atlantic",
	"Mex",     "Mexico",
	"Can",     "Canada",
	"Mex",     "Mexican",
	"Can",     "Canadian",
	"Ont",     "Ontario",
	"Carib",   "Caribbean",
	"Carib",   "Carribean",
	"PR",      "Puerto Rico",
	"Br",      "Bridge",
	"I",       "Isle",
	"LI",      "Long Island",
	"I",       "Island",
	"Pk",      "Park",
	"Rng",     "Range",
	"Rgn",     "Region",
	"Riv",     "River",
	"Snd",     "Sound",
	"L",       "Lake",
	"Gt",      "Great",
	"Mt",      "Mountain",
	"Fath",    "Fathom",
	"Pln",     "Plain",
	"Sup",     "Superior",
	"Ch",      "Church",
	"Recnl",   "Recreational",
	"Rec",     "Recreation",
	"Chnl",    "Channel",
	"Crnr",    "Corner",
	"Lwr",     "Lower",
	"Upr",     "Upper",
	"Exc",     "Except",
	"Ind",     "Indian",
	"Geo",     "George",
	"Entr",    "Entrance",
	"Pt",      "Point",
	"Pt ",     "Port ",
	"vl",      "ville",
	"bg",      "burg",
	"mt",      "mont",
	"Bdr",     "Border",
	"Frnt",    "Front",
	"Natl",    "National",
	"Cntry",   "Country",
	"Vly",     "Valley",
	"Dsrt",    "Desert",
	"Wht",     "White",
	"Adjcnt",  "Adjacent",
	"Cnty",    "County",
	"Cnties",  "Counties",
	"Cty",     "City",
	"Ft",      "Fort",
	"Fld",     "Field",
	"Chsapke", "Chesapeake",
	"Pk",      "Peak",
	"Fthills", "Foothills",
	"Bsn",     "Basin",
	"Wd",      "Wood",
	"Hd",      "Head",
	"Slt",     "Salt",
	"Dntn",    "Downtown",
	"Cst",     "Coast",
	"Spg",     "Spring",
	"Med",     "Medicine",
	"Clr",     "Clear",
	"Wtr",     "Watr",
	"Wtr",     "Water",
	"wtr",     "water",
	"Crk",     "Creek",
	"Inlnd",   "Inland",
	"Inlt",    "Inlet",
	"Ltl",     "Little",
	"Penin",   "Peninsula",
	"Lt",      "Light",
	"Int",     "Interior",
	"Bl",      "Bluff",
	"Adm",     "Admiral",
	"Eng",     "English",
	"Hbr",     "Harbor",
	"Insd",    "Inside",
	"Gnd",     "Grand",
	"Concep",  "Conception",
	"Inr",     "Inner",
	"Out",     "Outer",
	"4",       "Four",
	"C",       "Cape",
	"St",      "Strait",
	"Sta",     "Santa",
	"Sn",      "San",
	"fr",      "from",
	"-",       " to ",
	" & ",     " and ",
	"&",       " & ",
	"O",       "Ocean"
};

#else

extern endian_t	mch_endian;
extern int	maptyp;
extern char	stateno[NUMSTNO][3];
extern char	abbrev_tbl[NUMABBR*2][32];

#endif

/*
 * proto-types
 */
long	shp_get_llong (		unsigned char	*cp );
short	shp_get_lshort (	unsigned char	*cp );
long	shp_get_blong (		unsigned char	*cp );
short	shp_get_bshort (	unsigned char	*cp );
void	shp_get_point (		unsigned char	*buffer,
                     		float		*ptx, 
				float		*pty, 
				int		*iret );
void	*shp_mnew (		size_t		size );
void	shp_mfree (		void		*ptr );
void	shp_mfreeall (		void );
void	shp_mdiag (		void );
void	shp_rdbh (		FILE		*fp,
				dbf_header	*header,
				int		*iret );
void	shp_rdbf (		FILE		*fp,
				int		rec,
				dbf_header	*dbfh, 
				int		*iret );
void	shp_rshx (		FILE		*fp,
				int		rec,
				shx_record	*indx,
				int		*iret );
void	shp_rshp (		FILE		*fp,
				dbf_header	*dbfhdr,
                		shx_record	*shxrec,
				shp_record	**shprecp,
				int		*iret );
void	shp_splt (		shp_record	*shprec,
				int		numrec,
				int		*iret );
void	shp_mtyp (		char		filnms[][LLPATH],
				int		numfil,
				int		*iret );
void	shp_drec (		shp_record	**shprecp,
				int		*numrec,
				int		*iret );
void	shp_gkey (		shp_record	*onerec,
				char		*reckey,
				int		*iret );
void	shp_strip (		shp_part	**prtlstp,
				int		*numprt,
				float		tol,
				int		*iret );
void	shp_join (		shp_part	**prtlstp,
				int		*numprt, 
				int		*iret );
void	shp_cmap (		shp_part	*prtlst,
				char		*mapnam,
				float		ratio,
				int		*iret );
void	shp_thin (		shp_part	*oneprt,
				float		threshold,
				int		*iret );
void	shp_wprt (		FILE		*fp,
				shp_part	*oneprt,
				int		*iret );
void	shp_wfld (		FILE		*fp,
				shp_record	*onerec,
				int		*iret );
void	shp_wrec (		FILE		*fp,
				shp_record	*shprec,
				int		*iret );
void	shp_cmbn (		shp_record	*shprec,
				int		*numrec,
				int		*iret );
void	shp_gctr (		shp_record	*onerec, 
				float		*xctr,
				float		*yctr, 
                		int		*iret );
void	shp_ctbl (		shp_record	*shprec,
				int		numrec,
				int		*iret );
void	shp_cbnd (		shp_record	*shprec, 
				int		recnum,
				int		*iret );
void	shp_bhdr (		shp_record	*onerec, 
				char		*header,
				char		*subhdr,
                  		char		*hdrnam,
				float		*cenlat,
				float		*cenlon,
				int		*iret );

#endif
