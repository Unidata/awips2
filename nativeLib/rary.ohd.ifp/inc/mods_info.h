/*--------------------------------------------------------------*/
/* Mods_info.h							*/
/*								*/
/* Header file for structures to hold the information needed	*/
/* to process run-time MODs.					*/
/*								*/
/* Written by George Smith, HRL, September 1990			*/
/*								*/
/* Modified to allow multiple time series for the A1 type and	*/
/* multiple operations for the B1, B2, and B3 types.		*/
/* George Smith, HRL, November 2, 1990.				*/
/*								*/
/* The five basic types of mod_info structures come from the	*/
/* five type of MOD formats as defined in NWSRFS Users Manual	*/
/* VI.5.3C-FCEXEC-MOD.						*/
/*								*/
/* Types A1 and A2 are used for time series MODs.		*/
/* Types B1, B2, and B3 are used for MODs associated		*/
/* with specific operations (i.e., other mods).			*/
/*                                                              */
/* Modified to allow for new date fields (valid_date)           */
/* D. Page - 05/19/95                                           */
/*--------------------------------------------------------------*/



/*--------------------------------------------------------------*/
/* The following definitions are used to determine which Mod	*/
/* format a particular mod_info_structure is for. The		*/
/* structures are combined into a union (similar to Xlib	*/
/* XEvents) so that once the appropriate mod format type is	*/
/* found you can extract the info needed through the		*/
/* appropriate variant of the ModInfo union.			*/
/*--------------------------------------------------------------*/

#ifndef Mods_info_h
#define Mods_info_h

#define         Mod_format_A1   1
#define         Mod_format_A2   2
#define         Mod_format_B1   3
#define         Mod_format_B2   4
#define         Mod_format_B3   5

/*--------------------------------------------------------------*/
/* Define ModDates structure to hold dates and indication of	*/
/* whether the date is a single date, a single date, the start	*/
/* of a range, or the end of a range.				*/
/*--------------------------------------------------------------*/

#define         SINGLE_DATE     0
#define         START_RANGE     1
#define         END_RANGE       2
#define         SEGMENT         0
#define         FGROUP          1

#define 	MAX_MODS	100

typedef struct {
	int     date;           /* Date - Julian hours          */
	int     type_of_date;   /* Type indicator, 0, 1, or 2   */
} ModDates;

#ifndef ts_info_pm_h
#define ts_info_pm_h
typedef struct {
	int     ts_type, delta_t;
	char    ts_id[9], data_type[5];
	}       TS_INFO;
#endif

TS_INFO                  ts_info[20];
typedef struct{
		int     first;           
		int     last;           
		int     min_range;  
		char    seg_str[100][20]; 
		char    first_rangeseg[100]; 
} SEL_RANGE;

SEL_RANGE		range_selected;

/*----------------------*/
/*  A1 Format structure	*/
/*----------------------*/
typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* Start date, maybe optional   */
	int end_date;           /* End date, maybe optional     */
	int valid_date;         /* Valid date, maybe optional   */ /* 05/18/95 */
	char segment_id[9];     /* Segment identifier           */
	int number_of_ts;       /* Number of time series        */ /* 11/02/90 */
	TS_INFO *info[10];      /* Time series information      */ /* 11/02/90 */
	ModDates dates[20];     /* Dates and type indicators    */
} mod_A1_struct;


/*----------------------*/
/*  A2 Format structure	*/
/*----------------------*/
typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* Start date, always defined   */
	int end_date;           /* end date, maybe optional     */
	int valid_date;         /* Valid date, maybe optional   */ /* 05/18/95 */
	char segment_id[9];     /* segment identifier           */
	TS_INFO *info;          /* time series information      */
	int num_values;         /* number of values entered     */
	float *values;          /* pointer to data values       */
	char keyword[6];        /* keyword, optional FIRST/LAST */
	char optype[9];         /* operation type, optional     */
	char opname[9];         /* operation name, optional     */
} mod_A2_struct;


/*----------------------*/
/*  B1 Format structure	*/
/*----------------------*/
typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, maybe optional   */
	int end_date;           /* end date, maybe optional     */
	int valid_date;         /* Valid date, maybe optional   */ /* 05/18/95 */
	int type_of_id;         /* SEGMENT or FGROUP            */
	char id[18];             /* segment or fgroup identifier */
	char keyword[5];        /* keyword, mandatory           */
	ModDates dates[20];     /* dates and type indicators    */
	int number_of_opers;    /* number of operations         */ /* 11/02/90 */
	char opname[10][9];     /* operation name, optional     */ /* 11/02/90 */
} mod_B1_struct;


/*----------------------*/
/*  B2 Format structure	*/
/*----------------------*/
typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, always defined   */
	int end_date;           /* end date, maybe optional     */
	int valid_date;         /* Valid date, maybe optional   */ /* 05/18/95 */
	int type_of_id;         /* SEGMENT or FGROUP            */
	char id[18];             /* segment or fgroup identifier */
	char keyword[9][7];     /* keywords, mandatory          */
	float values[9];        /* values assoc with keywords   */
	int number_of_opers;    /* number of operations         */ /* 11/02/90 */
	char opname[10][9];     /* operation name, optional     */ /* 11/02/90 */
} mod_B2_struct;


/*----------------------*/
/*  B3 Format structure	*/
/*----------------------*/
typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, always defined   */
	int end_date;           /* end date, maybe optional     */
	int valid_date;         /* Valid date, maybe optional   */ /* 05/18/95 */
	int type_of_id;         /* SEGMENT or FGROUP            */
	char id[18];             /* segment or fgroup identifier */
	int num_values;         /* number of values entered     */
	float *values;          /* pointer to data values       */
	char keyword[5];        /* keyword, optional            */
	int number_of_opers;    /* number of operations         */ /* 11/02/90 */
	char opname[10][9];     /* operation name, optional     */ /* 11/02/90 */
} mod_B3_struct;

typedef union _ModInfo {
	int type;		/* Must not be changed; 1st element!!	*/
	mod_A1_struct          a1;
	mod_A2_struct          a2;
	mod_B1_struct          b1;
	mod_B2_struct          b2;
	mod_B3_struct          b3;
} ModInfo;

#endif
