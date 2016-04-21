/*              mods_info.h               */

#ifndef mods_info_h
#define mods_info_h

/*
 *  Header file for structures to hold the information needed
 *   to process run-time MODs.
 *
 *       written by George Smith, HRL, September 1990
 *   Modified to allow multiple time series for the A1 type and
 *    multiple operations for the B1, B2, and B3 types.
 *       George Smith, HRL, November 2, 1990.
 */

/*
 *  The five basic types of mod_info structures come from the five types
 *  of MOD formats as defined in NWSRFS Users Manual VI.5.3C-FCEXEC-MOD.
 *
 *  Types A1 and A2 are used for time series MODs.
 *  Types B1, B2, and B3 are used for MODs associated with
 *     specific operations (i.e., other mods).
 */

/*
 *  The following definitions are used to determine which
 *  Mod format a particular mod_info_structure is for.
 *  The structures are combined into a union (similar to
 *  Xlib XEvents) so that once the appropriate mod format
 *  type is found you can extract the info needed through
 *  the appropriate variant of the ModInfo union.
 */

#define         Mod_format_A1   1
#define         Mod_format_A2   2
#define         Mod_format_B1   3
#define         Mod_format_B2   4
#define         Mod_format_B3   5

/*
 *  define ModDates structure to hold dates and
 *  and indication of whether the date is
 *  a single date, the start of a range, or
 *  the end of a range.
 */

#define         SINGLE_DATE     0
#define         START_RANGE     1
#define         END_RANGE       2
#define         SEGMENT         0
#define         FGROUP          1

typedef struct {
	int     date;           /* date - Julian hours          */
	int     type_of_date;   /* type indicator, 0, 1, or 2   */
} ModDates;

#ifndef ts_info_pm_h
#define ts_info_pm_h

typedef struct {
	int     ts_type, delta_t;
	char    ts_id[9], data_type[5];
	}       TS_INFO;
#endif

/*
 *  A1 format structure
 */

typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, maybe optional   */
	int end_date;           /* end date, maybe optional     */
	char segment_id[9];     /* segment identifier           */
	int number_of_ts;       /* number of time series        */ /* 11/02/90 */
	TS_INFO *info[10];      /* time series information      */ /* 11/02/90 */
	ModDates dates[20];     /* dates and type indicators    */
} mod_A1_struct;

/*
 *  A2 format structure
 */

typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, always defined   */
	int end_date;           /* end date, maybe optional     */
	char segment_id[9];     /* segment identifier           */
	TS_INFO *info;          /* time series information      */
	int num_values;         /* number of values entered     */
	float *values;          /* pointer to data values       */
	char keyword[6];        /* keyword, optional FIRST/LAST */
	char optype[9];         /* operation type, optional     */
	char opname[9];         /* operation name, optional     */
} mod_A2_struct;

/*
 *  B1 format structure
 */

typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, maybe optional   */
	int end_date;           /* end date, maybe optional     */
	int type_of_id;         /* SEGMENT or FGROUP            */
	char id[9];             /* segment or fgroup identifier */
	char keyword[5];        /* keyword, mandatory           */
	ModDates dates[20];     /* dates and type indicators    */
	int number_of_opers;    /* number of operations         */ /* 11/02/90 */
	char opname[10][9];     /* operation name, optional     */ /* 11/02/90 */
} mod_B1_struct;

/*
 *  B2 format structure
 */

typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, always defined   */
	int end_date;           /* end date, maybe optional     */
	int type_of_id;         /* SEGMENT or FGROUP            */
	char id[9];             /* segment or fgroup identifier */
	char keyword[9][7];     /* keywords, mandatory          */
	float values[9];        /* values assoc with keywords   */
	int number_of_opers;    /* number of operations         */ /* 11/02/90 */
	char opname[10][9];     /* operation name, optional     */ /* 11/02/90 */
} mod_B2_struct;

/*
 *  B3 format structure
 */

typedef struct {
	int type;
	char command[9];        /* Mod command name             */
	int start_date;         /* start date, always defined   */
	int end_date;           /* end date, maybe optional     */
	int type_of_id;         /* SEGMENT or FGROUP            */
	char id[9];             /* segment or fgroup identifier */
	int num_values;         /* number of values entered     */
	float *values;          /* pointer to data values       */
	char keyword[5];        /* keyword, optional            */
	int number_of_opers;    /* number of operations         */ /* 11/02/90 */
	char opname[10][9];     /* operation name, optional     */ /* 11/02/90 */
} mod_B3_struct;

typedef union _ModInfo {
	int type;                   /* must not be changed; first element   */
	mod_A1_struct          a1;
	mod_A2_struct          a2;
	mod_B1_struct          b1;
	mod_B2_struct          b2;
	mod_B3_struct          b3;
} ModInfo;

/*
 *  The following code is used by the time series mods keyword functions.
 */

#ifndef ChooseTStypestruct
#define ChooseTStypestruct


void     do_first();
void     do_last();
void     do_opType();


int     Do_First;
int     Do_Last;
int     Do_Before_operation;



static xs_menu_struct Choose_TStype_struct[] =
	{
	{"First" , do_first, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Last" , do_last, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_LB[] =
	{
	{"Last" , do_last, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_BL[] =
	{
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Last" , do_last, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_FB[] =
	{
	{"First" , do_first, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_B[] =
	{
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

#define MAX_CHARS_IN_TSMOD_KEYWORD 17

#endif  /*    end definition of ChooseTStypestruct      */

#endif  /*   mods_info_h   */
