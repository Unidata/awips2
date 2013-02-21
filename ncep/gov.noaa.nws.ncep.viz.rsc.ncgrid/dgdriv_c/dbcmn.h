char 	eLibClass	[50];
char 	ePlugIn		[50];
char	eSrc		[50];
char	eMdl		[50];
char	ePrt		[50];
char	eGrid		[50];
int	eCount;
char    ePhysEl         [65];
char    eSecId          [65];
char    eDistnctField   [200];
char    eConstraintF    [200];
char    eConstraintV    [200];
char    eParameters     [500];

char    eIcao		[5];
char	eProduct	[5];
char	eResolution	[10];
char    eElAngle	[10];

char 	gArea		[500];
char	gDattim		[25];
char	gNavDattim	[25];
char	gCord		[25];
char    gLevel          [25];
char	gParm		[25];
char	gCycle		[25];
char	gEventName	[50];

/*
 * callback stuff
 *
 * 1. for the data
 *
 */
void (*dataClbkPtr)(char*);
int  rdataBackSize; 
float *rdataBack;
/*
 * 2. for the navigation
 */
void (*navClbkPtr)(char*);
char *navStrBack;
int  navStrLength;
/*
 * 3. for the datauri
 */
void (*duriClbkPtr)(char*);
char *duriStrBack;
int  duriStrLength;
/*
 * 4. for the idiagnostic message back to Dgdriv
 */
void (*diagClbkPtr)(char*);
/*
 * 5. for the cycle forecast hours
 */
void (*fhrsClbkPtr)(void);
char *fhrsStrBack;
int  fhrsStrLength;
/*
 * 6. for the ensemble members
 */
void (*flnmClbkPtr)(char*);
char *flnmStrBack;
int  flnmStrLength;

/*
 * 7. for the sub - grid CRS
 */
void (*subgCrsClbkPtr)(char*);
char *subgCrsStrBack;
int  subgCrsStrLength;

typedef struct  a2dtinfo {
        char    alias[30];      /* Template alias               */
        char    path[26];       /* Template path                */
        char    template[49];       /* Template                     */
} A2DTinfo;

typedef struct a2data_t {
        int     numtmpl;        /* Number of templates          */
        A2DTinfo  *info;          /* Template information         */
} A2Data_t;
