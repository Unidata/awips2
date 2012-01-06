#ifndef _CONFIG_H
#define _CONFIG_H

#define MAXDATATYPES 32

#define LOCALNSHARPCONFIGFILE  "nsharpconf"
#define GLOBALNSHARPCONFIGFILE "/NAWIPS/nascrpts/resource/nsharpconf"

typedef struct configdata_t {
	char menuname[16];	       /* Name as appears on menu */
	char filename[256];            /* Where to find the data */
	char searchpattern[256];       /* Wildcard sequence for patterns */
	char timelist[256][16];        /* times in data file */
	char station[16];              /* Selected station */
	char time[16];                 /* Selected time */
	char stations[4096][20];       /* Stations in data file */
	int  ntimes;                   /* Number of times in data file */
	int  timeptr;		       /* Index of this time in the list */
	int  nstns;                    /* Number of stations in data file */
	int  stype;                    /* Sounding type (see globals.h) */
	int  id;                       /* index of this entry in config file */
} configdata_t;

extern configdata_t *curdatatype_ptr;   /* Pointer to current data type */
extern configdata_t  datatype[];
extern int           ndatatypes;

typedef struct configsymtab {
	int  symbol;
	char name[16];
} configsymtab;

/* Function prototypes */
int   readconfigfile(void);
void  setconfigdatapointer(int num);
char *getlatestfile(char *searchstring);

#endif  /* _CONFIG_H */
