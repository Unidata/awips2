#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
/* For stat() */
#include <sys/types.h>
#include <sys/stat.h>
#include "globals.h"
#include "globfiles.h"
#include "config.h"

configdata_t *curdatatype_ptr;          /* Pointer to current data type */
configdata_t  datatype[MAXDATATYPES];   /* The data types */
int           ndatatypes;

/* 
 * Make sure the array index in symbols matches the #defines in 
 * Sndglib/sndglib.h
 * This needs to get fixed I guess.
 */
configsymtab symbols[] = {
	                  {NSHARP_SFC,  "NSHARP_SFC"},
	                  {NSHARP_OBS,  "NSHARP_OBS"},
	                  {NSHARP_PFC,  "NSHARP_PFC"},
	                  {NSHARP_MDL,  "NSHARP_MDL"},
	                  {NSHARP_ARCH, "NSHARP_ARCH"}, 
	                  {NSHARP_ACARS,"NSHARP_ACARS"}, 
	                  {0,NULL}
	                 };

/*
If you change filenames then you need to set ntimes and nstns to 0

Also, before just copying filenames see if it's the same as what's there
already. if it's the same do nothing.
*/

int readconfigfile(void)
{
	char *ptr, *ptr2, bufr[512];
	FILE *fp;
	int   x, field, needfilename;

	ndatatypes = 0;

	/* First try to open local config file */
	if ((fp = fopen(LOCALNSHARPCONFIGFILE, "r")) == NULL) {

	  /* That failed. Let's try the global one */
	  if ((fp = fopen(GLOBALNSHARPCONFIGFILE, "r")) == NULL) {
	    /* Give up */
	    fprintf(stderr,
	      "Could not read either a local or global config file.\n");
	    return (1);
	  }
	}

	while (fgets(bufr, sizeof(bufr), fp) != NULL) {
	  if (*bufr == '#') continue;
	  bufr[strlen(bufr)-1] = '\0';

	  if ((ptr = strtok(bufr,"|")) == NULL) continue;

	  for (x=strlen(ptr)-1; x >=0; x--) {
	    if (!isspace(ptr[x])) break;
	  }

	  strncpy(datatype[ndatatypes].menuname, ptr, x+1);
	  datatype[ndatatypes].menuname[x+1] = '\0';

	  field = 1;
	  needfilename = 0;

	  while ((ptr = strtok((char *)NULL, "|")) != NULL) {

	    /* Remove leading and trailing white space */
	    while (isspace(*ptr)) ptr++;
	    ptr2 = strchr(ptr,' ');
	    if (ptr2) *ptr2 = '\0';

	    switch (field) {
	      case 1:
	        if (!strcmp(ptr,"LATEST_FILE"))
	          needfilename = 1;
	        else
	          strcpy(datatype[ndatatypes].filename, ptr);
	      break;
	      case 2:
	        x=0;
	        datatype[ndatatypes].stype = -1;
	        while (symbols[x].name[0] != '\0') {
  	          if (strcmp(symbols[x].name, ptr) == 0) {
	            datatype[ndatatypes].stype = symbols[x].symbol;
	            break;
  	          }
	          x++;
	        }
	      break;
	      case 3:
	        strcpy(datatype[ndatatypes].searchpattern, ptr);
	        if (needfilename) {
	          char *latestfile = getlatestfile(ptr);
	          if (latestfile) {
	            strcpy(datatype[ndatatypes].filename, latestfile);
	            free(latestfile);
	          } 
	        }
	      break;
	    }
	    field++;
	  }

	  datatype[ndatatypes].ntimes = 0;
	  datatype[ndatatypes].nstns  = 0;
	  datatype[ndatatypes].id     = ndatatypes;

	  ndatatypes++;
	  if (ndatatypes == MAXDATATYPES)
	    break;
	}

	(void)fclose(fp);
	return (0);
}

void setconfigdatapointer(int num)
{
	if (num < 0 || num > ndatatypes) return;

	curdatatype_ptr = &datatype[num];
}

char *getlatestfile(char *searchstring)
{
	char **filenames, *ptr=NULL;
	int    i, status, latest;
	time_t latesttime=0;
	struct stat statbufr;

	filenames = getfiles(searchstring, &status);

	if (!filenames || status != 0) {
	  fprintf(stderr,"An error occured trying to find files.\n");
	  return NULL;
	}

	i = 0;
	while (filenames[i] != NULL) {
	  /* Stat the file */
	  status = stat(filenames[i], &statbufr);
	  if (status == 0 && statbufr.st_ctime > latesttime) {
	    latesttime = statbufr.st_ctime;
	    latest = i;
	  }
	  i++;
	}

	if (latesttime > 0)
	  ptr = strdup(filenames[latest]);

	i = 0;
	while (filenames[i] != NULL) {
	  free(filenames[i]);
	  i++;
	}
	free(filenames);

	return ptr;
}

int read_nsharp_config(void)
{
	/**********************************************************************************/
	/* READ_NSHARP_CONFIG                                                             */
	/*                                                                                */
	/* Routine will read the nsharp configuration file "nsharp.cfg" to determine      */
        /* various parameters.  If no file is found, default values are set               */
	/**********************************************************************************/	
	FILE *fp;
	char bufr[80];

        if ((fp = fopen("nsharp.cfg", "r")) == NULL) 
		{
            	fprintf(stderr, "Could not read nsharp.cfg file. Use default.\n");
		strcpy(sars_filename, "                                                                                ");
		//strcpy(sars_filename, "/users/thompson/nsharp/bigsharp5/nlist.txt");
		strcpy(sars_filename, "/export/cdbsrv/cchen/Desktop/bigsharp9-original/nlist.txt");
                strcpy(sup_filename, "                                                                                ");
                strcpy(sup_filename, "/export/cdbsrv/cchen/Desktop/bigsharp9-original/sup.txt");
		return(1);
          	}

	fgets(sars_filename, sizeof(bufr), fp);
        fgets(sup_filename, sizeof(bufr), fp);
}
