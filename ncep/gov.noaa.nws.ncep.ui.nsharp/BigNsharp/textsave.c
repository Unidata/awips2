#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "globals.h"
#include "profile.h"
#include "textsave.h"

int save_text(char *filename)
{
	FILE *fp;
	char *txt=NULL;

	fp = fopen(filename, "w");
	if (!fp) {
	  return 1;
	}

	txt = createtextsounding();

	if (txt && *txt) {
	  fprintf(fp, "%s", txt);
	}

	free(txt);

	(void)fclose(fp);

	return 0;
}

char *createtextsounding(void)
{
	int n, m, pos;
	char bufr[MAXLEV*256], *ptr=NULL, fmt[16];

	memset(bufr, 0, sizeof bufr);

	if (numlvl < 1 || !globalsndg)
	  return ptr;

	for (n=0; n<globalsndg->nparms; n++) {
	  m = (n == 0) ? 9 : 10;
	  sprintf(fmt, "%%%ds", m);

	  m = (n == 0) ? 0 : (n * 10) - 1;
	  sprintf(bufr + m, fmt, globalsndg->parms[n]);
	}
	strcat(bufr, "\n");

	pos = strlen(bufr);

	/* Fill in the text area with the sounding */
	for (n=0; n<numlvl; n++) {
	  /* generate our line */
	  for (m=0; m<globalsndg->nparms; m++) {
	    sprintf(bufr+pos,"%9.2f", sndg[n][m]);
	    pos += 9;
	    *(bufr + pos) = ' ';
	    pos++;

	    /* Try to avoid overflows */
	    if (strlen(bufr) >= ((MAXLEV*256)-1)) break;
	  }
	  *(bufr + pos) = '\n';
	  pos++;
	}

	ptr = strdup(bufr);
	return ptr;
}
