#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

void write_qpf_grids(char *fname)

{
char message [ GAGEQC_MESSAGE_LEN ];
FILE *fp = NULL;
extern struct hrap_grid *hrap_grid;
extern struct pcp *pcp;
int i,j;
int iflag;

fp=fopen(fname,"w");

if(fp==NULL)
{

            logMessage("could not open %s\n",fname);
	     	memset(message,'\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n",fname);
	logMessage(message);
        return;
}
else
{
		     	memset(message,'\0',150);
	sprintf(message,"Opened file: %s\n",fname);
	logMessage(message);

}

chmod(fname,S_IRUSR | S_IWUSR | 
            S_IRGRP | S_IWGRP |
            S_IROTH | S_IWOTH);

iflag=1;

fprintf(fp,"%d %d %d %d %d\n",
    hrap_grid->hrap_minx,hrap_grid->hrap_miny,hrap_grid->maxi,hrap_grid->maxj,
    iflag);

for(i=0;i<hrap_grid->maxi;i++) {
 
for(j=0;j<hrap_grid->maxj;j++) {

               fprintf(fp," %5d",pcp->value[i][j]);

	  }
}

fclose(fp);

return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
     
