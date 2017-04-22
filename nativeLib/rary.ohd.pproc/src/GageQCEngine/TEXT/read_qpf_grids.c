#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

int read_qpf_grids(int k,char *fname)

{
extern int pcp_in_use[500];
FILE *fp;
extern struct hrap_grid *hrap_grid;
int i,j,h;
char kbuf[10000];
char message [ GAGEQC_MESSAGE_LEN ];
extern struct pcp *pcp;
int minhrapi,minhrapj,maxhrapi,maxhrapj;
int ghrapi,ghrapj;
int ii,jj;
int ier;
char *p;
int gmini,gminj,gmaxi,gmaxj;
int iflag;

fp=fopen(fname,"r");


if(fp==NULL) {

            logMessage("could not open %s\n",fname);
             	bzero(message,150);
	sprintf(message,"Could not open file: %s\n",fname);
	logMessage(message);

             return(-1);            

	   }
else
{
	             	bzero(message,150);
	sprintf(message,"Opened file: %s\n",fname);
	logMessage(message);

}
for(i=0;i<hrap_grid->maxi;i++) {

for(j=0;j<hrap_grid->maxj;j++) {

          pcp->value[i][j]=0;

	}

}

minhrapi=hrap_grid->hrap_minx;
minhrapj=hrap_grid->hrap_miny;
maxhrapi=hrap_grid->hrap_minx + hrap_grid->maxi;
maxhrapj=hrap_grid->hrap_miny + hrap_grid->maxj;

p=fgets(kbuf,100,fp);

ier=sscanf(kbuf,"%d %d %d %d %d\n",&gmini,&gminj,&gmaxi,&gmaxj,&iflag);

if(ier==4)
        iflag=0;

for(i=0;i<gmaxi;i++) {

     if(iflag==0) {
     
     fread(kbuf,sizeof(char),gmaxj*4,fp);
     kbuf[gmaxj*4]=0; 
     
     }

     else {
     
     fread(kbuf,sizeof(char),gmaxj*6,fp);
     kbuf[gmaxj*6]=0; 

    }

     /* get hrap coord of gridded data */

     ghrapi=gmini+i;

     ii=ghrapi-minhrapi;

     if(ghrapi >= minhrapi && ghrapi < maxhrapi) {  
 
     h=0;

     for(j=0;j<gmaxj;j++) {

            ghrapj=gminj+j;

            jj=ghrapj-minhrapj;

            if(ghrapj >= minhrapj && ghrapj < maxhrapj)  {
            
                         pcp->value[ii][jj]=atoi(&kbuf[h]);      

                }

            if(iflag==0)
                 h=h+4;

            else
                 h=h+6;
                 
	  }

   }

   }

fclose(fp);

/*copy to internal file */

write_file("pcp",k,pcp);

pcp_in_use[k]=1;

return(1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
     
