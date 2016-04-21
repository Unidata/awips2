
/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

void read_mean_montly_precip ( char *fname,
                               int smonth,
                               int emonth)
{

/*---------------------------------------*/
/*  routine to read isohyet data         */
/* called from routine read_maps         */
/*---------------------------------------*/

 extern char *mon[];
 extern int isohyets_used;
 char message [ GAGEQC_MESSAGE_LEN ];
 int i,j,ier,maxj,maxi,max_value,k,kk;
 float lat,lon;
 extern struct isoh *isoh;
 FILE *fp;
 char *p,kbuf[20000],fbuf[100];
 float max_lat,max_lon,delta_lat,delta_lon,total_lat,total_lon;
 
 max_value=0;

 isoh=(struct isoh *) calloc(1,sizeof(struct isoh));
 if(isoh==NULL) {

                   printf("could not allocate isoh space\n");
                   exit(1);

                  }          

 strcpy(fbuf,fname);
 strcat(fbuf,"jan");

 fp=fopen(fbuf,"r");

 if(fp==NULL) {

              printf("could not open %s\n",fname);
	      	memset(message,'\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n",fbuf);
	logMessage(message);

              exit(1);

              }
else
{
		      	memset(message,'\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Opened file: %s\n",fbuf);
	logMessage(message);


}

 p=fgets(kbuf,80,fp);

 ier=sscanf(kbuf,"%f %f %f %f %f %f",&max_lat,&max_lon,&total_lat,
                                     &total_lon,&delta_lat,&delta_lon);

 fclose(fp);

 delta_lat=delta_lat/60.;
 delta_lon=delta_lon/60.;

 maxi=(total_lon/delta_lon+.001);
 maxj=(total_lat/delta_lat+.001);

printf("maxi %d maxj %d\n",maxi,maxj);
 isoh->maxi=maxi;
 isoh->maxj=maxj;
 isoh->max_lat=max_lat;
 isoh->max_lon=max_lon;
 isoh->total_lat=total_lat;
 isoh->total_lon=total_lon;
 isoh->delta_lat=delta_lat;
 isoh->delta_lon=delta_lon;

 isoh->coord=(struct icoord**) calloc(maxi,sizeof(struct icoord *));

 isoh->value=(short int***) calloc(12,sizeof(short int **));

 if(isoh->coord==NULL || isoh->value==NULL) {

                            printf("no memory for isoh array\n");
                            exit(1);

			  }


 for(k=0;k<12;k++) {

        ier=is_good(k,smonth,emonth);

        if(ier == -1)
                   continue;

        isoh->value[k]=(short int **)calloc(maxi,sizeof(short int *));

        if(isoh->value[k]==NULL) {

                printf("no memory for isoh array\n");
                exit(1);

	      }

      }

 for(i=0;i<maxi;i++) {

      isoh->coord[i]=(struct icoord *) calloc(maxj,sizeof(struct icoord));

      if(isoh->coord[i]==NULL) {

                printf("no memory for isoh array\n");
                exit(1);

	      }

      for(k=0;k<12;k++) {

        ier=is_good(k,smonth,emonth);

        if(ier==-1)
                continue;

        isoh->value[k][i]=(short int *) calloc(maxj,sizeof(short int));

        if(isoh->value[k][i]==NULL) {

                printf("no memory for isoh array\n");
                exit(1);

	      }

    }

    }

/*create screen coordinates */

/* j increments latitude  i increments longitude */

 for(j=0;j<maxj;j++) {

 for(i=0;i<maxi;i++) {
  
         lat=max_lat-(float)j*delta_lat;
         lon=max_lon-(float)i*delta_lon;

         get_isoh_pixels(i,j,lat,lon);

    }

}

for(k=0;k<12;k++) {

 ier=is_good(k,smonth,emonth);

 if(ier==-1)
         continue;

 sprintf(fbuf,"%s%s",fname,mon[k]);

 fp=fopen(fbuf,"r");

 if(fp==NULL) {

              printf("could not open %s\n",fbuf);
	      	memset(message,'\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n",fbuf);
	logMessage(message);

              exit(1);

              }
else
{
		      	memset(message,'\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Opened file: %s\n",fbuf);
	logMessage(message);


}
 p=fgets(kbuf,80,fp);


/* read in data file */
/* i is longitude j is latitude */

 for(j=0;j<maxj;j++) {

 fread(kbuf,sizeof(char),maxi*5,fp);
 kbuf[maxi*5]=0; 
 kk=0;

/* read in longitude records */

 for(i=0;i<maxi;i++) {
  
         isoh->value[k][i][j]=atoi(&kbuf[kk]);
        
         kk=kk+5;

    }

}

fclose(fp);

}

isohyets_used=1;
return;

}
void get_isoh_pixels(int i, int j, float lat,float lon)

{

     extern struct dval dval;
     long r,s;
     float conv=.0174;
     extern struct isoh *isoh;


     
     r=dval.a * cos(lat*conv)/(1+sin(lat*conv))
       * cos((lon-dval.lo-90)*conv) + dval.xo +.5;

     isoh->coord[i][j].x=r;

     s=dval.a * cos(lat*conv)/(1+sin(lat*conv))
       * sin((lon-dval.lo-90)*conv) + dval.yo + .5;
       
     isoh->coord[i][j].y=s;

     return;

   }



















