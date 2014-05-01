#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

extern struct maxmin *maxmin;

void get_maxmin_coord(char *fname,int smonth,int emonth,int type)

{
 extern char *mon[];
 extern int maxmin_used;
 extern int init_maxmin;
 int i,j,ier,maxj,maxi,k,kk;
 float lat,lon;
 FILE *fp;
 char message [ GAGEQC_MESSAGE_LEN ];
 char *p,kbuf[20000],fbuf[100];
 float max_lat,max_lon,delta_lat,delta_lon,total_lat,total_lon;

 strcpy(fbuf,fname);
 strcat(fbuf,"jan");

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

 ier=sscanf(kbuf,"%f %f %f %f %f %f",&max_lat,&max_lon,&total_lat,
                                     &total_lon,&delta_lat,&delta_lon);

 fclose(fp);

 delta_lat=delta_lat/60.;
 delta_lon=delta_lon/60.;

  if(init_maxmin==-1) {
 
 maxmin=(struct maxmin *) calloc(1,sizeof(struct maxmin));
 if(maxmin==NULL) {

                   printf("could not allocate maxmin space\n");
                   exit(1);

                  }          
}


 maxi=(total_lon/delta_lon+.001);
 maxj=(total_lat/delta_lat+.001);

 maxmin->maxi=maxi;
 maxmin->maxj=maxj;
 maxmin->max_lat=max_lat;
 maxmin->max_lon=max_lon;
 maxmin->total_lat=total_lat;
 maxmin->total_lon=total_lon;
 maxmin->delta_lat=delta_lat;
 maxmin->delta_lon=delta_lon;

 maxmin->coord=(struct icoord**) calloc(maxi,sizeof(struct icoord *));

 maxmin->maxvalue=(short int***) calloc(12,sizeof(short int **));
 maxmin->minvalue=(short int***) calloc(12,sizeof(short int **));

 if(maxmin->coord==NULL || maxmin->maxvalue==NULL || maxmin->maxvalue==NULL) {

                            printf("no memory for maxmin array\n");
                            exit(1);

			  }

 for(k=0;k<12;k++) {

        ier=is_good(k,smonth,emonth);

        if(ier == -1)
                   continue;

        maxmin->maxvalue[k]=(short int **)calloc(maxi,sizeof(short int *));
        maxmin->minvalue[k]=(short int **)calloc(maxi,sizeof(short int *));

        if(maxmin->maxvalue[k]==NULL || maxmin->minvalue[k]==NULL) {

                printf("no memory for maxmin array\n");
                exit(1);

	      }

      }

 for(i=0;i<maxi;i++) {

      maxmin->coord[i]=(struct icoord *) calloc(maxj,sizeof(struct icoord));

      if(maxmin->coord[i]==NULL) {

                printf("no memory for maxmin array\n");
                exit(1);

	      }

      for(k=0;k<12;k++) {

        ier=is_good(k,smonth,emonth);

        if(ier==-1)
                continue;

        maxmin->maxvalue[k][i]=(short int *) calloc(maxj,sizeof(short int));
        maxmin->minvalue[k][i]=(short int *) calloc(maxj,sizeof(short int));

        if(maxmin->maxvalue[k][i]==NULL || maxmin->minvalue[k][i]==NULL) {

                printf("no memory for maxmin array\n");
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

         get_maxmin_pixels(i,j,lat,lon);

    }

}


maxi=maxmin->maxi;
maxj=maxmin->maxj;

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
  
         if(type==0)
         maxmin->maxvalue[k][i][j]=atoi(&kbuf[kk]);
        
         if(type==1)
         maxmin->minvalue[k][i][j]=atoi(&kbuf[kk]);
        
         kk=kk+5;

    }

}

fclose(fp);

init_maxmin=1;

}

maxmin_used=1;
return;

}
void get_maxmin_pixels(int i, int j, float lat,float lon)

  {

     extern struct dval dval;
     long r,s;
     float conv=.0174;
     extern struct maxmin *maxmin;
     
     r=dval.a * cos(lat*conv)/(1+sin(lat*conv))
       * cos((lon-dval.lo-90)*conv) + dval.xo +.5;

     maxmin->coord[i][j].x=r;

     s=dval.a * cos(lat*conv)/(1+sin(lat*conv))
       * sin((lon-dval.lo-90)*conv) + dval.yo + .5;
       
     maxmin->coord[i][j].y=s;

     return;

   }



















