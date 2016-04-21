#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

void read_bad_values(char *fname,int m)

{
extern struct bad_daily_values bad_values[6000];
FILE *fp;
char bfile[100],hb5[10],pc[10],ibuf[100];
char message [ GAGEQC_MESSAGE_LEN ];
float fvalue;
int i,ier,iquart;
char *p;

strcpy(bfile,fname);

fp=fopen(bfile,"r");

if(fp==NULL)
{
	memset (message, '\0', GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n", bfile);
	logMessage(message);
        return;
}
else
{
memset ( message, '\0', GAGEQC_MESSAGE_LEN );
	sprintf(message,"Opened file: %s\n", bfile);
	logMessage(message);
}
for(i=0;i<6000;i++) {

       if(bad_values[i].used==1)
                       continue;

        p=fgets(ibuf,80,fp);

        if(p==NULL)
                  break;

        ier=sscanf(ibuf,"%s %s %d %f",hb5,pc,&iquart,&fvalue);

        bad_values[i].used=1;
        strcpy(bad_values[i].hb5,hb5);
        strcpy(bad_values[i].parm,pc);
        bad_values[i].day=m;
        bad_values[i].quart=iquart;
        bad_values[i].fvalue=fvalue;

      }

fclose(fp);

return;

}

void purge_bad_values(int iday)

{

extern struct bad_daily_values bad_values[6000];
int i,k;

for(k=0;k<5;k++) {

for(i=0;i<6000;i++) {

        if(bad_values[i].used==1 && 
           bad_values[i].day==iday &&
           bad_values[i].quart==k)
               bad_values[i].used=0;

      }

}

return;

}

extern struct bad_daily_values bad_values[6000];
extern struct pdata pdata[10];
extern struct station * station;
extern int max_stations;

int get_bad_values(int iday,int iquart)
        
{

int i,j,h;

for(i=0;i<6000;i++) {

      if(bad_values[i].used==0)
                       continue;

      if(bad_values[i].day != iday ||
         bad_values[i].quart != iquart)
                          continue;

        for(j=0;j<max_stations;j++) {

            if(strcmp(bad_values[i].hb5,station[j].hb5)==0 && 
               bad_values[i].parm[4]==station[j].parm[4]) {

               if(pdata[iday].stn[j].frain[iquart].qual == 5 &&
                  bad_values[i].fvalue != pdata[iday].stn[j].rain[iquart].data &&
                  pdata[iday].stn[j].rain[iquart].data >= 0) {

                  /* eliminate all bad values for current month */                                  
                   for(h=0;h<6000;h++) {
 
                      if(bad_values[i].used==0)
                          continue;

                      if(bad_values[i].day != iday ||
                          bad_values[i].quart != iquart)
                          continue;

                      bad_values[h].used=0;

		    }

                     /* swap in level 1 data */

                     return(1);
          
		     }

                     else {

                         pdata[iday].stn[j].frain[iquart].qual=1;
                         pdata[iday].stn[j].frain[iquart].data=bad_values[i].fvalue;

		       }

		   }

		}

    }

return(0); 
    
}

extern struct bad_daily_values bad_values[6000];

void write_bad_values(char *fname,int iday)

{

FILE *fp = NULL;
char bfile[100],ibuf[100];
char message [ GAGEQC_MESSAGE_LEN ];
int i,ier;

strcpy(bfile,fname);

fp=fopen(bfile,"w");

if(fp !=NULL)
{
	memset(message, '\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Opened file: %s\n", bfile);
	logMessage(message);

for(i=0;i<6000;i++) {
         
        if(bad_values[i].used==0)
                        continue;

        if(iday==bad_values[i].day) {

        if(bad_values[i].fvalue < 0) {
	
	        logMessage("attempt to write value < 0\n");
		 continue;
		 }
		 
	       

        ier=sprintf(ibuf,"%s %s %d %f\n",bad_values[i].hb5,bad_values[i].parm,
                    bad_values[i].quart,bad_values[i].fvalue);

        fputs(ibuf,fp);

      }

      }

fclose(fp);
}
else
{
	memset(message,'\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n",bfile);
	logMessage(message);
}
return;

}

 
extern struct bad_daily_values bad_values[6000];

void update_bad_values(int iday)

{

int i,j,h,k;

for(i=0;i<6000;i++) {

      if(bad_values[i].used==0)
                          continue;

      if(bad_values[i].day != iday)
                          continue;

      bad_values[i].used=0;

   }

for(j=0;j<max_stations;j++) {

        for(k=0;k<5;k++) {

             if(pdata[iday].stn[j].frain[k].qual != 1)
                                          continue;

             for(h=0;h<6000;h++) {

                      if(bad_values[h].used!=0)
                                     continue;

                      bad_values[h].used=1;
                      /*
                      bad_values[h].fvalue=pdata[iday].stn[j].frain[k].data;
                      */
                      
                      bad_values[h].fvalue=pdata[iday].stn[j].rain[k].data;
                      
                      strcpy(bad_values[h].hb5,station[j].hb5);
                      strcpy(bad_values[h].parm,station[j].parm);
                      bad_values[h].day=iday;
                      bad_values[h].quart=k;
     
                      break;

		    }
	   }

      }

return;

}

        
extern struct bad_daily_values bad_values[6000];

void restore_bad_values(int iday,
		        struct station * station,
			int max_stations
			)

{

int i,j,k;

for(k=0;k<5;k++) {

for(i=0;i<6000;i++) {

      if(bad_values[i].used==0)
                          continue;

      if(bad_values[i].day != iday ||
         bad_values[i].quart != k)
                          continue;

        for(j=0;j<max_stations;j++) {

            if(strcmp(bad_values[i].hb5,station[j].hb5)==0 && 
               bad_values[i].parm[4]==station[j].parm[4]) {

                      pdata[iday].stn[j].frain[k].data=bad_values[i].fvalue;
                      pdata[iday].stn[j].frain[k].qual=1;
		      
		      /* fix for 6 hourly bad but 24 hour good */
		      
		      if(k >=0 && k <=3 &&
		       pdata[iday].stn[j].rain[4].data >= 0) {
		       
		        logMessage("auto set bad\n");
		       
		         pdata[iday].stn[j].frain[4].qual=1;
			 
			 }

                      break;

                           }

		    }
      }


}

return;

}
         
extern struct bad_daily_values bad_values[6000];

int is_bad(int iday,int iquart,char *hb5,char *parm)

{

int i,j;

for(i=0;i<6000;i++) {

      if(bad_values[i].used==0)
                          continue;

      if(bad_values[i].day != iday ||
         bad_values[i].quart != iquart)
                          continue;

        for(j=0;j<max_stations;j++) {

            if(strcmp(bad_values[i].hb5,hb5)==0 && 
               bad_values[i].parm[4]==parm[4]) 
                       return(1);
 
 		    }
      }

return(0);

}

        
extern struct bad_daily_values bad_daily_values[6000];

void post_bad_values(int iday)

{

int i,j,k;

for(k=0;k<5;k++) {

for(i=0;i<6000;i++) {

      if(bad_values[i].used==0)
                          continue;

      if(bad_values[i].day != iday ||
         bad_values[i].quart != k)
                          continue;

        for(j=0;j<max_stations;j++) {

            if(strcmp(bad_values[i].hb5,station[j].hb5)==0 && 
               bad_values[i].parm[4]==station[j].parm[4]) {

                  if(pdata[iday].stn[j].frain[k].data==bad_values[i].fvalue){

                          pdata[iday].stn[j].frain[k].data=
                                    bad_values[i].fvalue;
        
                          pdata[iday].stn[j].frain[k].qual=1;
        
			}

                      break;

		}

	  }
    }

}
return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
  
              








