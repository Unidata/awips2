#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"


void read_bad_tvalues(char *fname,int m)

{
extern struct bad_daily_values bad_tvalues[6000];
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
	memset(message, '\0', GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n",bfile);
	logMessage(message);
        return;
}
else
{
	memset(message, '\0',GAGEQC_MESSAGE_LEN);
	sprintf(message,"Opened file: %s\n",bfile);
	logMessage(message);
}
for(i=0;i<6000;i++) {

       if(bad_tvalues[i].used==1)
                       continue;

        p=fgets(ibuf,80,fp);

        if(p==NULL)
                  break;

        ier=sscanf(ibuf,"%s %s %d %f",hb5,pc,&iquart,&fvalue);

        bad_tvalues[i].used=1;
        strcpy(bad_tvalues[i].hb5,hb5);
        strcpy(bad_tvalues[i].parm,pc);
        bad_tvalues[i].day=m;
        bad_tvalues[i].quart=iquart;
        bad_tvalues[i].fvalue=fvalue;

      }

fclose(fp);

return;

}

void purge_bad_tvalues(int iday)

{

extern struct bad_daily_values bad_tvalues[6000];
int i,k;

for(k=0;k<6;k++) {

for(i=0;i<6000;i++) {

        if(bad_tvalues[i].used==1 && 
           bad_tvalues[i].day==iday &&
           bad_tvalues[i].quart==k)
               bad_tvalues[i].used=0;

      }

}

return;

}


int get_bad_tvalues(int iday,int iquart)
        
{

extern struct bad_daily_values bad_tvalues[6000];
extern struct station * tstation;
extern struct tdata tdata[10];
extern int max_tstations;
int i,j,h;

for(i=0;i<6000;i++) {

      if(bad_tvalues[i].used==0)
                       continue;

      if(bad_tvalues[i].day != iday ||
         bad_tvalues[i].quart != iquart)
                          continue;

        for(j=0;j<max_tstations;j++) {

            if(strcmp(bad_tvalues[i].hb5,tstation[j].hb5)==0 && 
               bad_tvalues[i].parm[4]==tstation[j].parm[4]) {

               if(tdata[iday].stn[j].tlevel2[iquart].qual == 5 &&
                  bad_tvalues[i].fvalue != tdata[iday].stn[j].tlevel1[iquart].data &&
                  tdata[iday].stn[j].tlevel1[iquart].data >= 0) {

                   logMessage("for station %s %f %d\n",tstation[j].hb5,bad_tvalues[i].fvalue,
                     tdata[iday].stn[j].tlevel1[iquart].data);              

                  /* eliminate all bad values for current month */                                  
                   for(h=0;h<6000;h++) {
 
                      if(bad_tvalues[i].used==0)
                          continue;

                      if(bad_tvalues[i].day != iday ||
                          bad_tvalues[i].quart != iquart)
                          continue;

                      bad_tvalues[h].used=0;

		    }

                     /* swap in level 1 data */

                     return(1);
          
		     }

                     else {

                         tdata[iday].stn[j].tlevel2[iquart].qual=1;
                         tdata[iday].stn[j].tlevel2[iquart].data=bad_tvalues[i].fvalue;

		       }

		   }

		}

    }

return(0); 
    
}


void write_bad_tvalues(char *fname,int iday)

{

extern struct bad_daily_values bad_tvalues[6000];
FILE *fp;
char bfile[100],ibuf[100];
char message[GAGEQC_MESSAGE_LEN];
int i,ier;

strcpy(bfile,fname);

fp=fopen(bfile,"w");

if(fp != NULL)
{
	memset(message, '\0', GAGEQC_MESSAGE_LEN);
	sprintf(message,"Opened file: %s\n",bfile);
	logMessage(message);

for(i=0;i<6000;i++) {
         
        if(bad_tvalues[i].used==0)
                        continue;

        if(iday==bad_tvalues[i].day) {

        ier=sprintf(ibuf,"%s %s %d %f\n",bad_tvalues[i].hb5,bad_tvalues[i].parm,
                    bad_tvalues[i].quart,bad_tvalues[i].fvalue);

        fputs(ibuf,fp);

      }

      }

fclose(fp);
}
else
{
	memset(message, '\0', GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n",bfile);
	logMessage(message);
}
return;

}

 

void update_bad_tvalues(int iday)

{

extern struct station * tstation;
extern int max_tstations;
extern struct bad_daily_values bad_tvalues[6000];
extern struct tdata tdata[10];
int i,j,h,k;
char parm[10];

for(i=0;i<6000;i++) {

      if(bad_tvalues[i].used==0)
                          continue;

      if(bad_tvalues[i].day != iday)
                          continue;

      bad_tvalues[i].used=0;

   }

for(j=0;j<max_tstations;j++) {

        for(k=0;k<6;k++) {

             if(tdata[iday].stn[j].tlevel2[k].qual != 1)
                                          continue;

             for(h=0;h<6000;h++) {

                      if(bad_tvalues[h].used!=0)
                                     continue;

                      bad_tvalues[h].used=1;
                                          
                      bad_tvalues[h].fvalue=tdata[iday].stn[j].tlevel1[k].data;
                      
                      strcpy(bad_tvalues[h].hb5,tstation[j].hb5);
		      
		      strcpy(parm,tstation[j].parm);
		      
		      if(k<4) 		      
		         parm[5]='Z';
			 
		      else if(k==4) 		      
		         parm[5]='X';
		        
		      if(k==6) 		      
		         parm[5]='N'; 
			 
                      strcpy(bad_tvalues[h].parm,parm);
                      bad_tvalues[h].day=iday;
                      bad_tvalues[h].quart=k;
     
                      break;

		    }
	   }

      }

return;

}

        

void restore_bad_tvalues(int iday,
		         struct station * tstation,
	                 int max_tstations)

{

extern struct tdata tdata[10];
extern struct bad_daily_values bad_tvalues[6000];
int i,j,k;

for(k=0;k<6;k++) {

for(i=0;i<6000;i++) {

      if(bad_tvalues[i].used==0)
                          continue;

      if(bad_tvalues[i].day != iday ||
         bad_tvalues[i].quart != k)
                          continue;

        for(j=0;j<max_tstations;j++) {

            if(strcmp(bad_tvalues[i].hb5,tstation[j].hb5)==0 && 
               bad_tvalues[i].parm[4]==tstation[j].parm[4]) {

                      tdata[iday].stn[j].tlevel2[k].data=bad_tvalues[i].fvalue;
                      tdata[iday].stn[j].tlevel2[k].qual=1;

                      break;

                           }

		    }
      }


}

return;

}
         

int is_tbad(int iday,int iquart,char *hb5,char *parm)

{

extern int max_tstations;
extern struct bad_daily_values bad_tvalues[6000];
int i,j;

for(i=0;i<6000;i++) {

      if(bad_tvalues[i].used==0)
                          continue;

      if(bad_tvalues[i].day != iday ||
         bad_tvalues[i].quart != iquart)
                          continue;
	
        for(j=0;j<max_tstations;j++) {

            if(strcmp(bad_tvalues[i].hb5,hb5)==0 && 
               bad_tvalues[i].parm[4]==parm[4]) 
                       return(1);
 
 		    }
      }

return(0);

}

        

void post_bad_tvalues(int iday)

{
extern struct station * tstation;
extern struct tdata tdata[10];
extern int max_tstations;
extern struct bad_daily_values bad_daily_values[6000];
extern struct bad_daily_values bad_tvalues[6000];

int i,j,k;

for(k=0;k<6;k++) {

for(i=0;i<6000;i++) {

      if(bad_tvalues[i].used==0)
                          continue;

      if(bad_tvalues[i].day != iday ||
         bad_tvalues[i].quart != k)
                          continue;

        for(j=0;j<max_tstations;j++) {

            if(strcmp(bad_tvalues[i].hb5,tstation[j].hb5)==0 && 
               bad_tvalues[i].parm[4]==tstation[j].parm[4]) {

                  if(tdata[iday].stn[j].tlevel2[k].data==bad_tvalues[i].fvalue){

                          tdata[iday].stn[j].tlevel2[k].data=
                                    bad_tvalues[i].fvalue;
        
                          tdata[iday].stn[j].tlevel2[k].qual=1;
        
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
  
              








