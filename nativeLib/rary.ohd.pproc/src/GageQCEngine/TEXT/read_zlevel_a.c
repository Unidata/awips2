#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

char message[LOG_MESSAGE_LEN];

int read_zlevel_a(char *fname,time_t tget,int i,
                  struct zdata zdata[], struct station * zstation,
                  int max_zstations )
   
{
 FILE *fr = NULL;
 int j,k,ier,m,qual;
 char kbuf[100],*p,*q,*r,buf[100],hb5[10];
 int number_found[5];
 char pc,datbuf[50],parmbuf[50];
 int maxk,startk;
 struct tm *gm;

 fr=fopen(fname,"r");
 if(fr==NULL)
{
	bzero(message,150);
	sprintf(message,"Could not open file: %s\n",fname);
	logMessage(message);
}
else
{
	bzero(message,150);
	sprintf(message,"Opened file: %s\n",fname);
	logMessage(message);
	
}
 zdata[i].data_time=tget;

 gm=gmtime(&tget);
 
 zdata[i].ztime=(gm->tm_year+1900)*10000+(gm->tm_mon+1)*100+gm->tm_mday;
 
 for(j=0;j<5;j++)
       number_found[j]=0;

 for(k=0;k<max_zstations;k++) {
 
                  for(m=0;m<5;m++) {

                               zdata[i].stn[k].zlevel1[m].data=-1;
                               zdata[i].stn[k].zlevel1[m].qual=-1;

			     }

		}

if(fr==NULL) {

              for(k=0;k<5;k++) {
	      
                        zdata[i].used[k]=0;
                        zdata[i].level[k]=0;
			
			}
            
              return(-1);

	    }

 for(k=0;k<5;k++) {
 
       zdata[i].used[k]=1;
       zdata[i].level[k]=1;

        }

/* initialize structure */

 for(;;) {

         bad:
         p=fgets(kbuf,100,fr);
         if(p==NULL)
                   break;

         if(kbuf[0]==':')
                        continue;

         ier=sscanf(&kbuf[2],"%s %s %s",hb5,datbuf,parmbuf);
         if(ier==0)
                         continue;

         p=strchr(parmbuf,'/');
         if(p==NULL)
                 continue;

         pc=*(p+5);

         for(j=0;j<max_zstations;j++) {

                  if(strcmp(hb5,zstation[j].hb5)==0 && 
                     pc==zstation[j].parm[4]) {  

                             break;

			   }

		}

         if(j==max_zstations) {

                      continue;

		    }

         q=kbuf;
         for(k=0;k<3;k++) {

                   p=strchr(q,'/');
                   if(p==NULL)
                             break;

                   q=p+1;
                  
		   }

         if(k!=3)
                          continue;  /* missing */

         maxk=4;
         startk=0;

         for(k=startk;k<maxk;k++) {

                   zdata[i].stn[j].zlevel1[k].qual=0;

                   if((p=strchr(q,'/'))==NULL &&
                      (p=strchr(q,'\n'))==NULL)
                                          goto bad;

                   *p=0;

                   strcpy(buf,q);

                   if((p=strchr(buf,'.'))==NULL) {

                                      zdata[i].stn[j].zlevel1[k].data=-1;
                                      zdata[i].stn[j].zlevel1[k].qual=-1;

			                  }

                   else  {

                        number_found[k]++;

                        zdata[i].stn[j].zlevel1[k].data=atof(buf);
                        
                        r=p+2;
                        qual=8;
                        while(*r != 0) {

                          if(*r != ' '){

                             if(*r=='S')
                                qual=8;

                             zdata[i].stn[j].zlevel1[k].qual=qual;

                             break;

			  }

                                 else 
                                        r++;

		                   }


		 }

          p=strchr(q,0);
          q=p+1;      
         
		 }

       }


fclose(fr);

for(j=0;j<4;j++) {

     if(number_found[j] == 0)
              zdata[i].used[j]=0;

   }


return(1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}























