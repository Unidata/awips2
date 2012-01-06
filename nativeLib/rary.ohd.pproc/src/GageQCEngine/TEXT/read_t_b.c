#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

int read_t_b ( char *fname, time_t tget, int i )
{
 extern struct tdata tdata[10];
 extern struct station *tstation;
 extern int max_tstations;
 FILE *fr = NULL;
 int j,k,ier,m,qual;
 char kbuf[100],*p,*q,*r,buf[100],hb5[10];
 char message [ GAGEQC_MESSAGE_LEN ];
 int number_found[6];
 char pc,datbuf[50],parmbuf[50];
 int maxk,startk;
 struct tm *gm = NULL;
 int uflag[6];
 int kread;
 int reset_flag;
 int rier;
 int offset;
 
 reset_flag=0;
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
 tdata[i].data_time=tget;

 gm=gmtime(&tget);
 
 tdata[i].ztime=(gm->tm_year+1900)*10000+(gm->tm_mon+1)*100+gm->tm_mday;
 
 for(j=0;j<6;j++) {
 
       number_found[j]=0;
       uflag[j]=0;
       
       }

 for(k=0;k<max_tstations;k++) {
 
                  for(m=0;m<6;m++) {
		
                               tdata[i].stn[k].tlevel2[m].data=-1;
                               tdata[i].stn[k].tlevel2[m].qual=-1;

			     }
			     
			     
		}

if(fr==NULL) {

               reset:
	       
               for(k=0;k<max_tstations;k++) {
 
                  for(m=0;m<6;m++) {

                               tdata[i].stn[k].tlevel2[m].data=
                               tdata[i].stn[k].tlevel1[m].data;
                               
                               tdata[i].stn[k].tlevel2[m].qual=
                               tdata[i].stn[k].tlevel1[m].qual;
                               
                               if(tdata[i].stn[k].tlevel2[m].data>=0)
                                         number_found[m]++;
                               

			     }

		}
		
     for(m=0;m<6;m++) {
             
              if(number_found[m] == 0)
                        tdata[i].used[m]=0;
			
              tdata[i].level[m]=1;
	      
                   }
                   
     post_bad_tvalues(i);
     
     return(-1);

	    }

 for(k=0;k<6;k++) {
 
       tdata[i].used[k]=1;
       tdata[i].level[k]=2;
       
       }

/* initialize structure */

 for(;;) {

         bad:
         p=fgets(kbuf,100,fr);
         if(p==NULL)
                   break;

         if(kbuf[0]==':')
                        continue;

         ier=sscanf(&kbuf[3],"%s %s %s",hb5,datbuf,parmbuf);
         if(ier==0)
                         continue;

         p=strchr(parmbuf,'/');
         if(p==NULL)
                 continue;

         if(*(p+6)=='X') {

                   offset=7;
                   kread=1;	 	       
                   startk=4;
		   maxk=5;
		   
		   }
		   
	 else if(*(p+6)=='N') {
	 
	           offset=7;
	           kread=1;
	           startk=5;
		   maxk=6;
		   
		   }
		   
         else      {
	 
	           offset=0;
	           kread=5;
	           startk=0;
		   maxk=6;
	 
	 
	 }		   
		   
		   
         pc=*(p+5);

         for(j=0;j<max_tstations;j++) {

                  if(strcmp(hb5,tstation[j].hb5)==0 && 
                     pc==tstation[j].parm[4]) {  

                             break;

			   }

		}

         if(j==max_tstations) {

                      continue;

		    }

         q=kbuf;
         for(k=0;k<kread;k++) {

                   p=strchr(q,'/');
                   if(p==NULL)
                             break;

                   q=p+1;
                  
		   }

         if(k!=kread)
                          continue;  /* missing */

         for(k=startk;k<maxk;k++) {

                   tdata[i].stn[j].tlevel2[k].qual=0;

                   if((p=strchr(q,'/'))==NULL &&
                      (p=strchr(q,'\n'))==NULL)
                                          goto bad;
					  
		   *p=0;

                   strcpy(buf,q+offset);
		   			  

                   if((p=strchr(buf,'m'))!=NULL ||
                      (p=strchr(buf,'M'))!=NULL ||
		       (buf[0]==' ' && buf[1]==' ' &&
			buf[2]==' ' && buf[3]==' ')) {
                                 
			              if(tdata[i].stn[j].tlevel1[k].data > -1)
					      reset_flag=1;
			   
                                      tdata[i].stn[j].tlevel2[k].data=-1;
                                      tdata[i].stn[j].tlevel2[k].qual=-1;

					     }
                   else {

		   p=buf;

                   number_found[k]++;
			
                  
                   tdata[i].stn[j].tlevel2[k].data=atoi(buf);
			
                   r=p;
                   qual=8;
		   
	           while(*r != 0) {

			    if(isalpha(*r) != 0) {

                             if(*r=='S' || *r=='A')
                                qual=0;

                             else if(*r=='F')
                                qual=1;

                             else if(*r=='W')
                                qual=2;

                             else if(*r=='Q')
                                qual=3;

                             else if(*r=='V')
                                qual=8;

                             else if(*r=='E')
                                qual=5;

                             else if(*r=='L')
                                qual=6;
                              
                             tdata[i].stn[j].tlevel2[k].qual=qual;
                            
			     if((qual==5) && 
                                tdata[i].stn[j].tlevel1[k].data >= 0) {
                         
                                 ier=is_tbad(i,k,tstation[j].hb5,
                                            tstation[j].parm);

                                 if(ier==0) {

                                    logMessage("new data overwriting missing %s %s %d\n",
                                             tstation[j].hb5,tstation[j].parm,tdata[i].stn[j].tlevel1[k].data);

                                           rier=-2;
                                           reset_flag=1;

                                             }

					 }

                                 if((qual!=5 && qual != 2)
                                    && tdata[i].stn[j].tlevel1[k].data >= 0
                                    && tdata[i].stn[j].tlevel1[k].data !=
                                    tdata[i].stn[j].tlevel2[k].data) {

                                    logMessage("new data overwriting old %s %s %d %d\n",
                                             tstation[j].hb5,tstation[j].parm,
					     tdata[i].stn[j].tlevel1[k].data,tdata[i].stn[j].tlevel2[k].data);
                                     
                                           rier=-2;
                                           reset_flag=1;

					 }

                                 if(qual!=5 && qual != 6 && qual != 2
                                    && tdata[i].stn[j].tlevel1[k].data < -98
                                    && tdata[i].stn[j].tlevel2[k].data >=0) {
                                     
                                    logMessage("data set bad level 1 overwriting level 2 %s %s %d\n",
                                             tstation[j].hb5,tstation[j].parm,tdata[i].stn[j].tlevel1[k].data);
                                       
                                           rier=-2;
                                           reset_flag=1;

				 }
					 
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

if(reset_flag==1)
            goto reset;
  

fclose(fr);

for(m=0;m<6;m++) {

              if(uflag[m]==1) {

                   number_found[m]=0;
		   
                   for(k=0;k<max_tstations;k++) {
 
                               tdata[i].stn[k].tlevel2[m].data=
                               tdata[i].stn[k].tlevel1[m].data;
                               
                               tdata[i].stn[k].tlevel2[m].qual=
                               tdata[i].stn[k].tlevel1[m].qual;
                               
                               if(tdata[i].stn[k].tlevel2[m].data>-1)
                                         number_found[m]++;
                               

			     }
			     	
                 tdata[i].level[m]=1;
		

 }
              

}

for(j=0;j<6;j++) {

     if(number_found[j] == 0) {
     
              tdata[i].used[j]=0;
              tdata[i].level[j]=1;

              }
	
	ier=get_bad_tvalues(i,m);
        if(ier==1) {

         logMessage("new data overwriting old bad data\n");
          goto reset;

	}
	
   }

return(1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}























