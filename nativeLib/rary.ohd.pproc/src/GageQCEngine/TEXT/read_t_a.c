#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

int read_t_a(char *fname,time_t tget,int i,
             struct station * tstation,
	     int max_tstations)
{
 extern struct tdata tdata[];
 //extern struct station * tstation;
 //extern int max_tstations;
 int type=0;
 FILE *fr;
 char temp[250];
 int count,length;
 int cnt,pos;
 int j,k,ier,m,qual;
 char kbuf[100],*p = NULL,*q = NULL,*r = NULL ,buf[100],hb5[10];
 char message [ GAGEQC_MESSAGE_LEN ];
 int number_found[6];
 char pc,datbuf[50],parmbuf[50];
 int maxk,startk;
 struct tm *gm;
 int kread;
 int offset;

 fr=fopen(fname,"r");
 if(fr==NULL)
{
	memset ( message, '\0', GAGEQC_MESSAGE_LEN );
	sprintf(message,"Could not open file: %s\n",fname);
	logMessage(message);
}
else
{
        memset ( message, '\0', GAGEQC_MESSAGE_LEN );
	sprintf(message,"Opened file: %s\n",fname);
	logMessage(message);

}
 
 tdata[i].data_time=tget;

 gm=gmtime(&tget);
 
 tdata[i].ztime=(gm->tm_year+1900)*10000+(gm->tm_mon+1)*100+gm->tm_mday;
 
 for(j=0;j<6;j++)
       number_found[j]=0;

 for(k=0;k<max_tstations;k++) {
 
                  for(m=0;m<6;m++) {

                               tdata[i].stn[k].tlevel1[m].data=-99;
                               tdata[i].stn[k].tlevel1[m].qual=-99;

			     }

		}

if(fr==NULL) {
              for(k=0;k<6;k++) {
	      
                        tdata[i].used[k]=0;
                        tdata[i].level[k]=0;
		}
            
              return(-1);

	    }

 for(k=0;k<6;k++) {
 
       tdata[i].used[k]=1;
       tdata[i].level[k]=1;

        }

/* initialize structure */

 for(;;) {


         bad:
         p=fgets(kbuf,100,fr);






	 
         if(p==NULL)
                   break;

         if(kbuf[0]==':')
                        continue;

         
	 if(kbuf[0]=='.' && kbuf[1]=='E')
	 {
	    type=1;
	 }
	 if(kbuf[0]=='.' && kbuf[1]=='A')
	 {
	    type=0;
	 }
	 

if(type == 0)
     {
	 cnt = 0;
	 pos = 0;
	 count = 0;
	 length = 0;

	 memset(temp, '\0', 250);
	 if(kbuf != NULL)
	 {
	    strcpy(temp,kbuf);
	    length = strlen(temp)+1;
	       for(count=0;count<length;count++)
	       {
	          if(temp[count] == '/')
		  {
		     cnt++;
		     if(cnt == 2)
		     {
		       pos = count;
		       break;
		     }
		  }
	       }
	       if(cnt == 2)
	       {
	          for(;pos<length-1;count++)
	          {
	            kbuf[count] = temp[count+1];
	          }
	       }
	 }
      }

	 ier=sscanf(&kbuf[2],"%s %s %s",hb5,datbuf,parmbuf);
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
                   kread=3;	 
	           startk=0;
		   maxk=4;
	 
	 
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

                   tdata[i].stn[j].tlevel1[k].qual=0;

                   if((p=strchr(q,'/'))==NULL &&
                      (p=strchr(q,'\n'))==NULL)
                                          goto bad;

                   *p=0;

                   strcpy(buf,q+offset);
		   
                   if((p=strchr(buf,'m'))!=NULL ||
                      (p=strchr(buf,'M'))!=NULL) {
                                 
                                      tdata[i].stn[j].tlevel1[k].data=-99;
                                      tdata[i].stn[j].tlevel1[k].qual=-99;

					     }
					     
	           else {
		     
		   p=buf;

                   number_found[k]++;

                 
                   tdata[i].stn[j].tlevel1[k].data=atoi(buf);			
            
                   r=p;
                   qual=8;
		   
                   while(*r != 0) {

			    if(isalpha(*r) != 0) {

                             if(*r=='S' || *r=='A')
                                qual=8;

                             else if(*r=='F')
                                qual=1;

                             else if(*r=='V')
                                qual=8;

                             else if(*r=='E')
                                qual=5;
				
			     tdata[i].stn[j].tlevel1[k].qual=qual;	



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

for(j=0;j<6;j++) {

     if(number_found[j] == 0)
              tdata[i].used[j]=0;

   }


return(1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
