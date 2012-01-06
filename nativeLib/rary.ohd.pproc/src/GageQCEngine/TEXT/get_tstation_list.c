#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

int is_good(int,int,int);
int max_tstations;

extern char message[150];

struct station *tstation;

void get_tstation_list(char *fname,int smonth,int emonth)

{

 extern char station_climo_file[100];
 extern char tstation_list_custom_file[100];
 extern struct dval dval;
 extern int mpe_dqc_max_temp_neighbors;

 int i,ier,sflag,field,j,xadd,yadd;
 //extern struct station tstation[1000];
 FILE *fp;
 char *p,kbuf[200],hb5[10],parm[10];
 double dist1,dist2,dist,sorted[mpe_dqc_max_temp_neighbors];
 int m,l,h,k;
 float lat,lon;
 float conv=.0174;
 long t,u;
 float elev;
 float f[15];
 int tabval;
 
 max_tstations=0;

 fp=fopen(fname,"r");

 if(fp==NULL) {

             logMessage("could not open %s\n",fname);
	bzero(message,150);
	sprintf(message,"Could not open file: %s\n",fname);
	logMessage(message);

              exit(1);

              }
else
{
		bzero(message,150);
	sprintf(message,"Opened file: %s\n",fname);
	logMessage(message);

}
 i=0;
 
 tabval='\t';
 for(;;) {

    p=fgets(kbuf,120,fp);
    if(p==NULL)
              break;

    if(i==1000)
              break;

    p=strchr(kbuf,'\n');

    if(p!=NULL)
             *p=0;

    j=0;
    while(kbuf[j] != 0) {
    
             if(kbuf[j]=='\t')
                  kbuf[j]=' ';
                  
             j++;             
	
      }

    tstation[i].hb5=calloc(10,sizeof(char));
    tstation[i].name=calloc(50,sizeof(char));
    tstation[i].parm=calloc(10,sizeof(char));
    tstation[i].cparm=calloc(10,sizeof(char));
    tstation[i].max=calloc(12,sizeof(float));
    tstation[i].min=calloc(12,sizeof(float));
    tstation[i].index=calloc(mpe_dqc_max_temp_neighbors,sizeof(short int));
    
    ier=sscanf(kbuf,"%s %s %f %f %f %d ",tstation[i].hb5,
               tstation[i].parm,&tstation[i].lat,
               &tstation[i].lon,&elev,&tstation[i].tip);
	        
    tstation[i].elev=(int)elev;
       
    if(strncasecmp("TAI",tstation[i].parm,3)!=0)     
                      continue;
		      
		      
    for(m=0;m<i;m++) {
    
            if(strcmp(tstation[i].hb5,tstation[m].hb5)==0 &&
               tstation[i].parm[4]==tstation[m].parm[4]){
               
                                  /* logMessage("duplicate tstation %s\n",tstation[i].hb5);
                                    */
				    break;
                                    
                                    }
                                    
                      }          
          

    if(ier != 6)
               continue;
	       
	       

    lat=tstation[i].lat;
    lon=tstation[i].lon;

    t=dval.a * cos(lat*conv)/(1+sin(lat*conv))
      * cos((lon-dval.lo-90)*conv) + dval.xo +.5;

    tstation[i].x=t;

    u=dval.a * cos(lat*conv)/(1+sin(lat*conv))
      * sin((lon-dval.lo-90)*conv) + dval.yo + .5;
       
    tstation[i].y=u;

    sflag=1;
    field=0;
    for(j=0;j<strlen(kbuf);j++) {

                 if(kbuf[j] !=' ' && sflag==1) {
                                    sflag=0; 
                                    field++;

				  }
      
                 else if(kbuf[j]==' ')
                          sflag=1;

                 if(field==7) {

                          kbuf[j+49]=0;

                          strcpy(tstation[i].name,&kbuf[j]);
                          break;

			}

                }
		
  i++;

  }

max_tstations=i;

fclose(fp);

for(i=0;i<max_tstations;i++){
         
                 for(m=0;m<12;m++) {
		 
                             tstation[i].max[m]=-99; 
                             tstation[i].min[m]=-99;
			     
			     }

}

fp=NULL;
if(station_climo_file[0]!=0)
        fp=fopen(station_climo_file,"r");
       
if(fp==NULL) {
	bzero(message,150);
	sprintf(message,"Could not open file: %s\n",station_climo_file);
	logMessage(message);


          for(i=0;i<max_tstations;i++) {
                   
          for(k=0;k<12;k++) {

            ier=is_good(k,smonth,emonth);

            if(ier == -1)
                   continue;

             tstation[i].max[k]=(get_lmaxmin(tstation[i].lat,tstation[i].lon,k,0)/10);

             if(tstation[i].max[k] < 0)
                     logMessage("No max temp climatology for %s\n",tstation[i].hb5);
                      
             strcpy(tstation[i].cparm,"TAIPBXM");
	     
             tstation[i].min[k]=(get_lmaxmin(tstation[i].lat,tstation[i].lon,k,1)/10);

             if(tstation[i].min[k] < 0)
                     logMessage("No min temp climatology for %s\n",tstation[i].hb5);
		      
	         
	   }

       }

}

else {
	bzero(message,150);
	sprintf(message,"Opened file: %s\n",station_climo_file);
	logMessage(message);

     for(;;) {
     
         p=fgets(kbuf,200,fp);
         if(p==NULL)
                 break;
                 
         ier=sscanf(kbuf,"%s %s %f %f %f %f %f %f %f %f %f %f %f %f",
             hb5,parm,&f[0],&f[1],&f[2],&f[3],&f[4],&f[5],&f[6],&f[7],&f[8],
             &f[9],&f[10],&f[11]);
         
         if(ier != 14)
                    continue;
             
         for(i=0;i<max_tstations;i++) {
           
                 if(strcmp(tstation[i].hb5,hb5)==0 &&
		    tstation[i].parm[4]==parm[4]) {
		    
		              if(parm[5]=='X') {
                 
                              for(m=0;m<12;m++)
                                  tstation[i].max[m]=f[m];
				  
				  }
				  
			      else if(parm[5]=='N') {
                 
                              for(m=0;m<12;m++)
                                  tstation[i].min[m]=f[m];
				  
				  }  
				                                    
                              strcpy(tstation[i].cparm,parm); 
			      
                              }
                              
                          }

                            
               }
	       
	 rewind(fp);
	 
	 for(;;) {
	
         p=fgets(kbuf,200,fp);
         if(p==NULL)
                 break;
                 
         ier=sscanf(kbuf,"%s %s %f %f %f %f %f %f %f %f %f %f %f %f",
             hb5,parm,&f[0],&f[1],&f[2],&f[3],&f[4],&f[5],&f[6],&f[7],&f[8],
             &f[9],&f[10],&f[11]);
         
         if(ier != 14)
                    continue;
	       
         if(strncmp(parm,"TAIPB",5) != 0 &&
	    strncmp(parm,"TAIRZ",5) != 0)
	                            continue;	       
	    
         for(i=0;i<max_tstations;i++) {
           
                 if(strcmp(tstation[i].hb5,hb5)==0 &&
		    ((parm[5]=='X' && tstation[i].max[0] < 0) ||
		     (parm[5]=='N' && tstation[i].min[0] < 0) ||
		     strncmp(tstation[i].cparm,"TAIPB",5)==0)) {
		    
                              if(parm[5]=='X') {
                 
                              for(m=0;m<12;m++)
                                  tstation[i].max[m]=f[m];
				  
				  }
				
			      else if(parm[5]=='N') {
                 
                              for(m=0;m<12;m++)
                                  tstation[i].min[m]=f[m];
				  
				  }  
                                                                
                              strcpy(tstation[i].cparm,parm);
			         
                             
                              }
                              
                          }

                            
               }    
	       
fclose(fp);	       
	       
}

strcpy(kbuf,fname);
strcat(kbuf,".tcustom");
strcpy(tstation_list_custom_file,kbuf);

fp=fopen(kbuf,"r");

if(fp!=NULL) {
		bzero(message,150);
	sprintf(message,"Opened file: %s\n",kbuf);
	logMessage(message);

     for(;;) {

           p=fgets(kbuf,80,fp);
           if(p==NULL)
                   break;

           p=strchr(kbuf,'\n');
           *p=0;

           ier=sscanf(kbuf,"%s %s %d %d",hb5,parm,&xadd,&yadd);

           for(j=0;j<max_tstations;j++) {

                   if(strcmp(hb5,tstation[j].hb5)==0) {
                     
                      if(strncmp(parm,tstation[j].parm,3)==0 &&
                              parm[4]==tstation[j].parm[4]) {

                              tstation[j].xadd=xadd;
                              tstation[j].yadd=yadd;
                              break;

			    }
 
		 }
 
	 }
	 }
	 
   fclose(fp); 

   }        
else
{
		bzero(message,150);
	sprintf(message,"Could not open file: %s\n",kbuf);
	logMessage(message);

}
for(i=0;i<max_tstations;i++) {

        for(l=0;l<mpe_dqc_max_temp_neighbors;l++)
                    sorted[l]=9999999;

        for(m=0;m<max_tstations;m++) {
        
               if(i==m)
                     continue; 

               dist1=tstation[i].lat-tstation[m].lat;
               
               dist2=(tstation[i].lon-tstation[m].lon)*
	              cos((tstation[i].lat+tstation[m].lat)/2*conv);

               dist=pow(dist1,2)+pow(dist2,2);
 
               for(l=0;l<mpe_dqc_max_temp_neighbors;l++) {

                       if(dist < sorted[l]) {

                               for(h=mpe_dqc_max_temp_neighbors-1; h > l; h--) {

                                     sorted[h]=sorted[h-1];
                                              
                                     tstation[i].index[h]=
                                     tstation[i].index[h-1];
   
				   }

                               sorted[l]=dist;
                                              
                               tstation[i].index[l]=m;

                               break;

			     }
                      

		     }

	     }

      }

return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/get_tstation_list.c,v $";
 static char rcs_id2[] = "$Id: get_tstation_list.c,v 1.3 2007/05/23 20:54:55 whfs Exp $";}
/*  ===================================================  */

}






