#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

void get_zlevel(int j,
		struct station * station,
		struct station * zstation,
		int max_stations,
		int max_zstations )
{

extern struct pdata pdata[10];
extern struct zdata zdata[10];
int m,i,h,l,ii;
double lat1,lon1,fdist,fdata,fvalue,lat,lon,testdist,padj,distlon;
float conv=.0174;
int h1,h2,j1,j2;

if(max_zstations==0){

             for(m=0;m<max_stations;m++) {
             
	     for(h=0;h<4;h++) {
	     
             pdata[j].stn[m].frzlvl[h]=-99;

              }
	      }
	      
             return;

              }

for(m=0;m<max_stations;m++) {

          /* only estimate missing data */

          for(h=0;h<4;h++) {

          if(h==3) {
	  
	          if(j==0) {
		  
		  
		           j1=j;
	                   j2=j;
	       
	                   h1=h;
	                   h2=h;
	  
		  
		           }
			   
                  else     {
		  
		   
		           j1=j;
	                   j2=j-1;
	       
	                   h1=h;
	                   h2=0;
	  
		  
		          }
	  
	  
	          }
		  
	  else {
	  
	       j1=j;
	       j2=j;
	       
	       h1=h;
	       h2=h+1;
	  
	       }	  
		 
          lat1=station[m].lat;
          lon1=station[m].lon;

          fdist=0.0;
          fdata=0.0;
          l=0;

          for(ii=0;ii<5;ii++){
   
                 i=station[m].zindex[ii];

                 /* dont estimate unless good or forced good */

                 if(zdata[j1].stn[i].zlevel2[h1].qual!=0 &&
                    zdata[j1].stn[i].zlevel2[h1].qual!=8 &&
                    zdata[j1].stn[i].zlevel2[h1].qual!=3 &&
		    zdata[j2].stn[i].zlevel2[h2].qual!=0 &&
                    zdata[j2].stn[i].zlevel2[h2].qual!=8 &&
                    zdata[j2].stn[i].zlevel2[h2].qual!=3)
                                          continue;

                 /*dont use missing stations */
                 
                 if(zdata[j1].stn[i].zlevel2[h1].data < 0 ||
		    zdata[j2].stn[i].zlevel2[h2].data < 0)
                                          continue;

                 lat=zstation[i].lat;
                 lon=zstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 testdist= pow((double)(lat1-lat),2) +
                           pow((double)distlon,2);

                 if(testdist==0.0)
                             testdist=.0001;

                 padj=(zdata[j1].stn[i].zlevel2[h1].data+
                       zdata[j2].stn[i].zlevel2[h2].data)/2;
		       
                 fdist=1/testdist+fdist;
                 fdata=padj/testdist + fdata;

                 l++;

	       }


          if(l < 5) {

          fdist=0.0;
          fdata=0.0;
          l=0;

          for(i=0;i<max_zstations;i++){
   
                 if(zdata[j1].stn[i].zlevel2[h1].qual!=0 &&
                    zdata[j1].stn[i].zlevel2[h1].qual!=8 &&
                    zdata[j1].stn[i].zlevel2[h1].qual!=3 &&
		    zdata[j2].stn[i].zlevel2[h2].qual!=0 &&
                    zdata[j2].stn[i].zlevel2[h2].qual!=8 &&
                    zdata[j2].stn[i].zlevel2[h2].qual!=3)
                                          continue;

                 /*dont use missing stations */
                 
                 if(zdata[j1].stn[i].zlevel2[h1].data < 0 ||
		    zdata[j2].stn[i].zlevel2[h2].data < 0)
                                          continue;

                 lat=zstation[i].lat;
                 lon=zstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 testdist= pow((double)(lat1-lat),2) +
                           pow((double)distlon,2);

                 if(testdist==0.0)
                             testdist=.0001;

                 padj=(zdata[j1].stn[i].zlevel2[h1].data+
                       zdata[j2].stn[i].zlevel2[h2].data)/2;
		       
                 fdist=1/testdist+fdist;
                 fdata=padj/testdist + fdata;

                 l++;

	       }

	}
	
	     if(l==0)
	            fvalue=-99;
	

             else
                    fvalue=fdata/fdist*1000;
       
             pdata[j].stn[m].frzlvl[h]=(int)fvalue;

 
               
	}


}

}

