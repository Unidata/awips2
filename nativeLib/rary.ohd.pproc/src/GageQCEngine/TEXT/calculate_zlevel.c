#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

void calculate_zlevel()

{
extern struct zdata zdata[10];
int j,h,k;
extern int max_zstations;
int j1,j2=0,k1=0,k2=0;

for(j=0;j<MAX_GAGEQC_DAYS;j++) {

     for(k=0;k<4;k++) {

             if(zdata[j].used[k]==0) {

                           j1=-1;

	                   if((j==0 && k==3) ||
			       (j==MAX_GAGEQC_DAYS-1 && k==0))
			                          continue;
			   			  
			   if((k==1 || k==2) &&
			       zdata[j].used[k-1] != 0 &&
			       zdata[j].used[k+1] != 0) {
			       
			         j1=j;
				 j2=j;
				 k1=k-1;
				 k2=k+1;
			       
			       
			       }
			   
			   if((k==0) &&  
			       zdata[j].used[1] != 0 &&
			       zdata[j+1].used[3] != 0) {
			   
			       j1=j;
			       j2=j+1;
			       k1=1;
			       k2=3;
			   
			   
			    }
			   
			   if((k==3) &&			  
	                       zdata[j].used[2] != 0 &&
			       zdata[j-1].used[0] != 0) {
			   
			       j1=j;
			       j2=j-1;
			       k1=2;
			       k2=0;
	     
	                    }
	    
			    if(j1==-1)
			            continue;
				    
			    zdata[j].used[k]=6;
			    
			    for(h=0;h<max_zstations;h++) {			    
			    
			    if(zdata[j1].stn[h].zlevel1[k1].data >= 0 &&
			       zdata[j2].stn[h].zlevel1[k2].data >= 0) {
			       
			       zdata[j].stn[h].zlevel1[k].data=
			       (zdata[j1].stn[h].zlevel1[k1].data +
  			        zdata[j2].stn[h].zlevel1[k2].data)/2;
			    
			    	 zdata[j].stn[h].zlevel1[k].qual=5;		    
			    
			              }
			    
			    if(zdata[j1].stn[h].zlevel2[k1].data >= 0 &&
			       zdata[j2].stn[h].zlevel2[k2].data >= 0) {
			       
			       zdata[j].stn[h].zlevel2[k].data=
			       (zdata[j1].stn[h].zlevel2[k1].data +
  			        zdata[j2].stn[h].zlevel2[k2].data)/2;
			    
			    	 zdata[j].stn[h].zlevel2[k].qual=5;		    
			    
			              }
			    
			    }
			    
	     
	            }


      }

}


}
