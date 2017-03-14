#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

void check_consistency(int j,
		       struct station * station,
		       int max_stations)

{

extern int mpe_dqc_max_precip_neighbors;
int k,m;
float rtotal;
extern struct pdata pdata[10];
int i,ii;

for(k=0;k<max_stations;k++) {

        pdata[j].stn[k].tcons=1;
	 
	if(pdata[j].stn[k].frain[4].data < 0 ||
	   pdata[j].stn[k].frain[4].qual==1)
	                                 continue;
	
        rtotal=0;
        for(m=0;m<4;m++) {
	 
                if(pdata[j].stn[k].frain[m].data >= 0)
                            rtotal=rtotal+pdata[j].stn[k].frain[m].data;
        
	         }
	      
        if(fabs(rtotal-pdata[j].stn[k].frain[4].data) > .01) 
	                pdata[j].stn[k].tcons=-1;
			
			
  }
 
for(k=0;k<max_stations;k++) {

        for(m=0;m<5;m++) {

            if(pdata[j].stn[k].frain[m].qual==1 ||
	       pdata[j].stn[k].frain[m].data < 0)
	                           continue;

             pdata[j].stn[k].scons[m]=1;
	     
	     for(ii=0; ii < mpe_dqc_max_precip_neighbors; ii++){
   
                 i=station[k].index[ii];

                 if(station[i].lat == station[k].lat &&
                    station[i].lon == station[k].lon &&
		    pdata[j].stn[i].frain[m].qual != 1 &&
		    pdata[j].stn[i].frain[m].data >=0 &&
		    pdata[j].stn[i].frain[m].data !=
		    pdata[j].stn[k].frain[m].data) {
		    
		       pdata[j].stn[k].scons[m]=-1;
		       
		       }
		         
		    }
		    
		    
		}    
		    
  }
	       
      			

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}			
			
                  
