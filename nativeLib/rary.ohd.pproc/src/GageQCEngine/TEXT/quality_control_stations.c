#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

void quality_control_stations(int j,
		              struct station * station,
			      int max_stations)

{

extern int mpe_dqc_max_precip_neighbors;
extern int isom;
extern int isohyets_used;
extern struct pdata pdata[10];
extern int method;
int m,k,i,l,maxl,h;
double lat1,lon1,fdist,fdata,lat,lon,testdist[3000],isoh=0.,padj[3000],fvalu,fstd,fdif,distlon;
int good,ogood;
float conv=.0174;
float valdif,isoh1=0.;
int ii;

for(k=4;k>=0;k--) {

    if(pdata[j].used[k]==0)
                       continue;

    ogood=0;
    again:
    good=0;

    for(m=0;m<max_stations;m++) {

          lat1=station[m].lat;
          lon1=station[m].lon;

      /*get isohyet */

          if(isohyets_used != 0)
              isoh1=station[m].isoh[isom];

          /* if data is missing dont quality control */

          if(pdata[j].stn[m].frain[k].data < 0)
                                  continue;

          l=0;
          for(ii=0;ii<mpe_dqc_max_precip_neighbors;ii++){
   
                 i=station[m].index[ii];

                 /* only use good or forced good gages to estimate others */
 
                 if(pdata[j].stn[i].frain[k].qual != 0 &&
                    pdata[j].stn[i].frain[k].qual != 8 &&                  
                    pdata[j].stn[i].frain[k].qual != 6 &&
                    pdata[j].stn[i].frain[k].qual != 3 &&
                    pdata[j].stn[i].frain[k].qual != 4)
                                          continue;
                 
                 if(pdata[j].stn[i].frain[k].data < 0)
                                          continue;

                 lat=station[i].lat;
                 lon=station[i].lon;

                 if(isohyets_used != 0)
                        isoh=station[i].isoh[isom];

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 testdist[l]= pow((double)(lat1-lat),2) +
                           pow((double)(distlon),2);

                 if(testdist[l]==0.0)
                             testdist[l]=.000001;

                 if(method==2 && isoh > 0 && isoh1 > 0)    
                     padj[l]=pdata[j].stn[i].frain[k].data*isoh1/isoh;
 
                 else
                     padj[l]=pdata[j].stn[i].frain[k].data;
  
                 l++;

	  }

          if(l < 5) {

          for(i=0;i<max_stations;i++){
   
                 if(i==m)
                      continue;

                 /* only use good or forced good gages to estimate others */
 
                 if(pdata[j].stn[i].frain[k].qual != 0 &&
                    pdata[j].stn[i].frain[k].qual != 8 &&                
                    pdata[j].stn[i].frain[k].qual != 6 &&
                    pdata[j].stn[i].frain[k].qual != 3 &&
                    pdata[j].stn[i].frain[k].qual != 4)
                                          continue;
                 
                 if(pdata[j].stn[i].frain[k].data < 0)
                                          continue;

                 lat=station[i].lat;
                 lon=station[i].lon;

                 if(isohyets_used != 0)
                        isoh=station[i].isoh[isom];

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 testdist[l]= pow((double)(lat1-lat),2) +
                           pow((double)(distlon),2);

                 if(testdist[l]==0.0)
                             testdist[l]=.000001;

                 if(method==2 && isoh > 0 && isoh1 > 0)    
                     padj[l]=pdata[j].stn[i].frain[k].data*isoh1/isoh;
 
                 else
                     padj[l]=pdata[j].stn[i].frain[k].data;
  
                 l++;
	  }

	}

           /* get weighted mean */
 
           maxl=l;

           fdata=0;
           fdist=0;

           for(l=0;l<maxl;l++) {

                  fdata=fdata + padj[l]/testdist[l];
                  fdist=fdist + 1/testdist[l];                    
           
		}

           /* fvalu is estimated number */

           if(maxl==0)
                   fdist=1;

           fvalu=fdata/fdist;

           pdata[j].stn[m].frain[k].estimate=fvalu;    

           fstd=0.0;

           for(l=0;l<maxl;l++) {

                 fstd=fstd + fabs(padj[l]-fvalu)/testdist[l]; 

	       }

           fstd=fstd/fdist;

           /* if estimated data 0 */

           if(fstd==0.0 &&
              (pdata[j].stn[m].frain[k].qual == 0   ||
               pdata[j].stn[m].frain[k].qual == 1   ||
               pdata[j].stn[m].frain[k].qual == 5   ||
               pdata[j].stn[m].frain[k].qual == 6   ||
               pdata[j].stn[m].frain[k].qual == 4   ||
               pdata[j].stn[m].frain[k].qual == 2)) {

                                 pdata[j].stn[m].frain[k].stddev=-9999;
                                 continue;
           
			       }

           else if(fstd==0.0 && pdata[j].stn[m].frain[k].data <= .1) {

                        pdata[j].stn[m].frain[k].stddev=0;
                        pdata[j].stn[m].frain[k].qual=8;

                        good++;

                        continue;

		      }

           else if(fstd==0.0){

                pdata[j].stn[m].frain[k].stddev=-9999;
                               
                pdata[j].stn[m].frain[k].qual=3;

                if(k==4) {

                       for(h=0;h<4;h++)
                       
                              if( pdata[j].stn[m].frain[h].qual != 6)
                                            pdata[j].stn[m].frain[h].qual=3;

		     }			 

                continue;

            }     

           /* perhaps change this to be more robust */

           if(fstd < .05)
                   fstd=.05;
 
           else if(fstd > .20)
                   fstd=.20;
            
           fdif= fabs(pdata[j].stn[m].frain[k].data-fvalu);
           fdif=fdif/fstd; 

           valdif=fabs(pdata[j].stn[m].frain[k].data-fvalu);

           /* standard deviation check */
           /* check only if difference between actual and estimated > .1 */
 
           pdata[j].stn[m].frain[k].estimate=fvalu;
           pdata[j].stn[m].frain[k].stddev=fdif;

          if(pdata[j].stn[m].frain[k].qual == 0   ||
             pdata[j].stn[m].frain[k].qual == 1   || 
             pdata[j].stn[m].frain[k].qual == 5   ||
             pdata[j].stn[m].frain[k].qual == 6   ||
             pdata[j].stn[m].frain[k].qual == 4   ||
             pdata[j].stn[m].frain[k].qual == 2)
                                 continue;



           if(fdif > pdata[j].stddev && valdif > .10) {



                pdata[j].stn[m].frain[k].qual=3;
/*
                if(k==4) {

                       for(h=0;h<4;h++)
                             pdata[j].stn[m].frain[h].qual=3;

		     }
*/
			    }

           else {

                good++;
                if(pdata[j].stn[m].frain[k].qual==3)
                          pdata[j].stn[m].frain[k].qual=8;

	      }
  
        }

   if(good > ogood){

                  ogood=good;
                   
                  goto again;             

		}

 
  }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

