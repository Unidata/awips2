#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

/* j is the day of the data. */

void estimate_daily_stations ( int j,
                               struct station * station,
                               int max_stations )
{

   extern int mpe_dqc_max_precip_neighbors;
   extern int isom;
   extern int isohyets_used;
   extern struct pdata pdata[10];
   extern int method;
   int m,k,i,l,ii;
   int mpe_dqc_min_good_stations;
   float conv = .0174;
   double distlon;
   double lat1;
   double lon1;
   double fdist;
   double fdata;
   double fvalue[4];
   double lat;
   double lon;
   double testdist;
   double fmult;
   double ftotal;
   double isoh=0.;
   double isoh1=0.;
   double padj;
   double stotal;

   int num_missing;

   if ( pdata[j].data_time == 0 )
   {
      return;
   }

   /* this routine will estimate 6 hourly periods when 24 hour rain exists */


   for(m=0;m<max_stations;m++)
   {
      /* dont estimate missing 24 hour stations */
      if(pdata[j].stn[m].frain[4].data < 0 ||
         pdata[j].stn[m].frain[4].qual==4)
                               continue;


          /* search for a missing 6 hourly period */

          for(k=0;k<4;k++) {
          
               if(pdata[j].stn[m].frain[k].data >= 0
                  && pdata[j].stn[m].frain[k].qual==2)
                                            continue;
					    
	       if(pdata[j].stn[m].frain[k].qual==1)
	                                    break;				    
                                            
               if(pdata[j].stn[m].rain[k].data < 0)
                               break;

	     }

          if(k==4)
                continue;

          /* dont estimate stations forced good, bad or estimated */

          if(pdata[j].stn[m].frain[4].qual==1 ||            
             pdata[j].stn[m].frain[4].qual==5)
                                            continue;

          /* at least  one missing 6 hourly period found */

          lat1=station[m].lat;
          lon1=station[m].lon;

          /*get isohyet */

          if(isohyets_used!=0)
                 isoh1=station[m].isoh[isom];

	  /* first */        for(k=0;k<4;k++) {

             fdist=0.0;
             fdata=0.0;

             l=0;

             for(ii=0;ii<mpe_dqc_max_precip_neighbors;ii++){

                 i=station[m].index[ii]; 

                 /* dont estimate unless good or forced good */

                 if(pdata[j].stn[i].frain[k].qual!=0 &&
                    pdata[j].stn[i].frain[k].qual!=8 &&
                    pdata[j].stn[i].frain[k].qual!=3 &&
		    pdata[j].stn[i].frain[k].qual!=2)
                                          continue;
	     
                 /*dont use missing stations */
                 
                 if(pdata[j].stn[i].frain[k].data < 0)
                                          continue;

                 lat=station[i].lat;
                 lon=station[i].lon;

                 if(isohyets_used != 0)
                        isoh=station[i].isoh[isom];

                 distlon =  (lon1 - lon) * cos ((lat1 + lat) / 2 * conv);

                 testdist= pow((double)(lat1-lat),2) +
                           pow((double)(distlon),2);

                 if(testdist==0.0)
                 {
                    testdist=.000001;
                 }
  
                 testdist = 1 / testdist;

                 if(method==2 && isoh > 0 && isoh1 > 0)
                 {
                      padj=pdata[j].stn[i].frain[k].data*isoh1/isoh;
                 }
                 else
                 {
                      padj=pdata[j].stn[i].frain[k].data;
                 }
 
                 fdist=testdist+fdist;
                 fdata=padj*testdist + fdata;
                 l++;

 

	       }
	 

             if(l < mpe_dqc_min_good_stations) {

             fdist=0.0;
             fdata=0.0;

             l=0;

             for(i=0;i<max_stations;i++){
                  
                 if(i==m)
                      continue;

                 if(pdata[j].stn[i].frain[k].qual!=0 &&
                    pdata[j].stn[i].frain[k].qual!=8 &&
                    pdata[j].stn[i].frain[k].qual!=3 &&
		    pdata[j].stn[i].frain[k].qual!=2)
                                          continue;

              
                 
                 if(pdata[j].stn[i].frain[k].data < 0)
                                          continue;

                 lat=station[i].lat;
                 lon=station[i].lon;

                 if(isohyets_used != 0)
                 {
                        isoh=station[i].isoh[isom];
                 }

                 testdist= pow((double)(lat1-lat),2) +
                           pow((double)(lon1-lon),2);

                 if(testdist==0.0)
                             testdist=.0001;

                  testdist = 1 / testdist;

                 if(method==2 && isoh > 0 && isoh1 > 0)
                 {
                      padj=pdata[j].stn[i].frain[k].data*isoh1/isoh;
                 }
                 else
                 {
                      padj=pdata[j].stn[i].frain[k].data; 
                 }

                 fdist=testdist+fdist;
                 fdata=padj*testdist + fdata;
                 l++;
	     }
           
	     }

           if(l != 0)
               fvalue[k]=fdata/fdist;
               
           else
               fvalue[k]=-9999;

	   }

            
       if(fvalue[0]==-9999 ||
          fvalue[1]==-9999 ||
          fvalue[2]==-9999 ||
          fvalue[3]==-9999) {         
       
          for(k=0;k<4;k++) {
       
                 pdata[j].stn[m].frain[k].qual=6;
                 pdata[j].stn[m].frain[k].data=pdata[j].stn[m].frain[4].data/4;
                 
                 }
                 
             continue;
             
            }  
	  
	  
        ftotal=0.0;
        stotal=0.0;
        num_missing=0;

	for(k=0;k<4;k++) {

              if(pdata[j].stn[m].rain[k].data >= 0 &&
	         pdata[j].stn[m].frain[k].qual!=1) 
                          stotal=stotal+pdata[j].stn[m].frain[k].data;        
                      
              else {

                  num_missing++;
     
                  ftotal=ftotal+fvalue[k];
    
		}
         
	}

        stotal=pdata[j].stn[m].frain[4].data-stotal;

        if(stotal < 0)
                  stotal=0;

        if(ftotal==0.0)
                  fmult=0;

        else
                  fmult=stotal/ftotal;

	for(k=0;k<4;k++) {

                 if(pdata[j].stn[m].rain[k].data >= 0 &&
		 pdata[j].stn[m].frain[k].qual!=1) 
                          continue;
                   
                 if(ftotal != 0) {

                 pdata[j].stn[m].frain[k].data=fvalue[k]*fmult;
                 pdata[j].stn[m].frain[k].qual=6;


	       }

                 else {

                 pdata[j].stn[m].frain[k].data=stotal/(float)num_missing;
                 pdata[j].stn[m].frain[k].qual=6;

		 }


	
} 
 

}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/estimate_daily_stations.c,v $";
 static char rcs_id2[] = "$Id: estimate_daily_stations.c,v 1.2 2007/06/06 13:28:26 whfs Exp $";}
/*  ===================================================  */

}

