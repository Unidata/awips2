#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

/* j is the day number. */

void estimate_missing_tstations(int j,
		                const struct station * tstation,
				int max_tstations,
				const struct tdata * tdata)

{

extern int isom;
extern int maxmin_used;
extern int mpe_dqc_max_temp_neighbors;
extern int mpe_dqc_min_good_stations;

int m,i,h,l,ii;
double lat1=0.,lon1=0.,fdist,fdata,fval,lat,lon,distlon;
double temp,dist;
float conv=.0174;
float df;
float a,b;
float temp_climo;
float temp_climo1;

/*estimate max/mins first */

for(m=0;m<max_tstations;m++) {

          if(tstation[m].elev <= 0)
		                   continue;

          for(h=4;h<6;h++) {

          if(tdata[j].used[h]==0)
                            continue;
		
          /* only estimate missing data */
	    
          if(tdata[j].stn[m].tlevel2[h].data > -99 && 
             tdata[j].stn[m].tlevel2[h].qual != 5 && 
             tdata[j].stn[m].tlevel2[h].qual != 1)
                               continue;

          tdata[j].stn[m].tlevel2[h].data=-99;

          if((h==4 && tstation[m].max[isom]  <= -99) ||
	     (h==5 && tstation[m].min[isom]  <= -99))
	                                continue;
                  
          /* Retrieve the climate information for this station. */
          if ( maxmin_used == 1 )
          {
             temp_climo1 = (tstation[m].max[isom] + tstation[m].min[isom] ) / 2;
          }

          lat1=tstation[m].lat;
          lon1=tstation[m].lon;

          fdist=0.0;
          fdata=0.0;
          l=0;

          for(ii=0;ii<mpe_dqc_max_temp_neighbors;ii++){
   
                 i=tstation[m].index[ii];

                 /* dont estimate unless good or forced good */

                 if(tdata[j].stn[i].tlevel2[h].qual!=0 &&
                    tdata[j].stn[i].tlevel2[h].qual!=8 &&
                    tdata[j].stn[i].tlevel2[h].qual!=6 &&
                    tdata[j].stn[i].tlevel2[h].qual!=3 &&
		    tdata[j].stn[i].tlevel2[h].qual!=2)
                                          continue;

                 /*dont use missing tstations */
                 
                 if(tdata[j].stn[i].tlevel2[h].data ==-99)
                                          continue;
					  
                 if((h==4 && tstation[i].max[isom]  <= -99) ||
	            (h==5 && tstation[i].min[isom]  <= -99))
	                                continue;
	 				
 		 if(tstation[i].elev <= 0)
		                        continue;
								
                 lat=tstation[i].lat;
                 lon=tstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 dist= pow((double)(lat1-lat),2) +
                       pow((double)distlon,2);
		       
                 dist=pow(dist,.5)*60;

                 if(dist==0.0)
                             dist=.0001;
               
                 df=50*abs(tstation[m].elev-tstation[i].elev)/5280;

	         dist=dist+df;
	       
	         dist=1/dist;
	       	       
                 temp=tdata[j].stn[i].tlevel2[h].data;

                 if(h==4) {
	       	        			
                   temp=(temp+(tstation[m].max[isom]-
                               tstation[i].max[isom]))*dist;	
			      
			      }

                 if(h==5) {
	       	        			
                   temp=(temp+(tstation[m].min[isom]-
                               tstation[i].min[isom]))*dist;
			      }

                 fdata=fdata+temp;

                 fdist=fdist+dist;                    

                 l++;
               
                 if(l==8)
                            break;
                 
	         }


          if(l < mpe_dqc_min_good_stations) {

          fdist=0.0;
          fdata=0.0;
          l=0;

          for(i=0;i<max_tstations;i++){
   
                 if(i==m)
                      continue;

                 if(tdata[j].stn[i].tlevel2[h].qual!=0 &&
                    tdata[j].stn[i].tlevel2[h].qual!=8 &&
                    tdata[j].stn[i].tlevel2[h].qual!=6 &&
                    tdata[j].stn[i].tlevel2[h].qual!=3 &&
		    tdata[j].stn[i].tlevel2[h].qual!=2)
                                          continue;

                 /*dont use missing tstations */
                 
                 if(tdata[j].stn[i].tlevel2[h].data ==-99)
                                          continue;
					  
                 if((h==4 && tstation[i].max[isom]  <= -99) ||
	            (h==5 && tstation[i].min[isom]  <= -99))
	                                continue;
					
		 if(tstation[i].elev <= 0)
		                        continue;			
                 lat=tstation[i].lat;
                 lon=tstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 dist= pow((double)(lat1-lat),2) +
                           pow((double)distlon,2);
			   
                 dist=pow(dist,.5)*60;
                 
		 if(dist==0.0)
                             dist=.0001;

                 df=50*abs(tstation[m].elev-tstation[i].elev)/5280;

	         dist=dist+df;
	       
	         dist=1/dist;
	       	       
                 temp=tdata[j].stn[i].tlevel2[h].data;

                 if(h==4) {
	       	        			
                   temp=(temp+(tstation[m].max[isom]-
                               tstation[i].max[isom]))*dist;	
			      
			      }

                 if(h==5) {
	       	        			
                   temp=(temp+(tstation[m].min[isom]-
                               tstation[i].min[isom]))*dist;
			      }

                 fdata=fdata+temp;

                 fdist=fdist+dist;                    

                 l++;
               
	       }

	}
	
       if(l == 0)       
               fval=-99;
 
       else
               fval=fdata/fdist;
	       
       tdata[j].stn[m].tlevel2[h].data=(short int)fval;
       tdata[j].stn[m].tlevel2[h].qual=5;
       
      /*logMessage("estimate %s %d %d\n",tstation[m].hb5,h,
        tdata[j].stn[m].tlevel2[h].data);  */ 
         
     }

     for(h=0;h<4;h++) {

          if(tdata[j].used[h]==0)
                            continue;
			    
	  if(tdata[j].stn[m].tlevel2[h].data != -99 &&
	           (tdata[j].stn[m].tlevel2[h].qual==0 ||
                    tdata[j].stn[m].tlevel2[h].qual==8 || 
                    tdata[j].stn[m].tlevel2[h].qual==6 || 
                    tdata[j].stn[m].tlevel2[h].qual==3 || 
		    tdata[j].stn[m].tlevel2[h].qual==2))
	                             continue;
				     	
          tdata[j].stn[m].tlevel2[h].data=-99;
	    
	  if(tdata[j].stn[m].tlevel2[4].data == -99 ||
             tdata[j].stn[m].tlevel2[5].data == -99)
                                         continue; 
					 
	  if(tdata[j].stn[m].tlevel2[4].qual==1 ||                       
	     tdata[j].stn[m].tlevel2[5].qual==1) 
                                           continue;
					     								 						  
          fdist=0.0;
          fdata=0.0;
          l=0;
	
	  for(ii=0;ii<mpe_dqc_max_temp_neighbors;ii++){
   
                 i=tstation[m].index[ii];

                 /* dont estimate unless good or forced good */

                 if(tdata[j].stn[i].tlevel2[h].qual!=0 &&
                    tdata[j].stn[i].tlevel2[h].qual!=8 &&
                    tdata[j].stn[i].tlevel2[h].qual!=6 &&
                    tdata[j].stn[i].tlevel2[h].qual!=3 &&
		    tdata[j].stn[i].tlevel2[h].qual!=2)
                                          continue;

                 /*dont use missing tstations */
                 
                 if(tdata[j].stn[i].tlevel2[h].data ==-99 ||
		    tdata[j].stn[i].tlevel2[h].a < -98 )
                                          continue;
					  
	       	 if(tstation[i].elev <= 0)
		                        continue;
									
                 /* Retrieve the climate information for this station. */
                 if ( maxmin_used == 1 )
                 {
                    temp_climo = (tstation[i].max[isom] + tstation[i].min[isom] ) / 2;
                 }

                 lat=tstation[i].lat;
                 lon=tstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 dist= pow((double)(lat1-lat),2) +
                       pow((double)distlon,2);
		       
                 dist=pow(dist,.5)*60;
                 
		 if(dist==0.0)
                             dist=.0001;
               
                 df=50*abs(tstation[m].elev-tstation[i].elev)/5280;

	         dist=dist+df;
	       
	         dist=1/dist;
	       	       
                 if ( ( maxmin_used == 1 ) && ( temp_climo1 > -99 ) && ( temp_climo > -99 ) )
                 {
                    fdata = fdata + tdata[j].stn[i].tlevel2[h].a * dist * ( temp_climo1 / temp_climo );
                 }
                 else
                 {
                    fdata = fdata + tdata[j].stn[i].tlevel2[h].a * dist;
                 }

                 fdist=fdist+dist;                    

                 l++;
               
                 if(l==8)
                            break;
                 
	         }


          if(l < mpe_dqc_min_good_stations) {

          fdist=0.0;
          fdata=0.0;
          l=0;

          for(i=0;i<max_tstations;i++){
   
                 if(i==m)
                      continue;

                 if(tdata[j].stn[i].tlevel2[h].qual!=0 &&
                    tdata[j].stn[i].tlevel2[h].qual!=8 &&
                    tdata[j].stn[i].tlevel2[h].qual!=6 &&
                    tdata[j].stn[i].tlevel2[h].qual!=3 &&
		    tdata[j].stn[i].tlevel2[h].qual!=2)
                                          continue;

                 /*dont use missing tstations */
                 
                 if(tdata[j].stn[i].tlevel2[h].data ==-99 ||
		    tdata[j].stn[i].tlevel2[h].a < -98)
                                          continue;
					  
		 if(tstation[i].elev <= 0)
		                        continue;

                 /* Retrieve the climate information for this station. */
                 if ( maxmin_used == 1 )
                 {
                    temp_climo = (tstation[i].max[isom] + tstation[i].min[isom] ) / 2;
                 }
								
                 lat=tstation[i].lat;
                 lon=tstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 dist= pow((double)(lat1-lat),2) +
                           pow((double)distlon,2);

                 if(dist==0.0)
                             dist=.0001;

                 df=50*abs(tstation[m].elev-tstation[i].elev)/5280;

	         dist=dist+df;
	       
	         dist=1/dist;
	       	    
                 if ( ( maxmin_used == 1 ) && ( temp_climo1 > -99 ) && ( temp_climo > -99 ) )
                 {
                    fdata = fdata + tdata[j].stn[i].tlevel2[h].a * dist * ( temp_climo1 / temp_climo );
                 }
                 else
                 {
                    fdata = fdata + tdata[j].stn[i].tlevel2[h].a * dist;
                 }

                 fdist=fdist+dist;                    

                 l++;
               
	       }

	}
	  
        if(l == 0) 
               tdata[j].stn[m].tlevel2[h].data=-99;
 
        else {
	   
	       a=fdata/fdist;
	       	       	       
	       b=a*(float)(tdata[j].stn[m].tlevel2[4].data - 
	             tdata[j].stn[m].tlevel2[5].data)+
	            (float)tdata[j].stn[m].tlevel2[5].data;
		      
               tdata[j].stn[m].tlevel2[h].data=(short int)b;
               tdata[j].stn[m].tlevel2[h].qual=5;	       
	       
	       if(tdata[j].stn[m].tlevel2[4].data==-99 || 
	          tdata[j].stn[m].tlevel2[5].data==-99)
	                  tdata[j].stn[m].tlevel2[h].data =-99;
     
            /*  logMessage("for %s %f %d %d %d\n",tstation[m].hb5,a,
	              tdata[j].stn[m].tlevel2[4].data,
		      tdata[j].stn[m].tlevel2[5].data,
		       tdata[j].stn[m].tlevel2[h].data); */
     
               }

			    
       }	    

  }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/estimate_missing_tstations.c,v $";
 static char rcs_id2[] = "$Id: estimate_missing_tstations.c,v 1.5 2007/05/23 20:52:44 whfs Exp $";}
/*  ===================================================  */

}
