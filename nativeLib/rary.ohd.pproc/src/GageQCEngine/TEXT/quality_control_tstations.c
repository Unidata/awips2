#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"

/* j is the day number. */ 
void quality_control_tstations(int j,
		               struct station * tstation,
			       int max_tstations)

{

extern int isom;
extern struct tdata tdata[10];
extern int mpe_dqc_max_temp_neighbors;
int m,k,i,l;
double lat1,lon1,fdist,fdata,lat,lon,fvalu,distlon;
int good,ogood;
float conv=.0174;
int ii;
float dist,df,temp=0.,fdif;
float a,b;
    
for(k=4;k < 6;k++) {

      if(tdata[j].used[k]==0)
                       continue;
	
      ogood=0;
      again:
      good=0;
	  
      for(m=0;m<max_tstations;m++) {

          if(tstation[m].elev <= 0)
		                        continue;
			       
          lat1=tstation[m].lat;
          lon1=tstation[m].lon;
	
          if((k==4 && tstation[m].max[isom]  <= -99) ||
             (k==5 && tstation[m].min[isom]  <= -99))
                               continue;
	       
          if(tdata[j].stn[m].tlevel2[k].data == -99)
                                  continue;
				  
          /* check if daily maximum is less than daily minimum temperature 
		    for a station */
		    
	  /* if (tdata[j].stn[m].tlevel2[4].data < tdata[j].stn[m].tlevel2[5].data)
	      continue;				  */
                    
	  fdist=0.0;
          fdata=0.0;
          l=0;

          for(ii=0;ii<mpe_dqc_max_temp_neighbors;ii++){
   
                 i=tstation[m].index[ii];

                 /* only use good or forced good gages to estimate others */
 
                 if(tdata[j].stn[i].tlevel2[k].qual != 0 &&
                    tdata[j].stn[i].tlevel2[k].qual != 8 &&                  
                    tdata[j].stn[i].tlevel2[k].qual != 6 &&
                    tdata[j].stn[i].tlevel2[k].qual != 3 &&
                    tdata[j].stn[i].tlevel2[k].qual != 2)
                                          continue;
                 
                 if(tdata[j].stn[i].tlevel2[k].data == -99)
                                          continue;

                 if((k==4 && tstation[i].max[isom]  <= -99) ||
	            (k==5 && tstation[i].min[isom]  <= -99))
	                                continue;
					
                 if(tstation[i].elev <= 0)
		                        continue;
					
                 /* check if surrounding daily maximum is less than daily minimum temperature 
		    for a station */
		    
		 /* if (tdata[j].stn[i].tlevel2[4].data < tdata[j].stn[i].tlevel2[5].data)
		     continue; */
		        					
					
                 lat=tstation[i].lat;
                 lon=tstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);

                 dist= pow((double)(lat1-lat),2) +
                       pow((double)(distlon),2);
			   
	         dist=pow(dist,.5)*60;
		 
                 if(dist==0.0)
                             dist=.0001;
               	
		 df=50*abs(tstation[m].elev-tstation[i].elev)/5280;	
				 		   
                 dist=dist+df;
			 
		 if(dist==0.0)
                             dist=.000001;  
			     
		 dist=1/dist;
		 
                 if(k==4)
		     temp=(tdata[j].stn[i].tlevel2[k].data+
		     (tstation[m].max[isom]-tstation[i].max[isom]))*dist;
		     
		 if(k==5)
		     temp=(tdata[j].stn[i].tlevel2[k].data+
		     (tstation[m].min[isom]-tstation[i].min[isom]))*dist;
		 
                 l++;
		 
		 
                 fdata=fdata+temp;

                 fdist=fdist+dist;                    

		 if(l==5)
		        break;
			
	  }

          if(l < 5) {

	  fdist=0.0;
          fdata=0.0;
	  l=0;
	  
          for(i=0;i<max_tstations;i++){
   
                 if(i==m)
                      continue;

                 /* only use good or forced good gages to estimate others */
 
                 if(tdata[j].stn[i].tlevel2[k].qual != 0 &&
                    tdata[j].stn[i].tlevel2[k].qual != 8 &&                
                    tdata[j].stn[i].tlevel2[k].qual != 6 &&
                    tdata[j].stn[i].tlevel2[k].qual != 3 &&
                    tdata[j].stn[i].tlevel2[k].qual != 2)
                                          continue;
                 
                 if(tdata[j].stn[i].tlevel2[k].data == -99)
                                          continue;
					  
		 if((k==4 && tstation[i].max[isom]  <= -99) ||
	            (k==5 && tstation[i].min[isom]  <= -99))
	                                continue;
								  
                 if(tstation[i].elev <= 0)
		                        continue;
					
                 /* check if surrounding station daily maximum is less than daily minimum temperature 
		    for a station */
		    
		 /* if (tdata[j].stn[i].tlevel2[4].data < tdata[j].stn[i].tlevel2[5].data)
		     continue;					*/
					
                 lat=tstation[i].lat;
                 lon=tstation[i].lon;

                 distlon=(lon1-lon)*cos((lat1+lat)/2*conv);
		 
                 dist= pow((double)(lat1-lat),2) +
                       pow((double)(distlon),2);
			   
	         dist=pow(dist,.5)*60;
		 
		 if(dist==0.0)
                             dist=.0001;
			     	
		 df=50*abs(tstation[m].elev-tstation[i].elev)/5280;	
				 		   
                 dist=dist+df;
		 		 
		 if(dist==0.0)
                             dist=.000001;     
                 
                 dist=1/dist;
		 
                 if(k==4)
		     temp=(tdata[j].stn[i].tlevel2[k].data+
		     (tstation[m].max[isom]-tstation[i].max[isom]))*dist;
		     
		 if(k==5)
		     temp=(tdata[j].stn[i].tlevel2[k].data+
		     (tstation[m].min[isom]-tstation[i].min[isom]))*dist;
		     
		 fdata=fdata+temp;

                 fdist=fdist+dist;  
  
                 l++;
		 
		 
	  }

	}
           
        if(l==0)
                fdist=1;

        fvalu=fdata/fdist;

        tdata[j].stn[m].tlevel2[k].estimate=(short int)fvalu;    

        fdif=fabs(tdata[j].stn[m].tlevel2[k].data-fvalu);

        tdata[j].stn[m].tlevel2[k].estimate=fvalu;       

        if(tdata[j].stn[m].tlevel2[k].qual == 0   ||
          tdata[j].stn[m].tlevel2[k].qual == 1   || 
          tdata[j].stn[m].tlevel2[k].qual == 5   ||
          tdata[j].stn[m].tlevel2[k].qual == 6   ||
          tdata[j].stn[m].tlevel2[k].qual == 2)
                              continue;
			      
        /* set the quality code as Questionable if the maximum temperautre is 
	   lower than a minimum temperature*/	   	 
		    
        if (tdata[j].stn[m].tlevel2[4].data < tdata[j].stn[m].tlevel2[5].data)  
	    tdata[j].stn[m].tlevel2[k].qual=3;			      

        if(fdif > tdata[j].stddev) 
                       tdata[j].stn[m].tlevel2[k].qual=3;

        else {

             good++;
             if(tdata[j].stn[m].tlevel2[k].qual==3)
                       tdata[j].stn[m].tlevel2[k].qual=8;

	   }
  
  
  }
  
       if(good > ogood){

                  ogood=good;
		                     
                  goto again;             

		}
		
	
}
		
for(k=0;k<4;k++) {

      if(tdata[j].used[k]==0)
                            continue;
      ogood=0;
      again6:
      good=0;		    
	
      for(m=0;m<max_tstations;m++) {

          if(tstation[m].elev <= 0)
		                        continue;
			       
          lat1=tstation[m].lat;
          lon1=tstation[m].lon;		    
	
          if((tstation[m].max[isom]  <= -99) ||
             (tstation[m].min[isom]  <= -99))
                               continue;
				     	
          if(tdata[j].stn[m].tlevel2[k].data==-99)
	                                        continue;
	    
	  /* if(tdata[j].stn[m].tlevel2[4].data == -99 ||
             tdata[j].stn[m].tlevel2[5].data == -99)
                                         continue;  */
					 
	  if(tdata[j].stn[m].tlevel2[4].qual==1 ||                       
	     tdata[j].stn[m].tlevel2[5].qual==1) 
                                           continue;
					   
          /* check if daily maximum is less than daily minimum temperature 
            for a station */
		    
	  /* if (tdata[j].stn[m].tlevel2[4].data < tdata[j].stn[m].tlevel2[5].data)
	     continue;		 */
	     
	  /* check if the 6h period temperature data is out of daily minimum or
	     maximum temperature */
	     
	  /* if ((tdata[j].stn[m].tlevel2[k].data > tdata[j].stn[m].tlevel2[4].data) ||
	      (tdata[j].stn[m].tlevel2[k].data < tdata[j].stn[m].tlevel2[5].data))
	      
	     continue; */
					     								 						  
          fdist=0.0;
          fdata=0.0;
          l=0;
	
	  for(ii=0;ii<mpe_dqc_max_temp_neighbors;ii++){
   
                 i=tstation[m].index[ii];

                 /* dont estimate unless good or forced good */

                 if(tdata[j].stn[i].tlevel2[k].qual!=0 &&
                    tdata[j].stn[i].tlevel2[k].qual!=8 &&
                    tdata[j].stn[i].tlevel2[k].qual!=6 &&
                    tdata[j].stn[i].tlevel2[k].qual!=3 &&
		    tdata[j].stn[i].tlevel2[k].qual!=2)
                                          continue;

                 /*dont use missing tstations */
                 
                 if(tdata[j].stn[i].tlevel2[k].data ==-99 ||
		    tdata[j].stn[i].tlevel2[k].a < -98 )
                                          continue;
					  
		 if(tstation[i].elev <= 0)
		                        continue;
					
		
		 /* check if daily maximum is less than daily minimum temperature 
		    for a station */
		    
		 /* if (tdata[j].stn[i].tlevel2[4].data < tdata[j].stn[i].tlevel2[5].data)
		     continue; */
		 
		 /* check if the 6h period temperature data is out of daily minimum or
	           maximum temperature */
	     
		 /* if ((tdata[j].stn[i].tlevel2[k].data > tdata[j].stn[i].tlevel2[4].data) ||
		     (tdata[j].stn[i].tlevel2[k].data < tdata[j].stn[i].tlevel2[5].data))
                    continue;*/
		    
									
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
	       	       
                 temp=tdata[j].stn[i].tlevel2[k].a*dist;

                 fdata=fdata+temp;

                 fdist=fdist+dist;                    

                 l++;
               
                 if(l==5)
                            break;
                 
	         }


          if(l < 5) {

          fdist=0.0;
          fdata=0.0;
          l=0;

          for(i=0;i<max_tstations;i++){
   
                 if(i==m)
                      continue;

                 if(tdata[j].stn[i].tlevel2[k].qual!=0 &&
                    tdata[j].stn[i].tlevel2[k].qual!=8 &&
                    tdata[j].stn[i].tlevel2[k].qual!=6 &&
                    tdata[j].stn[i].tlevel2[k].qual!=3 &&
		    tdata[j].stn[i].tlevel2[k].qual!=2)
                                          continue;

                 /*dont use missing tstations */
                 
                 if(tdata[j].stn[i].tlevel2[k].data ==-99 ||
		    tdata[j].stn[i].tlevel2[k].a < -98)
                                           continue;
					   
		 if(tstation[i].elev <= 0)
		                        continue;
					
                 /* check if daily maximum is less than daily minimum temperature 
		    for a station */
		    
		 /* if (tdata[j].stn[i].tlevel2[4].data < tdata[j].stn[i].tlevel2[5].data)
		     continue;	 */
		     
		 /* check if the 6h period temperature data is out of daily minimum or
	           maximum temperature */
	     
		 /* if ((tdata[j].stn[i].tlevel2[k].data > tdata[j].stn[i].tlevel2[4].data) ||
		     (tdata[j].stn[i].tlevel2[k].data < tdata[j].stn[i].tlevel2[5].data))
                    continue;    			*/	
								
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
	       	    
                 temp=tdata[j].stn[i].tlevel2[k].a*dist;

                 fdata=fdata+temp;

                 fdist=fdist+dist;                    

                 l++;
               
	       }

	}
	   if(l==0)
                   fdist=1;

           a=fdata/fdist;
	       	       	       
	   b=a*(float)(tdata[j].stn[m].tlevel2[4].data - 
	             tdata[j].stn[m].tlevel2[5].data)+
	            (float)tdata[j].stn[m].tlevel2[5].data;		          
			            	    
           tdata[j].stn[m].tlevel2[k].estimate=(short int)b;    

           fdif=fabs(tdata[j].stn[m].tlevel2[k].data-b);
        
           if(tdata[j].stn[m].tlevel2[k].qual == 0   ||
             tdata[j].stn[m].tlevel2[k].qual == 1   || 
             tdata[j].stn[m].tlevel2[k].qual == 5   ||
             tdata[j].stn[m].tlevel2[k].qual == 6   ||
             tdata[j].stn[m].tlevel2[k].qual == 2)
                                 continue;
           
	   /* check if the 6h period temperature data is out of daily minimum or
               maximum temperature, set the quality code as Questionalbe */
	     
           if ((tdata[j].stn[m].tlevel2[k].data > tdata[j].stn[m].tlevel2[4].data) ||
	       (tdata[j].stn[m].tlevel2[k].data < tdata[j].stn[m].tlevel2[5].data))
	      tdata[j].stn[m].tlevel2[k].qual = 3;
	       
	       
           if(fdif > tdata[j].stddev) 
                          tdata[j].stn[m].tlevel2[k].qual=3;

           else {

                good++;
                if(tdata[j].stn[m].tlevel2[k].qual==3)
                          tdata[j].stn[m].tlevel2[k].qual=8;

	      }
  
  
  }
 
       if(good > ogood){

                  ogood=good;
                   
                  goto again6;             

		}

 
}


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/quality_control_tstations.c,v $";
 static char rcs_id2[] = "$Id: quality_control_tstations.c,v 1.6 2007/10/23 17:08:17 lawrence Exp $";}
/*  ===================================================  */

}

