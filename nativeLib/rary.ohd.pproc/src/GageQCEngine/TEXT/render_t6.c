#include "gageqc_types.h"
#include "gageqc_defs.h"
#include "mpe_log_utils.h"

#include <stdlib.h>



/* Converts point precipitation data in struct tdata to hrap grid.  The
   closest tstations are precalculated for each grid point - this
   speeds computations for data sets with many precipitation points.
   If there are no good precipitation points for a grid point, then
   a recalculation is made using all precipitation points. 1/R**2
   interpolation is used.  If requested, the final interpolation
   is scaled using seasonal isohyets. The grid is saved as a disk
   file and used later for a raster or vector (HRAP) plot. */

void render_t6(int pcpn_day,int pcpn_time,int pcpn_time_step,
               int max_tstations, const struct station * tstation,
                  const struct hrap_grid * hrap_tgrid,
                  const struct tdata * tdata,
                  int * pcp_in_use )

{
	int i, j, h, hh, time_pos, htotal;
	double distance, dist1, dist2, dist, value;
	double temp;
	extern struct pcp *pcp;
extern int mpe_dqc_max_temp_neighbors;
extern struct pcp *spf;
extern struct pcp *tpf;

float conv=.0174;
float df;
float a,b;
int total1,total2;

total1=0;total2=0;


if(pcp_in_use[190+pcpn_day]== -1 ||
   pcp_in_use[200+pcpn_day]== -1)
               return;

time_pos=pcpn_time;

read_file("pcp",190+pcpn_day,spf);
read_file("pcp",200+pcpn_day,tpf);

for(i=0;i<hrap_tgrid->maxi;i++) {

for(j=0;j<hrap_tgrid->maxj;j++) {
 
           if(hrap_tgrid->owner[i][j]==-1) {
           
                                 pcp->value[i][j]=-9999;
                                 continue;
                                 
                                 }

           
           value=0.0;
           distance=0.0;
           htotal=0;

           for(h=0;h<mpe_dqc_max_temp_neighbors;h++) {

               hh=hrap_tgrid->gage[i][j].tindex[h];

               if(tdata[pcpn_day].stn[hh].tlevel2[time_pos].data ==-99 ||
	          tdata[pcpn_day].stn[hh].tlevel2[time_pos].a < -98)
                                                 continue;
                  
               if(tdata[pcpn_day].stn[hh].tlevel2[time_pos].qual != 0 &&
                  tdata[pcpn_day].stn[hh].tlevel2[time_pos].qual != 8 &&
                  tdata[pcpn_day].stn[hh].tlevel2[time_pos].qual != 6 &&
                  tdata[pcpn_day].stn[hh].tlevel2[time_pos].qual != 3 &&
		  tdata[pcpn_day].stn[hh].tlevel2[time_pos].qual != 2)
                          continue;
			  
               if(tstation[hh].elev <= 0)
		                        continue;
		
               dist1=hrap_tgrid->coord[i][j].lat-tstation[hh].lat;    
               dist2=(hrap_tgrid->coord[i][j].lon-tstation[hh].lon)*
	             cos((hrap_tgrid->coord[i][j].lat+tstation[hh].lat)/2*conv);
		     
               dist=pow(dist1,2)+pow(dist2,2);

               dist=pow(dist,.5)*60;

               if(dist < .00001)
                        dist=.00001;
                              
               df=50*abs(hrap_tgrid->elev[i][j]-tstation[hh].elev)/5280;

	       dist=dist+df;
	       
	       dist=1/dist;
	       	       
               temp=tdata[pcpn_day].stn[hh].tlevel2[time_pos].a*dist;
             
               value=value+temp;

               distance=distance+dist;                    

               htotal++;
               
               if(htotal ==5) {
	       
	                    total1++;
                            break;

                         }

	     }


           if(htotal < 5) {
          
           total2++;
           value=0.0;
           distance=0.0;
           htotal=0;

           for(h=0;h<max_tstations;h++) {

               if(tdata[pcpn_day].stn[h].tlevel2[time_pos].data ==-99 ||
	          tdata[pcpn_day].stn[h].tlevel2[time_pos].a < -98)
                                                 continue;

               if(tdata[pcpn_day].stn[h].tlevel2[time_pos].qual != 0 &&
                  tdata[pcpn_day].stn[h].tlevel2[time_pos].qual != 8 &&
                  tdata[pcpn_day].stn[h].tlevel2[time_pos].qual != 6 &&
                  tdata[pcpn_day].stn[h].tlevel2[time_pos].qual != 3 &&
      		  tdata[pcpn_day].stn[h].tlevel2[time_pos].qual != 2)
                            continue;

               if(tstation[h].elev <= 0)
		                        continue;
	      				
               dist1=hrap_tgrid->coord[i][j].lat-tstation[h].lat;    
               dist2=(hrap_tgrid->coord[i][j].lon-tstation[h].lon)*
	             cos((hrap_tgrid->coord[i][j].lat+tstation[h].lat)/2*conv);
		     
               dist=pow(dist1,2)+pow(dist2,2);

               dist=pow(dist,.5)*60;

               if(dist < .00001)
                        dist=.00001;
                              
               df=50*abs(hrap_tgrid->elev[i][j]-tstation[h].elev)/5280;

	       dist=dist+df;
	       
	       dist=1/dist;
	       	       
               temp=tdata[pcpn_day].stn[h].tlevel2[time_pos].a*dist;
               
               value=value+temp;

               distance=distance+dist;                    

               htotal++;
               

	     }

      }

           if(htotal == 0) 
               pcp->value[i][j]=-9999;
 
           else {
	   
	       a=value/distance;
	       	     
	       b=a*(float)(spf->value[i][j] - tpf->value[i][j])+
	         (float)tpf->value[i][j];
		
	       pcp->value[i][j]=(int)b;
	       
	       if(spf->value[i][j]==-9999 || tpf->value[i][j] == -9999)
	                    pcp->value[i][j]=-9999;
     
               }
     
	 }

}

time_pos=150+pcpn_day*4+3-pcpn_time;

pcp_in_use[time_pos]=1;

write_file("pcp",time_pos,pcp);

logMessage("totals are %d %d\n",total1,total2);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/render_t6.c,v $";
 static char rcs_id2[] = "$Id: render_t6.c,v 1.2 2007/05/23 20:56:41 whfs Exp $";}
/*  ===================================================  */

}
