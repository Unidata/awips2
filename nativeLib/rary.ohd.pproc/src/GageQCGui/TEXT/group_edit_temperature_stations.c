#include "gageqc_gui.h"
#include "map_library.h"

extern int group_qual;
extern int plot_view;
extern int points_flag;
extern int qpf_on;
extern int flf_on;
extern int maxmin_on;
extern int isom;
extern int pcpn_time_step;
extern int map_flag;
extern int pcp_flag;
extern int pcpn_day;
extern int grids_flag;
extern int dflag [ ];
extern int pcpn_time;
extern struct tdata tdata [ ];
extern int old_isom;
extern int max_tstations;
extern int temperature_filter_value;
extern struct station * tstation;
extern int gage_char[];
extern int tsmax;
static struct ts ts [ ];
extern int qflag [ ];
extern struct dval dval;



void group_edit_temperature_stations(int map_num, int x,int y)

{

int time_pos;
int i,m,k;
float lat,lon;
double testdist,maxdist;
int isave;
extern int pcp_in_use [ ];
   int x1;
   int y1;


   extern Widget rpbutton;



if(pcpn_time_step==0)
                  time_pos=pcpn_time;

else if(pcpn_time_step==1)
                  time_pos=4;

else if(pcpn_time_step==2)
                  time_pos=5;

isave=-1;
maxdist=9999;

for(i=0;i<max_tstations;i++)
{
     if(tdata[pcpn_day].stn[i].tlevel2[time_pos].data ==-99 )
     {
        continue;
     }
    
     if(tdata[pcpn_day].stn[i].tlevel2[time_pos].data < temperature_filter_value )
     {
        continue;
     }

     lat=tstation[i].lat;
     lon=tstation[i].lon;

                                    
     for(m=0;m<tsmax;m++) {
     
           if(strncmp(&tstation[i].parm[3],ts[m].abr,2)==0 && dflag[m+1] == 1)
                                   break;

                           }
                            
                           
     if(m==tsmax)
                continue;

           
     for(m=0;m<9;m++) {

             if(m==tdata[pcpn_day].stn[i].tlevel2[time_pos].qual &&
                 qflag[m]==1) 
                            break;

		       }

     if(m==9) 
              continue;


      mConvertLatLon2XY (lat, -1 * lon, &x1, &y1);

      testdist = pow ((double) (x - (float)x1), 2) + 
                 pow ((double) (y - (float)y1), 2);
      testdist = pow (testdist, .5);


if(testdist < maxdist) {

                        isave=i;
                        maxdist=testdist;

			   }
                            
                          } 
               
if(isave==-1)
            return;


if(tdata[pcpn_day].stn[isave].tlevel2[time_pos].qual != group_qual)
{
   XtSetSensitive(rpbutton,True);
}

tdata[pcpn_day].stn[isave].tlevel2[time_pos].qual=group_qual;

if(pcpn_time_step==0)
                  time_pos=150+pcpn_day*4+pcpn_time;

else if(pcpn_time_step==1)
                  time_pos=190+pcpn_day;

else if(pcpn_time_step==2)
                  time_pos=200+pcpn_day;
		  
pcp_in_use[time_pos]=-1;

tdata[pcpn_day].used[pcpn_time]=2;

if(pcpn_time_step==1 || pcpn_time_step==2) {
            
for(k=0;k<4;k++) {
                 
             time_pos=150+pcpn_day*4+k;

             pcp_in_use[time_pos]=-1;
      
             if(tdata[pcpn_day].used[k]!=0)
                           tdata[pcpn_day].used[k]=2;
}

}

send_expose ( );
return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/group_edit_temperature_stations.c,v $";
 static char rcs_id2[] = "$Id: group_edit_temperature_stations.c,v 1.4 2007/10/18 16:09:02 lawrence Exp $";}
/*  ===================================================  */

}
