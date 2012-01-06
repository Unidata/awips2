#include <gageqc_gui.h>

#include "map_library.h"
#include "mpe_log_utils.h"

int group_edit = 0;
extern Widget group_dialog;
extern int pcpn_day;
extern int pcp_in_use [];
extern struct pdata pdata [];
extern Widget diswidget[6];
extern Widget gpbutton;
extern Widget rowcol1;
extern int pcpn_time_step;
extern int pcp_flag;
extern int points_flag;
extern int grids_flag;
extern int map_flag;
extern struct tdata tdata [ ];
extern int pcpn_time;
extern struct station * station;
extern struct station * tstation;
extern int max_stations;
extern int max_tstations;

void apply_group()

{

int k,time_pos;
Arg args[10];
         
for(k=0;k<5;k++) {
      
             if(k < 4)
                             time_pos=pcpn_day*4 +k;

             else
                             time_pos=40+pcpn_day;

             pcp_in_use[time_pos]=-1;
      

             if(pdata[pcpn_day].used[k]!=0)
                           pdata[pcpn_day].used[k]=2;
   }
 
for(k=1;k<7;k++)
{
               XtSetSensitive(diswidget[k],False);
}
         
if(pcpn_time_step==0)
                   time_pos=pcp_flag;

else
                   time_pos=40+pcpn_day;
      
if(points_flag==1 && pcp_in_use[time_pos]==-1)
                   k=0;

else if(points_flag==1 && grids_flag==-1 && map_flag==-1)
                   k=0; 
                   
else if(points_flag==-1 && grids_flag==1 && map_flag==-1)
                   k=1;   
                    
else if(points_flag==-1 && grids_flag==-1 && map_flag==1)
                   k=2;   
                     
else if(points_flag==1 && grids_flag==1 && map_flag==-1)
                   k=3;   
                        
else if(points_flag==1 && grids_flag==-1 && map_flag==1)
                   k=4;   
                          
else if(points_flag==-1 && grids_flag==-1 && map_flag==-1)
                   k=5;   
                        
XtSetArg(args[0],XmNmenuHistory,diswidget[k]);
XtSetValues(rowcol1,args,1);   

update_bad_values(pcpn_day);

estimate_daily_stations(pcpn_day, station, max_stations); 

estimate_partial_stations(pcpn_day, station, max_stations);
  
quality_control_stations(pcpn_day, station, max_stations);

check_consistency(pcpn_day, station, max_stations);

restore_bad_values(pcpn_day, station, max_stations);

XtDestroyWidget(group_dialog);

send_expose ( );
group_edit=0;

XtSetSensitive(gpbutton,True);                 

return;

}

void apply_tgroup()

{

int k,time_pos;
Arg args[10];

logMessage("start\n");

if(pcpn_time_step==1) {

        if(tdata[pcpn_day].used[4]!=0)
                           tdata[pcpn_day].used[4]=2;
			   
	pcp_in_use[190+pcpn_day]=-1;
	
	}		   

else if(pcpn_time_step==2) {

        if(tdata[pcpn_day].used[5]!=0)
                           tdata[pcpn_day].used[5]=2;
			   
	pcp_in_use[200+pcpn_day]=-1;
	
	}
	
else {			   

        if(tdata[pcpn_day].used[pcpn_time]!=0)
                           tdata[pcpn_day].used[pcpn_time]=2;
			   
	pcp_in_use[150+pcp_flag]=-1;		   
			   
			   }
			   
logMessage("start1\n");

if(pcpn_time_step==1 || pcpn_time_step==2) {
			          
for(k=0;k<4;k++) {
      
             time_pos=150+pcpn_day*4 + k;

             pcp_in_use[time_pos]=-1;
      
             if(tdata[pcpn_day].used[k]!=0)
                           tdata[pcpn_day].used[k]=2;
			  			   
   }
     
}  

for(k=1;k<7;k++)
{
               XtSetSensitive(diswidget[k],False);
}
        
if(pcpn_time_step==0)
                   time_pos=150+pcp_flag;

else if(pcpn_time_step==1)
                   time_pos=190+pcpn_day;
 
else if(pcpn_time_step==2)
                   time_pos=200+pcpn_day;
          
if(points_flag==1 && pcp_in_use[time_pos]==-1)
                   k=0;

else if(points_flag==1 && grids_flag==-1 && map_flag==-1)
                   k=0; 
                   
else if(points_flag==-1 && grids_flag==1 && map_flag==-1)
                   k=1;   
                    
else if(points_flag==-1 && grids_flag==-1 && map_flag==1)
                   k=2;   
                     
else if(points_flag==1 && grids_flag==1 && map_flag==-1)
                   k=3;   
                        
else if(points_flag==1 && grids_flag==-1 && map_flag==1)
                   k=4;   
                          
else if(points_flag==-1 && grids_flag==-1 && map_flag==-1)
                   k=5;   
                        
XtSetArg(args[0],XmNmenuHistory,diswidget[k]);
XtSetValues(rowcol1,args,1);   

update_bad_tvalues(pcpn_day);

estimate_daily_tstations(pcpn_day, tstation, max_tstations);  

quality_control_tstations(pcpn_day, tstation, max_tstations);

restore_bad_tvalues(pcpn_day, tstation, max_tstations);

XtDestroyWidget(group_dialog);

send_expose ( );
group_edit=0;

XtSetSensitive(gpbutton,True);                 

return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/group_edit_calls.c,v $";
 static char rcs_id2[] = "$Id: group_edit_calls.c,v 1.3 2007/10/18 16:09:00 lawrence Exp $";}
/*  ===================================================  */

}

