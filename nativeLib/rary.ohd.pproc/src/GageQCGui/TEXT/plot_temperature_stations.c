 
#include <Xm/Xm.h>
#include <math.h>

#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_library.h"
#include "map_resource.h"

static char * color_map_a [ ] = { "cyan1",
                                  "salmon",
                                  "Orange1",
                                  "Yellow1",
                                  "magenta1",
                                  "green1",
                                  "Green4",
                                  "Gray74",
                                  "white",
				  "cyan1" };

void plot_temperature_stations(int type, int map_number)

{
extern int funct[];

extern struct station * tstation;
extern int elevation_filter_value;
extern int temperature_filter_value;
extern int temperature_reverse_filter_value;
extern struct ts ts[20];
extern int tsmax;
extern int find_station_flag;
extern struct tdata tdata[10];
extern Font font[];
extern XFontStruct *info_font[];
extern int max_tstations;
extern int pcpn_time,pcpn_day;
extern int pcpn_time_step;
int typemax=9;
int temp = 0;
extern int dflag[10];
extern int qflag[10];
char *typename[10];
int i,xc,yc,text_width,length,m;
int x1,y1,xadd,yadd;
float xrat=0.,yrat=0.,mult=0.,dmult=0.,cmult=0.;
long ymin;
Dimension width,height;
signed long XSIZE,YSIZE;
float lat,lon;
char tbuf[100],mbuf[100];
int yheight;   
int time_pos;
//extern int funct[];
//int mm;


/*initialize ymin*/
ymin = 0;


typename[0]="Verified";
typename[1]="Screened";
typename[2]="Time Distributed";
typename[3]="Manual";
typename[4]="Questionable";
typename[5]="Partial";
typename[6]="Estimated";
typename[7]="Bad";
typename[8]="Missing";

if(pcpn_time_step==1)
                  time_pos=4;
		  
else if(pcpn_time_step==2)
                  time_pos=5;
		  
else
		  		  
    time_pos=pcpn_time;


yheight=info_font[4]->ascent;

if(type==0)
         return;


width = _get_map_width(map_number);
height = _get_map_height (map_number);

XSIZE=(long)width*10L;
YSIZE=(long)height*10L;

xrat=(float)XSIZE/(float)12800;
yrat=(float)YSIZE/(float)9600;


if(xrat < yrat)
             mult=xrat;

else
             mult=yrat;

cmult=mult*dmult;

for(i=0;i<max_tstations;i++) {
     
     lat=tstation[i].lat;
     lon=tstation[i].lon;

lon *= -1;
mConvertLatLon2XY ( lat, lon, & x1, & y1 );
mSetColor ( color_map_a [ 8 ]  );


     if(tstation[i].elev >= 0 && tstation[i].elev < elevation_filter_value)
                  continue;
		                                             
     for(m=0;m<tsmax;m++) {
     
           if(strncmp(&tstation[i].parm[3],ts[m].abr,2)==0 && dflag[m+1] == 1)
                                   break;

                           }
                         
     if(m==tsmax)
                continue;

     for(m=0;m<9;m++) 
     {
        if(m==tdata[pcpn_day].stn[i].tlevel2[time_pos].qual && qflag[m]==1)
	{
           break;
	}
	else if (m == 7 && qflag[7] == 1 && 
	         tdata[pcpn_day].stn[i].tlevel2[time_pos].data == -99 &&
		 tdata[pcpn_day].stn[i].tlevel2[time_pos].qual == -99)
           break;		 
     }

     if(m==9)
       continue;

/* locate station in data stream */
    
     /*if((type==4 || type==5) 
         && tdata[pcpn_day].stn[i].tlevel2[time_pos].data == -99)
  		continue;*/
		
     if ((type == 4 || type == 5) &&
         (tdata[pcpn_day].used[time_pos] == 0) && tdata[pcpn_day].level == 0)
	 continue;		


     /* Added logic to filter on temperature value - 10/11/2007. */
     if ((type == 4 || type == 5) &&
         (tdata[pcpn_day].stn[i].tlevel2[time_pos].data < temperature_filter_value ) &&
         (tdata[pcpn_day].stn[i].tlevel2[time_pos].data != -99 ) )
     {
        continue;
     }

     /* Added logic to filter on temperature value - 10/11/2007. */
     if ((type == 4 || type == 5) &&
         (tdata[pcpn_day].stn[i].tlevel2[time_pos].data > temperature_reverse_filter_value ) &&
         (tdata[pcpn_day].stn[i].tlevel2[time_pos].data < 110 ) )
     {
        continue;
     }

     /* Draw the temperature station point. */
     mDrawLine ( M_EXPOSE, map_number, x1+1, y1+1, x1+1, y1-1);
     mDrawLine ( M_EXPOSE, map_number, x1+1, y1-1, x1-1, y1-1);
     mDrawLine ( M_EXPOSE, map_number, x1-1, y1-1, x1-1, y1+1);
     mDrawLine ( M_EXPOSE, map_number, x1-1, y1+1, x1+1, y1-1);

     if(type==1)
              strcpy(tbuf,tstation[i].hb5);

     else if(type==2) {

              strcpy(tbuf,&tstation[i].parm[4]);
              tbuf[1]=0;

	    }

     else if(type==3)
              strcpy(tbuf,tstation[i].name);

     else if(type==4) {
     
              if ((tdata[pcpn_day].used[time_pos]==0) &&
	          (tdata[pcpn_day].level == 0))
                                       continue;


              if (tdata[pcpn_day].stn[i].tlevel2[time_pos].data == -99 &&
                  tdata[pcpn_day].stn[i].tlevel2[time_pos].qual == -99)
		  strcpy(mbuf, "m");
	      else	  
                  sprintf(mbuf,"%d",tdata[pcpn_day].stn[i].tlevel2[time_pos].data);
 
              strcpy(tbuf,mbuf);

	    }
	    
     else if(type==5) {
     
              if(tstation[i].tip==0)
	                        continue;
				
              strcpy(tbuf,tstation[i].hb5);
	      
	      }
     else if(type==6) {

              sprintf(tbuf,"%d",tstation[i].elev);
     }

     m=tdata[pcpn_day].stn[i].tlevel2[time_pos].qual;
                                             

if(m<=9 && m>=0)
{
   mSetColor ( color_map_a [ m ] );
}

/* set font color for missing data */

if (qflag[7] == 1 && tdata[pcpn_day].stn[i].tlevel2[time_pos].data == -99 &&
    tdata[pcpn_day].stn[i].tlevel2[time_pos].qual == -99)
    
    mSetColor(color_map_a[7]);
    
length=strlen(tbuf);
     text_width=XTextWidth(info_font[4],tbuf,length); 

     xadd=tstation[i].xadd;
     yadd=tstation[i].yadd;

     if(xadd < 0) 
                    xc=x1 - text_width;
                        
     else
                    xc=x1+3;

     if(yadd < 0)
                    yc=y1;

     else
                    yc=y1+yheight;

       mDrawText ( M_EXPOSE, map_number, xc, yc, tbuf );

     if(i==find_station_flag) {

              find_station_flag=-1;
		mDrawLine ( M_EXPOSE, map_number, xc, yc, xc+text_width, yc );

	    }
	   
       }          


yheight=info_font[4]->ascent;



i=0;
temp = 0;
for(m=0;m<typemax;m++) {

              if( m==5 )
	      {
	         temp++;
	         continue; 
              }
    
	
	  mSetColor ( color_map_a [ funct[m] ] );
mDrawLine ( M_EXPOSE, map_number, 15 ,ymin/10+(m+1-temp)*yheight-
			yheight/2, 35, ymin/10+(m+1-temp)*yheight-yheight/2);


	     mSetColor ( color_map_a [ 8 ] );
	     mDrawText ( M_EXPOSE, map_number,35,ymin/10+(m+1-temp)*yheight,
	     		typename[m]);

	    }

return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/plot_temperature_stations.c,v $";
 static char rcs_id2[] = "$Id: plot_temperature_stations.c,v 1.6 2007/10/18 20:27:04 lawrence Exp $";}
/*  ===================================================  */

}
