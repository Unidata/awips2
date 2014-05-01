/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <Xm/Xm.h>

#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_library.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

extern int max_stations;

void plot_precip_stations (int type, int map_number)
{
   char ** color_map_a = NULL;
   char ** color_map_n = NULL;
   extern float reverse_filter_value;
   extern int elevation_filter_value;
   extern int dcmode;
   extern int funct[];
   extern int tcmode;
   extern int dmvalue;
   extern struct ts ts[20];
   extern int tsmax;
   extern float filter_value;
   extern int frzlvl_flag;
   extern int gage_char[2];
   extern int find_station_flag;
   extern struct pdata pdata[10];
   extern XFontStruct *info_font[10];
   extern struct station *station;
   extern int dflag[10];
   extern int qflag[10];
   extern int pcpn_time, pcpn_day;
   extern int pcpn_time_step;
   int typemax = MAX_GAGEQC_TYPE;
   char *typename[10];
   int i, xc, yc, text_width, length, m;
   int x1, y1, xadd, yadd;
   long ymin;
   Dimension width, height;
   signed long XSIZE, YSIZE;
   float lat, lon;
   char tbuf[100], mbuf[100];
   int yheight;
   int time_pos;
   int mm;
   
   color_map_a = get_color_map_a ( );
   color_map_n = get_color_map_n ( );

/* Initialize ymin. */
   ymin = 0;

   typename[0] = "Verified";
   typename[1] = "Screened";
   typename[2] = "Time Distributed";
   typename[3] = "Manual";
   typename[4] = "Questionable";
   typename[5] = "Partial";
   typename[6] = "Estimated";
   typename[7] = "Bad";
   typename[8] = "Missing";

   if (pcpn_time_step == 0)
   {
      time_pos = pcpn_time;
   }
   else
   {
      time_pos = 4;
   }

   yheight = info_font[4]->ascent;

   if (type == 0)
      return;

   width = _get_map_width (map_number);
   height = _get_map_height (map_number);

   XSIZE = (long) width *10L;
   YSIZE = (long) height *10L;

   for (i = 0; i < max_stations; i++)
   {
      lat = station[i].lat;
      lon = station[i].lon;

      /* Convert the station's lat/lon coordinates to pixel coordinates. */
      lon *= -1;
      mConvertLatLon2XY (lat, lon, &x1, &y1);

      mSetColor (color_map_n[15]);

      if (station[i].elev >= 0 && station[i].elev < elevation_filter_value)
      {
	 continue;
      }

      if (tcmode == 1 && pdata[pcpn_day].stn[i].tcons == 1)
      {
	 continue;
      }

      if (tcmode == 0 && pdata[pcpn_day].stn[i].tcons == -1)
      {
	 continue;
      }
      
      if (dcmode == 0 && pdata[pcpn_day].stn[i].scons[time_pos] == -1)
      {
	 continue;
      }

      if (dcmode == 1 && pdata[pcpn_day].stn[i].scons[time_pos] == 1)
      {
	 continue;
      }

      if (station[i].tip == 0 && gage_char[0] == -1)
      {
	 continue;
      }

      if (station[i].tip == 1 && gage_char[1] == -1)
      {
	 continue;
      }

      for (m = 0; m < tsmax; m++)
      {
	 if (strncmp (&station[i].parm[3], ts[m].abr, 2) == 0
	     && dflag[m + 1] == 1)
	 {
	    break;
	 }
      }

      if (m == tsmax)
      {
	 continue;
      }

      for (m = 0; m < 9; m++)
      {

	 if ((m == pdata[pcpn_day].stn[i].frain[time_pos].qual)
	       && qflag[m] == 1)
	    break;
	 else if (m == 7 && qflag[7] == 1 && pdata[pcpn_day].stn[i].frain[time_pos].data==-1 &&
	          pdata[pcpn_day].stn[i].frain[time_pos].qual == -1)
	    break;   

      }
     
      /*
      if (m == 9 && (type == 4 || type == 5 ))
	 continue;
      */
      
      if (m == 9)
         continue;            
	 
/* locate station in data stream */


 /*     if ((type == 4 || type == 5)
	  && pdata[pcpn_day].stn[i].frain[time_pos].data == -1)
	 continue;

      if ((type == 5) && pdata[pcpn_day].stn[i].frain[time_pos].data == -1)
	 continue;
        */
		 
      if ((type == 4 || type == 5) && (pdata[pcpn_day].used[time_pos] == 0) &&
          (pdata[pcpn_day].level == 0 ))
	 continue;	 
      
      if ((type == 4 || type == 5)
	  && (pdata[pcpn_day].stn[i].frain[time_pos].data < filter_value) 
	  && (pdata[pcpn_day].stn[i].frain[time_pos].data != -1)
	  && (pdata[pcpn_day].stn[i].frain[time_pos].qual != -1))
	 continue;
	 
      if ((type == 4 || type == 5)
	  && (pdata[pcpn_day].stn[i].frain[time_pos].data >
	      reverse_filter_value)
	  && (pdata[pcpn_day].stn[i].frain[time_pos].data < 20.00))
	 continue;

      mDrawLine (M_EXPOSE, map_number, x1 + 1, y1 + 1, x1 + 1, y1 - 1);
      mDrawLine (M_EXPOSE, map_number, x1 + 1, y1 - 1, x1 - 1, y1 - 1);
      mDrawLine (M_EXPOSE, map_number, x1 - 1, y1 - 1, x1 - 1, y1 + 1);
      mDrawLine (M_EXPOSE, map_number, x1 - 1, y1 + 1, x1 + 1, y1 - 1);

      if (type == 1)
      {
	 strcpy (tbuf, station[i].hb5);
      }

      else if (type == 2)
      {

	 strcpy (tbuf, &station[i].parm[4]);
	 tbuf[1] = 0;

      }

      else if (type == 3)
	 strcpy (tbuf, station[i].name);

      else if (type == 4)
      {

	 if ((pdata[pcpn_day].used[time_pos] == 0) &&
	     (pdata[pcpn_day].level == 0 ))
	    continue;

	 if (pdata[pcpn_day].stn[i].frain[time_pos].data == -2)
	    continue;
	 
	 /* if point data is missing, use character 'm' */
	    
	 if ((pdata[pcpn_day].stn[i].frain[time_pos].data == -1) 
	     && (pdata[pcpn_day].stn[i].frain[time_pos].qual == -1))   
             strcpy(mbuf, "m");
	 else     
	     sprintf (mbuf, "%5.2f", pdata[pcpn_day].stn[i].frain[time_pos].data);

	 if (mbuf[0] == '0')
	    strcpy (tbuf, &mbuf[1]);

	 else
	    strcpy (tbuf, mbuf);

      }

      else if (type == 5)
      {

	 if ( (pdata[pcpn_day].used[time_pos] == 0) &&
	      (pdata[pcpn_day].level == 0))
	    continue;

	 if (pdata[pcpn_day].stn[i].frain[time_pos].data == -2)
	    continue;         
	    	 
	 sprintf (mbuf, "%5.2f",
		  pdata[pcpn_day].stn[i].frain[time_pos].stddev);

	 strcpy (tbuf, mbuf);

      }

      if (m == 9)
	 m = 7;

      /* XSetForeground(display,gc,amap[m]);   */
      mSetColor (color_map_a[m]);
      length = strlen (tbuf);
      text_width = XTextWidth (info_font[4], tbuf, length);

      xadd = station[i].xadd;
      yadd = station[i].yadd;

      if (xadd < 0)
	 xc = x1 - text_width;

      else
	 xc = x1 + 3;

      if (yadd < 0)
	 yc = y1;

      else
	 yc = y1 + yheight;

      /* XDrawString(display,pix,gc,xc,yc,tbuf,length); */
      mDrawText (M_EXPOSE, map_number, xc, yc, tbuf);

      if (i == find_station_flag)
      {
	 find_station_flag = -1;
	 /* XDrawLine(display,pix,gc,xc,yc,xc+text_width,yc); */
	 mDrawLine (M_EXPOSE, map_number, xc, yc, xc + text_width, yc);
      }

      if (pdata[pcpn_day].stn[i].snoflag[time_pos] != 0)
      {
	 /* XSetForeground(display,gc,amap[1]);   */
	 mSetColor (color_map_a[1]);

	 /* XDrawLine(display,pix,gc,xc,yc,xc+text_width,yc); */
	 mDrawLine (M_EXPOSE, map_number, xc, yc, xc + text_width, yc);
      }


      if (pdata[pcpn_day].stn[i].sflag[time_pos] == 1)
      {
	 /* XSetForeground(display,gc,amap[0]);  */
	 mSetColor (color_map_a[0]);
	 /* XDrawLine(display,pix,gc,xc,yc,xc+text_width,yc); */
	 mDrawLine (M_EXPOSE, map_number, xc, yc, xc + text_width, yc);
      }


      if (frzlvl_flag == 1 && station[i].tip == 0 &&
	  (pdata[pcpn_day].stn[i].frain[time_pos].estimate > .005 ||
	   pdata[pcpn_day].stn[i].frain[time_pos].data > .005))
      {
	 if (time_pos == 4)
	 {
	    for (mm = 0; mm < 4; mm++)
	    {


	       if (pdata[pcpn_day].stn[i].frzlvl[mm] < -98 ||
		   station[i].elev < 0)
		  continue;

	       if ((pdata[pcpn_day].stn[i].frzlvl[mm] - dmvalue) <
		   station[i].elev)
		  break;


	    }

	    if (mm == 4)
	       continue;

	 }

	 else
	 {

	    if (pdata[pcpn_day].stn[i].frzlvl[time_pos] < -98 ||
		station[i].elev < 0)
	       continue;

	    if ((pdata[pcpn_day].stn[i].frzlvl[time_pos] - dmvalue) >=
		station[i].elev)
	       continue;


	 }

	 /* XSetForeground(display,gc,amap[1]); */
	 mSetColor (color_map_a[1]);

	 /* XDrawLine(display,pix,gc,xc,yc,xc+text_width,yc); */
	 mDrawLine (M_EXPOSE, map_number, xc, yc, xc + text_width, yc);

      }


   }


   /* XSetFont(display,gc,font[4]); */
   /* Differences in font and/or differences in font size? */

   yheight = info_font[4]->ascent;

   /* Draw the precip QC legend. */
   for (m = 0; m < typemax; m++)
   {

      mSetColor (color_map_a[funct[m]]);

      mDrawLine (M_EXPOSE, map_number, 15, ymin / 10 + (m + 1) * yheight -
		 yheight / 2, 35,
		 ymin / 10 + (m + 1) * yheight - yheight / 2);

      mSetColor (color_map_n[15]);

      mDrawText (M_EXPOSE, map_number, 35, ymin / 10 + (m + 1) * yheight,
		 typename[m]);

   }

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/GageQCGui/RCS/plot_precip_stations.c,v $";
 static char rcs_id2[] = "$Id: plot_precip_stations.c,v 1.7 2007/05/23 21:52:58 whfs Exp $";}
/*  ===================================================  */

}
