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
#include "map_resource.h"

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

void plot_freezing_stations ( int type, int map_number )
{
   char ** color_map_a = NULL;
   char ** color_map_n = NULL;
   extern int find_station_flag;
   extern struct zdata zdata[10];
   extern struct station *zstation;
   extern int max_zstations;
   extern int pcpn_time, pcpn_day;
   extern float freezing_filter_value;
   extern float freezing_reverse_filter_value;
   extern XFontStruct *info_font[];
   int legend_entry_pos;
   int typemax = 9;
   static char *typename[10] = { "Verified",
                                 "Screened",
                                 "Time Distributed",
                                 "Manual",
                                 "Questionable",
                                 "Partial",
                                 "Calculated",
                                 "Bad",
                                 "Missing" };
   Dimension width;
   Dimension height;
   int i, xc = 0, yc = 0, text_width = 0, length, m;
   int x1, y1, xadd, yadd;
   long ymin = 0;
   float lat, lon;
   char tbuf[100], mbuf[100];
   int yheight;
   int time_pos;
   extern int funct[];
  
   color_map_a = get_color_map_a ( );
   color_map_n = get_color_map_n ( );

   /*typename[0] = "Verified";
   typename[1] = "Screened";
   typename[2] = "Time Distributed";
   typename[3] = "Manual";
   typename[4] = "Questionable";
   typename[5] = "Partial";
   typename[6] = "Calculated";
   typename[7] = "Bad";
   typename[8] = "Missing"; */

   time_pos = pcpn_time;

   yheight=info_font[4]->ascent;

   if (type == 0)
   {
      return;
   }

   width = _get_map_width (map_number);
   height = _get_map_height (map_number);

   for (i = 0; i < max_zstations; i++)
   {
      /* Filter out any stations according to the point
         and reverse point value filters. */
      if ( ( zdata[pcpn_day].stn[i].zlevel2[time_pos].data > 0 ) &&
           ( zdata[pcpn_day].stn[i].zlevel2[time_pos].data < freezing_filter_value ) )
      {
         continue;
      }

      if ( ( zdata[pcpn_day].stn[i].zlevel2[time_pos].data > freezing_reverse_filter_value ) &&
         ( zdata[pcpn_day].stn[i].zlevel2[time_pos].data < 20.0 ) )
      {
         continue;
      }

      lat = zstation[i].lat;
      lon = zstation[i].lon;

      /* Convert the station's lat/lon coordinates to pixel coordinates. */
      lon *= -1;
      mConvertLatLon2XY (lat, lon, &x1, &y1);
      mSetColor ("White");

      /* XSetForeground(display,gc,qmap[15]); */

      if ( ( type == 4 || type == 5 ) && 
           ( zdata[pcpn_day].stn[i].zlevel2[time_pos].data == -1 ) )
      {
	 continue;
      }

      /* Draw the freezing level station point. */
      mDrawLine (M_EXPOSE, map_number, x1 + 1, y1 + 1, x1 + 1, y1 - 1);
      mDrawLine (M_EXPOSE, map_number, x1 + 1, y1 - 1, x1 - 1, y1 - 1);
      mDrawLine (M_EXPOSE, map_number, x1 - 1, y1 - 1, x1 - 1, y1 + 1);
      mDrawLine (M_EXPOSE, map_number, x1 - 1, y1 + 1, x1 + 1, y1 - 1);

      if (type == 1)
      {
	 strcpy (tbuf, zstation[i].hb5);
      }
      else if (type == 2)
      {
	 strcpy (tbuf, &zstation[i].parm[4]);
	 tbuf[1] = 0;
      }
      else if (type == 3)
      {
	 strcpy (tbuf, zstation[i].name);
      }
      else if (type == 4)
      {

	 if (zdata[pcpn_day].used[time_pos] == 0)
         {
	    continue;
         }

	 sprintf (mbuf, "%5.2f",
		  zdata[pcpn_day].stn[i].zlevel2[time_pos].data);

	 if (mbuf[0] == '0')
         {
	    strcpy (tbuf, &mbuf[1]);
         }
	 else
         {
	    strcpy (tbuf, mbuf);
         }
      }

      m = zdata[pcpn_day].stn[i].zlevel2[time_pos].qual;
      mSetColor ( color_map_a[m]);
      length=strlen(tbuf);
      text_width=XTextWidth(info_font[4],tbuf,length); 

      /* Add the label offsets. */
      xadd = zstation[i].xadd;
      yadd = zstation[i].yadd;

      if (xadd < 0)
      {
	 xc = x1 - text_width;
      }
      else
      {
	 xc = x1 + 3;
      }

      if (yadd < 0)
      {
	 yc = y1;
      }
      else
      {
	 yc = y1 + yheight;
      }

      mDrawText (M_EXPOSE, map_number, xc, yc, tbuf);

      if (i == find_station_flag)
      {
	 find_station_flag = -1;
	 mDrawLine (M_EXPOSE, map_number, xc, yc, xc + text_width, yc);
      }

   }

   yheight=info_font[4]->ascent;

   /* Draw the freezing level legend. */
   legend_entry_pos = 0;

   for (m = 0; m < typemax; m++)
   {
      if (m != 0 && m != 6 && m != 7 && m != 3 )
      {
	 continue;
      }

      mSetColor ( color_map_a[funct[m]] );
      mDrawLine (M_EXPOSE, map_number, 15, ymin / 10 + 
                 (legend_entry_pos + 1) * yheight - yheight / 2, 35,
                 ymin / 10 + (legend_entry_pos + 1) * yheight - yheight / 2);
      mSetColor (color_map_n[15]);
      mDrawText (M_EXPOSE, map_number, 35, ymin / 10 + 
                 (legend_entry_pos + 1) * yheight, typename[m]);
      mDrawLine (M_EXPOSE, map_number, xc, yc, xc + text_width, yc);
  
      ++legend_entry_pos;


   }

   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/plot_freezing_stations.c,v $";
 static char rcs_id2[] = "$Id: plot_freezing_stations.c,v 1.2 2007/10/18 16:09:06 lawrence Exp $";}
/*  ===================================================  */

}
