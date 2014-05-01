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

#include "gageqc_gui.h"
#include "get_colorvalues.h"
#include "get_mpe_colors.h"

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
static const char * dqc_nodata_color = "black";
 
static char *color_map_a[] = { "Aquamarine",
   "OrangeRed",
   "Orange",
   "Yellow",
   "VioletRed",
   "SpringGreen4",
   "Green3",
   "Grey",
   "White"
};

static char *color_map_n[] = { "Grey",
   "Grey",
   "Blue",
   "Aquamarine",
   "LightGreen",
   "DarkGreen",
   "Violet",
   "Purple",
   "Blue",
   "Blue",
   "Yellow",
   "Yellow",
   "Yellow2",
   "VioletRed",
   "Red",
   "White"
};

/* global variables */

int   dqc_precip_numcol;   
int   dqc_freezing_numcol;    
int   dqc_temp_numcol;                                      
char ** dqc_colorlevels_name;
double *dqc_colorlevels_value;
 
char ** get_precip_base_colormap ( )
{  
   char cv_use_name[COLOR_USE_NAME_LEN + 1];
   int  cv_default_dur;
   int  cv_numcol;
   NamedColorSetGroup * dqc_default_pColors = NULL;   
   extern int map_flag;
   extern int pcpn_time_step;
   extern int grids_flag;
   
   /* set parameters for search in ColorValue table */
     
  
   if (pcpn_time_step == 1) /* 24h is selected */
   {
   
      if (grids_flag == 1)
      {
         strcpy(cv_use_name, "24hGRID_PRECIP");
         cv_default_dur = 86400;
      } 
      else if (map_flag == 1)
      {
         strcpy(cv_use_name, "24hMAREA_PRECIP");
         cv_default_dur = 86400; /* 24 hours duration for MAP*/
      
      }
      else 
      {
         strcpy(cv_use_name, "24hGRID_PRECIP");
         cv_default_dur = 86400;      
      }
   
   }
   else
   {
       /* 6hr is selected */
   
      if (grids_flag == 1)
      {
         strcpy(cv_use_name, "6hGRID_PRECIP");
         cv_default_dur = 21600;
      } 
      else if (map_flag == 1)
      {
         strcpy(cv_use_name, "6hMAREA_PRECIP");
         cv_default_dur = 21600; /* 6 hours duration for MAP*/
      
      }
      else 
      {
         strcpy(cv_use_name, "6hGRID_PRECIP");
         cv_default_dur = 21600;      
      }
   
   }
   
       
   /* get default color */
   
   dqc_default_pColors = get_mpe_default_colors ();
  
   /* get the color name from colorvalue table */   
  
   set_dqc_colorvalues(cv_use_name, cv_default_dur, & cv_numcol,
                       dqc_default_pColors);
        
   dqc_precip_numcol = cv_numcol;
     
   
   return dqc_colorlevels_name;
  
}

char ** get_freezing_base_colormap ( )
{
   int  cv_numcol;
   char cv_use_name[COLOR_USE_NAME_LEN + 1];
   int  cv_default_dur;
   NamedColorSetGroup * dqc_default_pColors = NULL;
   extern int map_flag;
   
   /* set parameters for search in ColorValue table */
      
   if (map_flag == 1)
   {
      strcpy(cv_use_name, "6hMAREA_FREEZL");
      cv_default_dur = 21600; /* 6 hours duration for MAZ*/
   }
   else 
   {
      strcpy(cv_use_name, "6hGRID_FREEZL");
      cv_default_dur = 21600; 
     
   }      
   /* get default color */
   
   dqc_default_pColors = get_mpe_default_colors ();
   
   /* get the color name from colorvalue table */
   
   set_dqc_colorvalues(cv_use_name, cv_default_dur, & cv_numcol,
                       dqc_default_pColors);
        
   dqc_freezing_numcol = cv_numcol;
   
   return dqc_colorlevels_name;   
  
}

char ** get_temperature_base_colormap ( )
{
   int  cv_numcol;
   char cv_use_name[COLOR_USE_NAME_LEN + 1];
   int  cv_default_dur;
   NamedColorSetGroup * dqc_default_pColors = NULL;
   extern int map_flag;
   extern int pcpn_time_step;
   extern int grids_flag;
   
   /* set parameters for search in ColorValue table */
      
   if (pcpn_time_step == 0) 
   {
      if (grids_flag == 1)
      {
         strcpy(cv_use_name, "sixhGRID_TEMP");
         cv_default_dur = 21600; 
      }
      else if (map_flag == 1)
      {
         strcpy(cv_use_name, "sixhMAREA_TEMP");
         cv_default_dur = 21600; 
      }
      else
      {
         strcpy(cv_use_name, "sixhGRID_TEMP");
         cv_default_dur = 21600; 
      }
   
   }
   else if (pcpn_time_step == 1 )
   {      
      strcpy(cv_use_name, "maxGRID_TEMP");
      cv_default_dur = 86400; 
   }
   else if (pcpn_time_step == 2 )
   {
      strcpy(cv_use_name, "minGRID_TEMP");
      cv_default_dur = 86400; 
   }
       
           
   /* get default color */
   
   dqc_default_pColors = get_mpe_default_colors ();
   
   /* get the color name from colorvalue table */
   
   set_dqc_colorvalues(cv_use_name, cv_default_dur, & cv_numcol,
                       dqc_default_pColors);
        
   dqc_temp_numcol = cv_numcol;
   
   return dqc_colorlevels_name;
}

char ** get_color_map_n ( )
{
   return color_map_n;
}

char ** get_color_map_a ( )
{
   return color_map_a;
}

const char * get_dqc_nodata_color ( )
{
   return dqc_nodata_color;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/GageQCGui/RCS/colormaps.c,v $";
 static char rcs_id2[] = "$Id: colormaps.c,v 1.5 2007/05/23 21:50:44 whfs Exp $";}
/*  ===================================================  */

}

