/*  This routine eill create a new file in the 
 *  $HOME/.ifp_files/mods_from_ofs/FGroups directory
 *  by concatenating all files for the forecast 
 *  group in the $HOME/.ifp_files/mods_from_ofs 
 *  Modified from the final_mods_to_ofs routine
 *  D. Page - 8 Oct. 1995
 */
 
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <X11/Intrinsic.h>
#include "ifp_atoms.h"

#define COMMAND_LINE_LENGTH 200

void create_new_FGroup_file(w)
   Widget    w;
{
   int       format;
   long      nitems, left;
   long      offset = 0;
   Atom      type;

   int       number_of_run_segments;
   int       i;

   Display   *display;
   Window    root;

   char      system_command_line[COMMAND_LINE_LENGTH];
   char      *forecastGroup_name;
   char      *run_segments;
   char      tmp_name[20];
   
   display = XtDisplay(w);
   root = DefaultRootWindow(display);

   if(XGetWindowProperty
     (
      display,
      root,
      IFPA_forecast_group,
      offset,
      (long) 9,
      FALSE,
      IFPA_forecast_group_type,
      &type,
      &format,
      &nitems,
      (unsigned long *)&left,
      (unsigned char **)&forecastGroup_name
     ) == Success && type == IFPA_forecast_group_type)
   {
      ; /*  Do nothing - continue */
   }
   else  
   {
      printf("The forecast group name is not available in ");
      printf("create_new_FGroup_file.\n");
      printf("Cannot generate a list of mods for file\n");
      return;
   }

   if(XGetWindowProperty
     (
      display,
      root,
      IFPA_run_segments,
      offset,
      (long) 801,
      FALSE,
      IFPA_run_segments_type,
      &type,
      &format,
      &nitems,
      (unsigned long *)&left,
      (unsigned char **)&run_segments
     ) == Success && type == IFPA_run_segments_type)
   {
      ; /*  Do nothing - continue */
   }
   else   
   {
      printf("The run segments are not available in ");
      printf("create_new_FGroup_file.\n");
      printf("Cannot generate a list of mods for file\n");
      return;
   }

   number_of_run_segments = strlen(run_segments) / 8;
for(i = 0; i < 8; i++)
      if(forecastGroup_name[i] == ' ')
      {
         forecastGroup_name[i] = '\0';
         break;
      }

   memset(tmp_name,'\0',20);
   strcpy(tmp_name,"FG_RANGE_");
   strcat(tmp_name, forecastGroup_name);

   memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

   strcpy(system_command_line,
	     "chmod 664 $HOME/.ifp_files/mods_from_ofs/");
   strcat(system_command_line,tmp_name);
   system(system_command_line);

   memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
    
   strcat(system_command_line,
	      "cat -s $HOME/.ifp_files/mods_from_ofs/");
   strcat(system_command_line,tmp_name);
   strcat(system_command_line,
	      " > $HOME/.ifp_files/mods_from_ofs/FGroups/");
   strcat(system_command_line, forecastGroup_name);
 
   system(system_command_line);

   /*  First, make sure all the files are editable in the
    *  FGroups directory.
    */
   memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
   strcpy(system_command_line, 
          "chmod 664 $HOME/.ifp_files/mods_from_ofs/FGroups/*");
   system(system_command_line);
          
   /*
    * Copy all individual segment files for this forecast
    * group to one file in the FGroup directory.  This
    * file is named for the forecast group.
    */
   for(i = 0; i < number_of_run_segments; i++)
   {
      /*
       * Append this segment's mods to the FGroup file.
       */
       memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

       strcpy(system_command_line,
	      "cat -s $HOME/.ifp_files/mods_from_ofs/");

       strncat(system_command_line, run_segments, 8);

       strcat(system_command_line,
                 " >> $HOME/.ifp_files/mods_from_ofs/FGroups/");

       strcat(system_command_line, forecastGroup_name);

       system(system_command_line);
     
       run_segments += 8;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/create_new_FGroup_file.c,v $";
 static char rcs_id2[] = "$Id: create_new_FGroup_file.c,v 1.4 2006/04/18 15:29:00 aivo Exp $";}
/*  ===================================================  */

}
