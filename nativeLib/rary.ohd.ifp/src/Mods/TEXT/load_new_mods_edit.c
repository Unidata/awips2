/*  Function to go through the ModArray and write each
 *  mod to a buffer then add it to the newMods_str 
 *  and set the text field of the widget to show the
 *  contents of the newMods_str.
 *
 *  Written by:  D. Page - HRL
 *               7 Oct. 1995
 */

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern char *make_mod_A1_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_A2_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B1_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B2_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B3_string(Mods_everythingStruct *, int, Display *);

void load_new_mods_edit(Mods_everythingStruct *data)
{
	char            *buffer;
	char            all_newMods_str[1000];
        int             buffer_len=0;
        int             tot_newMods_len=0;
        int             i;
        Display         *display;
   
   /* Just return if there are no new mods */     
   if(data->ModIndex < 1)
      return;

   memset(all_newMods_str, '\0', 1000);
   
   display = XtDisplay(data->viewerWidgets->newModsText);    
        
   for(i = 0; i < data->ModIndex; i++)
   {
      switch (data->ModArray[i]->type)
      {
         case Mod_format_A1:
           buffer = (char *) make_mod_A1_string(data, i, display);
            break;

         case Mod_format_A2:
           buffer = (char *) make_mod_A2_string(data, i, display);
            break;

         case Mod_format_B1:
           buffer = (char *) make_mod_B1_string(data, i, display);
            break;

         case Mod_format_B2:
           buffer = (char *) make_mod_B2_string(data, i, display);
            break;

         case Mod_format_B3:
           buffer = (char *) make_mod_B3_string(data, i, display);
            break;
      }

      buffer_len = strlen(buffer) + 1;
      tot_newMods_len = tot_newMods_len + buffer_len;
      
      strcat(all_newMods_str, buffer);
      
   }
   /* Now set the text string for the newModsText widget */
   XmTextSetString(data->viewerWidgets->newModsText, all_newMods_str);
   		   
   /* free the buffer space */
   if(buffer != NULL)/*--AV -- */
       free(buffer);
   /*free(all_newMods_str); --AV --*/  

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/load_new_mods_edit.c,v $";
 static char rcs_id2[] = "$Id: load_new_mods_edit.c,v 1.2 2002/02/11 19:30:11 dws Exp $";}
/*  ===================================================  */

}
