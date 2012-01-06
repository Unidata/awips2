/* Function to reset the text string in the fromFilefgMods
 * viewer widget and reset the text in the appropriate file 
 * in the mods directory to the original values they had 
 * when current run of IFP was started.
 *
 *  Written by:  D. Page - HRL
 *               16 Sept. 1995
 */

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern int write_str_to_file(char *, char *, char *);
extern char     *get_fgroup_name();

void undo_fgroupmods_from_file_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;
	char            *FG_id;

   FG_id = get_fgroup_name();
   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods/FG_RANGE_");
   strcat(filePathName, FG_id);

   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   /* Set the original string in the viewer widget */
   XmTextSetString(data->viewerWidgets->fromFileFGroupModsText, 
		   data->fromFilefgMods_str);
		   
   /* Write the original string to the file and print error message if
      the write fails.
   */
   if(data->fromFilefgMods_str == NULL)return;
   if(write_str_to_file(filePathName, data->fromFilefgMods_str, "w") == FALSE)
      printf("Could not undo - error opening file: %s\n", filePathName);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/undo_fgroupmods_from_file_edit.c,v $";
 static char rcs_id2[] = "$Id: undo_fgroupmods_from_file_edit.c,v 1.5 2006/04/18 15:30:08 aivo Exp $";}
/*  ===================================================  */

}

