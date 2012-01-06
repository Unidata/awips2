/* Function to reset the text string in the ofsMods
 * viewer widget and reset the text in the appropriate file 
 * in the mods directory to the original values they had 
 * when current run of IFP was started.
 *
 *  Written by:  D. Page - HRL
 *               5 Oct. 1995
 */

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern char *GetSource(char *);
extern void create_new_FGroup_file(Widget);

void undo_mods_from_ofs_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods_from_ofs/");
   strcat(filePathName, data->SegmentName);

   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   /* Set the original string in the viewer widget */
   XmTextSetString(data->viewerWidgets->ofsModsText, 
		   data->ofsMods_str);
		   
   /* Write the original string to the file. Print error message if
      the write fails and create a new FGroup file if it succeeds.
   */
   if (data->ofsMods_str == NULL) return;
   if(write_str_to_file(filePathName, data->ofsMods_str, "w") == FALSE)
      printf("Could not undo - error opening file: %s\n", filePathName);
   else
      create_new_FGroup_file(data->viewerWidgets->ofsModsText);
      		   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/undo_ofs_mods_edit.c,v $";
 static char rcs_id2[] = "$Id: undo_ofs_mods_edit.c,v 1.4 2006/04/18 15:30:18 aivo Exp $";}
/*  ===================================================  */

}
