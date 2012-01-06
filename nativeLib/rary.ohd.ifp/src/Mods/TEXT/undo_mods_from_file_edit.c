/* Function to reset the text string in the fromFileMods
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

void undo_mods_from_file_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods/");
   strcat(filePathName, data->SegmentName);

   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   /* Set the original string in the viewer widget */
   XmTextSetString(data->viewerWidgets->fromFileModsText, 
		   data->fromFileMods_str);
		   
   /* Write the original string to the file and print error message if
      the write fails.
   */
   if(data->fromFileMods_str == NULL) return;
   if(write_str_to_file(filePathName, data->fromFileMods_str, "w") == FALSE)
      printf("Could not undo - error opening file: %s\n", filePathName);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/undo_mods_from_file_edit.c,v $";
 static char rcs_id2[] = "$Id: undo_mods_from_file_edit.c,v 1.4 2006/04/18 15:30:12 aivo Exp $";}
/*  ===================================================  */

}

