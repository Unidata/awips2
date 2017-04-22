/* Function to read the appropriate file in the mods
*  directory into a buffer and set the text field of the 
*  widget to show the contents of that buffer.
*
*  Written by:  D. Page - HRL
*               16 Sept. 1995
*/

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern char *GetSource(char *);

void load_mods_from_file_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods/");
   strcat(filePathName, data->SegmentName);

   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   data->fromFileMods_str = (char *) GetSource(filePathName);

   XmTextSetString(data->viewerWidgets->fromFileModsText, 
		   data->fromFileMods_str);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/load_mods_from_file_edit.c,v $";
 static char rcs_id2[] = "$Id: load_mods_from_file_edit.c,v 1.3 2006/04/18 15:29:20 aivo Exp $";}
/*  ===================================================  */

}
