/* Function to read the appropriate file in the mods
*  directory into a buffer and set the text field of the 
*  widget to show the contents of that buffer.
*
*  Written by:  D. Page - HRL
*               16 Sept. 1995
*/

#include <string.h>
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern char *GetSource(char *);
extern char     *get_fgroup_name();

void load_fgroupmods_from_file_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;

	char            *FG_id;

   FG_id = get_fgroup_name();
   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods/FG_RANGE_");
   strcat(filePathName,FG_id);
   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   data->fromFilefgMods_str = (char *) GetSource(filePathName);

   XmTextSetString(data->viewerWidgets->fromFileFGroupModsText, 
		   data->fromFilefgMods_str);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/load_fgroupmods_from_file_edit.c,v $";
 static char rcs_id2[] = "$Id: load_fgroupmods_from_file_edit.c,v 1.4 2006/04/18 15:29:15 aivo Exp $";}
/*  ===================================================  */

}
