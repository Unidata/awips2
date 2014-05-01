/* Function to read the appropriate file in the mods_from_ofs
*  directory into a buffer and set the text field of the 
*  widget to show the contents of that buffer.
*
*  Written by:  D. Page - HRL
*               17 May 1995
*/

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern char *GetSource(char *);

void load_mods_from_ofs_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods_from_ofs/");
   strcat(filePathName, data->SegmentName);

   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   data->ofsMods_str = (char *) GetSource(filePathName);

   XmTextSetString(data->viewerWidgets->ofsModsText, 
		   data->ofsMods_str);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/load_ofs_mods_edit.c,v $";
 static char rcs_id2[] = "$Id: load_ofs_mods_edit.c,v 1.3 2006/04/18 15:29:25 aivo Exp $";}
/*  ===================================================  */

}
