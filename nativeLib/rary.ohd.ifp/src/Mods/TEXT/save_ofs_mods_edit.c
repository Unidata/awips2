/* Function to get the text stored in the ofsModsText 
 *  widget, compare it to the original contents of the
 *  mods_from_ofs file, and, if they're different, write
 *  the new text to the file and post an atom so that the
 *  Rerun button in IFP_Map will be highlighted.
 *
 *  Written by:  D. Page - HRL
 *               4 Oct. 1995
 *  Modified by: D. Page - HRL 21 Nov. 96
 */

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

extern void post_mod_files_updated_atom(int *);
extern int write_str_to_file(char *, char *, char *);
extern void create_new_FGroup_file(Widget);
extern char* GetSource(char*);
void save_mods_from_ofs_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;
	char            *buffer;
    FILE            *ofsMods_fp;
    int             *mod_files_updated;


/* 
   If buffer ==NULL return (there is nothing in the textfield and hence 
   nothing to save)
   If the file DNE or is empty, then save the mod to the file
   else check to see if the contents of the file and the contents of the 
   textfield are different. 
   If yes, then save to file
   else do nothing
*/

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *) getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods_from_ofs/");
   strcat(filePathName, data->SegmentName);


   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   buffer = XmTextGetString(data->viewerWidgets->ofsModsText);
   /* printf(" buffer =%s data->ofsMods_str =%s \n",buffer , data->ofsMods_str); */
   /* printf(" strlen(buffer) =%i strlen(..)=%i \n",strlen(buffer) , strlen(data->ofsMods_str)); */
   /*  if(  (buffer == NULL ) || strlen(buffer)==0) { return; }   */
   if(  (buffer == NULL ) || strlen(buffer)==0) *buffer='\0';


    

   /* printf(" get source = %s \n" ,GetSource(filePathName)); */
   if ( (char *)GetSource(filePathName) == NULL ) {
      if(write_str_to_file(filePathName, buffer, "w") == TRUE)
      {   
          mod_files_updated = (int*)malloc(sizeof(int));
          *mod_files_updated = TRUE;
          post_mod_files_updated_atom(mod_files_updated);
          free(mod_files_updated);
          
          /* call routine to update the FGroups file */
          create_new_FGroup_file(data->viewerWidgets->ofsModsText);
      }
      else
          printf("Could not save - error opening file: %s\n", filePathName);

   }else { 


/*   if(strcmp(data->ofsMods_str, buffer) != 0)*/
   /* if(buffer == NULL )*buffer = "   "; */
   if(data->ofsMods_str == NULL )
   data->ofsMods_str = XmTextGetString(data->viewerWidgets->ofsModsText);
   /*if(lx_strcmp(data->ofsMods_str, buffer) != 0)*//*changed by kwz*/
   if(strcmp(data->ofsMods_str, buffer) != 0)
   {
      if(write_str_to_file(filePathName, buffer, "w") == TRUE)
      {   
          mod_files_updated = (int*)malloc(sizeof(int));
          *mod_files_updated = TRUE;
          post_mod_files_updated_atom(mod_files_updated);
          free(mod_files_updated);
          
          /* call routine to update the FGroups file */
          create_new_FGroup_file(data->viewerWidgets->ofsModsText);
      }
      else
          printf("Could not save - error opening file: %s\n", filePathName);
   }
   else
      XtFree(buffer);
}
return;
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/Mods/RCS/save_ofs_mods_edit.c,v $";
 static char rcs_id2[] = "$Id: save_ofs_mods_edit.c,v 1.8 2007/05/16 16:40:38 aivo Exp $";}
/*  ===================================================  */

}
