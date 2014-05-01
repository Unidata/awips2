
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

extern void post_fgmod_files_updated_atom(int *);
extern int write_str_to_file(char *, char *, char *);
extern void create_new_FGroup_file(Widget);
extern char     *get_fgroup_name();

void undo_mods_from_ofsfgroup_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;

	char            *FG_id;


   
   FG_id = get_fgroup_name();

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));

   strcat(filePathName,
	   "/.ifp_files/mods_from_ofs/FG_RANGE_");
   strcat(filePathName,FG_id);

   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   /* Set the original string in the viewer widget */
   XmTextSetString(data->viewerWidgets->ofsFGroupModsText, 
		   data->ofsfgMods_str);
		   
   /* Write the original string to the file. Print error message if
      the write fails and create a new FGroup file if it succeeds.
   */
   if (data->ofsfgMods_str == NULL) return;
   if(write_str_to_file(filePathName, data->ofsfgMods_str, "w") == FALSE)
      printf("Could not undo - error opening file: %s\n", filePathName);
   else
      create_new_FGroup_file(data->viewerWidgets->ofsFGroupModsText);
      		   


}



void load_mods_from_ofsfgroup_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;
	char            *FG_id;


   FG_id = get_fgroup_name();

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));

   strcat(filePathName,
	   "/.ifp_files/mods_from_ofs/FG_RANGE_");
   strcat(filePathName,FG_id);

   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';

   data->ofsfgMods_str = (char *) GetSource(filePathName);


   XmTextSetString(data->viewerWidgets->ofsFGroupModsText, 
		   data->ofsfgMods_str);


}

void save_mods_from_ofsfgroup_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;
	char            *buffer;
    FILE            *ofsfgMods_fp;
    int             *fgmod_files_updated;
	char            *FG_id;
	char            *tmpbuf;

   fgmod_files_updated = (int*)malloc(sizeof(int));/*--AV */
   *fgmod_files_updated = FALSE;
   FG_id = get_fgroup_name();

   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));

   strcat(filePathName,
	   "/.ifp_files/mods_from_ofs/FG_RANGE_");
   strcat(filePathName,FG_id);
   
   if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';
  
   buffer = XmTextGetString(data->viewerWidgets->ofsFGroupModsText);
 
   
   /*Check for NULL value -- set to blank strings */
   if(buffer == NULL )
        *buffer = (char ) "   ";
   
   if( data->ofsfgMods_str == NULL)
   data->ofsfgMods_str=XmTextGetString(data->viewerWidgets->ofsFGroupModsText);
 
   /* debug ***
   if( buffer == NULL || data->ofsfgMods_str == NULL) {
   
   printf ("one of them has NULL value \n");
   return;
   }
   *****/
   if(strcmp(data->ofsfgMods_str, buffer) != 0)
   /*AV lx_strcmp did not work */
   /*if(lx_strcmp(data->ofsfgMods_str, buffer) != 0)*//*changed by kwz*/
   {
         if(write_str_to_file(filePathName, buffer, "w") == TRUE)
         {   
          
        
             *fgmod_files_updated = TRUE;
          
             post_fgmod_files_updated_atom(fgmod_files_updated);
             free(fgmod_files_updated);
          
          /* call routine to update the FGroups file */
             create_new_FGroup_file(data->viewerWidgets->ofsFGroupModsText);
         }
         else
        
          printf("Could not save - error opening file: %s\n", filePathName);
        
    }
    else
    {
          if(buffer != NULL) XtFree(buffer);
    }
    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/fgroupmods.c,v $";
 static char rcs_id2[] = "$Id: fgroupmods.c,v 1.7 2006/04/18 15:29:05 aivo Exp $";}
/*  ===================================================  */

}
