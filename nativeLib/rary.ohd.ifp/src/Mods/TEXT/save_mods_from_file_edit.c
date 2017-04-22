/* Function to get the text stored in the fromFileModsText 
 *  widget, compare it to the original contents of the
 *  mods file, and, if they're different, write
 *  the new text to the file and post an atom so that the
 *  Rerun button in IFP_Map will be highlighted.
 *
 *  Written by:  D. Page - HRL
 *               4 Oct. 1995
 */

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
int lx_strcmp(char *, char*);
extern void post_mod_files_updated_atom(int *);
extern int write_str_to_file(char *, char *, char *);
extern char* GetSource(char*);
void save_mods_from_file_edit(Mods_everythingStruct *data)
{
	char            filePathName[100];
	char            *blank_ptr;
	char            *buffer;
        FILE            *fromFileMods_fp;
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
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods/");
   strcat(filePathName, data->SegmentName);

         if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';
      
         buffer = XmTextGetString(data->viewerWidgets->fromFileModsText);
          /* if(  (buffer == NULL ) || strlen(buffer)==0) { return; }  */
         if(  (buffer == NULL ) || strlen(buffer)==0) *buffer='\0'; 



        if ( (char *)GetSource(filePathName) ==NULL ) {
              if(write_str_to_file(filePathName, buffer, "w") == TRUE) {
                  mod_files_updated = (int*)malloc(sizeof(int));
                  *mod_files_updated = TRUE;
                  post_mod_files_updated_atom(mod_files_updated);
                  free(mod_files_updated);
              } else
                 printf("Could not save - error opening file: %s\n", filePathName);
        }else {


           if(data->fromFileMods_str == NULL )
               data->fromFileMods_str = XmTextGetString(data->viewerWidgets->fromFileModsText); 
           if(lx_strcmp(data->fromFileMods_str, buffer) != 0) {
              if(write_str_to_file(filePathName, buffer, "w") == TRUE) {   
                   mod_files_updated = (int*)malloc(sizeof(int));
                   *mod_files_updated = TRUE;
                   post_mod_files_updated_atom(mod_files_updated);
                   free((void *)mod_files_updated);
              } else
                   printf("Could not save - error opening file: %s\n", filePathName);
          } else
             XtFree(buffer);
      }
return;
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/Mods/RCS/save_mods_from_file_edit.c,v $";
 static char rcs_id2[] = "$Id: save_mods_from_file_edit.c,v 1.7 2007/05/16 16:38:44 aivo Exp $";}
/*  ===================================================  */

}

/*strcmp core dumps with NULL parameter. so ensure no NULL goes to strcmp.---kwz*/
int lx_strcmp(char* str1, char* str2)
{ int num ;
  if ((str1==NULL)&&(str2==NULL)) num=0 ;
  else if (str1==NULL) num=-1 ;
  else if (str2==NULL) num=1 ;
  else num=strcmp(str1,str2) ;

  return num;
}
