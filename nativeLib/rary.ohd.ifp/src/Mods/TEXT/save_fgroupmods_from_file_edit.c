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
extern char* GetSource(char*);
extern void post_fgmod_files_updated_atom( int * );
extern int write_str_to_file( char *, char *, char * );
extern char *get_fgroup_name();

void save_fgroupmods_from_file_edit( Mods_everythingStruct *data )
{
	char            filePathName[100];
	char            *blank_ptr;
	char            *buffer;
    FILE            *fromFileFgMods_fp;
    int             *fgmod_files_updated;
	char            *modsfg_str;
    char            *tmpbuf;
   modsfg_str = get_fgroup_name();
   memset(filePathName, '\0', 100);
   strcpy(filePathName, (char *)getenv("HOME"));
   strcat(filePathName, "/.ifp_files/mods/FG_RANGE_");
   strcat(filePathName, modsfg_str);
   /*
   printf("save_fgroupmods_from_file_edit-filepath %s \n",filePathName);
   */

/*
 *  If buffer ==NULL return (there is nothing in the textfield and hence
 *  nothing to save)
 *  If the file DNE or is empty, then save the mod to the file
 *  else check to see if the contents of the file and the contents of the
 *  textfield are different.
 *  If yes, then save to file
 *  else do nothing
 */
   
      if((blank_ptr = strchr(filePathName, ' ')) != NULL) *blank_ptr = '\0';
      
      buffer = XmTextGetString(data->viewerWidgets->fromFileFGroupModsText);
       
   
      if( ((char *)GetSource(filePathName) == NULL) && !( ((char *)buffer == NULL) || strlen(buffer)==0 )  ) {
             /* File DNE and text field is not empty  -- save to file update atom*/
           
           if(write_str_to_file(filePathName, buffer, "w") == TRUE) {
                 fgmod_files_updated = (int*)malloc(sizeof(int));
                 *fgmod_files_updated = 1;
                 post_fgmod_files_updated_atom(fgmod_files_updated);
                 free(fgmod_files_updated);
           } else
                 printf("Could not save - error opening file: %s\n", filePathName);

      }else if ( ( (char *)GetSource(filePathName) ==NULL) &&(  ( (char *)buffer == NULL) || strlen(buffer)==0 )  ) {
             /* File DNE and text field is empty  -- do nothing */
             
          return ;

     } else{   /* File exists and buffer is either empty or not empty */

       if( data->fromFilefgMods_str == NULL )
      		data->fromFilefgMods_str=XmTextGetString(data->viewerWidgets->fromFileFGroupModsText);
  
       if(  (buffer == NULL ) || strlen(buffer)==0) *buffer='\0';  
 
       if(strcmp(data->fromFilefgMods_str, buffer) != 0){ 
         
            if(write_str_to_file(filePathName, buffer, "w") == TRUE)
            {   
          
             fgmod_files_updated = (int*)malloc(sizeof(int)); 
             *fgmod_files_updated = 1;
             post_fgmod_files_updated_atom(fgmod_files_updated);
             free(fgmod_files_updated);
             }
             else
             printf("Could not save - error opening file: %s\n", filePathName);
       }
       else{    
                 if(buffer != NULL)XtFree(buffer);

       }

     }
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/save_fgroupmods_from_file_edit.c,v $";
 static char rcs_id2[] = "$Id: save_fgroupmods_from_file_edit.c,v 1.7 2006/04/18 15:29:39 aivo Exp $";}
/*  ===================================================  */

}
