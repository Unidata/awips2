/* Function to read the Mod_globalPrefs file and fill the
 * Mod_globalPrefs_t structure
 *
 * Written by D. Page - 9 Oct. 1995
 */
 
#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"

#define MAXCHAR 121

extern int get_apps_defaults(char *, int *, char *, int *);
void read_globalPrefs(Mods_everythingStruct *data)
{
   FILE         *fp;
   char         fname[100];
   int          len, len2;
   char         *PARSE_TOKEN = " \t";
   char         line[MAXCHAR];
   char         *token;
   char         *value;
   int          num_prefs;
   
  /*----------------------------------------------------------------*/   
   memset(fname, '\0', 100);
   
   /* call routine to get the overlays directory path and rfc name*/
   len = strlen("ifp_options_dir");
   get_apps_defaults("ifp_options_dir", &len, fname, &len2);
   
   strcat(fname, "/ModSettings/ModGlobalPrefs");
   
   /* Open the file if possible */
   if( (fp = fopen(fname, "r")) == NULL)
      printf("Problem opening file: %s\n", fname);
   else    /*  only do the following if there's we opened the file */
   {
  
      /* Process first line - read only the first field for ofs_mods_editable */
      fgets(line, MAXCHAR, fp);
      token = strtok(line, PARSE_TOKEN);
      if((strcmp(token, "FALSE")) == 0)
         data->Mod_globalPrefs->ofs_mods_editable = FALSE;
      else if((strcmp(token, "TRUE")) == 0)
         data->Mod_globalPrefs->ofs_mods_editable = TRUE;
      else
      {
         printf("value in %s for ofs_mods_editable is invalid", fname); 
         printf(" - will be set to FALSE\n");
         data->Mod_globalPrefs->ofs_mods_editable = FALSE;
      }
      
      fclose(fp);
   }
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/read_globalPrefs.c,v $";
 static char rcs_id2[] = "$Id: read_globalPrefs.c,v 1.2 2002/02/11 19:48:34 dws Exp $";}
/*  ===================================================  */

}
         
        
   
   
