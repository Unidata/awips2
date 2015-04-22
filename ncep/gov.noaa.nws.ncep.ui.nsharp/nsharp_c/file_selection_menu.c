#include "gui.h"
#include "sharp95.h"

void file_selection_menu ( char *conffile, char *confdir, _NXMmenuItem **menu, 
			   XtCallbackProc cbfunc, int *iret)
/************************************************************************
 * file_selection_menu							*
 * 									*
 * 									*
 * 									*
 **									*
 * Log:									*
 * T. Piper/SAIC	01/04	changed cfl_ropn to cfl_tbop		*
 ***********************************************************************/
{
int ier, ii, num=0;
char line[256];
FILE *fp;
_NXMmenuItem *ltest;

/*----------------------------------------------------------------------*/

*iret = 0;
ltest = NULL;

fp = cfl_tbop(conffile, confdir, &ier);

if ( fp == NULL || ier != 0 ) {
   *iret = -1;
   return;
   }

while(fgets(line,80,fp) != NULL)
   if(line[0] != '!') num++;

rewind(fp);
ltest = malloc(sizeof(_NXMmenuItem) * (size_t)(num + 1));

ii = 0;
while((ii < num)&&(fgets(line,80,fp) != NULL))
   {
   if(line[0] != '!')
      {
      if(line[strlen(line)-1] == '\n') line[strlen(line)-1] = '\0';
      ltest[ii].label = (char *)malloc(strlen(line)+1);
      sprintf(ltest[ii].label,"%s",line);
      ltest[ii].class        = &xmCascadeButtonGadgetClass;
      ltest[ii].mnemonic     = 0;
      ltest[ii].accelerator  = NULL;
      ltest[ii].accel_text   = NULL;
      ltest[ii].callback     = cbfunc;
      ltest[ii].which_widget = (long)ii;
      ltest[ii].subitems     = NULL;
      ltest[ii].sub_buttons  = NULL;
      ii++;
      }
   }
ltest[ii].label        = NULL;
ltest[ii].class        = &xmCascadeButtonGadgetClass;
ltest[ii].mnemonic     = 0;
ltest[ii].accelerator  = NULL;
ltest[ii].accel_text   = NULL;
ltest[ii].callback     = NULL;
ltest[ii].which_widget = (long)ii;
ltest[ii].subitems     = NULL;
ltest[ii].sub_buttons  = NULL;

*menu = ltest;

fclose(fp);

}
