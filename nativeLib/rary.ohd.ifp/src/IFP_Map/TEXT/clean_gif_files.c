#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"

/*-------------------------------------------------------------------------
    clean_gif_files(Widget,the_widget_struct,XmAnyCallbackStruct)
    
    removes gif files from users directory
--------------------------------------------------------------------------*/

#define COMMAND_LINE_LENGTH 200

void clean_gif_files(Widget w, the_widget_struct *someWidgets, 
                 XmAnyCallbackStruct *call_data)
                 
{
   char      system_command_line[COMMAND_LINE_LENGTH]; 
   
   memset(system_command_line, '\0', COMMAND_LINE_LENGTH);   
   
   strcpy(system_command_line, "rm ");
   strcat(system_command_line, (char *)getenv("HOME"));
   strcat(system_command_line, "/.ifp_files/gif_files/*");
   system(system_command_line);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/clean_gif_files.c,v $";
 static char rcs_id2[] = "$Id: clean_gif_files.c,v 1.2 2006/04/07 13:29:28 aivo Exp $";}
/*  ===================================================  */

}   
