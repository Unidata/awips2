#include <stdio.h>
#include <fcntl.h>
#include <X11/Intrinsic.h>
#include <Xm/MessageB.h>
#include "ifp_atoms.h"

/*-------------------------------------------------------------------------
     gif_files_to_ofs(Widget)
     
  This function calls a script that creates the gif tar file to 
   be transferred back to the OFS at the end of an IFP session.  
   This was copied from the final_mods_to_ofs function and modified.
  In the script, the gif files are tarred together in a file named by the 
   forecast group.  This file is then copied back to the official
   place for gif files as determined by get_apps_defaults.  The tar file
   is then expanded into individual files then the tar is deleted.  Finally,
   the files in the home directory are deleted.
 
  Written by Donna Page, HRL, 7/31/95
-------------------------------------------------------------------------*/


#define COMMAND_LINE_LENGTH 200

gif_files_to_ofs(Widget send_gifs_shell)
{
 int       format, nitems, left;
 long      offset = 0;
 Atom      type;

 int       i;

 Display   *display;
 Window    root;

 Widget    gif_files_to_ofs_dialog, button;
 Arg       wargs[5];
 int       n;
 char      message[150];
 XmString  xmMessageString;
 XEvent    event;

 char      system_command_line[COMMAND_LINE_LENGTH];
 char      *forecastGroup_name;

 int             stay_in_gif_files_while_loop;
 int             has_been_mapped;

 int    len, len2;

 display = XtDisplay(send_gifs_shell);
 root = DefaultRootWindow(display);

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_forecast_group,
	offset,
	(long) 9,
	FALSE,
	IFPA_forecast_group_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&forecastGroup_name
	) == Success && type == IFPA_forecast_group_type)
	{
	 ; /*  Do nothing - continue */
	}
 else   {
	printf("The forecast group name is not available in ");
	printf("gif_files_to_ofs.\n");
	printf("Cannot generate a list of mods to send to the OFS\n");
	return;
	}

for(i = 0; i < 8; i++)
   if(forecastGroup_name[i] == ' ')
     {
      forecastGroup_name[i] = '\0';
      break;
     }

 gif_files_to_ofs_dialog = XmCreateWorkingDialog(send_gifs_shell,
					    "gif_files_to_ofs_dialog",
					    NULL, 0);

 button = XmMessageBoxGetChild(gif_files_to_ofs_dialog, XmDIALOG_OK_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(gif_files_to_ofs_dialog, XmDIALOG_HELP_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(gif_files_to_ofs_dialog, XmDIALOG_CANCEL_BUTTON);
 XtUnmanageChild(button);

 memset(message, '\0', 150);
 strcpy(message, "GIF files for forecast group\n                 ");
 strcat(message, forecastGroup_name);
 strcat(message, "\n  are being combined into one file\n");
 strcat(message, "      for transfer to the OFS.");
 xmMessageString = XmStringCreateLtoR(message, XmSTRING_DEFAULT_CHARSET);

 n = 0;
 XtSetArg(wargs[n], XmNdialogTitle,
	  XmStringCreate("Sending GIF files", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
 XtSetValues(gif_files_to_ofs_dialog, wargs, n);

/*
 * Call a script that will create and send a tar file of
 * the gif files from the users home directory to 
 * the system wide files (set by tht ifp_gif_files
 * token for get_apps_defaults) and then extract the files. 
 */
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
 
 len = strlen("ifp_scripts_dir");
 get_apps_defaults("ifp_scripts_dir", &len, system_command_line, &len2); 

 strcat(system_command_line, "/copy_gif_files ");
 strcat(system_command_line, forecastGroup_name);

 XtManageChild(gif_files_to_ofs_dialog);
/*
 * We need to process events to get the message to appear
 *  in the information box.
 * gfs - this changed after release 3.2 of operating system
 *       4/3/92
 */
 stay_in_gif_files_while_loop = TRUE;
 has_been_mapped = FALSE;

 XSynchronize(display, 1);
 n = 0;
 while(stay_in_gif_files_while_loop)
      {
       XNextEvent(display, &event);
       if(event.type == FocusIn && has_been_mapped)
	    stay_in_gif_files_while_loop = FALSE;
       if(event.type == MapNotify) has_been_mapped = TRUE;
       n++;
       if(n > 400) stay_in_gif_files_while_loop = FALSE; /* failsafe */
       XtDispatchEvent(&event);
      }
 XSynchronize(display, 0);

 system(system_command_line);

 XtUnmanageChild(gif_files_to_ofs_dialog);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/gif_files_to_ofs.c,v $";
 static char rcs_id2[] = "$Id: gif_files_to_ofs.c,v 1.2 2006/04/07 13:30:02 aivo Exp $";}
/*  ===================================================  */

}
