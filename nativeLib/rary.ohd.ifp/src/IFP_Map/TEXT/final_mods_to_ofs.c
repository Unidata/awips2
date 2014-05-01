/*
 * This function creates the mods file to be transferred back
 *  to the OFS at the end of an IFP session.
 * First, the mods originally copied from the OFS files to start this
 *  session are copied into the output file.  Then any new mods
 *  generated during this IFP session are appended to the end
 *  of the output file.
 * As the mods are appended to the output file, the individual
 *  mods files are deleted.
 * Finally, the file is copied to the OFS files.
 *
 * Written by George Smith, HRL, 9/21/91
 * Modified to display "Making one Mods file..." message - gfs, 2/18/92
 * Modified by D. Page to to work with users mods files and OFS mod files
 *    dp - 18 Nov 1992
 * Modified by G. Smith to change "cat" parameter from -q to -s for
 *    compatibility with HP cat command - 29 Sept 93
 * Modified by D. Page to delete the individual files in mods_from_file
 *    and changed how one_segment_name is filled and used - 8 Oct. 95
 * Modified by D. Vo to save FG_RANGE_FGid file in mods_from_file to mods_to_ofs
 *    and deleted FG_RANGE_FGid file in mods - 13 Jul. 99 
 */

#include <stdio.h>
#include <fcntl.h>
#include <X11/Intrinsic.h>
#include <Xm/MessageB.h>
#include "ifp_atoms.h"
#include "libXifp.h"
#include "c_call_f/close_all_open_files.h"/*-- added by AV -- */

#define COMMAND_LINE_LENGTH 200
#define DATE_STR_LENGTH 13


char *convert2_dates_str(date *sys_dates);


final_mods_to_ofs(send_mods_shell)

 Widget    send_mods_shell;
{
 date	   *start_run_date,
 	   *end_run_date,
 	   *end_obs_date;

 int       format, nitems, left;
 long      offset = 0;
 Atom      type;
 
 int       number_of_run_segments;
 int       i, j;

 Display   *display;
 Window    root;

 Widget    final_mods_to_ofs_dialog, button;
 Arg       wargs[5];
 int       n;
 char      message[150];
 XmString  xmMessageString;
 XEvent    event;

 char      system_command_line[COMMAND_LINE_LENGTH];
 char      mods_pathname[80];
 char      *forecastGroup_name;
 char      *run_segments, *temp_run_segments, *one_segment_ptr;
 char      one_segment_name[9];
 char      fg_range_fgid[20];

 char      *cmdline_start_run_date, 
	   *cmdline_end_run_date, 
	   *cmdline_end_obs_date;

 XtIntervalId    final_mods_timer_id;
 int             stay_in_final_mods_while_loop;
 int             has_been_mapped;

 int    file_descriptor;
 long   file_length;
 int    len, len2;
 

 display = XtDisplay(send_mods_shell);
 root = DefaultRootWindow(display);
 cmdline_start_run_date =  (char *)malloc(DATE_STR_LENGTH);
 cmdline_end_run_date =  (char *)malloc(DATE_STR_LENGTH);
 cmdline_end_obs_date =  (char *)malloc(DATE_STR_LENGTH);

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_start_date,
	offset,
	(long) sizeof(date),
	FALSE,
	IFPA_run_start_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&start_run_date
	) == Success && type == IFPA_run_start_date_type)
	{
	 ; /*  Do nothing - continue */
	}
 else   {
	printf("Start_run_date is not available in ");
	printf("final_mods_to_ofs.\n");
	return;
	}


 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_end_date,
	offset,
	(long) sizeof(date),
	FALSE,
	IFPA_run_end_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&end_run_date
	) == Success && type == IFPA_run_end_date_type)
	{
	 ; /*  Do nothing - continue */
	}
 else   {
	printf("end_run_date is not available in ");
	printf("final_mods_to_ofs.\n");
	return;
	}

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_end_obs_date,
	offset,
	(long) sizeof(date),
	FALSE,
	IFPA_run_end_obs_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&end_obs_date
	) == Success && type == IFPA_run_end_obs_date_type)
	{
	 ; /*  Do nothing - continue */
	}
 else   {
	printf("end_obs_date is not available in ");
	printf("final_mods_to_ofs.\n");
	return;
	}


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
	printf("final_mods_to_ofs.\n");
	printf("Cannot generate a list of mods to send to the OFS\n");
	return;
	}

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_segments,
	offset,
	(long) 801,
	FALSE,
	IFPA_run_segments_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned  char **)&run_segments
	) == Success && type == IFPA_run_segments_type)
	{
	 ; /*  Do nothing - continue */
	}
 else   {
	printf("The run segments are not available in ");
	printf("final_mods_to_ofs.\n");
	printf("Cannot generate a list of mods to send to the OFS\n");
	return;
	}




number_of_run_segments = strlen(run_segments) / 8;

for(i = 0; i < 8; i++)
   if(forecastGroup_name[i] == ' ')
     {
      forecastGroup_name[i] = '\0';
      break;
     }

 final_mods_to_ofs_dialog = XmCreateWorkingDialog(send_mods_shell,
					    "final_mods_to_ofs_dialog",
					    NULL, 0);

 button = XmMessageBoxGetChild(final_mods_to_ofs_dialog, XmDIALOG_OK_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(final_mods_to_ofs_dialog, XmDIALOG_HELP_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(final_mods_to_ofs_dialog, XmDIALOG_CANCEL_BUTTON);
 XtUnmanageChild(button);

 memset(message, '\0', 150);
 strcpy(message, "Run-time Mods for forecast group\n                 ");
 strcat(message, forecastGroup_name);
 strcat(message, "\n  are being combined into one file\n");
 strcat(message, "      for transfer to the OFS.");
 xmMessageString = XmStringCreateLtoR(message, XmSTRING_DEFAULT_CHARSET);

 n = 0;
 XtSetArg(wargs[n], XmNdialogTitle,
	  XmStringCreate("Make one Mods file", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
 XtSetValues(final_mods_to_ofs_dialog, wargs, n);

temp_run_segments = run_segments;
for(i = 0; i < number_of_run_segments; i++)
   {
    for(j = 0; j < 8; j++)
	printf("%c", *(temp_run_segments++));
    printf("%c",((i+1)%5 == 0 || i == number_of_run_segments-1) ? '\n' : ' ');
   }
   
/*
 * Begin creating a file for mods to be copied back to the OFS files.
 * Will create the file by using the UNIX command 'cat' to
 *  first copy the current mods_from_ofs file for this forecast
 *  group to a file in the mods_to_ofs directory.  Then
 *  append any mods generated by this ifp session to the
 *  mods_to_ofs/'fg_name' file.
 */
 
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

 strcpy(system_command_line,
	"cat -s $HOME/.ifp_files/mods_from_ofs/FGroups/");
 strcat(system_command_line, forecastGroup_name);

 strcat(system_command_line,
	" > $HOME/.ifp_files/mods_to_ofs/");
 strcat(system_command_line, forecastGroup_name);

 system(system_command_line);
 
/*
* Append FG/Range mods to the output file.
*/
 memset(fg_range_fgid,'\0',20);
 strcpy(fg_range_fgid,"FG_RANGE_");
 strcat(fg_range_fgid,forecastGroup_name);

 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

 strcpy(system_command_line,
	    "cat -s $HOME/.ifp_files/mods/");
 strcat(system_command_line,fg_range_fgid);
 strcat(system_command_line,
	    " >> $HOME/.ifp_files/mods_to_ofs/");

 strcat(system_command_line, forecastGroup_name);

 XtManageChild(final_mods_to_ofs_dialog);

/*
 * We need to process events to get the message to appear
 *  in the information box.
 * gfs - this changed after release 3.2 of operating system
 *       4/3/92
 */
 stay_in_final_mods_while_loop = TRUE;
 has_been_mapped = FALSE;

XSynchronize(display, 1);
n = 0;
 while(stay_in_final_mods_while_loop)
      {
       XNextEvent(display, &event);
       if(event.type == FocusIn && has_been_mapped)
	    stay_in_final_mods_while_loop = FALSE;
       if(event.type == MapNotify) has_been_mapped = TRUE;
       n++;
       if(n > 400) stay_in_final_mods_while_loop = FALSE; /* failsafe */
       XtDispatchEvent(&event);
      }
XSynchronize(display, 0);
	
system(system_command_line); 

/*
 * Remove this forecast group's mods_from_ofs file.
 */
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

 strcpy(system_command_line, "rm -f $HOME/.ifp_files/mods_from_ofs/");

 strcat(system_command_line,fg_range_fgid);

 system(system_command_line);

 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

 strcpy(system_command_line, "rm -f $HOME/.ifp_files/mods_from_ofs/FGroups/");

 strcat(system_command_line, forecastGroup_name);

 system(system_command_line);
/*
 * Copy any mods from the OFS for this forecast group to
 *  the file to be transferred back to the OFS.
 */
 for(i = 0; i < number_of_run_segments; i++)
    {
    /*
     * Append this segment's mods to the output file.
     */
     memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

     strcpy(system_command_line,
	    "cat -s $HOME/.ifp_files/mods/");

     strncat(system_command_line, run_segments, 8);

     strcat(system_command_line,
	    " >> $HOME/.ifp_files/mods_to_ofs/");

     strcat(system_command_line, forecastGroup_name);

     system(system_command_line);
    /*
     * Remove this segment's mod file.
     */
     memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

     strcpy(system_command_line, "rm -f $HOME/.ifp_files/mods/");
    /*
     * Add segment name to command, don't include trailing blanks.
     */
     memset(one_segment_name, '\0', 9);
     one_segment_ptr = run_segments;
     
     for(j = 0; j < 8; j++)
	 if(*one_segment_ptr == ' ')
	    break;
	 else
	    strncat(one_segment_name, one_segment_ptr++, 1);
	    
     strcat(system_command_line, one_segment_name);
     system(system_command_line);
     
     /* also delete the corresponding file in mods_from_file dir
      * 8 Oct. 95 - dp
      */
     memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
     strcpy(system_command_line, "rm -f $HOME/.ifp_files/mods_from_ofs/");
     strcat(system_command_line, one_segment_name);
     system(system_command_line);
     
     run_segments += 8;
    }
/*
 * Remove the .ifp_files/mods/FG_RANGE_FGid mod file.
 */
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

 strcpy(system_command_line, "rm -f $HOME/.ifp_files/mods/");

 strcat(system_command_line, fg_range_fgid);

 system(system_command_line);
 
/*
 * If the mods file for the entire forecast group is empty
 *  do not send it to the OFS.
 * See if file length is zero.
 *
 * User requires to send empty mods back to ofs (r23-53) -- gzhou 10/2003
 */
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

 strcpy(system_command_line, (char *)getenv("HOME"));
 strcat(system_command_line,
	"/.ifp_files/mods_to_ofs/");

 strcat(system_command_line, forecastGroup_name);
 /*
  * Now have mods file name in system_command_line variable.
  */
  file_descriptor = open (system_command_line, O_RDONLY);
  file_length = lseek (file_descriptor, 0, SEEK_END);
  /*
  *  copy run_dates for ofs command_lind dates
  */
  cmdline_start_run_date = convert2_dates_str(start_run_date);
  cmdline_end_obs_date = convert2_dates_str(end_obs_date);
  cmdline_end_run_date = convert2_dates_str(end_run_date);

/*Modifed by gzhou 10/2003 -- bug r23-53
  User needs to be able to write zero mods back to ofs 

if(file_length > 0)

*/
if(file_length >= 0)
  {
   /*
    * Build the copy command to send the mods file for
    *  this forecast group to the OFS.
    */
    memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

    /* call routine to determine the path name for mods */
    memset(mods_pathname, '\0', 60);

    len = strlen("ofs_mods_dir");
    get_apps_defaults("ofs_mods_dir", &len, mods_pathname, &len2); 

    strcpy(system_command_line, "cp $HOME/.ifp_files/mods_to_ofs/");
    strcat(system_command_line, forecastGroup_name);
    strcat(system_command_line, " ");
    strcat(system_command_line, mods_pathname);

    XtUnmanageChild(final_mods_to_ofs_dialog);

    memset(message, '\0', 150);
    strcpy(message, "Run-time Mods for forecast group\n                 ");
    strcat(message, forecastGroup_name);
    strcat(message, "\n   are being sent to the OFS.");
    xmMessageString = XmStringCreateLtoR(message, XmSTRING_DEFAULT_CHARSET);

    n = 0;
    XtSetArg(wargs[n], XmNdialogTitle,
	     XmStringCreate("Copy Mods to OFS", XmSTRING_DEFAULT_CHARSET)); n++;
    XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
    XtSetValues(final_mods_to_ofs_dialog, wargs, n);

    XtManageChild(final_mods_to_ofs_dialog);
   /*
    * We need to process all events to get the message to appear
    *  in the information box.
    */
    while(XEventsQueued(display, QueuedAfterFlush) > 0)
	 {
	  XNextEvent(display, &event);
	  XtDispatchEvent(&event);
	 }

    system(system_command_line);

    /* change permissions on ofs mods file so others can read and write it */
/*    memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
    strcpy(system_command_line, "chmod 664 ");
    strcat(system_command_line, mods_pathname);
    strcat(system_command_line, "/");
    strcat(system_command_line, forecastGroup_name);
    system(system_command_line);
*//*Comment out because if mods file is in read-only, then it should stays
read-only. kwz,2/10/04*/

    /* Add call to close_all_open_files to get past limit
       of 60 open files for a process on the HPs.  This
       will close files IFP doesn't need and allow a
       call to fcst in the fcst_script to open all
       the files it needs.  dp - 18 July 1995
    */

    CLOSE_ALL_OPEN_FILES();
    /* call script to start appropriate batch run of ofs */
    memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
    len = strlen("ifp_scripts_dir");
    get_apps_defaults("ifp_scripts_dir", &len, &system_command_line[0], &len2);
    strcat(system_command_line, "/");
    strcat(system_command_line, "fcst_script");
    strcat(system_command_line, " ");
    strcat(system_command_line, forecastGroup_name);
    strcat(system_command_line, " ");
    strcat(system_command_line, cmdline_start_run_date);
    strcat(system_command_line, " ");
    strcat(system_command_line, cmdline_end_obs_date);
    strcat(system_command_line, " ");
    strcat(system_command_line, cmdline_end_run_date);

    system(system_command_line);

	/*Added by gzhou 10/2003*/
	if(file_length == 0)
    	printf("Empty mods sent to ofs for forecast group %s\n", forecastGroup_name);

    XtUnmanageChild(final_mods_to_ofs_dialog);
  }                                               /* end if file_length >= 0 */
else
  {                                               /* if file_length < 0    */
    memset(system_command_line, '\0', COMMAND_LINE_LENGTH);

    strcpy(system_command_line, "sleep 4");

    memset(message, '\0', 150);
/* gzhou  change the message
    strcpy(message, "There are no Run-time Mods for forecast group\n");
*/
    strcpy(message, "Unable to send Run-time Mods for forecast group\n");
    strcat(message, "                         ");
    strcat(message, forecastGroup_name);
    xmMessageString = XmStringCreateLtoR(message, XmSTRING_DEFAULT_CHARSET);

    n = 0;
    XtSetArg(wargs[n], XmNdialogTitle,
/*	     XmStringCreate("No Mods sent to OFS",*/
	     XmStringCreate("Trouble to send Mods to OFS",
				 XmSTRING_DEFAULT_CHARSET)); n++;
    XtSetArg(wargs[n], XmNmessageString, xmMessageString); n++;
    XtSetValues(final_mods_to_ofs_dialog, wargs, n);

    XtManageChild(final_mods_to_ofs_dialog);
   /*
    * We need to process all events to get the message to appear
    *  in the information box.
    */
    while(XEventsQueued(display, QueuedAfterFlush) > 0)
	 {
	  XNextEvent(display, &event);
	  XtDispatchEvent(&event);
	 }
/*
    printf("no mods for forecast group %s\n", forecastGroup_name);
*/
    system(system_command_line);
    
    /* Add the code to call the fcst_script even when there are no mods
       for the FG.  Need to keep it in the loop so the dialog stays up
       until it's finished. - dp 18 Nov. 1997
    */
    CLOSE_ALL_OPEN_FILES();

    /* call script to start appropriate batch run of ofs */
    memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
    len = strlen("ifp_scripts_dir");
    get_apps_defaults("ifp_scripts_dir", &len, &system_command_line[0], &len2);
    strcat(system_command_line, "/");
    strcat(system_command_line, "fcst_script");
    strcat(system_command_line, " ");
    strcat(system_command_line, forecastGroup_name);
    strcat(system_command_line, " ");
    strcat(system_command_line, cmdline_start_run_date);
    strcat(system_command_line, " ");
    strcat(system_command_line, cmdline_end_obs_date);
    strcat(system_command_line, " ");
    strcat(system_command_line, cmdline_end_run_date);

    system(system_command_line);

    XtUnmanageChild(final_mods_to_ofs_dialog);
  }                                               /* end if file_length == 0 */
  
/*  Write out the time that mods were written back to ofs to a file */
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
 strcpy(system_command_line, "date > $HOME/.ifp_files/local/mods_sent_to_ofs");
 system(system_command_line);

/*  Write out the forecast group of the mods sent back to OFS */
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
 strcpy(system_command_line, "echo ");
 strcat(system_command_line, forecastGroup_name);
 strcat(system_command_line, " >> $HOME/.ifp_files/local/mods_sent_to_ofs");
 system(system_command_line);

/*  Add the date in numerical form to be used for date comparison - 3 Sept. 97 */
 memset(system_command_line, '\0', COMMAND_LINE_LENGTH);
 strcpy(system_command_line, "date +%Y%m%d.%H%M%S >> $HOME/.ifp_files/local/mods_sent_to_ofs");
 system(system_command_line);
 
 
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/final_mods_to_ofs.c,v $";
 static char rcs_id2[] = "$Id: final_mods_to_ofs.c,v 1.13 2006/04/07 13:29:50 aivo Exp $";}
/*  ===================================================  */

}


/* converts run_dates to string format */
char *convert2_dates_str(date *sys_dates)
{
    char   *command_line_dates;

    command_line_dates = (char *)malloc(DATE_STR_LENGTH);

    sprintf(command_line_dates, "%02d%02d%02d%02d%4s",
               sys_dates->month,
               sys_dates->day,
               (sys_dates->year % 100),
               sys_dates->hour,
               sys_dates->time_zone);

    return command_line_dates;
}
