#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "libXs.h"
#include "plot.h"
#include "ifp_struct.h"

#define MAX_BUF_LEN 200
/*--------------------------------------------------------------------
        save_ifp_gif_file(plot_cb_struct)
        
        Saves a gif file of the IFP Plot window into a users
        $HOME/.ifp_files/gif_files directory.
--------------------------------------------------------------------*/        
        
void	save_ifp_gif_file(plot_cb_struct *plot_data)
{
   	static Cursor	watch = (Cursor)NULL;
	Window		window;
	Display		*display;
 	XTextProperty	text_prop_return;
	Status		status;
	Widget		toplevel;
 
  	char		cmdbuf[MAX_BUF_LEN],
	   		dest[MAX_BUF_LEN],
	   		path[MAX_BUF_LEN],
			buf[MAX_BUF_LEN],
			window_orig[MAX_BUF_LEN],
			**list_return,
	   		*str,
	   		*segment_name;
	const char      tokensep[] = " ";   		
	
	int		count_return,
	                len,
                        len2,
                        n,
                        plot_num;
                        
	Arg             wargs[5];
	XmString        xm_segment_name;
		

	/*
		Initialize local variables.
	*/
	display = XtDisplay(plot_data->main_plot_shell);
	window = XtWindow(plot_data->main_plot_shell);	
	if (! watch)
	   	watch = XCreateFontCursor(display, XC_watch);

	
        /*
                Set the pointer to a watch cursor.
                Raise the window to the top.
        */
        XDefineCursor(display, window, watch);
        XRaiseWindow(display, window);
        XmUpdateDisplay(plot_data->main_plot_shell);
	
	
        /*      
                Get the segment name   
        */
        n=0;
        XtSetArg(wargs[n], XmNlabelString, &xm_segment_name); n++;
        XtGetValues(plot_data->seg_name_widget, wargs, n);
        segment_name = xs_get_string_from_xmstring(xm_segment_name);
        segment_name = strtok(segment_name, tokensep);
	
	/*
	        Figure out which Tulsa plot in the segment
	*/
	plot_num = get_plot_num(plot_data->main_plot_shell);
	
	/*	
		Build the path name for the output gif file.
	*/
	str = (char *)getenv("HOME");
	
        if (str)
	{ 
	   	sprintf(path,"%s/.ifp_files/gif_files/temp.xwd",str);
		sprintf(dest, "%s/.ifp_files/gif_files/%s.%d.gif",
		        str, segment_name, plot_num);
	}
        else
        {
                printf("Environment variable for HOME not found");
                return;
        }


	/*
		retrieve the original window name
	*/	
	status = XGetWMName(display, window, &text_prop_return);

	/*
		Bail on failure.
	*/
	if (!status)
	{
		fprintf(stderr, "Error - XGetWMName returned %d\n",status);
	   	return;
	}   
	
	XTextPropertyToStringList(&text_prop_return,
				  &list_return, &count_return);
	
	if (count_return > 0)
	{  
		strcpy(window_orig, list_return[0]);
		XFreeStringList(list_return);
	}
	
	/*      
	        Set title to new name
	*/
	n=0;
	XtSetArg(wargs[n], XmNtitle, dest); n++;
	XtSetValues(plot_data->main_plot_shell, wargs, n);

	
	/*      
	        Update the display 
	*/
	XmUpdateDisplay(plot_data->main_plot_shell);
	
	
	/*
		Dump the window to an X windows dump file.
	*/
	sprintf(cmdbuf, "/usr/bin/X11/xwd -name %s -out %s\n", 
		dest, path);
	system(cmdbuf);
	
	
	/*
		Convert the dump file to GIF format.
	*/
	/*sprintf(cmdbuf, "imconv %s %s\n", path, dest);--by AV--*/
	/* --AV-- On hp use imconv for converting image file     */
	/*        On linux use convert for converting image file */
#ifdef LINX
        sprintf(cmdbuf, "convert %s %s\n", path, dest);
#else
        sprintf(cmdbuf, "imconv %s %s\n", path, dest);
#endif
	system(cmdbuf);	
	
	
	/*
		Delete the initial dump file.
	*/
	(void) unlink((const char *) path);
	
	
	/*
                Set cursor to original pointer.
        */
        XUndefineCursor(display, window);
        XmUpdateDisplay(plot_data->main_plot_shell);
	return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/save_ifp_gif_file.c,v $";
 static char rcs_id2[] = "$Id: save_ifp_gif_file.c,v 1.3 2006/04/07 14:10:58 aivo Exp $";}
/*  ===================================================  */

}
