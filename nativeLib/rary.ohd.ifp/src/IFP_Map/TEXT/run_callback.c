
/* **********************************************************************

	fgm_callback.c
		callback functions & accessory functions for fgmap.c


	Coded by:               Tom Adams - 2/??/90
	Modifications by:       Tom Adams & George Smith (NWS/OH/HRL)
				Tom Adams 01/09/92 to correct bugs relate to handling multiple trees
				Ai Vo     - 5/11/01 turn off sacsnow switch when
                                                    goto segment is selected
                                                    When select rerun, activate sacsnow switch
   ********************************************************************** */


#include "libXifp.h"
#include "ifp_atoms.h"
#include "libXs.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "ifp_help.h"

static int end_NWSRFS_run();


/* passed from non_univ_tech_cbs.c */
extern int             sacsnowCurrentVal;
extern int             ssUpstreamFlag;
extern int             fgroup_created_once;
extern int             first_time;
/* passed from  disp_funcs.c */
extern char            selectedSegmentStr[9];
int TSTVAL ;
/* **************************************************************

	invert_tree_widget()
		callback function to highlight the node selected

   ************************************************************** */

void invert_tree_widget(w)
	Widget  w;
{

	int     fg, bg;

/*      Get the widget's current colors                         */

 if(w == NULL) return;
 XtVaGetValues(w, XtNforeground, &fg, XtNbackground, &bg, NULL);

/*     Reverse them and set the new colors                      */

 XtVaSetValues(w, XtNforeground, bg, XtNbackground, fg, NULL);

}



/* **************************************************************

	change_widget_color()

   ************************************************************** */

void change_widget_color(w, pixel)
	Widget          w;
	int             pixel;
{
	Arg     wargs[1];


/*     Set the new color...                     */

XtSetArg(wargs[0], XtNbackground, pixel);
XtSetValues(w, wargs, 1);


}



/***************************************************************

	border_white()
		callback function to turn the border white for the node selected

***************************************************************/

void border_white(w, client_data, call_data)
	Widget  w;
	caddr_t    client_data;
	caddr_t    call_data;
{
	Arg     wargs[1];
	int     pixel;

/*     Set the enabling widget's borderColor to white                     */
       pixel = get_pixel_by_name(w,"white");

       XtSetArg(wargs[0], XtNborderColor, pixel);
       XtSetValues(w, wargs, 1);
}



/*********************************************************

	unhilite()
		un_hilites the selected widget

********************************************************/

void unhilite(w, enable, call_data)
	Widget                 w;
	Widget                 enable;
	caddr_t                call_data;
{
	Arg     wargs[1];
	int     bg;

/*     Set the enabling widget's borderColor to the tree background   */

XtSetArg(wargs[0], XtNbackground, &bg);
XtGetValues(w, wargs, 1);

XtSetArg(wargs[0], XtNborderColor, bg);
XtSetValues(enable, wargs, 1);

}



/*********************************************************

	post_change()
	      lists the widget name when the widget button is selected

********************************************************/

void post_change(w, segment_name, call_data)
	Widget          w;
	char            segment_name[9];
	caddr_t         call_data;
{
	XChangeProperty(XtDisplay(w),
			DefaultRootWindow(XtDisplay(w)),
			IFPA_seg_deleted, IFPA_seg_deleted_type,
			8, PropModeReplace,
			(unsigned char *) segment_name,
			strlen(segment_name));
}




/* **************************************************************

	show_run_segments()


   ************************************************************** */

void show_run_segments(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

if(someWidgets->run_info_shell == NULL) return;


if(call_data->set) XtPopup(someWidgets->run_info_shell, XtGrabNone);
else               XtPopdown(someWidgets->run_info_shell);


}



/* **************************************************************

	show_deleted_segments()


   ************************************************************** */

void show_deleted_segments(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{


if(call_data->set) XtPopup(someWidgets->delete_shell, XtGrabNone);
else               XtPopdown(someWidgets->delete_shell);


}


/* **************************************************************

	set_universal()


   ************************************************************** */

void set_universal(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{
	Arg     wargs[1];


someWidgets->universal_widget = w;
XtPopup(someWidgets->universalTech_popup_shell, XtGrabNone);
XmProcessTraversal(someWidgets->universalTech_ok_widget, XmTRAVERSE_CURRENT);

/* printf("Run_NWSRFS - universalTech_popup_shell window id = %d\n",
	  XtWindow(someWidgets->universalTech_popup_shell));            */

}


/* **************************************************************

	set_non_universal()


   ************************************************************** */

void set_non_universal(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

 XtPopup(someWidgets->non_universalTech_popup_shell, XtGrabNone);
 XmProcessTraversal(someWidgets->non_universalTech_ok_widget, XmTRAVERSE_CURRENT);

}


/* **************************************************************

	rerun_segment()


   ************************************************************** */

void rerun_segment(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;

{

	Arg             wargs[2];
	Display         *display;
	Window          root;

	long            offset = 0;
	Atom            type;
	int             format, nitems, left;
	int             i, last_item, number_of_items;
	int             ok_to_write_mods = TRUE;
        char            segment_name_str[9];
	char            *segment_name, *string;
        char            *first_blank;
        node            *the_segment;




display = XtDisplay(w);
root = DefaultRootWindow(display);

if(mods_pending(someWidgets, "Rerun")) return;

 /* Window change property to signal the forecast program to call 'Write_mods()'...             */
 XChangeProperty(
	 display,
	 root,
	 IFPA_ok_to_write_mods,
	 IFPA_ok_to_write_mods_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&ok_to_write_mods,
	 sizeof(int)
	 );

 XmToggleButtonSetState(someWidgets->showRatingCurve_widget, FALSE, FALSE);
 XmToggleButtonSetState(someWidgets->showOperationsTable_widget, FALSE, FALSE);

segment_run_type = RERUN_SEGMENT;
stay_in_CEX25 = FALSE;


if(XGetWindowProperty
	(
	display,
	root,
	IFPA_current_segment,
	offset,
	(long) 9,
	FALSE,
	IFPA_current_segment_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&segment_name) == Success && type == IFPA_current_segment_type)
	{

	XChangeProperty(        /* Window change property for the next forecast segment name...         */
		display,
		root,
		IFPA_rerun_segment,
		IFPA_rerun_segment_type,
		8,
		PropModeReplace,
		(unsigned char *) segment_name,
		strlen(segment_name)
		);
	}
        
/* AV 5/11/01 -- when switching on the technique, rerun also  
pass non-universal techniques to other NWSRFS/IFP components*/


 
 if((first_blank = strstr(segment_name, " ")) != NULL) *first_blank = '\0';
 /*Aivo added --when hit rerun check non-universal techniques window**********/
 
 for(i = 0; i <= sub_group_num; i++)
 {
        
	if((the_segment = find_it(someWidgets->head[i], segment_name)) != NULL) break;
        
 }
 
 /* restores value of sacsnow switch from non-universal techniques window*/
 the_segment->techniques->sac_snow = sacsnowCurrentVal;
 /* Pass non-universal techniques to other NWSRFS/IFP components         */
 /*      through an X Window property...                                 */
 non_univ_techs_to_window_property(segment_name, w, someWidgets);

/* AV 5/11/01 end */
}


/* **************************************************************

	run_next_segment()


   ************************************************************** */

void run_next_segment(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{
	Arg             wargs[2];
	Display         *display;
	Window          root;

	long            offset = 0;
	Atom            type;

	int             j, number_of_Segments;
	int             format, nitems, left;
	int             i, last_item, number_of_items;
	int             *number_of_mods_to_write;
	int             *number_of_mods_to_delete;
	int             show_operationsTable;

	node            *this_node;

	char            *segment_name, *string;
	char            segment_name_str[9];
	char            *first_blank;

	XmString        *xmstring_Segments;
	XmString        xmShowString;



display = XtDisplay(w);
root = DefaultRootWindow(display);


if(mods_pending(someWidgets, "Next")) return;

if(XGetWindowProperty
	(
	display,
	root,
	IFPA_number_of_mods_to_write,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_number_of_mods_to_write_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&number_of_mods_to_write) == Success && type == IFPA_number_of_mods_to_write_type)
	{
	if(*number_of_mods_to_write > 0 ) 

		{
		popup_mods_not_written_to_file_warningDialog(someWidgets);
		return;
		}
	}

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_num_mods_to_delete_fromFile,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_num_mods_to_delete_fromFile_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&number_of_mods_to_delete) == Success && type == IFPA_num_mods_to_delete_fromFile_type)
	{
	if(*number_of_mods_to_delete > 0)
		{
		popup_modsNotDeleted_warningDialog(someWidgets);
		return;
		}
	}


if(XGetWindowProperty
	(
	display,
	root,
	IFPA_current_segment,
	offset,
	(long) sizeof(char)*9,
	FALSE,
	IFPA_current_segment_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&segment_name) == Success && type == IFPA_current_segment_type)
	{

	XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
	XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
	XtGetValues(someWidgets->listWidget_for_forecastSegments, wargs, 2);

	/* Find the next segment name...                                                                */
	/* Segments names appear in the list widget in computational order... upstream -> downstream... */
	for(j = 0; j < number_of_Segments; j++)
		{

		XmStringGetLtoR(xmstring_Segments[j], XmSTRING_DEFAULT_CHARSET, &string);
		if(strncmp(string, segment_name,strlen(segment_name)) == 0)
			{
        /* Change made to account for finding a match of segment names with the last           */
        /*  segment name in the list.  That is, if j == number_of_Segments there is no valid   */
        /*  segment name (string) for the j + 1 element of the list.  This situation can occur */
        /*  when the last segment in a forecast has no Tulsa plot, and that segment is entered */
        /*  by choosing "Next" while in the segment just upstream of the last one.             */
        /* Modified by gfs - hrl - 5 Oct 1994.                                                 */
                        if (j < number_of_Segments - 1)
                         {
			    XmStringGetLtoR(xmstring_Segments[j + 1], 
                                            XmSTRING_DEFAULT_CHARSET, &string);
                          
                         }
                        else
                        
			    XmStringGetLtoR(xmstring_Segments[j], 
                                            XmSTRING_DEFAULT_CHARSET, &string);
                       
			strcpy(segment_name_str, string);
			break;
			}
		}

	}


 show_operationsTable = FALSE;

 XChangeProperty(
	 display,
	 root,
	 IFPA_show_operations_table,
	 IFPA_show_operations_table_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&show_operationsTable,
	 sizeof(int)
	 );

 /* XSync(display, 0);  */

 XChangeProperty(        /* Window change property for the next forecast segment name...         */
	 display,
	 root,
	 IFPA_next_segment,
	 IFPA_next_segment_type,
	 8,
	 PropModeReplace,
	 (unsigned char *) segment_name_str,
	 strlen(segment_name_str)
	 );

 /* If the next segment 'segment_name_str' is the last segment, there is no 'next_segment' to go to...  */
 /*     so, dim the 'next_widget' button to prevent this from happening...                              */
 XmStringGetLtoR(xmstring_Segments[number_of_Segments - 1], XmSTRING_DEFAULT_CHARSET, &string);
 if(strcmp(segment_name_str, string) == 0)
	{
	XtSetSensitive(someWidgets->next_widget, FALSE);
	inLastSegment = TRUE;
	}

 if((first_blank = strstr(segment_name_str, " ")) != NULL) *first_blank = '\0';

 
 /******** AIVO 5/16/01******/
 /* set sacsnow switch to on when finished goto_selected_segment else set to off */
 for(i = 0; i <= sub_group_num; i++)
 {
    
     if((this_node = find_it(someWidgets->head[i], segment_name_str)) != NULL) break;      
 }
 /* set selectedSegmentStr == END when completing goto selected segment */
  
 if(((strcmp(selectedSegmentStr, "END") == 0 )|| (ssUpstreamFlag == 1)) && (sacsnowCurrentVal == 1))
 {
     if(sacsnowCurrentVal == 1)
     {
         this_node->techniques->sac_snow = 1;
         TSTVAL = 1;
     }
    
 }
 else 
 {
     this_node->techniques->sac_snow = 0;
     TSTVAL =0;
    
    
 }
 


 
 
 /*non_univ_techs_to_window_property(segment_name_str, w, someWidgets);*/
 if((strcmp(selectedSegmentStr, segment_name_str) == 0) || (ssUpstreamFlag == 1))
 {
     if(sacsnowCurrentVal == 1)
     
         this_node->techniques->sac_snow = 1;
         TSTVAL = 1;
     /* set this str to flag ending of goto segment */
     strcpy(selectedSegmentStr,"END");
     ssUpstreamFlag == 0;
    
 }
 /* a debug print statement *************
 printf(" segment_name_str = %s selectedseg = %s ssUpstr= %d  sacsnow=%d currentsacsnow =%d\n",segment_name_str, 
                   selectedSegmentStr,ssUpstreamFlag,this_node->techniques->sac_snow, sacsnowCurrentVal); 
 */                 
 /* Pass non-universal techniques to other NWSRFS/IFP components         */
 /*      through an X Window property...                                 */ 
 non_univ_techs_to_window_property(segment_name_str, w, someWidgets);
 /************AV 5/16/01*******************/
 XmToggleButtonSetState(someWidgets->showRatingCurve_widget, FALSE, FALSE);
 XmToggleButtonSetState(someWidgets->showOperationsTable_widget, FALSE, FALSE);

 XtSetSensitive(someWidgets->showRatingCurve_widget, FALSE);

 segment_run_type = NEXT_SEGMENT;
 stay_in_CEX25 = FALSE;


}


/* **************************************************************

	revert_to_original_tree()


   ************************************************************** */

void revert_to_original_tree(Widget w, the_widget_struct *someWidgets, caddr_t *call_data)
{
	Widget          tree;
	Widget          horizScrollBar;
	Arg             wargs[5];
	Display         *display;
	Window          treeWindow, root;
	char            *segmentSelected;
	XmString        xmShowString;
	int             type, format, nitems, left;
	int             i, numberOfListItems;
	int             j, num_list;
	long            offset = 0;

	int             n;
	Dimension       scrollBar_height;
	Dimension       tree_height;
	Dimension       tree_width;
	Dimension       shell_height;
	Dimension       shell_width;
	Dimension       parent_margin;
	Position        tree_position;



 display = XtDisplay(global_toplevel);
 root = DefaultRootWindow(display);

 XDeleteProperty(display, root, IFPA_run_segments);

 XtVaGetValues(someWidgets->delete_list, XmNitemCount, &numberOfListItems, NULL);

 for(i = 0; i < numberOfListItems; i++)
	{
	XmListDeletePos(someWidgets->delete_list, 0);
	}

 XtVaSetValues(someWidgets->delete_list,
	       XmNvisibleItemCount, 1,
	       XmNitemCount,        1,
	       NULL);



 treeWindow = XtWindow(someWidgets->run_info_shell);
 if(treeWindow > 0)     /* Check if the widget has ever been mapped &, therefore, if it's windowID > 0; */
	{               /* if we don't check & we try to UnmapWindow, we'll get an X protocol error...  */
	XtPopdown(someWidgets->run_info_shell);
	XtDestroyWidget(someWidgets->run_info_bb);
	}
 someWidgets->viewDates = NULL;



 if(someWidgets->run_info_shell == NULL) return;

 for(i = 0; i <= sub_group_num; i++)
	{
	if(someWidgets->head[i] != NULL)
		{
		treeWindow = XtWindow(someWidgets->head[i]->parent_widget);
		XUnmapWindow(display, treeWindow);
		XUnmapSubwindows(display, treeWindow);
		destroy_tree_children(someWidgets->head[i]);
		XtDestroyWidget(someWidgets->head[i]->parent_widget);
		}
	someWidgets->head[i] = NULL;
	}

 create_upstream_seg_tree(someWidgets, someWidgets->rc_for_tree_widgets);

 XtVaGetValues(someWidgets->rc_for_tree_widgets, XmNmarginHeight, &parent_margin, NULL);
 tree_position = (Position)RC_BORDER_WIDTH;
 for(i = 0; i <= sub_group_num; i++)
	{
	if(someWidgets->head[i] != NULL)
		{
		if(i)   {       /* i > 0...                                                                     */
			for(j = i - 1; j >= 0; j--)
				{       /* Place the current tree widget after the first non-NULL tree...       */
				if(someWidgets->head[j] != NULL)
					{
					XtVaGetValues(someWidgets->head[j]->parent_widget, XmNheight, &tree_height, NULL);

					tree_position += (Position)(tree_height + (Dimension)RC_BORDER_WIDTH);
					break;
					}
				}
			}

		/* Set the vertical position of the next TreeWidget...                                          */
		XtVaSetValues(someWidgets->head[i]->parent_widget,
			      XmNy, tree_position + (Position)parent_margin, NULL);
		}
	}

 set_snowModel_buttons_to_default(w, someWidgets, NULL);

 /* Delete all of the items in Non-universal List widget...                      */
 XtVaGetValues(someWidgets->non_univ_list, XmNitemCount, &num_list, NULL);
 for(j = 0; j < num_list; j++) XmListDeletePos(someWidgets->non_univ_list, 1);


 /* Re-fill the list of items in Non-universal List widget...                    */
 fill_list(&num_list);
 XtVaSetValues(someWidgets->non_univ_list,
	       XmNitems,            xmstr_Run_list,
	       XmNitemCount,        num_list,
	       XmNvisibleItemCount, num_list,
	       NULL);

 numberOfSubgroupsSelected = 0;

 XtSetSensitive(someWidgets->keepSubset_widget, FALSE);
 XtSetSensitive(someWidgets->deleteSegments_widget, FALSE);
 XtSetSensitive(someWidgets->reset_widget, FALSE);
 XtSetSensitive(someWidgets->revert_widget, FALSE);
 XtSetSensitive(someWidgets->showRunSegments_widget, FALSE);

 add_overlays(someWidgets->main_canvas, someWidgets, NULL);

}


/* **************************************************************

	exit_nwsrfs()


   ************************************************************** */

void exit_nwsrfs(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

	Display         *display;
	Window          root;
	int             IFP_is_already_running;
	int             quitFlag;



 display = XtDisplay(global_toplevel);
 root = DefaultRootWindow(display);

 if(!end_NWSRFS_run(w, someWidgets)) return;

 XDeleteProperty(display, root, IFPA_IFP_NWSRFS_is_running);
 XSync(display, 0);

/*----------------------------------------------------------------------*/
/* sleep for a few seconds so that ifp_nwsrfs program can finish cleanly*/
/* (i.e., write final time series to file and end normally) before      */
/* the IFP_Map program ends and the shell they both were running in     */
/* closes.  This is only needed when running from the main menu where   */
/* a new shell is created to run the IFP_Map program.                   */
/*                                                                      */
/*              added by gfs, 30 Nov 1992                               */
/*              changed to 1 second on HPs by gfs, 1 Sept 1993          */
/*----------------------------------------------------------------------*/

 system("sleep 1");
 
 // add call to ifp_cleanup to cleanup local copy of DHM grids
 fork_cleanup_script();
 exit(0);

}



/* **************************************************************

	select_next_segment()


   ************************************************************** */

void select_next_segment(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

	Display         *display;
	Window          root;

	long            offset = 0;
	Atom            type;

	int             format, nitems, left;
	int             *number_of_mods_to_write;
	int             *number_of_mods_to_delete;


display = XtDisplay(w);
root = DefaultRootWindow(display);

if(XGetWindowProperty
	(
	display,
	root,
	IFPA_number_of_mods_to_write,
	offset,
	(long) 9,
	FALSE,
	IFPA_number_of_mods_to_write_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&number_of_mods_to_write) == Success && type == IFPA_number_of_mods_to_write_type)
	{
	if(*number_of_mods_to_write > 0) 
		{
		popup_mods_not_written_to_file_warningDialog(someWidgets);
		return;
		}
	}

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_num_mods_to_delete_fromFile,
	offset,
	(long) 9,
	FALSE,
	IFPA_num_mods_to_delete_fromFile_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&number_of_mods_to_delete) == Success && type == IFPA_num_mods_to_delete_fromFile_type)
	{
	if(*number_of_mods_to_delete > 0)
		{
		popup_modsNotDeleted_warningDialog(someWidgets);
		return;
		}
	}

 XtMapWidget(someWidgets->select_next_shellWidget);

}


/* **************************************************************

	set_run_dates()


   ************************************************************** */

void set_run_dates(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{
	char    bin_command[80];
        int     len, len2;

	memset(bin_command, '\0', 80);
	/* call routine to get the path name for bin files */
        len = strlen("ifp_bin_dir");
	get_apps_defaults("ifp_bin_dir", &len, bin_command, &len2);

	strcat(bin_command, "/set_dates");

	system(bin_command);
}



/* **************************************************************

	show_Tulsa_plot()


   ************************************************************** */

void show_Tulsa_plot(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{
	Display         *display;
	Window          root;
	int             showPlot;
	int             isRealized;



display = XtDisplay(w);
root = DefaultRootWindow(display);

if(call_data->set) showPlot = TRUE;
else               showPlot = FALSE;


 XChangeProperty(
	 display,
	 root,
	 IFPA_show_tulsa_plot,
	 IFPA_show_tulsa_plot_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&showPlot,
	 sizeof(int)
	 );


}




/* **************************************************************

	show_plot_TS()


   ************************************************************** */

void show_plot_TS(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{
	Display         *display;
	Window          root;
	int             showPlot;
	int             isRealized;



display = XtDisplay(w);
root = DefaultRootWindow(display);

if(call_data->set) showPlot = TRUE;
else               showPlot = FALSE;


 XChangeProperty(
	 display,
	 root,
	 IFPA_show_plot_TS,
	 IFPA_show_plot_TS_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&showPlot,
	 sizeof(int)
	 );


}




/* **************************************************************

	show_TimeSeries_table()


   ************************************************************** */

void show_TimeSeries_table(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{
	Display         *display;
	Window          root;
	int             showTable;
	int             isRealized;



display = XtDisplay(w);
root = DefaultRootWindow(display);

if(call_data->set) showTable = TRUE;
else               showTable = FALSE;


 XChangeProperty(
	 display,
	 root,
	 IFPA_show_tables,
	 IFPA_show_tables_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&showTable,
	 sizeof(int)
	 );

}




/* **************************************************************

	show_other_mods()


   ************************************************************** */

void show_other_mods(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{
	Display         *display;
	Window          root;
	int             showMods;
	int             isRealized;



display = XtDisplay(w);
root = DefaultRootWindow(display);


if(call_data->set) showMods = TRUE;
else               showMods = FALSE;


 XChangeProperty(
	 display,
	 root,
	 IFPA_show_mods,
	 IFPA_show_mods_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&showMods,
	 sizeof(int)
	 );



}


/* **************************************************************

	set_leave_NWSRFS_to_Yes()


   ************************************************************** */

void set_leave_NWSRFS_to_Yes(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

 if(!someWidgets->GoToNextForecastGroup)
	{
	leave_NWSRFS  = TRUE;
	exit_nwsrfs(w, someWidgets, NULL);
	}

}




/* **************************************************************

	continue_with_next_segment()


   ************************************************************** */

void continue_with_next_segment(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

	Arg             wargs[1];
	Display         *display;
	Window          root;

	int             number_of_mods_to_write = 0;



display = XtDisplay(w);
root = DefaultRootWindow(display);

 XChangeProperty(
	 display,
	 root,
	 IFPA_number_of_mods_to_write,
	 IFPA_number_of_mods_to_write_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&number_of_mods_to_write,
	 sizeof(int)
	 );

 run_next_segment(w, someWidgets, NULL);


}



/* **************************************************************

	continue_to_next_TulPlot()


   ************************************************************** */

void continue_to_next_TulPlot(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{
	Arg             wargs[1];
	Display         *display;
	Window          root;

	int             continueFlag;


display = XtDisplay(w);
root = DefaultRootWindow(display);

if(mods_pending(someWidgets, "Continue")) return;

 continueFlag = TRUE;

	XChangeProperty(
		display,
		root,
		IFPA_continue_to_next_operation,
		IFPA_continue_to_next_operation_type,
		8,
		PropModeReplace,
		(unsigned char *)&continueFlag,
		sizeof(int)
		);

 XmToggleButtonSetState(someWidgets->showOperationsTable_widget, FALSE, FALSE);
 XmToggleButtonSetState(someWidgets->showRatingCurve_widget, FALSE, FALSE);

}


void popup_NWSRFS_stopped_working(someWidgets, id)

 the_widget_struct       *someWidgets;
 XtIntervalId            *id;
{
 char           *message_string;
 Arg            wargs[5];
 XmString       xmMessage;
	Position        tree_shell_x, tree_shell_y, stopped_x, stopped_y;
	Dimension       tree_shell_width, tree_shell_height;
	Dimension       stopped_width, stopped_height;
	int             n;

/*
 * This callback invoked if the forecast program (hydrologic
 *  part of NWSRFS) has run for more than the time allowed.
 *
 * Go through some hocus pocus to let the new line characters
 *  (i.e. \n) be properly interpreted by the text widget.
 *  Hopefully the Xt resource translation manager will be
 *  fixed someday and this patch can be removed.  gfs, 8/6/91
 */
 XtSetArg(wargs[0], XmNmessageString, &xmMessage);

 XtGetValues(someWidgets->NWSRFS_stopped_shell, wargs, 1);

 XmStringGetLtoR(xmMessage, XmSTRING_DEFAULT_CHARSET, &message_string);

/*
 * Set text for the popup shell so that the shell width is correct.
 */
 n = 0;
 XtSetArg(wargs[n], XmNmessageString, XmStringCreateLtoR(message_string,
	  XmSTRING_DEFAULT_CHARSET)); n++;

 XtSetValues(someWidgets->NWSRFS_stopped_shell, wargs, n);

/*
 * Get position and size of tree_shell widget for use in
 *  positioning the stopped popup.
 */
 n = 0;
 XtSetArg(wargs[n], XmNx, &tree_shell_x); n++;
 XtSetArg(wargs[n], XmNy, &tree_shell_y); n++;
 XtSetArg(wargs[n], XmNwidth, &tree_shell_width); n++;
 XtSetArg(wargs[n], XmNheight, &tree_shell_height); n++;

 XtGetValues(someWidgets->tree_shell, wargs, n);
/*
 * Get width of stopped popup to try to center the
 *  popup on the tree_shell horizontally.
 */
 n = 0;
 XtSetArg(wargs[n], XmNwidth, &stopped_width); n++;
 XtSetArg(wargs[n], XmNheight, &stopped_height); n++;

 XtGetValues(someWidgets->NWSRFS_stopped_shell, wargs, n);

 stopped_x = tree_shell_x + 0.5 * tree_shell_width -
			    0.5 * stopped_width;
 if(stopped_x < 0) stopped_x = 0;
 if(stopped_x + stopped_width > Screen_Width)
		stopped_x = Screen_Width - stopped_width;
 stopped_y = tree_shell_y + tree_shell_height;
 if(stopped_y + stopped_height > Screen_Height)
		stopped_y = Screen_Height - stopped_height;
/*
 * Set location for the popup shell.
 */
 n = 0;
 XtSetArg(wargs[n], XmNx, stopped_x); n++;
 XtSetArg(wargs[n], XmNy, stopped_y); n++;

 XtSetValues(someWidgets->NWSRFS_stopped_shell, wargs, n);

 XtUnmanageChild(someWidgets->NWSRFS_working_shell);

 XtManageChild(someWidgets->NWSRFS_stopped_shell);

 XtSetSensitive(run_cascade[0], TRUE);
 XtSetSensitive(run_cascade[1], FALSE);
 XtSetSensitive(run_cascade[3], FALSE);

 XtSetSensitive(someWidgets->begin_widget, FALSE);
 XtSetSensitive(someWidgets->rerun_widget, FALSE);
 XtSetSensitive(someWidgets->next_widget, FALSE);
 XtSetSensitive(someWidgets->continue_widget, FALSE);
}



/* **************************************************************

	go_to_selected_segment()


   ************************************************************** */
extern int TSTVAL;
void go_to_selected_segment(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
       
{
	Display         *display;
	Window          root;

	char            currentSegment[9];
	char            *first_blank;

	Arg             wargs[5];
	Atom            type;

	long            offset = 0;
	int             format, nitems, left;
	int             *number_of_TulPlots;
	int             show_operationsTable;
	int             ok_to_write_mods = TRUE;
       
	XmString        xmShowString;
        node            *the_node;
        int             i;


 display = XtDisplay(w);
 root = DefaultRootWindow(display);
 

 fgroup_created_once = 0; /*reset after save fgroup mods data */
 if(mods_pending(someWidgets, "Go to segment")) return;
 
 XtSetSensitive(someWidgets->go_to_widget, FALSE);

 strcpy(currentSegment, someWidgets->current_segment);
 

 /* If 'currentSegment' has any trailing blanks, remove them...                                         */
 if((first_blank = strstr(currentSegment, " ")) != NULL) *first_blank = '\0';

 if (is_computationally_before(currentSegment, someWidgets))
 {       /* The selected segment is computationally before the current segment...                */
	restore_default_colors(someWidgets->previous_segment);

	invert_tree_widget(someWidgets->previous_segment);
	select_nextSegment(w, someWidgets, NULL);
        /* aivo added */
       
        
        if(sacsnowCurrentVal == 1)
        {
            for(i = 0; i <= sub_group_num; i++)
            {
    
                 if((the_node = find_it(someWidgets->head[i], currentSegment)) != NULL) break;      
            }
            the_node->techniques->sac_snow = 1;
            /* Pass non-universal techniques to other NWSRFS/IFP components         */
            /*      through an X Window property...                                 */ 
            non_univ_techs_to_window_property(currentSegment, w, someWidgets); 
        }
        /* a debug print statement ***********
        printf("currentsegmentr = %s selectedseg = %s ssUpstr= %d  sacsnow=%d currentsacsnow =%d\n",currentSegment, 
                   selectedSegmentStr,ssUpstreamFlag,the_node->techniques->sac_snow, sacsnowCurrentVal); 
          
        
        *************/
        
        /* End aivo ***/
 }
 else   {       /* The selected segment is computationally after the current segment...                 */

	XChangeProperty(
		display,
		root,
		IFPA_goto_downstream_segment,
		IFPA_goto_downstream_segment_type,
		8,
		PropModeReplace,
		(unsigned char *) someWidgets->segment_selected,
		strlen(someWidgets->segment_selected)
		);


	if(XGetWindowProperty
		(
		display,
		root,
		IFPA_number_of_TulPlots,
		offset,
		(long) 9,
		FALSE,
		IFPA_number_of_TulPlots_type,
		(Atom *)&type,
		(int *)&format,
		(unsigned long *)&nitems,
		(unsigned long *)&left,
		(unsigned char **)&number_of_TulPlots) == Success && type == IFPA_number_of_TulPlots_type){
		if(*number_of_TulPlots > 0)
			continue_to_next_TulPlot(w, someWidgets, NULL);
		else    run_next_segment(w, someWidgets, NULL);

		}
	}

 someWidgets->previous_segment = NULL;

 show_operationsTable = FALSE;

 XChangeProperty(
	 display,
	 root,
	 IFPA_show_operations_table,
	 IFPA_show_operations_table_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&show_operationsTable,
	 sizeof(int)
	 );

 XtSetSensitive(someWidgets->rerun_widget, FALSE);

 XmToggleButtonSetState(someWidgets->showOperationsTable_widget, FALSE, FALSE);
 XmToggleButtonSetState(someWidgets->showRatingCurve_widget, FALSE, FALSE);

 XtSetSensitive(someWidgets->showRatingCurve_widget, FALSE);

}



/* **************************************************************

	set_run_multiple()


   ************************************************************** */

void set_run_multiple(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{
	Arg             wargs[1];
	XmString        xmCurrentString;
	XmString        xmSingleString;
	XmString        xmMultipleString;
	Display         *display;
	Window          root;
	int             showPlot;
	int             isRealized;
	int             i;



 display = XtDisplay(w);
 root = DefaultRootWindow(display);

 
 xmSingleString = XmStringCreateSimple(SingleSegment);
 xmMultipleString = XmStringCreateSimple(GoToSegment);


 XtVaGetValues(w, XmNlabelString, &xmCurrentString, NULL);

 if(XmStringCompare(xmCurrentString, xmSingleString) == TRUE)
	{       /* The user has chosen to run multiple segments...                      */

	for(i = 0; i <= sub_group_num; i++)
		{
		if(someWidgets->head[i] != NULL)
			{
			remove_segment_callbacks(someWidgets->head[i]);
			add_segment_callbacks(someWidgets->head[i], handle_segment_selected, someWidgets);
			add_segment_callbacks(someWidgets->head[i], popup_segment_info,
					      someWidgets->head[i]->popup_shell);
			}
		}

	XtVaSetValues(w, XmNlabelString, xmMultipleString, NULL);
	}
 else   {       /* The user has chosen to run a single segment...                       */

	for(i = 0; i <= sub_group_num; i++)
		{
		if(someWidgets->head[i] != NULL)
			{
			remove_segment_callbacks(someWidgets->head[i]);
			add_segment_callbacks(someWidgets->head[i], post_change, someWidgets);
			add_segment_callbacks(someWidgets->head[i], amend_seg_list, someWidgets);
			add_segment_callbacks(someWidgets->head[i], popup_segment_info,
					      someWidgets->head[i]->popup_shell);
			}
		}

	if(someWidgets->previous_segment != NULL)
		{
		restore_default_colors(someWidgets->previous_segment);
		someWidgets->previous_segment = NULL;
		}
	XDeleteProperty(display, root, IFPA_goto_downstream_segment);

	XtVaSetValues(w, XmNlabelString, xmSingleString, NULL);
	}

 XmStringFree(xmCurrentString);
 XmStringFree(xmSingleString);
 XmStringFree(xmMultipleString);

}



/* **************************************************************

	show_operations_table()


   ************************************************************** */

void show_operations_table(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{
	Display         *display;
	Window          root;
	int             show_operationsTable;




display = XtDisplay(w);
root = DefaultRootWindow(display);


if(call_data->set) show_operationsTable = TRUE;
else               show_operationsTable = FALSE;


 XChangeProperty(
	 display,
	 root,
	 IFPA_show_operations_table,
	 IFPA_show_operations_table_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&show_operationsTable,
	 sizeof(int)
	 );


}



/* **************************************************************

	show_rating_curve()


   ************************************************************** */

void show_rating_curve(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{
	Display         *display;
	Window          root;
	int             show_ratingCurve;



display = XtDisplay(w);
root = DefaultRootWindow(display);


if(call_data->set) show_ratingCurve = TRUE;
else               show_ratingCurve = FALSE;


 XChangeProperty(
	 display,
	 root,
	 IFPA_show_rating_curve,
	 IFPA_show_rating_curve_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&show_ratingCurve,
	 sizeof(int)
	 );


}

/* **************************************************************

	continue_with_requested_command_saved()


   ************************************************************** */

void continue_with_requested_command_saved(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

	Arg             wargs[1];
	Display         *display;
	Window          root;

	int             currentModSaved = 1;



display = XtDisplay(w);
root = DefaultRootWindow(display);

 XChangeProperty(
	 display,
	 root,
	 IFPA_current_mod_saved,
	 IFPA_current_mod_saved_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&currentModSaved,
	 sizeof(int)
	 );

 if(strcmp(someWidgets->current_command, "Rerun") == 0)
	   rerun_segment(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Next") == 0)
	   run_next_segment(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Continue") == 0)
	continue_to_next_TulPlot(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Go to segment") == 0)
	go_to_selected_segment(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Quit") == 0)
	exit_nwsrfs(w, someWidgets, NULL);

 else
     {
      printf("in continue_with_requested_command_saved, ");
      printf("current_command not known = %s.\n",
	      someWidgets->current_command);
     }
}


/* **************************************************************

	continue_with_requested_command_deleted()

   ************************************************************** */

void continue_with_requested_command_deleted(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

	Arg             wargs[1];
	Display         *display;
	Window          root;

	int             numberOfModsToDeleteFromFile = 0;



display = XtDisplay(w);
root = DefaultRootWindow(display);

 XChangeProperty(
	 display,
	 root,
	 IFPA_num_mods_to_delete_fromFile,
	 IFPA_num_mods_to_delete_fromFile_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&numberOfModsToDeleteFromFile,
	 sizeof(int)
	 );

 if(strcmp(someWidgets->current_command, "Rerun") == 0)
	   rerun_segment(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Next") == 0)
	   run_next_segment(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Continue") == 0)
	continue_to_next_TulPlot(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Go to segment") == 0)
	go_to_selected_segment(w, someWidgets, NULL);

 else if(strcmp(someWidgets->current_command, "Quit") == 0)
	exit_nwsrfs(w, someWidgets, NULL);

 else
     {
      printf("in continue_with_requested_command_deleted, ");
      printf("current_command not known = %s.\n",
	      someWidgets->current_command);
     }
}



/* **************************************************************************

	tell_which_tree()
		identifies which tree widget is currently active:
		that is, which tree has had one of its children selected.

   ************************************************************************** */

void tell_which_tree(w, data, call_data)
	Widget                  w;
	tree_data_struct        *data;
	caddr_t                 call_data;
{

 whichTree_index = data->branch->which_tree;
 data->dataStruct->currentSegment_widget = w;

}



/* **************************************************************

	show_ForecastGroup_topology()


   ************************************************************** */

void show_ForecastGroup_topology(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

if(someWidgets->tree_shell == NULL) return;


if(call_data->set) XtPopup(someWidgets->tree_shell, XtGrabNone);
else               XtPopdown(someWidgets->tree_shell);


}



/* **************************************************************

	popup_FGroup_selectionDialog()


   ************************************************************** */

void popup_FGroup_selectionDialog(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

	int     answer;
	int     i, n;
	Arg     wargs[1];
	char    label[70];




 answer = AskUser(someWidgets->toplevel, "Are you sure you want to terminate the current NWSRFS run?");

 if(RET_YES == answer)
	{
	someWidgets->GoToNextForecastGroup = TRUE;

	if(!end_NWSRFS_run(w, someWidgets)) return;

	for(i = 0; i <= sub_group_num; i++) free(someWidgets->head[i]);
	for(i = 0; i < MAX_NUM_SUBGROUPS; i++) someWidgets->head[i] = (node *) NULL;

	NWSRFS_has_begun = FALSE;
	sub_group_num = -1;
	
	/*  Add code to update the files_date_label and mods_date_label.  
	    dp - Nov. 1997
	 */
	memset(label, '\0', 70);
        get_files_copied_date(label);
        
        n=0;
        XtSetArg(wargs[n], XmNlabelString, 
                 XmStringCreate(label, XmSTRING_DEFAULT_CHARSET)); n++;
        XtSetValues(someWidgets->files_date_label, wargs, n);    

	memset(label, '\0', 70);
        get_mods_copied_date(label);
        
        n=0;
        XtSetArg(wargs[n], XmNlabelString, 
                 XmStringCreate(label, XmSTRING_DEFAULT_CHARSET)); n++;
        XtSetValues(someWidgets->mods_date_label, wargs, n);    


	XtPopup(someWidgets->FcstGroup_selectionBoxShell, XtGrabNone);

	XtSetSensitive(someWidgets->FcstGroup_selectionBoxCancel, TRUE);

	XtSetSensitive(someWidgets->new_ForecastGroup_widget, FALSE);
	XtSetSensitive(someWidgets->next_widget, FALSE);
	XtSetSensitive(someWidgets->continue_widget, FALSE);
	XtSetSensitive(someWidgets->rerun_widget, FALSE);

	XtSetSensitive(someWidgets->universal_widget, FALSE);
	XtSetSensitive(someWidgets->non_universal_widget, FALSE);

	XtSetSensitive(someWidgets->showDeletedSegments_widget, FALSE);
	XtSetSensitive(someWidgets->showRunSegments_widget, FALSE);
	XtSetSensitive(someWidgets->showRatingCurve_widget, FALSE);
	XtSetSensitive(someWidgets->showOperationsTable_widget, FALSE);
	XtSetSensitive(someWidgets->showFGroup_Topology_widget, FALSE);

	XmToggleButtonSetState(someWidgets->showDeletedSegments_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showRunSegments_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showRatingCurve_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showOperationsTable_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showFGroup_Topology_widget, FALSE, FALSE);

	XtSetSensitive(someWidgets->showTimeSeriesTable_widget, FALSE);
	XtSetSensitive(someWidgets->showTulsaPlot_widget, FALSE);
	XtSetSensitive(someWidgets->showPlotTS_widget, FALSE);
	XtSetSensitive(someWidgets->showOtherMods_widget, FALSE);
	XtSetSensitive(someWidgets->showModsViewer_widget, FALSE);

	XmToggleButtonSetState(someWidgets->showTimeSeriesTable_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showTulsaPlot_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showPlotTS_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showOtherMods_widget, FALSE, FALSE);
	XmToggleButtonSetState(someWidgets->showModsViewer_widget, FALSE, FALSE);

	revert_to_default_view(someWidgets->main_canvas, someWidgets, NULL);
	someWidgets->GoToNextForecastGroup = FALSE;
	first_time = 1;
	}
 else if(RET_HELP == answer) popup_help_window(w, "QUIT_RUN_DIALOG", call_data);

}




/* **************************************************************

	void end_NWSRFS_run()


   ************************************************************** */

int end_NWSRFS_run(w, someWidgets)
	Widget                  w;
	the_widget_struct       *someWidgets;
{

	Display         *display;
	Window          root;
	int             quitFlag;
	int             answer;
	int             i;
        Atom            type;                   /* variables for XGetWindowProperty */  
        int             format;                 
        long            offset=0;
        long            nitems, left;
        
        int             *gif_done;
        int             tot_time=0;

 
 display = XtDisplay(w);
 root = DefaultRootWindow(display);

 quitFlag = TRUE;
         
 XChangeProperty
	 (
 	 display,
	 root,
	 IFPA_quit_NWSRFS,
	 IFPA_quit_NWSRFS_type,
	 8,
	 PropModeReplace,
	 (unsigned char *)&quitFlag,
	 sizeof(int)
	 );

 /* --------------------------------------------------------------------------- */
 /*                                                                             */
 /* The following 'if' statements require that a Forecast Group has been        */
 /* selected and that the window showing the Forecast Group topology has        */
 /* been created, though not necessarily displayed; consequently:               */
 /*                                                                             */
 /*             someWidgets->tree_shell != NULL                                 */
 /*                                                                             */
 /* --------------------------------------------------------------------------- */

 if(someWidgets->tree_shell != NULL)
 {
	if(mods_pending(someWidgets, "Quit")) return(FALSE);
		 
        /* wait till the last gif file has been created before
           popping up the last popup
        */
        if(get_save_gif_atom(someWidgets->tree_shell))
        {
           gif_done  = (int*)malloc(sizeof(int));
           *gif_done = FALSE;
           
           while(!*gif_done)
           {              
              if(XGetWindowProperty
                 (
                 display,
                 root,
                 IFPA_save_gif_file_done,
                 offset,
                 (long) sizeof(int),
                 FALSE,
                 IFPA_save_gif_file_done_type,
                 &type,
                 &format,
                 &nitems,
                 &left,
                 (unsigned char **)&gif_done
                 ) == Success && type == IFPA_save_gif_file_done_type)
              {
                 if(!*gif_done && (tot_time < 15) )
	         {
	            system("sleep 1");
	            tot_time++;
	         }
	         else if(tot_time >= 15)
	         {
	            printf("Trouble saving last gif file - quitting\n");
	            *gif_done=TRUE;
	         }
              }   
              else
                 printf("save_gif_file_done atom not posted\n");
           }
           
           free(gif_done);
        } 
            
        answer = popup_send_mods_Dialog(someWidgets);
        if(answer == RET_HELP) return(FALSE);       /* The user has chosen to cancel terminating    */
                                                    /* the current NWSRFS run...                    */
 }

 display = XtDisplay(global_toplevel);
 root = DefaultRootWindow(display);

 XDeleteProperty(display, root, IFPA_IFP_NWSRFS_is_running);

 segment_run_type = RUN_FINISHED;
 stay_in_CEX25 = FALSE;
 
 printf("This NWSRFS run is done; normal termination...\n");


 if(someWidgets->tree_shell != NULL)
	{
	XUnmapWindow(display, XtWindow(someWidgets->tree_shell));
	XUnmapSubwindows(display, XtWindow(someWidgets->tree_shell));

	/*--------------------------------------------------------------*/
	/* When the 'tree_shell' is destroyed, all tree widgets (the    */
	/* children of 'tree_shell') will be destroyed too; right now   */
	/* the tree widget DestroyMethod does not destroy its children, */
	/* so we have to handle destroying tree widget children         */
	/* explicitly...                                                */
	/*--------------------------------------------------------------*/
	for(i = 0; i <= sub_group_num; i++)
		{
		destroy_tree_children(someWidgets->head[i]);
		free(someWidgets->head[i]);
		someWidgets->head[i] = (node *) NULL;
		}

	XtDestroyWidget(someWidgets->tree_shell);
	someWidgets->tree_shell = NULL;
	}
	 
 return(TRUE);

}




/* **************************************************************

	popup_version_window()


   ************************************************************** */

void popup_version_window(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

 if(someWidgets->version_shell == NULL) return;


 if(call_data->set) XtManageChild(someWidgets->version_shell);
 else               XtUnmanageChild(someWidgets->version_shell);


}



/* **************************************************************

	reset_version_ToggleButton()


   ************************************************************** */

void reset_version_ToggleButton(w, button, call_data)
	Widget                  w;
	Widget                  button;
	XmAnyCallbackStruct     *call_data;
{

 XmToggleButtonSetState(button, FALSE, FALSE);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/run_callback.c,v $";
 static char rcs_id2[] = "$Id: run_callback.c,v 1.10 2006/04/07 13:30:22 aivo Exp $";}
/*  ===================================================  */

}

