
/* **************************************************************************************

	fg_subset.c
		Display a tree structure using the tree widget - based on fgmap.c

	Entered by: Tom Adams - 2/22/90

	Modifications by:  Tom Adams & George Smith (NWS/OH/HRL)
			   Tom Adams 11/19/90 for Motif Widget set
			   Tom Adams 01/09/92 to correct bugs related to handling multiple trees

	Source: X Window Systems Programming and Applications with Xt
			by Douglas Young

   ************************************************************************************** */


#include "libXifp.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "tree.h"
/*
 *  #include <X11/Xmu/Converters.h>
 *  #include <X11/Xaw/Tree.h>
*/

   FILE    *input_file;

void create_upstream_seg_tree(some_widgets, rc)
	the_widget_struct   *some_widgets;
	Widget              rc;
{

	Widget          tree;
	Arg             wargs[2];
	Window          root;
	Display         *display;

	char            input_file_name[80];
/*	char            segment_status[8];*//*testing, uncomment it ---kwz*/

	int             segment_status_flag;
	int             i;
	int             tree_height;
	int             tree_position;

	e19_data        e_19;




 display = XtDisplay(rc);
 root = DefaultRootWindow(display);

 skip_first_node = 0;

 fill_non_univ_techniques_struct();

/* printf("Inside 'create_upstream_seg_tree()'...\n");  */

/* ............................... Create a tree structure ............................ */

 strcpy(input_file_name, (char *)getenv("HOME"));
 strcat(input_file_name, "/.ifp_files/local/e19.data");

 if( (input_file = fopen (input_file_name, "r") ) != NULL )
 {

   if(XtIsRealized(some_widgets->rc_for_tree_widgets)) XtUnmapWidget(some_widgets->rc_for_tree_widgets);

   tree_position = RC_BORDER_WIDTH;
   sub_group_num = -1;

while (fscanf(input_file, "%s %d %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %d",
		e_19.id,
		&e_19.in_fgroup,
		e_19.name,
		e_19.description,
		e_19.f_group,          /* added 2/13/91 - gfs */
		e_19.c_group,          /* added 2/13/91 - gfs */
		e_19.upstream[0],      /* added 2/13/91 - gfs */
		e_19.upstream[1],      /* added 2/13/91 - gfs */
		e_19.upstream[2],      /* added 2/13/91 - gfs */
		e_19.upstream[3],      /* added 2/13/91 - gfs */
		e_19.upstream[4],      /* added 2/13/91 - gfs */
		e_19.downstream[0],    /* added 2/13/91 - gfs */
		e_19.downstream[1],    /* added 2/13/91 - gfs */
		&e_19.NumRC
	    ) != EOF )

	{
/* create space for e19 info in segment arrays          */
e_19.extra = (extra_e19*)malloc(sizeof(extra_e19)*e_19.NumRC);

for (i=0;i<e_19.NumRC;i++)
{fscanf(input_file,"%8s %[^~]~ %[^~]~ %f %f %[^~]~ \
 %f %f %f %f %f %f %f %f %f %f %d %[^~]~ %f %s",
	e_19.extra[i].RCName,
	e_19.extra[i].river_name,
	e_19.extra[i].station_name,
	&e_19.extra[i].latitude,
	&e_19.extra[i].longitude,
	e_19.extra[i].forecast_point_type,
	&e_19.extra[i].total_area,
	&e_19.extra[i].local_area,
	&e_19.extra[i].flood_stage,
	&e_19.extra[i].flood_flow,
	&e_19.extra[i].secondary_stage,
	&e_19.extra[i].warning_stage,
	&e_19.extra[i].warning_flow,
	&e_19.extra[i].gage_zero,
	&e_19.extra[i].record_stage,
	&e_19.extra[i].record_flow,
	&e_19.extra[i].date_of_record,
	e_19.extra[i].record_flood_comment,
	&e_19.extra[i].rating_curve_limit,
	e_19.extra[i].seg_status) ;
}/*end of for count1 loop*/	

  if (e_19.NumRC==0) segment_status_flag = UNKNOWN;
  else if(strcmp(e_19.extra[0].seg_status, "Normal") == 0) segment_status_flag = NORMAL;
  else if(strcmp(e_19.extra[0].seg_status, "Alert")  == 0) segment_status_flag = ALERT;
  else if(strcmp(e_19.extra[0].seg_status, "Flood")  == 0) segment_status_flag = FLOOD;
  else   /* Unknown */                                   segment_status_flag = UNKNOWN;

  if(strlen(e_19.id) == 1)
  {
    sub_group_num++;
    skip_first_node = 0;

    if(sub_group_num)
    {       /* sub_group_num > 0...                         */

      XtVaGetValues(tree, XmNheight, &tree_height, NULL);
      tree_position += (tree_height + RC_BORDER_WIDTH);

      /* Create the widgets representing the tree...          */
      show_tree(tree, some_widgets->head[sub_group_num - 1], NULL, some_widgets, sub_group_num - 1);
      /*  XawTreeForceLayout(tree);  */

      XtVaSetValues(tree, XmNy, tree_position, NULL);

    }


    /* Create a tree widget for this subgroup...    */
    tree = XtVaCreateManagedWidget("tree", XstreeWidgetClass, rc,
				      XtVaTypedArg, XtNbackground, XmRString, "white", strlen("white")+1,
				      /* XtNautoReconfigure, FALSE, */
				      NULL);
  }

	some_widgets->head[sub_group_num] =
		(node *)insert_node(&e_19, some_widgets->head[sub_group_num], segment_status_flag);
	}


   /*     Get the position for the last tree only...              */
  if(sub_group_num)
  {
    XtSetArg(wargs[0], XmNheight, &tree_height);
    XtGetValues(tree, wargs, 1);
    tree_position += (tree_height + RC_BORDER_WIDTH);
  }

  /* Create the widgets representing the last tree only, including if there's only one tree...           */
  show_tree(tree, some_widgets->head[sub_group_num], NULL, some_widgets, sub_group_num);
  /*  XawTreeForceLayout(tree);  */

  XtSetArg(wargs[0], XmNy, tree_position);
  XtSetValues(tree, wargs, 1);


  if(XtIsRealized(some_widgets->rc_for_tree_widgets)) XtMapWidget(some_widgets->rc_for_tree_widgets);

  non_univ_Techniques_init();
  XDeleteProperty(display, root, IFPA_run_segments);

  for(i = 0; i <= sub_group_num; i++)
  {
    if(some_widgets->head[i] != NULL) concat_the_ids(some_widgets->head[i]);
  }

/* Reset_ForecastGroupRegion(some_widgets, rad_data);   */

  fclose(input_file);
  }
  else
    printf("Problem opening %s !\n", input_file_name );
}



/* ************************************************************************

	create_partial_seg_tree ()
			a new segment tree is created with segments deleted

   ************************************************************************ */


void create_partial_seg_tree (the_node, some_widgets)
	node                    *the_node;
	the_widget_struct       *some_widgets;
{
	Widget          partial_tree;
	Widget          horizScrollBar;
	Display         *display;
	Window          treeWindow;
	Arg             wargs[3];
	int             i, j;
	Window          root;

	int             n;
	int             scrollBar_height;
	Dimension       tree_width;
	Dimension       shell_height;
	Dimension       shell_width;
	Dimension       tree_height;
	Position        tree_position;
	Dimension       parent_margin;


 display = XtDisplay(global_toplevel);

 memset(selected_string, '\0', 801);
 memset(delete_string, '\0', 801);


/*  XtUnmapWidget(some_widgets->rc_for_tree_widgets);   */

 if(the_node != NULL)   /* If a node is NULL, we've deleted that tree; so, skip re-creating the tree widget...  */
	{
	/* Create the tree widget       */
	partial_tree = XtVaCreateManagedWidget("partial_tree", XstreeWidgetClass, some_widgets->rc_for_tree_widgets,
					      XtVaTypedArg, XtNbackground, XmRString, "white", strlen("white")+1,
					      /* XtNautoReconfigure, FALSE, */
					      XtNmappedWhenManaged, FALSE,
					      NULL);
	the_node->parent_widget = partial_tree;


	/*      Create the widgets representing the tree...                             */
	 show_tree(partial_tree, the_node, NULL, some_widgets, whichTree_index);

	 /*  XawTreeForceLayout(partial_tree);  */
	 XtMapWidget(partial_tree);


/* ------------------------------------------------------------------------------------------------------

 n = 0;
 XtSetArg(wargs[n], XtNheight, &tree_height); n++;
 XtSetArg(wargs[n], XtNwidth, &tree_width); n++;
 XtGetValues(partial_tree, wargs, n);

 n = 0;
 XtSetArg(wargs[n], XmNhorizontalScrollBar, &horizScrollBar); n++;
 XtGetValues(some_widgets->sw_for_tree_widgets, wargs, n);

 if(horizScrollBar != NULL)
	{
	n = 0;
	XtSetArg(wargs[n], XmNheight, &scrollBar_height); n++;
	XtGetValues(horizScrollBar, wargs, n);
	}
 else scrollBar_height = 0;



 if(tree_height >= (int)Screen_Height - (MWM_BORDER_WIDTH + MWM_MENU_BORDER_HEIGHT))
	shell_height = (int)Screen_Height - (MWM_BORDER_WIDTH + MWM_MENU_BORDER_HEIGHT)
					  + some_widgets->fg_label_height + 15;
 else shell_height = tree_height + MWM_BORDER_WIDTH + MWM_MENU_BORDER_HEIGHT
					  + some_widgets->fg_label_height + 15;

 if(tree_width >= (int)Screen_Width - (MWM_BORDER_WIDTH * 2))
	shell_width = (int)Screen_Width - (MWM_BORDER_WIDTH * 2);
 else if(tree_width < 375) shell_width = 375 + MWM_BORDER_WIDTH;
 else shell_width = tree_width + MWM_BORDER_WIDTH;

 XtSetArg(wargs[0], XmNheight, shell_height);
 XtSetArg(wargs[1], XmNwidth, shell_width);
 XtSetValues(some_widgets->tree_shell, wargs, 2);

   ------------------------------------------------------------------------------------------------------ */

	}


 /* Set the positions of the tree widgets; skip over trees that have been deleted...                            */
 XtVaGetValues(some_widgets->rc_for_tree_widgets, XmNmarginHeight, &parent_margin, NULL);
 tree_position = (Position)RC_BORDER_WIDTH;
 for(i = 0; i <= sub_group_num; i++)
	{
	if(some_widgets->head[i] != NULL)
		{
		if(i)   {       /* i > 0...                             */
			for(j = i - 1; j >= 0; j--)
				{       /* Place the current tree widget after the first        */
					/* non-NULL tree...                                     */
				if(some_widgets->head[j] != NULL)
					{
					XtSetArg(wargs[0], XmNheight, &tree_height);
					XtGetValues(some_widgets->head[j]->parent_widget, wargs, 1);
					tree_position += (Position)(tree_height + (Dimension)RC_BORDER_WIDTH);
					break;
					}
				}
			}

		/* Set the vertical position of the next TreeWidget...                          */
		XtVaSetValues(some_widgets->head[i]->parent_widget,
			     XmNy, tree_position + (Position)parent_margin,
			     NULL);
		}
	}


 /*XtMapWidget(some_widgets->rc_for_tree_widgets);*/


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/map_subset.c,v $";
 static char rcs_id2[] = "$Id: map_subset.c,v 1.4 2006/04/07 13:30:12 aivo Exp $";}
/*  ===================================================  */

}




