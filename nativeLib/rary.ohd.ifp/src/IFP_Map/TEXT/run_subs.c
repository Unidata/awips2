/* ***************************************************************************

	Ancilliary functions for the Run_upstream program

******************************************************************************* */


#include "libXifp.h"
#include "ifp_atoms.h"
#include "libXs.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"

#define UP_STREAM	1
#define DOWN_STREAM	0

/*
 * #include <X11/Xmu/Converters.h>
 * #include <X11/Xaw/Tree.h>
 */
 
#include "tree.h"
#include "c_call_f/retrieve_hcl_techs_args.h"

static node                       *make_node();
static non_univ_techniques_struct *create_non_univ_techniques_struct();
static void                       get_MAP_data();

extern char                       **map_areas_in_seg();
extern Widget          sac_widget, snow_widget;

int    sacsnowCurrentVal;
extern int    ssUpstreamFlag;


node *find_it(current, id)
	node    *current;
	char    *id;
{
	node    *got_it;

	if (!current) return (NULL);
	else    {
                /*printf("IN runsub: currentsac_snow= %d %d \n",current->techniques->sac_snow,current->techniques->frost);*/
		if (!strcmp(current->e19.name, id)) return (current);
		else    {
			if(got_it = find_it (current->left, id))      return (got_it);
			if(got_it = find_it (current->mid_left, id))  return (got_it);
			if(got_it = find_it (current->center, id))    return (got_it);
			if(got_it = find_it (current->mid_right, id)) return (got_it);
			if(got_it = find_it (current->right, id))     return (got_it);
			}
		}

 return (NULL);

}



/* ***************************************************************************

	concat_the_ids()
			posts the run_segments atom & appends segment
			names to it for each segment in the run group

******************************************************************************* */


void concat_the_ids(current)
	node            *current;
{
	Display         *display;
	Window          root;
	char            string[9];
	int             i, length;

	char            *blank = " ";

	display = XtDisplay(global_toplevel);
	root = DefaultRootWindow(display);

	if (current->left != NULL)      concat_the_ids(current->left);
	if (current->mid_left != NULL)  concat_the_ids(current->mid_left);
	if (current->center != NULL)    concat_the_ids(current->center);
	if (current->mid_right != NULL) concat_the_ids(current->mid_right);
	if (current->right != NULL)     concat_the_ids(current->right);

	strcpy(string, current->e19.name);
	if((length = strlen(string)) < 8)
		{
		for(i = 1; i <= 8 - length; i++)
		    strcat(string, blank);
		}

	XChangeProperty
		(
		display,
		root,
		IFPA_run_segments,
		IFPA_run_segments_type,
		8,
		PropModeAppend,
		string,
		strlen(string)
		);
}



/* ***************************************************************************

	remove_segment_callbacks()

   *************************************************************************** */


void remove_segment_callbacks(current)
	node            *current;
{


	XtRemoveAllCallbacks(current->segment_widget, XmNactivateCallback);

	if (current->left != NULL)      remove_segment_callbacks(current->left);
	if (current->mid_left != NULL)  remove_segment_callbacks(current->mid_left);
	if (current->center != NULL)    remove_segment_callbacks(current->center);
	if (current->mid_right != NULL) remove_segment_callbacks(current->mid_right);
	if (current->right != NULL)     remove_segment_callbacks(current->right);


}



/* ***************************************************************************

	add_segment_callbacks()

   *************************************************************************** */


void add_segment_callbacks(current, callback_func, someWidgets)
	node                    *current;
	void                    (*callback_func) ();
	the_widget_struct       *someWidgets;
{

	tree_data_struct        *data;



 if(callback_func == post_change)
	XtAddCallback(current->segment_widget, XmNactivateCallback, callback_func, current->e19.name);
 else if((callback_func == tell_which_tree) || (callback_func == amend_seg_list))
	{
	data = (tree_data_struct *) malloc(sizeof(tree_data_struct));
	data->dataStruct = someWidgets;
	data->branch = current;

	XtAddCallback(current->segment_widget, XmNactivateCallback, callback_func, data);
	}
 else if(callback_func == popup_segment_info)

	XtAddCallback(current->segment_widget, XmNactivateCallback, callback_func, current->popup_shell);
  
 else   XtAddCallback(current->segment_widget, XmNactivateCallback, callback_func, someWidgets);

 if (current->left != NULL)      add_segment_callbacks(current->left, callback_func, someWidgets);
 if (current->mid_left != NULL)  add_segment_callbacks(current->mid_left, callback_func, someWidgets);
 if (current->center != NULL)    add_segment_callbacks(current->center, callback_func, someWidgets);
 if (current->mid_right != NULL) add_segment_callbacks(current->mid_right, callback_func, someWidgets);
 if (current->right != NULL)     add_segment_callbacks(current->right, callback_func, someWidgets);

}





/* ***************************************************************************

	is_computationally_before()

   *************************************************************************** */


int is_computationally_before(currentSegment, someWidgets)
	char                    *currentSegment;
	the_widget_struct       *someWidgets;
{

	Arg             wargs[2];
	XmString        *xmstring_Segments;

	char            *string;
	char            *first_blank;
	char            selectedSegment[9];

	int             number_of_Segments;
	int             i;
	int             currentSegment_position;
	int             selectedSegment_position;



 XtSetArg(wargs[0], XmNitems, &xmstring_Segments);
 XtSetArg(wargs[1], XmNitemCount, &number_of_Segments);
 XtGetValues(someWidgets->listWidget_for_forecastSegments, wargs, 2);

 strcpy(selectedSegment, someWidgets->segment_selected);
 if((first_blank = strstr(selectedSegment, " ")) != NULL) *first_blank = '\0';

 /* 
	Down stream return FALSE --- Up stream return TRUE  
*/

 for(i = 0; i < number_of_Segments; i++)
	{
	XmStringGetLtoR(xmstring_Segments[i], XmSTRING_DEFAULT_CHARSET, &string);
	if((first_blank = strstr(string, " ")) != NULL) *first_blank = '\0';

	if(strcmp(string, selectedSegment) == 0) selectedSegment_position = i;
	if(strcmp(string, currentSegment ) == 0) currentSegment_position  = i;
	/*
	else if(strcmp(string, currentSegment) == 0) currentSegment_position = i;
	*/
	}
 ssUpstreamFlag = 0;
 if ( selectedSegment_position == currentSegment_position ) 
 {
        ssUpstreamFlag = 1;
	return( UP_STREAM );
 }

 if(selectedSegment_position < currentSegment_position) 
 {
        ssUpstreamFlag = 1;
	return(UP_STREAM);
 }

 return( DOWN_STREAM );

}





/****************************************************************

	insert_node()
		inserts an integer into the appropriate node of a tree; if the tree
		doesn't exist, an initial node is allocated and the given key becomes
		the sort tree. Otherwise, this function follows the branches of the
		tree until a leaf is found.

****************************************************************/

node *insert_node(segment_data, head, segment_status_flag)
	e19_data        *segment_data;
	node            *head;
	int             segment_status_flag;
{
	int             next_char;
	char            test_this[50];

	node            *prev;
	node            *temp_ptr;
	node            *ptr = head;



/*      If the tree doesn't exist, create & return a new node.          */
/*      If first segment not in forecast group set skip flag            */
/*        and return head unchanged.                                    */

 if(!head)
	{
	if(segment_data->in_fgroup == 0)
		{
		skip_first_node = 1;
		return((node *) NULL);
		}
	else    {
		temp_ptr = make_node(head, NULL, segment_data, segment_status_flag);
		return (temp_ptr);
		}

	}

/*      In the middle of creating tree and hit segment not in forecast  */
/*        group.  Just return.                                          */

 if(segment_data->in_fgroup == 0)
	{
	return(head);
	}

/* ------------------------------------------------------------------------------------------

	Otherwise, find a leaf node, following the tree structure defined by "id", where:
		'a' defines a left-branch,
		'b' defines a mid_left-branch,
		'c' defines a center-branch,
		'd' defines a mid_right-branch,
		'e' defines a right-branch,

   ------------------------------------------------------------------------------------------ */

next_char = 1 + skip_first_node;
strcpy(test_this, segment_data->id);

while (ptr != NULL)
	{
	prev = ptr;

	switch (test_this[next_char])
		{
		case 'a' :
			ptr = ptr->left;
			break;
		case 'b' :
			ptr = ptr->mid_left;
			break;
		case 'c' :
			ptr = ptr->center;
			break;
		case 'd' :
			ptr = ptr->mid_right;
			break;
		case 'e' :
			ptr = ptr->right;
			break;
		default:
			ptr = NULL;
			break;
		}
	next_char++;
	}

/*      Make a new node and attach it to the appropriate branch                */

	switch (test_this[ next_char - 1 ])
		{
		case 'a' :
			prev->left = make_node(head, prev, segment_data, segment_status_flag);
			break;
		case 'b' :
			prev->mid_left = make_node(head, prev, segment_data, segment_status_flag);
			break;
		case 'c' :
			prev->center = make_node(head, prev, segment_data, segment_status_flag);
			break;
		case 'd' :
			prev->mid_right = make_node(head, prev, segment_data, segment_status_flag);
			break;
		case 'e' :
			prev->right = make_node(head, prev, segment_data, segment_status_flag);
			break;
		}
return (head);
}


/**********************************************************************

	make_node()
		creates a new node structure, stores the integer key, and initializes
		the node's subnode pointer to NULL.

*********************************************************************/

node *make_node(head, prev, segment_data, segment_status_flag)
	node            *head;
	node            *prev;
	e19_data        *segment_data;
	int             segment_status_flag;
{
 node *ptr = (node *) malloc(sizeof(node));

 ptr->status = segment_status_flag;
 ptr->computed_status = UNCOMPUTED;
 ptr->parent = prev;
 ptr->e19 = *segment_data;
 ptr->techniques = create_non_univ_techniques_struct();

 get_MAP_data(ptr);
 ptr->segment_region = XCreateRegion();      /* Create a Region of zero size...      */

 ptr->techniques->snow     = non_univ_Techniques->snow;
 ptr->techniques->frost    = non_univ_Techniques->frost;
 ptr->techniques->upsc     = non_univ_Techniques->upsc;
 ptr->techniques->upwe     = non_univ_Techniques->upwe;
 ptr->techniques->sac_snow = non_univ_Techniques->sac_snow;
 ptr->techniques->printsma = non_univ_Techniques->printsma;
 ptr->techniques->printsnw = non_univ_Techniques->printsnw;
 ptr->techniques->prtro    = non_univ_Techniques->prtro;
 ptr->techniques->tables   = non_univ_Techniques->tables;

 ptr->left = NULL;
 ptr->mid_left = NULL;
 ptr->center = NULL;
 ptr->mid_right = NULL;
 ptr->right = NULL;

 return (ptr);

}


/* *************************************************************************************

	show_tree()
		performs a pre-order traversal of the nodes and creates widgets
		for each node; show_tree() also sets the XtNsuperNode constraint
		for each widget to the widget previously created for the super
		node in the 5-tuple tree.

   ************************************************************************************* */

void show_tree(parent, branch, super_node, someWidgets, which_tree)
	Widget                  parent;
	node                    *branch;
	Widget                  super_node;
	the_widget_struct       *someWidgets;
	int                     which_tree;
{
	Widget          w, popup, select_button;
	Arg             wargs[6];
	int             n = 0;
	node            data;
	int             pixel;
	int             foreground, background;

	tree_data_struct        *tree_data;


 if(!branch) return;                    /* If we've hit a leaf, return...               */



/*      Create a widget for the node, specifying the given super_node constraint        */
 pixel = get_pixel_by_name(parent, flood_color_levels[4]);
 if(branch->e19.in_fgroup == 1)
	{
	if(branch->status == NORMAL)     pixel = get_pixel_by_name(parent, flood_color_levels[NORMAL]);
	else if(branch->status == ALERT) pixel = get_pixel_by_name(parent, flood_color_levels[ALERT]);
	else if(branch->status == FLOOD) pixel = get_pixel_by_name(parent, flood_color_levels[FLOOD]);
	else      /* UNKNOWN */          pixel = get_pixel_by_name(parent, flood_color_levels[UNKNOWN]);
	}
 else   {
	pixel = get_pixel_by_name(parent, flood_color_levels[4]);
	}


 w = XtVaCreateManagedWidget(branch->e19.name, xmPushButtonWidgetClass, parent,
			    XtVaTypedArg, XmNforeground, XmRString, flood_color_levels[4], strlen(flood_color_levels[4])+1,
			    XtNsuperNode,   super_node,
			    XtVaTypedArg, XmNlabelString, XmRString, branch->e19.name, strlen(branch->e19.name)+1,
			    XmNborderWidth, 3,
			    XmNborderColor, pixel,
			    NULL);

 if(branch->e19.in_fgroup != 1) XtSetSensitive(w, FALSE);


 branch->segment_widget = w;            /* Add the PushButton widget for this segment to the node structure...  */
 branch->parent_widget = parent ;       /* Identify which tree widget this segment belongs to...                */
 branch->which_tree = which_tree;       /* Identify which tree this segment belongs to...                       */


 if(super_node == NULL)
	{
	n = 0;
	XtSetArg(wargs[n], XtNforeground, &application_default_foreground); n++;
	XtSetArg(wargs[n], XtNbackground, &application_default_background); n++;
	XtGetValues(w, wargs, n);
	}

/*      Recursively create the subnodes, giving this node's widget as the super_node    */


 tree_data = (tree_data_struct *) malloc(sizeof(tree_data_struct));
 tree_data->dataStruct = someWidgets;
 tree_data->branch = branch;

 XtAddCallback (w, XmNactivateCallback, amend_seg_list, tree_data);

 /* Create popup windows for the 'e19 data'...    
                                     */
                                    
 build_popup(parent, w, branch);
 
 show_tree(parent, branch->left, w, someWidgets, which_tree);
 show_tree(parent, branch->mid_left, w, someWidgets, which_tree);
 show_tree(parent, branch->center, w, someWidgets, which_tree);
 show_tree(parent, branch->mid_right, w, someWidgets, which_tree);
 show_tree(parent, branch->right, w, someWidgets, which_tree);

}


/* *****************************************************************************

	invert_segment()
		inverts the selected segment's foreground & background colors

   ***************************************************************************** */

void invert_segment(segname, head)
	char    *segname;
	node    *head;
{
	node    *found;



 found = find_it(head, segname);

 if(found == NULL) return;
 else invert_tree_widget(found->segment_widget);


}




/* *****************************************************************************

	change_segment_color()

   ***************************************************************************** */

void change_segment_color(someWidgets, segname, pixel, status)
	the_widget_struct       *someWidgets;
	char                    *segname;
	int                     pixel;
	int                     status;
{

	node    *found;
	Arg     wargs[1];
	int     i;


 for(i = 0; i <= sub_group_num; i++)
	if((found = find_it(someWidgets->head[i], segname)) != NULL) break;

 if(found == NULL) return;
 else    {
	 change_widget_color(found->segment_widget, pixel);
	 found->computed_status = status;

	 XtSetArg(wargs[0], XmNforeground, application_default_background);
	 XtSetValues(found->segment_widget, wargs, 1);
	 }

}




/* *****************************************************************************

	create_non_univ_techniques_struct()


   ***************************************************************************** */

non_univ_techniques_struct *create_non_univ_techniques_struct()
{
	non_univ_techniques_struct      *techniques_struct;


techniques_struct = (non_univ_techniques_struct *) malloc(sizeof(non_univ_techniques_struct));

return(techniques_struct);

}



/* *****************************************************************************

	fill_non_univ_techniques_struct()


   ***************************************************************************** */

void fill_non_univ_techniques_struct()
{
 int   a_in, a_ipr, a_ipu, a_ioerr, a_iodbug;
 int   a_metric, a_iumgen, a_iumsac, a_iumapi;
 int   a_nhopdb, a_nhocal, a_local, a_nlstz;
 char   a_inptzc[5], a_modtzc[5];
 int   a_noutz, a_noutds, a_modwrn;
 int   a_nosnow, a_nofrze, a_iupwe, a_isac_snow, a_iupsc;
 int   a_iprsac, a_iprsnw, a_icrtro, a_iprhy, a_ifpr;
 int   a_idarun, a_ihrrun, a_ldacpd, a_lhrcpd, a_ldarun, a_lhrrun;
 int   a_now[5];

 non_univ_Techniques = (non_univ_techniques_struct *) malloc(sizeof(non_univ_techniques_struct));

 RETRIEVE_HCL_TECHS_ARGS(&a_in, &a_ipr, &a_ipu, &a_ioerr, &a_iodbug,
			 &a_metric, &a_iumgen, &a_iumsac, &a_iumapi,
			 &a_nhopdb, &a_nhocal, &a_local, &a_nlstz,
			 a_inptzc, a_modtzc,
			 &a_noutz, &a_noutds, &a_modwrn,
			 &a_nosnow, &a_nofrze, &a_iupwe, &a_isac_snow, &a_iupsc,
			 &a_iprsac, &a_iprsnw, &a_icrtro, &a_iprhy, &a_ifpr,
			 &a_idarun, &a_ihrrun, &a_ldacpd, &a_lhrcpd, &a_ldarun, &a_lhrrun,
			 a_now);
 
 a_inptzc[4] = '\0';
 a_modtzc[4] = '\0';
 /* store sac-snow switch value on =1 off =0 ,
    sacsnowCurrentVal is used in map_subset to grey out sac/snow button 
    if it is off*/
    
 /*printf("in run_sub retrieve isac_snow = %d\n",a_isac_snow);*/
 
 sacsnowCurrentVal = a_isac_snow;
 /* if sac_snow switch if off grey out SAC/SNOW buttons */
 if(sacsnowCurrentVal == 0)
 {
     XtSetSensitive(sac_widget, FALSE);
     XtSetSensitive(snow_widget, FALSE);
 }
 else
 {
     XtSetSensitive(sac_widget, TRUE);
     XtSetSensitive(snow_widget, TRUE);
 }
 
 
 /*     Fill 'SNOW' value...            */
 non_univ_Techniques->snow = (a_nosnow == 0) ? 1 : 0;

 /*     Fill 'FROST' value...           */
 non_univ_Techniques->frost = (a_nofrze == 0) ? 1 : 0;

 /*     Fill 'UPSC' value...            */
 non_univ_Techniques->upsc = a_iupsc;

 /*     Fill 'UPWE' value...            */
 non_univ_Techniques->upwe = a_iupwe;
 
 /*     Fill 'sac_snow' value...            */
 
 non_univ_Techniques->sac_snow = a_isac_snow;
 
 
 /*     Fill 'PRINTSMA' value...        */
 if (a_iprsac == 0) non_univ_Techniques->printsma = 2;
 else if(a_iprsac == 1) non_univ_Techniques->printsma = 1;
 else non_univ_Techniques->printsma = 0;  /* a_iprsac == -1 */
 
 /*     Fill 'PRINTSNW' value...        */
 if (a_iprsnw == 0) non_univ_Techniques->printsnw = 2;
 else if(a_iprsnw == 1) non_univ_Techniques->printsnw = 1;
 else non_univ_Techniques->printsnw = 0;  /* a_iprsnw == -1 */
 
 
 /*     Fill 'PRTRO'    value...        */
 non_univ_Techniques->prtro    = a_icrtro;
 
 /*     Fill 'TABLES'   value...        */
 if (a_iprhy == 0) non_univ_Techniques->tables = 2;
 else if(a_iprhy == 1) non_univ_Techniques->tables = 1;
 else non_univ_Techniques->tables = 0;  /* a_iprhy == -1 */
 
 

 
}



/* ********************************************************************************

	currentSegment_is_the_gotoSegment()

   ******************************************************************************** */

int currentSegment_is_the_gotoSegment(someWidgets)
	the_widget_struct       *someWidgets;
{

	char            string[9];
	char            *first_blank;
        int             i;
        node            *the_node;
        Widget          w;

 w = global_toplevel;  

 strcpy(string, someWidgets->segment_selected);
 if((first_blank = strstr(string, " ")) != NULL) *first_blank = '\0';
 
 if(strcmp(string, someWidgets->current_segment) == 0) return(TRUE);
 else return(FALSE);

}


/* ********************************************************************************

	mods_pending()

	This function checks to see if any mods have been made but not
	saved, or have been deleted but not saved.

	It returns TRUE if there are mods to be saved or deleted,
	FALSE if there is no pending mod activity.

	Written by George Smith, 9/25/91

   ******************************************************************************** */

int mods_pending(someWidgets, current_command)

	the_widget_struct      *someWidgets;
	char                   *current_command;
{

	int            *numberOfModsToDeleteFromFile;
	int            *currentModSaved;
	int            *currentRangeModSaved;

	long           offset = 0;
	Atom           type;
	int            format, nitems, left;
	int            range_created = 0;

	char            *first_range_mods_str;
	Display        *display;
	Window         root;



 display = XtDisplay(someWidgets->tree_shell);
 root = DefaultRootWindow(display);

 someWidgets->current_command = current_command;

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
	(unsigned char **)&numberOfModsToDeleteFromFile) == Success &&
	type == IFPA_num_mods_to_delete_fromFile_type){

	if(*numberOfModsToDeleteFromFile > 0)
		{
		popup_pending_mods_not_deleted_warningDialog(someWidgets);
		return(TRUE);
		}
	}

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_current_mod_saved,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_current_mod_saved_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&currentModSaved) == Success &&
	type == IFPA_current_mod_saved_type){

	if(*currentModSaved == 0)
		{
		popup_pending_mods_not_saved_warningDialog(someWidgets);
		return(TRUE);
		}
	}
/*
        if(XGetWindowProperty
	(
	display,
	root,
	IFPA_rangemods_files_updated,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_rangemods_files_updated_type,
	&type,
	&format,
	&nitems,
	&left,
	&first_range_mods_str) == Success &&
	type == IFPA_rangemods_files_updated_type){

	range_created = 1;
	}
	else
	range_created = 0;

*/

        if(XGetWindowProperty
	(
	display,
	root,
	IFPA_rangemods_saved,
	offset,
	(long) sizeof(int),
	FALSE,
	IFPA_rangemods_saved_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&currentRangeModSaved) == Success &&
	type == IFPA_rangemods_saved_type){

	if(*currentRangeModSaved == 1)
		{
		 popup_pending_mods_not_saved_warningDialog(someWidgets);
		 return(TRUE);
		}
	}

 return(FALSE);  /* get here only if no mods to delete and current mod saved... */

}






/* ********************************************************************************

	destroy_tree_children()

   ******************************************************************************** */

void destroy_tree_children(current)
	node    *current;
{

 if(current == (node *) NULL) return;

 XtDestroyWidget(current->segment_widget);

 if(current->left != NULL)      destroy_tree_children (current->left);
 if(current->mid_left != NULL)  destroy_tree_children (current->mid_left);
 if(current->center != NULL)    destroy_tree_children (current->center);
 if(current->mid_right != NULL) destroy_tree_children (current->mid_right);
 if(current->right != NULL)     destroy_tree_children (current->right);


}





/* *****************************************************************************

	get_MAP_data()


   ***************************************************************************** */

void get_MAP_data(head)
	node            *head;
{

	int     i;
	int     j;
	int     NumMAPs = 0;

	char    **MAP_names;




 MAP_names = map_areas_in_seg(head->e19.name, &NumMAPs);

/*
 if(MAP_names == NULL)
	{
	head->e19.num_MAP_basins = 0;
	return;
	}
*/

 /*-----------------------------------------------------*/
 /*     Make space for the data and Copy 'MAP_names'    */
 /*     & 'NumMAPs' into the node e19 structure,        */
 /*     then free 'MAP_names'...                        */
 /*-----------------------------------------------------*/

 head->MAP_data = (overlay_struct **) malloc(sizeof(overlay_struct *) * NumMAPs);
 head->e19.MAP_names = (char **) malloc(sizeof(char *) * NumMAPs);
 head->e19.num_MAP_basins = NumMAPs;

 for(i = 0; i < NumMAPs; i++)
	{
	head->e19.MAP_names[i] = (char *) malloc((strlen(MAP_names[i]) + 1) * sizeof(char));
	strcpy(head->e19.MAP_names[i], MAP_names[i]);
	}

 free(MAP_names);

 /*-----------------------------------------------------*/
 /*     For each MAP basin in this segment, namely,     */
 /*     'head->e19.name', point to the overlay          */
 /*     structure containing the MAP basin boundary     */
 /*     data; we're avoiding copying the data to save   */
 /*     space and we don't want to free the original    */
 /*     data because it may be reused & we don't want   */
 /*     to re-read it...                                */
 /*-----------------------------------------------------*/

 for(i = 0; i < NumMAPs; i++)
	{
	head->MAP_data[i] = NULL;

	for(j = 0; j < nummap; j++)
		{
		if(strcmp(mapbasin[j]->id, head->e19.MAP_names[i]) == 0)
			{
			head->MAP_data[i] = mapbasin[j];
			strcpy(mapbasin[j]->SegmentID, head->e19.name);
			break;
			}
		}
	}




/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/run_subs.c,v $";
 static char rcs_id2[] = "$Id: run_subs.c,v 1.6 2006/04/07 13:30:33 aivo Exp $";}
/*  ===================================================  */

}


