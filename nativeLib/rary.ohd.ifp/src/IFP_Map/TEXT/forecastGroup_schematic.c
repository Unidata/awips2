
/* ***************************************************************************************************


	forecastGroup_schematic.c

				Creates the GUI & schematic diagram for the current Forecast Group

	Coded by:               Tom Adams
	Affiliation:            NOAA/NWS/Office of Hydrology/HRL
	Date:                   09/30/92
	Major Changes:          09/30/92


   *************************************************************************************************** */



#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "ifp_help.h"
#include "help.h"


extern void post_univ_techniques_atoms(Widget);


/* ****************************************************************************

	 create_FcstGroup_schematic()

   **************************************************************************** */

void create_FcstGroup_schematic(forecastGroup_name, some_widgets)
	char                    *forecastGroup_name;
	the_widget_struct       *some_widgets;
{
	Widget          tree_shell, run_info_shell;
	Widget          run_form;
	Widget          sw, rc, horizScrollBar;
	Widget          forecast_group_label;

	int             i, num_list;
	int             j;

	Arg             wargs[9];
	Atom            type;

	int             n;
	Dimension       scrollBar_height;
	Dimension       form_height;
	Dimension       tree_width;
	Dimension       shell_height;
	Dimension       shell_width;
	Dimension       fg_label_height;
	Dimension       tree_height;
	Dimension       parent_margin;
	Position        tree_position;

	help_struct     *help_data;
	help_cb_struct  *context_help;

	xs_menu_widget_struct   *controlStruct;
	xs_menu_widget_struct   *optionsStruct;
	xs_menu_widget_struct   *displayStruct;
	xs_menu_widget_struct   *modsStruct;

	char            help_pathname[80];

 numberOfSubgroupsSelected = 0;
 NWSRFS_has_begun = FALSE;

 n = 0;
 XtSetArg(wargs[n], XmNmappedWhenManaged, FALSE); n++;
 tree_shell = XtCreatePopupShell("tree_shell", transientShellWidgetClass, global_toplevel, NULL, 0);
 some_widgets->tree_shell = tree_shell;


 run_form = XtVaCreateManagedWidget("run_form", xmFormWidgetClass, tree_shell,
				   XmNrubberPositioning, TRUE,
				   NULL);
 some_widgets->form_widget = run_form;


/* -----------------------------------------------------------------------------------  */


 forecast_group_label = XtVaCreateManagedWidget("forecast_group_label", xmLabelWidgetClass, run_form,
			XtVaTypedArg, XmNlabelString, XmRString, forecastGroup_name, strlen(forecastGroup_name)+1,
			XmNtopAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL
			);


 XtVaGetValues(forecast_group_label, XmNheight, &fg_label_height, NULL);

 some_widgets->fg_label_height = fg_label_height;

 sw = XtVaCreateManagedWidget                   /* Put the tree in a scrolled window to handle large trees...   */
		(
		"swindow",
		xmScrolledWindowWidgetClass,
		run_form,
		XmNscrollingPolicy,  XmAUTOMATIC,
		XmNtopAttachment,    XmATTACH_WIDGET,
		XmNtopWidget,        forecast_group_label,
		XmNleftAttachment,   XmATTACH_FORM,
		XmNrightAttachment,  XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
 some_widgets->sw_for_tree_widgets = sw;



 rc = XtVaCreateManagedWidget                   /* This is somewhat os a misnomer, but use of a BulletinBoard   */
		(                               /* widget is the same as a RowColumn widget with the resource   */
		"tree_rc",                      /* XmNpacking set to XmPACK_NONE...                             */
		xmBulletinBoardWidgetClass,
		sw,
		NULL);
 some_widgets->rc_for_tree_widgets = rc;



 XtPopup(tree_shell, XtGrabNone);

 create_upstream_seg_tree(some_widgets, rc);
 show_the_list(some_widgets);
 create_univ_Techniques_struct(some_widgets);
 post_univ_techniques_atoms(rc);
 create_Universal_Tech_popup(some_widgets);
 create_nonUniversal_Tech_popup(some_widgets);
 set_snowModel_buttons_to_default(global_toplevel, some_widgets, NULL);
 create_NWSRFS_working_Dialog(some_widgets);
 create_NWSRFS_stopped_Dialog(some_widgets);



/* Fill the Non-universal List widget with items from the segment names in the tree created     */
/*      in create_upstream_seg_tree()...                                                        */

 fill_list(&num_list);
 XtVaSetValues(some_widgets->non_univ_list,
	       XmNitems,            xmstr_Run_list,
	       XmNitemCount,        num_list,
	       XmNvisibleItemCount, num_list,
	       XmNselectionPolicy,  XmEXTENDED_SELECT,
	       NULL);


 XtVaGetValues(rc, XmNmarginHeight, &parent_margin, NULL);
 /* printf("parent_margin = %d\n", (int)parent_margin);         */

 tree_position = (Position)RC_BORDER_WIDTH;

 for(i = 0; i <= sub_group_num; i++)
	{
	if(i)   {       /* i > 0...                             */
		XtVaGetValues(some_widgets->head[i - 1]->parent_widget, XmNheight, &tree_height, NULL);

		tree_position += (Position)(tree_height + (Dimension)RC_BORDER_WIDTH);
		}

	XtVaSetValues(some_widgets->head[i]->parent_widget,
		     XmNy, tree_position + (Position)parent_margin,
		     NULL);
	}



 XtManageChild(tree_shell);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/forecastGroup_schematic.c,v $";
 static char rcs_id2[] = "$Id: forecastGroup_schematic.c,v 1.2 1995/11/17 17:19:56 page Exp $";}
/*  ===================================================  */

}


