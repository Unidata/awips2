/* File: is_focus_in_widget_tree.c
 *
 * Checks if the tree_root widget is the parent for the specified widget.
 */

#include <stdio.h>
#include <X11/Intrinsic.h>

int     is_focus_in_widget_tree(w, tree_root)

 Widget w;         /* widget to be tested */
 Widget tree_root; /* tree_root widget structure */
{
 if(XtParent(w) == NULL) return(FALSE);

 else if(w == tree_root) return(TRUE);

 else if(XtParent(w) == tree_root) return(TRUE);

 else return(is_focus_in_widget_tree(XtParent(w), tree_root));

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/is_focus_in_widget_tree.c,v $";
 static char rcs_id2[] = "$Id: is_focus_in_widget_tree.c,v 1.1 1995/09/08 14:57:33 page Exp $";}
/*  ===================================================  */

}
