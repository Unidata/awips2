/***************************************************************

Tree.c: The Tree widget source file

Entered by: Tom Adams

Modified by:  George Smith - to display tree with leaves at left

***************************************************************/


#include	<X11/Intrinsic.h>
#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>
#include	<X11/CoreP.h>
#include	<X11/CompositeP.h>
#include	<X11/ConstrainP.h>
#include        "tree.h"
#include        "treeP.h"
#define MAX(a,b)        ((a) > (b) ? (a) : (b))  

static void		Initialize();
static void		ConstraintInitialize();
static void             ConstraintDestroy();
static Boolean		ConstraintSetValues();
static void		Resize();
static Boolean		SetValues();
static XtGeometryResult	GeometryManager();
static void		ChangeManaged();
static void		insert_new_node();
static void		delete_node();
static void		new_layout();
static void		Redisplay();
static TreeOffsetPtr	create_offset();
static int              compute_positions();
static void		shift_subtree();
static void		set_positions();
static void             switch_horiz_positions();
static void		reset();
static Position		current_position();
static void		set_current_position();
static Position		sum_of_positions();


static XtResource resources[] = {
	{XtNhorizontalSpace,
	XtCSpace,
	XtRDimension,
	sizeof(Dimension),
	XtOffset(XsTreeWidget, tree.h_min_space),
	XtRString,
	"15"
	},
	{
	XtNverticalSpace,
	XtCSpace,
	XtRDimension,
	sizeof(Dimension),
	XtOffset(XsTreeWidget, tree.v_min_space),
	XtRString,
	"5"
	},
	{
	XtNforeground,
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffset(XsTreeWidget, tree.foreground),
	XtRString,
	"Black"
	},              };

static XtResource treeConstraintResources[] = {
	{
	XtNsuperNode,
	XtCSuperNode,
	XtRPointer,
	sizeof(Widget),
	XtOffset(TreeConstraints, tree.super_node),
	XtRPointer,
	NULL
	},                             };

XsTreeClassRec	XstreeClassRec = {

/*****************	core_class fields	*****************/

	{
	(WidgetClass) &constraintClassRec,      /*      superclass              */
	"Tree",                                 /*      class_name              */
	sizeof(XsTreeRec),                      /*      widget_size             */
	NULL,                                   /*      class_init              */
	NULL,                                   /*      class_part_init         */
	FALSE,                                  /*      class_inited            */
	Initialize,                             /*      initialize              */
	NULL,                                   /*      initialize_hook         */
	XtInheritRealize,			/*	realize			*/
	NULL,                                   /*      actions                 */
	0,                                      /*      num_actions             */
	resources,                              /*      resources               */
	XtNumber(resources),                    /*      num_resources           */
	NULLQUARK,                              /*      xrm_class               */
	TRUE,                                   /*      compress_motion         */
	TRUE,                                   /*      compress_exposure       */
	TRUE,                                   /*      compress_enterleave     */
	TRUE,                                   /*      visible_interest        */
	NULL,                                   /*      destroy                 */
	NULL,                                   /*      resize                  */
	Redisplay,                              /*      expose                  */
	SetValues,                              /*      set_values              */
	NULL,                                   /*      set_values_hook         */
	XtInheritSetValuesAlmost,               /*      set_values_almost       */
	NULL,                                   /*      get_values_hook         */
	NULL,                                   /*      accept_focus            */
	XtVersion,                              /*      version                 */
	NULL,                                   /*      callback_private        */
	NULL,                                   /*      tm_table                */
	NULL,                                   /*      query_geometry          */
	NULL,                                   /*      display_accelerator     */
	NULL,                                   /*      extension               */
	},

/****************	composite_class fields		**************/

	{
	GeometryManager,                        /*      geometry_manager        */
	ChangeManaged,                          /*      changed_managed         */
	XtInheritInsertChild,                   /*      insert_child            */
	XtInheritDeleteChild,                   /*      delete_child            */
	NULL,                                   /*      extension               */
	},

/****************	constraint_class fields		***************/

	{
	treeConstraintResources,		/*	subresources		*/
	XtNumber(treeConstraintResources),      /*      subresource_count       */
	sizeof(TreeConstraintsRec),             /*      constraint_size         */
	ConstraintInitialize,                   /*      initialize              */
	ConstraintDestroy,                      /*      destroy                 */
	ConstraintSetValues,                    /*      set_values              */
	NULL,                                   /*      extension               */
	},

/****************	Tree class fields		**********************/

	{
	0,                                       /*      ignore                  */
	}			};

WidgetClass XstreeWidgetClass = (WidgetClass) &XstreeClassRec;

static void Initialize(request, new)
	XsTreeWidget	request, new;
{
Arg		wargs[2];
XGCValues	values;
XtGCMask        valueMask;

/*	Make sure the widget's height & width are greater than zero	*/

	if (request->core.width <= 0)
		new->core.width = 5;
	if (request->core.height <= 0)
		new->core.height = 5;

/*	Create a graphics context for the connecting lines		*/

valueMask = GCForeground | GCBackground;
values.foreground = new->tree.foreground;
values.background = new->core.background_pixel;
new->tree.gc = XtGetGC ((Widget)new, valueMask, &values);

/*      Create the "fake" root widget                                   */

new->tree.tree_root = (Widget) NULL;
XtSetArg(wargs[0], XtNwidth, 1);
XtSetArg(wargs[1], XtNheight, 1);
new->tree.tree_root = 
	XtCreateWidget("root", widgetClass, (Widget)new, wargs, 2);

/*      Allocate the tables used by the layout algorithm                */

new->tree.horizontal = create_offset(40); /* changed by gfs, 9/12/91      */
new->tree.vertical = create_offset(40);  /* argument of 20 changed to 40 */
}


/**************************************************************

	ConstraintInitialize()
		method sets the n_sub_nodes & sub_nodes members of each
		child's constraint record to NULL & checks to see if the widget has a
		super node. If so, the child widget is added to the super node wdget's
		list of subnodes; otherwise the widget becomes a subnode of the 
		tree_root widget created by Tree widget.

**************************************************************/

static void ConstraintInitialize(request, new)
	Widget	request, new;
{
TreeConstraints tree_const = TREE_CONSTRAINT(new);
XsTreeWidget tw = (XsTreeWidget) new->core.parent;

/*      Initialize the widget to have no subnodes                          */

tree_const->tree.n_sub_nodes = 0;
tree_const->tree.max_sub_nodes = 0;
tree_const->tree.sub_nodes = (WidgetList) NULL;
tree_const->tree.x = tree_const->tree.y = 0;

/*      If this widget has a super node, add it to the widget's subnodes 
	   lists; otherwise, make it a subnode of the tree_root widget     */

	if (tree_const->tree.super_node)
		insert_new_node(tree_const->tree.super_node, new);
	else
		if (tw->tree.tree_root)
			insert_new_node(tw->tree.tree_root, new);
}


/**************************************************************

	SetValues()
		is called when a Tree widget resource is altered - checks the values of
		three resources. If the Tree widget's foreground color is altered, a new
		graphics context is created and the redraw flag is set to TRUE; if either 
		of the horizontal or vertical space resources is modified, SetValues()
		calls the auxillary functions new_layout to reposition all children.
		SetValues() returns the value of theredraw flag that indicates whether
		or not the Intrinsics should force the window to be redrawn.

**************************************************************/

static Boolean SetValues (current, request, new)
	XsTreeWidget    current, request, new;
{
int             redraw = FALSE;
XGCValues       values;
XtGCMask        valueMask;

/*      If the foreground color has changed, redo the GC's & indicate a redraw  */

	if (new->tree.foreground != current->tree.foreground ||
		new->core.background_pixel != current->core.background_pixel)
		{
		valueMask               = GCForeground | GCBackground;
		values.foreground       = new->tree.foreground;
		values.background       = new->core.background_pixel;
		XtReleaseGC((Widget)new, new->tree.gc);
		new->tree.gc            = XtGetGC ((Widget)new, valueMask, &values);
		redraw                  = TRUE;
		}
/*      If the minimum spacing has changed, recalculate the tree layout; new_layout()
		does a redraw, so we don't need SetValues to do another one             */

	if (new->tree.v_min_space != current->tree.v_min_space ||
	    new->tree.h_min_space != current->tree.h_min_space)
		{
		new_layout(new);
		redraw = FALSE;
		}
	return(redraw);
}

/***************************************************************

	ConstraintSetValues()
		invoked when a child's constraint resource is altered - the only resource
		in the Tree widget is the XtNsuperNode resource


***************************************************************/

static Boolean ConstraintSetValues (current, request, new)
	Widget  current, request, new;
{
TreeConstraints newconst        = TREE_CONSTRAINT(new);
TreeConstraints current_const   = TREE_CONSTRAINT(current);
XsTreeWidget    tw              = (XsTreeWidget) new->core.parent;

/*      If the super_node field has changed, remove the widget from the old widget's
		sub_nodes list and add it to the new one.                               */

	if (current_const->tree.super_node != newconst->tree.super_node)
		{
		if (current_const->tree.super_node)
			delete_node(current_const->tree.super_node, new);
		if (newconst->tree.super_node)
			insert_new_node (newconst->tree.super_node, new);

/*      If the Tree widget has been realized, compute new layout.                       */

		if (XtIsRealized((Widget)tw))   new_layout((Widget)tw);
		}
		return  (FALSE);
}

/***************************************************************

	insert_new_node()
		responsible for managing the sub_nodes list in each child's constraint
		record.

***************************************************************/

static void insert_new_node (super_node, node)
	Widget  super_node, node;
{
TreeConstraints super_const     = TREE_CONSTRAINT(super_node);
TreeConstraints node_const      = TREE_CONSTRAINT(node);
int             index           = super_const->tree.n_sub_nodes;

node_const->tree.super_node          = super_node;

/*      If there is now more room in the sub_nodes array, allocate additional space     */

	if (super_const->tree.n_sub_nodes == super_const->tree.max_sub_nodes)
		{
		super_const->tree.max_sub_nodes += (super_const->tree.max_sub_nodes / 2) + 2;
		super_const->tree.sub_nodes = (WidgetList) XtRealloc ((char *)super_const->tree.sub_nodes,
				(super_const->tree.max_sub_nodes) * sizeof (Widget));
		}

/*     Add the sub_node in the next available slot and increment the counter           */

	super_const->tree.sub_nodes[index] = node;
	super_const->tree.n_sub_nodes++;
}

/***************************************************************

	delete_node()
		removes a widget from the list of subnodes, closing any gap
		in the list caused by the removal of an entry, and decrementing
		the n_sub_nodes counter

**************************************************************/

static void delete_node(super_node, node)
	Widget  super_node, node;
{
TreeConstraints node_const = TREE_CONSTRAINT(node);
TreeConstraints super_const;
int     pos, i;

/*      Make sure the super_node exists                                                 */

	if(!super_node) return;
	super_const = TREE_CONSTRAINT(super_node);

/*      Find the sub_node on its super_nodes's list                                     */

	for(pos = 0; pos < super_const->tree.n_sub_nodes; pos++)
		if (super_const->tree.sub_nodes[pos] == node) break;
	if(pos == super_const->tree.n_sub_nodes) return;

/*      Decrement the number of sub_nodes                                               */

	super_const->tree.n_sub_nodes--;

/*      Fill in the gap left by the sub_node; zero the last slot for saftey             */

	for (i = pos; i < super_const->tree.n_sub_nodes; i++)
		super_const->tree.sub_nodes[i] = super_const->tree.sub_nodes[i+1];
	super_const->tree.sub_nodes[super_const->tree.n_sub_nodes] = 0;
}


/***************************************************************

	ConstraintDestroy()
		provides the constraint widget the opportunity to make
		any adjustments required by the deletion of a child widget.

**************************************************************/

static void ConstraintDestroy(w)
	Widget          w;
{

TreeConstraints tree_const = TREE_CONSTRAINT(w);
int     i;

/*      Remove the widget from its parent's sub-nodes list and make all this widget's sub-nodes         */
/*              sub-nodes of the parent                                                                 */

if(tree_const->tree.super_node)
	{
	delete_node(tree_const->tree.super_node, w);
	for(i = 0; i < tree_const->tree.n_sub_nodes; i++)
		insert_new_node(tree_const->tree.super_node, tree_const->tree.sub_nodes[i]);
	}
new_layout(w->core.parent);
}


/*****************************************************************************

	GeometryManager()
		invoked when a child of the Tree widget makes a geometry request. The Tree
		widget's management policy does not allow a child to change it's position,
		because the tree layout algorithm determines the position of the widget. The
		GeometryManager() granys all size request without question.

*****************************************************************************/

static XtGeometryResult GeometryManager (w, request, reply)
	Widget                  w;
	XtWidgetGeometry        *request;
	XtWidgetGeometry        *reply;
{
XsTreeWidget tw = (XsTreeWidget) w->core.parent;

/*      No position changes allowed                                             */

	if ((request->request_mode & CWX && request->x != w->core.x) ||
	    (request->request_mode & CWY && request->y != w->core.y))
	return (XtGeometryNo);

/*      Allow all resize requests                                               */

	if (request->request_mode & CWWidth) w->core.width = request->width;
	if (request->request_mode & CWHeight) w->core.height = request->height;
	if (request->request_mode & CWBorderWidth)
				w->core.border_width = request->border_width;

/*      Compute the new layout based on the new widget sizes                    */

	new_layout(tw);
	return (XtGeometryYes);
}

/********************************************************************************

	ChangeManaged()
		invoked whenever the Tree widget's set of managed children changes; it
		simply calls new_layout() to calculate the desired position of all
		children.

********************************************************************************/

static void ChangeManaged(tw)
	XsTreeWidget    tw;
{
new_layout(tw);
}

/*******************************************************************************

	Redisplay()
		is called whenever an Expose event occurs and is also called by other
		Tree xidget methods to redraw the lines connecting the nodes of the
		tree. This method loops through each child on the Tree widget's list
		of children, drawing a line from the right edge to the left edge of
		each of the widget's subnodes.

*******************************************************************************/

static void Redisplay (w, event, region)
	XsTreeWidget            w;
	XEvent                  *event;
	Region                  region;
{
int                     i, j;
TreeConstraints         tree_const;
Widget                  child;

/*      If the Tree widget is visible, visit each managed child                 */

	if (w->core.visible)
		for (i = 0; i < w->composite.num_children; i++)
		{
		child = w->composite.children[i];
		tree_const = TREE_CONSTRAINT(child);

/*      Draw a line between the right edge of each widget and  the left edge
		of each of it's sub_nodes. Don't draw lines from the fake
		tree_root.                                                      */

	if (child != w->tree.tree_root && tree_const->tree.n_sub_nodes)
		for (j = 0; j < tree_const->tree.n_sub_nodes; j++)
		    {
/*printf("i,j=%2d,%2d, start=(%4d,%4d), end=(%4d,%4d)\n", i, j,
		child->core.x, child->core.y + child->core.height / 2,
		tree_const->tree.sub_nodes[j]->core.x +
			tree_const->tree.sub_nodes[j]->core.width,
		tree_const->tree.sub_nodes[j]->core.y +
			tree_const->tree.sub_nodes[j]->core.height / 2);
*/
			XDrawLine(
				XtDisplay(w),
				XtWindow(w),
				w->tree.gc,
			    /*  child->core.x + child->core.width,  */
				child->core.x,
				child->core.y + child->core.height / 2,
			    /*  tree_const->sub_nodes[j]->core.x,   */
				tree_const->tree.sub_nodes[j]->core.x +
					tree_const->tree.sub_nodes[j]->core.width,
				tree_const->tree.sub_nodes[j]->core.y +
					tree_const->tree.sub_nodes[j]->core.height / 2
				);
		    }
		}
}

/**********************************************************************************

	new_layout()
		provides the top-level interface to the layout algorithm. The function
		resets the auxiliary tables used to store temporary information, then
		calls othe function to do the real work.

*********************************************************************************/

static void new_layout(tw)
	XsTreeWidget    tw;
{
	reset (tw->tree.vertical);                      /* Reset the auxiliary tables           */
	reset (tw->tree.horizontal);

	compute_positions (tw, tw->tree.tree_root, 0);  /* Compute each widget's x,y position   */
	set_positions(tw, tw->tree.tree_root, 0);       /* Move each widget into place          */

	/*  Switch leaves from right to left */
	switch_horiz_positions(tw, tw->tree.tree_root, 0, tw->core.width);

	if (XtIsRealized((Widget)tw))
		{
		XClearArea(XtDisplay(tw), XtWindow(tw), 0, 0, 0, 0, TRUE);
     /*         XClearWindow(XtDisplay(tw), XtWindow(tw));      */
     /*         Redisplay (tw, NULL, NULL);                     */
		}
}

/********************************************************************************

	compute_positions()
		is the main portion of the tree layout algorithm

********************************************************************************/

static int compute_positions(tw, w, level)
	XsTreeWidget    tw;
	Widget          w;
	long            level;
{
Position        current_hpos, current_vpos;
int             i, depth = 0;

TreeConstraints tree_const = TREE_CONSTRAINT(w);

current_hpos = current_position(tw->tree.horizontal, level);    /* Get the current position     */
current_vpos = current_position(tw->tree.vertical, level);      /*      for this level          */

set_current_position(tw->tree.horizontal, level,                /* Set the current horizontal   */
			MAX(current_hpos, w->core.width));      /* width to the max eidths of   */
								/* all widgets at this level    */

if (tree_const->tree.n_sub_nodes == 0)                          /* If the node has no sub_nodes */
	{                                                       /* set the vertical position    */
	tree_const->tree.y = current_vpos;                      /* to the next available space  */
	}
else    {
	Widget                  first_kid, last_kid;
	TreeConstraints         const1, const2;
	Position                top, bottom;

	/* If the node has sub_nodes, recursively figure the positions of each sub_node         */

	for (i = 0; i < tree_const->tree.n_sub_nodes; i++)
		depth = compute_positions(tw, tree_const->tree.sub_nodes[i], level + 1);

	/* Now that the vertical positions of all the children are known, find the vertical
		extent of all su_nodes.                                                         */

	first_kid       = tree_const->tree.sub_nodes[0];
	last_kid        = tree_const->tree.sub_nodes[tree_const->tree.n_sub_nodes - 1];
	const1          = TREE_CONSTRAINT(first_kid);
	const2          = TREE_CONSTRAINT(last_kid);
	top             = const1->tree.y + first_kid->core.height / 2;
	bottom          = const2->tree.y + last_kid->core.height / 2;

	tree_const->tree.y   = (top + bottom) / 2                    /* Set the node's position to   */
				- (w->core.height / 2);              /* the center of it's sub_nodes */

	/*      If this position is less than the next available position, correct it to
			be the next available position, calculate the amount it must be
			shifted, and shift the entire sub-tree                                  */

	if(tree_const->tree.y < current_vpos)
		{
		Dimension offset = current_vpos - tree_const->tree.y;
		for (i = 0; i < tree_const->tree.n_sub_nodes; i++)
			shift_subtree(tree_const->tree.sub_nodes[i], offset);

		/*      Adjust the next available space at all levels below the current level   */

		for (i = level + 1; i <= depth; i++)
			{
			Position pos = current_position(tw->tree.vertical, i);
			set_current_position(tw->tree.vertical, i, pos + offset);
			}
			tree_const->tree.y = current_vpos;
		}
	}

set_current_position(
		tw->tree.vertical,
		level,
		tw->tree.v_min_space + tree_const->tree.y + w->core.height
		);

return (MAX(depth, level));

}


/*************************************************************************

	shift_subtree()
		moves the entire subtree below the given widget by an integer offset

*************************************************************************/

static void shift_subtree(w, offset)
	Widget          w;
	Dimension       offset;
{
int             i;

TreeConstraints tree_const = TREE_CONSTRAINT(w);

tree_const->tree.y += offset;                                /* Shift the node by the offset         */

for (i = 0; i < tree_const->tree.n_sub_nodes; i++)           /* Shift each sub_node into place       */
	shift_subtree(tree_const->tree.sub_nodes[i], offset);
}

/* ***********************************************************************

	set_positions()
		sets the x-position of each widget and calls XtMoveWidget() t-o move
		each widget into place; if all the children don't fit in the Tree
		widget , the function makes a geometry request to the Tree widget's
		parent to attempt to enlarge the Tree Widget.

   *********************************************************************** */

static void set_positions(tw, w, level)
	XsTreeWidget            tw;
	Widget                  w;
	int                     level;
{
int                     i;
Dimension               replyWidth = 0, replyHeight = 0;
XtGeometryResult        result;

if(w)
	{
	TreeConstraints tree_const = TREE_CONSTRAINT(w);

	/* Add-up the sum of the width's of all nodes to this depth, and
		use it as the x-position                                                        */

	tree_const->tree.x = (level * tw->tree.h_min_space)
				+ sum_of_positions(tw->tree.horizontal, level);

	XtMoveWidget (w, tree_const->tree.x, tree_const->tree.y);  /* Move the widget into position       */

	/*      If the widget position plus its width or height doesn't fit, in the tree
			ask if the tree can be resized                                          */

	if(tw->core.width < tree_const->tree.x + w->core.width + 20 ||
	    tw->core.height < tree_const->tree.y + w->core.height + 5)
		{
		result = XtMakeResizeRequest
				(
				(Widget)tw,
				MAX(tw->core.width, tree_const->tree.x + w->core.width + 20),
				MAX(tw->core.height, tree_const->tree.y + w->core.height + 5),
				&replyWidth,
				&replyHeight
				);
		if (result == XtGeometryAlmost)              /* Accept any compromise                */
				XtMakeResizeRequest((Widget)tw, replyWidth, replyHeight, NULL, NULL);
		}
	for (i = 0; i < tree_const->tree.n_sub_nodes; i++)   /* Set the positions of all sub_nodes   */
			set_positions(tw, tree_const->tree.sub_nodes[i], level + 1);
	}
}

/**************************************************************************

	create_offset()
		allocates an array of the given size

**************************************************************************/

static TreeOffsetPtr create_offset(size)
	long            size;
{
TreeOffsetPtr offset    = (TreeOffsetPtr) XtMalloc(sizeof(TreeOffset));
offset->size            = size;
offset->array           = (Dimension *) XtMalloc(size * sizeof(Dimension));
return (offset);
}

/************************************************************************

	reset()
		zeroes all entries in a table

************************************************************************/

static void reset(offset)
	TreeOffsetPtr   offset;
{
long            i;

for (i = 0; i < offset->size; i++) offset->array[i] = 0;
}

/***********************************************************************

	current_position()
		returns the value in a givenposition in a table, if the requested
		position is greater than the size of the table, the function returns zero

***********************************************************************/

static Position current_position(offset, position)
	TreeOffsetPtr           offset;
	long                    position;
{
if (position >= offset->size) return (0);
return (offset->array[position]);
}

/**********************************************************************

	set_current_position()
		stores a value in a table at a given index position; if the
		index is larger than the size of the table, the table is
		enlarged using XtRealloc().

**********************************************************************/

static void set_current_position (offset, index, value)
	TreeOffsetPtr           offset;
	int                     index;
	Dimension               value;
{
if (index >= offset->size)
	{
	offset->size    = index + index / 2;
	offset->array   = (Dimension *) XtRealloc
					(
					(char *)offset->array,
					offset->size * sizeof(Dimension)
					);
	}
	offset->array[index] = value;
}

/**********************************************************************

	sum_of_positions()
		returns the sum of all values in a table up to the given position

**********************************************************************/

static Position sum_of_positions (offset, index)
	TreeOffsetPtr           offset;
	long                    index;
{
int             i;

Position sum    = 0;
long stop       = index;

if (index > offset->size) stop = offset->size;

for (i = 0; i < stop; i++)
	sum += offset->array[i];

return (sum);
}

/************************************************************************

	switch_horiz_positions()  -- taken from set_positions --
		switch the x-position of each widget and calls XtMoveWidget() to move
		each widget into place
		all widgets must fit in the tree because set_positions made
		geometry changes to assure this

************************************************************************/

static void switch_horiz_positions(tw, w, level, max_width)
	XsTreeWidget            tw;
	Widget                  w;
	int                     level, max_width;
{
int                     i;

if (w)
	{
	TreeConstraints tree_const = TREE_CONSTRAINT(w);

	/* Add-up the sum of the width's of all nodes to this depth, subtract from the
		max_width, and use it as the x-position                                                   */

	tree_const->tree.x = max_width - ( ((level+2) * tw->tree.h_min_space)
				+ sum_of_positions(tw->tree.horizontal, level));

	XtMoveWidget (w, tree_const->tree.x, tree_const->tree.y);  /* Move the widget into position       */

	for (i = 0; i < tree_const->tree.n_sub_nodes; i++)   /* Set the positions of all sub_nodes   */
			switch_horiz_positions(tw, tree_const->tree.sub_nodes[i], level + 1, max_width);
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/tree.c,v $";
 static char rcs_id2[] = "$Id: tree.c,v 1.2 2006/04/07 14:11:05 aivo Exp $";}
/*  ===================================================  */

}
