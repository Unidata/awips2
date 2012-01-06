/***********************************************************************

File: treeP.h

Type: private header file for the Tree_widget
Function: defines the class record for the Tree_widget


Source: X  Window Systems Programming and Applications with Xt
	- by Douglas A. Young

	-- Modified 7/17/90 for OSF/Motif Widgets for use on an IBM-RS6000

***********************************************************************/

#ifndef treeP_h
#define treeP_h

typedef struct _XsTreeClassPart
{
	int     ignore;
}
	XsTreeClassPart;

typedef struct _XsTreeClassRec
{
	CoreClassPart           core_class;
	CompositeClassPart      composite_class;
	ConstraintClassPart     constraint_class;
	XsTreeClassPart         tree_class;
}
	XsTreeClassRec;

extern XsTreeClassRec XstreeClassRec;

typedef struct
{
	Dimension               *array;
	int                     size;
}
	TreeOffset,     *TreeOffsetPtr;

typedef struct
{
	Dimension               h_min_space;
	Dimension               v_min_space;
	Pixel                   foreground;
	GC                      gc;
	TreeOffsetPtr           horizontal;
	TreeOffsetPtr           vertical;
	Widget                  tree_root;
}
	XsTreePart;

typedef struct _XsTreeRec
{
	CorePart                core;
	CompositePart           composite;
	ConstraintPart          constraint;
	XsTreePart              tree;
}
	XsTreeRec;


typedef struct _TreeConstraintsPart
{
	Widget          super_node;
	WidgetList      sub_nodes;
	long            n_sub_nodes;
	long            max_sub_nodes;
	Position        x, y;
}
	TreeConstraintsPart;


typedef struct _TreeConstraintsRec
{
TreeConstraintsPart     tree;
}
	TreeConstraintsRec, *TreeConstraints;


#define TREE_CONSTRAINT(w) ((TreeConstraints) ((w)->core.constraints))

#endif
