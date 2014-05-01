/* ****************************************************************************************************

   menus.c:  Simple menu package

   Source:      X Window Systems Programming with Xt - Motif Edition
		-- by Douglas Young


   Modifications by:    Thomas Adams & George Smith
			NWS/Office of Hydrology/Hydrologic Research Laboratory


   **************************************************************************************************** */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include "libXs.h"

xs_menu_widget_struct *xs_create_menu_buttons(title, menu, menulist, nitems)
	char            *title;
	Widget          menu;
	xs_menu_struct  *menulist;
	int             nitems;      /* number of items */
{
	Arg             wargs[6];
	int             i, n;        /* counters */
	int             num_buttons; /* number of button widgets */
	WidgetList      buttons;
	int             separators = 0;

	xs_menu_widget_struct   *widgetStruct;

/*  Allocate a widget list to hold all button widgets. */

buttons = (WidgetList) XtMalloc(nitems * sizeof(Widget));

widgetStruct = (xs_menu_widget_struct *) malloc(sizeof(xs_menu_widget_struct));

widgetStruct->parent = menu;

/* If a title is given, create Label and Separator widgets              */
if(title == (char *) NULL)
	{
	XtCreateManagedWidget(title, xmLabelWidgetClass, menu, NULL, 0);
	XtCreateManagedWidget("separator", xmSeparatorWidgetClass, menu, NULL, 0);
	}

for(i = 0; i < nitems; i++)     /* Count how many entries aren't buttons,       */
	{                       /* so we malloc space for buttons only...       */
	if(menulist[i].name == NULL)
		{
		separators++;
		}
	}

/*      Allocate space for each of the menu items (separators excluded)...      */
num_buttons = nitems - separators;
widgetStruct->widget_array = (xs_menu_widget_struct **) malloc(sizeof(xs_menu_widget_struct *) * num_buttons);

separators = 0;


/*  Create a menu button for each item in the menu. */

for(i = 0; i < nitems; i++)
	{
	if(menulist[i].name == NULL)
		{
		XtCreateManagedWidget("separator", xmSeparatorWidgetClass, menu, NULL, 0);
		separators++;   /* Count how many entries aren't buttons        */
		}
	else if(menulist[i].func)
		{               /* If there is a name & a callback, create a selectable         */
				/*      menu entry & register the callback function             */
		if(menulist[i].active == TRUE)
			{
			if(menulist[i].button_type == PUSH_BUTTON)
				{
				buttons[i - separators] =
					XmCreatePushButton(menu, menulist[i].name, NULL, 0);
					XtAddCallback(buttons[i - separators], XmNactivateCallback,
						      menulist[i].func, menulist[i].data);
				}
			else    {       /* menulist[i].button_type is TOGGLE_BUTTON...          */
				buttons[i - separators] =
					(Widget)XmCreateToggleButton(menu, menulist[i].name, NULL, 0);
					XtAddCallback(buttons[i - separators], XmNvalueChangedCallback,
						      menulist[i].func, menulist[i].data);
				}
			widgetStruct->widget_array[i - separators] =
				(xs_menu_widget_struct *) malloc(sizeof(xs_menu_widget_struct));
			widgetStruct->widget_array[i - separators]->parent = buttons[i - separators];
			widgetStruct->widget_array[i - separators]->widget_array = NULL;
			}
		else    {
			n = 0;
			XtSetArg(wargs[n], XtNsensitive, FALSE); n++;
			XtSetArg(wargs[n], XmNshadowThickness, 2); n++;
			buttons[i - separators] =
				XtCreateWidget(menulist[i].name,
					       xmLabelWidgetClass,
					       menu, wargs, n);

			widgetStruct->widget_array[i - separators] =
				(xs_menu_widget_struct *) malloc(sizeof(xs_menu_widget_struct));
			widgetStruct->widget_array[i - separators]->parent = buttons[i - separators];
			widgetStruct->widget_array[i - separators]->widget_array = NULL;
			}
		 }
	else if(!menulist[i].sub_menu)
		 {               /* If there is a name but no callback function, the entry       */
				 /*      must be a label, unless there is a submenu              */
		 buttons[i - separators] =
				XtCreateWidget(menulist[i].name,
					       xmLabelWidgetClass,
					       menu, NULL, 0);

		widgetStruct->widget_array[i - separators] =
			(xs_menu_widget_struct *) malloc(sizeof(xs_menu_widget_struct));
		widgetStruct->widget_array[i - separators]->parent = buttons[i - separators];
		widgetStruct->widget_array[i - separators]->widget_array = NULL;
		}
	else    {       /* The entry must be a submenu; so, create a pulldown menu pane and a   */
			/*      XmCascadeButton widget. Attach the menu pane & make a           */
			/*      recursive call to create the entries in the submenu...          */
		Widget  sub_menu;
		sub_menu = XmCreatePulldownMenu(menu, menulist[i].sub_menu_title, NULL, 0);
		XtSetArg(wargs[0], XmNsubMenuId, sub_menu);

		buttons[i - separators] =
				XtCreateWidget(menulist[i].name,
					       xmCascadeButtonWidgetClass,
					       menu, wargs, 1);

		widgetStruct->widget_array[i - separators] =
					xs_create_menu_buttons
						(
						menulist[i].sub_menu_title,
						sub_menu,
						menulist[i].sub_menu,
						menulist[i].n_sub_items
						);

/*                widgetStruct->widget_array[i - separators]->parent = buttons[i - separators]; */
		}
	}

/*      Manage all button widgets, menu panes are not managed...               */
XtManageChildren(buttons, nitems - separators);

return(widgetStruct);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/menus.c,v $";
 static char rcs_id2[] = "$Id: menus.c,v 1.2 2006/04/07 14:10:55 aivo Exp $";}
/*  ===================================================  */

}
