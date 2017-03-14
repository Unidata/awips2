/*  libXs.h:  declarations for the X-sample library                                                     */

#ifndef libXs_h
#define libXs_h

#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#ifndef XtRFloat
#define XtRFloat  "Float"
#endif

#define PUSH_BUTTON     1
#define TOGGLE_BUTTON   2



typedef struct _menu_widget_struct
	{
	Widget                          parent;         /* parent widget for a sub-menu                 */
	struct _menu_widget_struct      **widget_array; /* An array of structs that include all widgets */
	}       xs_menu_widget_struct;                  /*      found in the sub-menu of parent...      */



/*      Structure used to describe an entry in a menu... used by xs_create_menu_buttons()               */
typedef struct _menu_struct{
	char                    *name;                  /* Name of the button                           */
	void                    (*func)();              /* Callback to be invoked                       */
	caddr_t                 data;                   /* Data for the callback                        */
	int                     active;                 /* Boolean indicating that the button is active */
	int                     button_type;            /* Flag indicating PUSH_BUTTON or TOGGLE_BUTTON */
	struct _menu_struct     *sub_menu;              /* Data for the submenu of this button          */
	int                     n_sub_items;            /* Number of items in the sub_menu              */
	char                    *sub_menu_title;        /* Title of the submenu                         */
	}       xs_menu_struct;


#endif 
