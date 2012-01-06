
/* ******************************************************************************

	delete_atoms.c


	Coded by:       Tom Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/Hydrologic Research Laboratory

	Date:           06/27/91
	Revised:        06/27/91


   ****************************************************************************** */




#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>


#include "libXs.h"
#include "ifp_atoms.h"
#include "ifp_help.h"



 void   delete_the_atoms();
 void   quit_delete_atoms();
 void   update_atomList();
 void   delete_atom_from_List();




delete_atoms_main (argc, argv)
	int             argc;
	char            **argv;
{
	Widget          toplevel;
	Widget          form;
	Widget          scrolled_window, list;
	Widget          bottom_separator;
	Widget          quit_button, delete_button;

	XEvent          event;
	Display         *display;
	Window          root;

	XmString        *xmstring_atomName;
	char            *property_data;
	Atom            *new_atom;

	int             type, format, nitems, left;
	int             j, n;
	int             height, buttonHeight, listHeight, separatorHeight;

	Arg             wargs[8];

	long            offset = 0;

	Atom            *properties;
	int             num_properties;


 num_properties = 0;

 toplevel = XtInitialize(argv[0], "Delete_atoms", NULL, 0, &argc, argv);

 intern_the_atoms(toplevel);

 display = XtDisplay(toplevel);
 root = DefaultRootWindow(display);

 properties = XListProperties(display, root, &num_properties);


 form = XtCreateManagedWidget("delete_atoms_form", xmFormWidgetClass, toplevel, NULL, 0);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 scrolled_window = XtCreateManagedWidget("delete_atoms_sw", xmScrolledWindowWidgetClass, form, wargs, n);


 xmstring_atomName = (XmString *) XtMalloc(sizeof(XmString) * num_properties);

 for(j = 0; j < num_properties; j++)
	xmstring_atomName[j] = XmStringCreate(XGetAtomName(display, properties[j]), XmSTRING_DEFAULT_CHARSET);


 n = 0;
 XtSetArg(wargs[n], XmNitemCount, num_properties); n++;
 XtSetArg(wargs[n], XmNvisibleItemCount, 20); n++;
 XtSetArg(wargs[n], XmNitems, xmstring_atomName); n++;
 XtSetArg(wargs[n], XmNselectionPolicy, XmMULTIPLE_SELECT); n++;
 XtSetArg(wargs[n], XmNscrollBarDisplayPolicy, XmAUTOMATIC); n++;
 list = XtCreateManagedWidget
		(
		"delete_atoms_list",
		xmListWidgetClass,
		scrolled_window,
		wargs,
		n
		);


 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, scrolled_window); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 bottom_separator = XtCreateManagedWidget
		(
		"bottom_separator_widget",
		xmSeparatorWidgetClass,
		form,
		wargs,
		n
		);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, bottom_separator); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNwidth, 100); n++;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Delete", XmSTRING_DEFAULT_CHARSET)); n++;
 delete_button = XtCreateManagedWidget
		(
		"delete_button",
		xmPushButtonWidgetClass,
		form,
		wargs,
		n
		);
 XtAddCallback(delete_button, XmNactivateCallback, delete_the_atoms, list);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, bottom_separator); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNrightWidget, delete_button); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNwidth, 100); n++;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("Quit", XmSTRING_DEFAULT_CHARSET)); n++;
 quit_button = XtCreateManagedWidget
		(
		"quit_button",
		xmPushButtonWidgetClass,
		form,
		wargs,
		n
		);
 XtAddCallback(quit_button, XmNactivateCallback, quit_delete_atoms, NULL);

 XtSetArg(wargs[0], XmNheight, &buttonHeight);
 XtGetValues(quit_button, wargs, 1);

 XtSetArg(wargs[0], XmNheight, &listHeight);
 XtGetValues(list, wargs, 1);

 XtSetArg(wargs[0], XmNheight, &separatorHeight);
 XtGetValues(bottom_separator, wargs, 1);

 height = buttonHeight + listHeight + separatorHeight;

 XtSetArg(wargs[0], XmNheight, height);
 XtSetValues(toplevel, wargs, 1);


 XFree(properties);

 XtRealizeWidget(toplevel);

 XSelectInput(display, root, PropertyChangeMask);

/***************************    MAIN EVENT LOOP   **************************/

while(TRUE)             /*      we're checking for PropertyNotify events        */
	{
	XtNextEvent(&event);

/*      Check for property change events on the ROOT window before dispatching
		the event through the Intrinsics                                */

	switch  (event.type)
		{
		case PropertyNotify:
			if(event.xproperty.window == root && event.xproperty.state != PropertyDelete)
					update_atomList(event.xproperty.atom, list);
			else if(event.xproperty.window == root && event.xproperty.state == PropertyDelete)
					delete_atom_from_List(event.xproperty.atom, list);

			break;

		default:
			XtDispatchEvent(&event);

		}               /*      End of switch statement...      */
	}                       /*      End of main event loop...       */

}                               /*      End of main()...                */




/* ************************************************************************

	delete_the_atoms()
		callback function that deletes Atoms hanging from the root
		window

   ************************************************************************ */

void delete_the_atoms(w, list, call_data)
	Widget                  w;
	Widget                  list;
	XmAnyCallbackStruct     *call_data;
{

	Display         *display;
	Window          root;

	Arg             wargs[4];

	int             i, j;
	int             k = 0;
	int             num_selectedAtoms;
	XmString        *xmstr_selectedAtoms;
	XmString        *xmstring_temp;
	XmString        xmstring_atom;

	Atom            *properties;
	int             num_properties;




 display = XtDisplay(w);
 root = DefaultRootWindow(display);

 properties = XListProperties(display, root, &num_properties);

 XtSetArg(wargs[0], XmNselectedItems, &xmstr_selectedAtoms);
 XtSetArg(wargs[1], XmNselectedItemCount, &num_selectedAtoms);
 XtGetValues(list, wargs, 2);

 xmstring_temp = (XmString *) XtMalloc(sizeof(XmString) * num_selectedAtoms);

 for(i = 0; i < num_selectedAtoms; i++)
	{
	for(j = 0; j < num_properties; j++)
		{
		xmstring_atom = XmStringCreate(XGetAtomName(display, properties[j]), XmSTRING_DEFAULT_CHARSET);
		if(XmStringCompare(xmstring_atom, xmstr_selectedAtoms[i]))
			{
			XDeleteProperty(display, root, properties[j]);
			xmstring_temp[k++] = XmStringCopy(xmstring_atom);
			break;
			}
		}
	}

 XFree(properties);

}



/* ************************************************************************

	quit_delete_atoms()
		exits 'delete_atoms'

   ************************************************************************ */

void quit_delete_atoms(w, client_data, call_data)
	Widget                  w;
	caddr_t                 *client_data;
	XmAnyCallbackStruct    *call_data;
{

 exit(0);

}



/* ************************************************************************

	update_atomList()

   ************************************************************************ */

void update_atomList(new_atom, list)
	Atom            new_atom;
	Widget          list;
{

	Display         *display;
	Window          root;

	Arg             wargs[4];
	char            *new_atom_name;
	char            *string;
	int             j;
	int             num_listItems;
	int             newAtom_is_in_the_List = FALSE;

	XmString        xmstring_atomName;
	XmString        *xmstring_listItems;



 display = XtDisplay(list);
 root = DefaultRootWindow(display);

 XtSetArg(wargs[0], XmNitemCount, &num_listItems);
 XtSetArg(wargs[1], XmNitems, &xmstring_listItems);
 XtGetValues(list, wargs, 2);


 if(num_listItems == 0)
	{
	xmstring_atomName = XmStringCreate(XGetAtomName(display, new_atom), XmSTRING_DEFAULT_CHARSET);
	XmListAddItem(list, xmstring_atomName, 0);

	XtSetArg(wargs[0], XmNvisibleItemCount, 1);
	XtSetValues(list, wargs, 1);
	}
 else   {
	new_atom_name = XGetAtomName(display, new_atom);
	xmstring_atomName = XmStringCreate(new_atom_name, XmSTRING_DEFAULT_CHARSET);
	free(new_atom_name);

	for(j = 0; j < num_listItems; j++)
		{
		if(XmStringCompare(xmstring_listItems[j], xmstring_atomName)) newAtom_is_in_the_List = TRUE;
		}

	if(newAtom_is_in_the_List)
		{       /* Don't add an Atom of the same type to the list              */
		XtFree((char *)xmstring_atomName);
		return;
		}
	else    {
		XmListAddItem(list, xmstring_atomName, 0);
		XtFree((char *)xmstring_atomName);

		XtSetArg(wargs[0], XmNvisibleItemCount, num_listItems + 1);
		XtSetValues(list, wargs, 1);
		}
	 }

}




/* ************************************************************************

	delete_atom_from_List()

   ************************************************************************ */

void delete_atom_from_List(new_atom, list)
	Atom            new_atom;
	Widget          list;
{

	Display         *display;
	Window          root;

	Arg             wargs[4];
	char            *new_atom_name;
	char            *string;
	int             j;
	int             num_listItems;
	int             newAtom_is_in_the_List = FALSE;

	XmString        xmstring_atomName;
	XmString        *xmstring_listItems;



 display = XtDisplay(list);
 root = DefaultRootWindow(display);

 XtSetArg(wargs[0], XmNitemCount, &num_listItems);
 XtSetArg(wargs[1], XmNitems, &xmstring_listItems);
 XtGetValues(list, wargs, 2);


 if(num_listItems == 0)
	{
	return;
	}
 else   {
	new_atom_name = XGetAtomName(display, new_atom);
	xmstring_atomName = XmStringCreate(new_atom_name, XmSTRING_DEFAULT_CHARSET);
	free(new_atom_name);

	for(j = 0; j < num_listItems; j++)
		{
		if(XmStringCompare(xmstring_listItems[j], xmstring_atomName))
			{
			newAtom_is_in_the_List = TRUE;
			break;
			}
		}

	if(newAtom_is_in_the_List)
		{
		XmListDeleteItem(list, xmstring_atomName);
		XtFree((char *)xmstring_atomName);

		XtSetArg(wargs[0], XmNvisibleItemCount, num_listItems - 1);
		XtSetValues(list, wargs, 1);
		}
	 }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/delete_atoms/RCS/delete_atoms.c,v $";
 static char rcs_id2[] = "$Id: delete_atoms.c,v 1.2 2006/04/20 14:18:51 aivo Exp $";}
/*  ===================================================  */

}



