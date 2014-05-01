
#include "applicationShell1.h"
#include "callbacks.h"
#include "search_list.h"
#include "messageBox1.h"
#include "global_defs.h"
#include "help.h"


static int HelpListItemPosition();



/* ****************************************************************

	find_selected()
		Callback function for the 'Find' button:

		(1) search the help files for the selected expression;
		(2) report back the file names that contain the expression;
		(3) popup a dialog with a list of the Help topics that
		    correspond to the help files just found...

   **************************************************************** */

void find_selected(w, client_data, call_data)
	Widget                  w;
	_UxCapplicationShell1   *client_data;
	XmAnyCallbackStruct     *call_data;
{

	char                    *textField_item;
	char                    *buffer;
	Display                 *display;
	XWindowAttributes       attributes;


 display = XtDisplay(w);

 /*     Get the selection from the TextField widget, it's guaranteed to be what the user wants  */
 /*     to search for...                                                                        */
 textField_item = XmTextFieldGetString(client_data->UxtextField1);
 if(strlen(textField_item) == 0)
	{ /* There was no item in TextField widget, so Beep & return...                         */
	XBell(display, 100);
        
	return;
	}
 else if(strstr(textField_item, " ") == NULL)
	{ /* If the selection is a single word, test if it's a Keyword & do something...        */

	if(search_string_is_keyword(textField_item))
		{ /* This is the something to do if the selection is a Keyword...               */

		clear_text_from_widget(client_data->UxscrolledText1);
		buffer = get_Keyword_text();
                
		XmTextSetString(client_data->UxscrolledText1, buffer);
                if (buffer != NULL )free(buffer);/*AV added */
		}
	}


 /* If we can't find the string we're searching for, notify the user & return...                */
 if(!find_selection_locations(textField_item))
	{
	XtPopup(patternNotFound_ErrorDialog_widget, XtGrabNone);
	return;
	}

 /* Test if the 'searchList_popup' is Realized (ie, it's been created & mapped); if so, delete  */
 /* the old items from the list and re-fill it with the new HelpTopics that contain the search  */
 /* pattern; otherwise, we need to create the 'searchList_popup'...                             */
 if(searchList_popup != NULL)
	{
	if(XtIsRealized(searchList_popup))
		{
		XmListDeleteAllItems(foundCategoriesList_widget);
		fill_searchList(foundCategoriesList_widget);

		XGetWindowAttributes(display, XtWindowOfObject(searchList_popup), &attributes);
		if(attributes.map_state == IsUnmapped) XtMapWidget(searchList_popup);

		return;
		}
	}
 else   {
	searchList_popup = (Widget) create_transientShell1(client_data->UxapplicationShell1);
	XtPopup(searchList_popup, XtGrabNone);
	}

}


void close_selected(w, client_data, call_data)
	Widget                  w;
	_UxCapplicationShell1   *client_data;
	XmAnyCallbackStruct     *call_data;
{

 XtPopdown(client_data->UxapplicationShell1);

}


void help_selected(w, client_data, call_data)
	Widget                  w;
	_UxCapplicationShell1   *client_data;
	XmAnyCallbackStruct     *call_data;
{
}


void listItem_selected(w, client_data, call_data)
	Widget                  w;
	_UxCapplicationShell1   *client_data;
	XmListCallbackStruct    *call_data;
{

	char            *buffer;
	/*XmTextPosition  left;
	XmTextPosition  right;
	char            *prev_text;*/
        int             i;



 clear_text_from_widget(client_data->UxscrolledText1);

 /*     Get the HelpText for the new help category and display it in the ScrolledText widget...                 */
 buffer = (char *) get_help_text(call_data->item_position);
 XmTextSetString(client_data->UxscrolledText1, buffer);
 if(buffer != NULL) free(buffer); /*added by AV */

}



void set_helpItem_selected(w, client_data, call_data)
	Widget                  w;
	_UxCtransientShell1     *client_data;
	XmListCallbackStruct    *call_data;
{

  helpItemPositionSelected = call_data->item_position;

}


void show_helpItem_selected(w, client_data, call_data)
	Widget                  w;
	_UxCtransientShell1     *client_data;
	XmListCallbackStruct    *call_data;
{

	char    *string;


 XmListSelectItem(HelpTopicsList, call_data->item, TRUE);

 string = XmTextFieldGetString(HelptextField);
 highlight_search_string(HelpscrolledText, string);

}


void show_pattern_selected(w, client_data, call_data)
	Widget                  w;
	_UxCtransientShell1     *client_data;
	XmAnyCallbackStruct     *call_data;
{

	XmString        *xmstr_listItems;
	char            *string;


 XtVaGetValues(client_data->UxscrolledList2,
	       XmNitems,        &xmstr_listItems,
	       NULL);

 XmListSelectItem(HelpTopicsList, xmstr_listItems[helpItemPositionSelected - 1], TRUE);

 string = XmTextFieldGetString(HelptextField);
 highlight_search_string(HelpscrolledText, string);
 if (string != NULL)
    free(string);

}


void close_search_popup(w, client_data, call_data)
	Widget                  w;
	_UxCtransientShell1     *client_data;
	XmAnyCallbackStruct     *call_data;
{

	XtPopdown(client_data->UxtransientShell1);
	XtDestroyWidget(client_data->UxtransientShell1);
	searchList_popup = NULL;

}



void set_selected_text(w, client_data, call_data)
	Widget                  w;
	_UxCapplicationShell1   *client_data;
	XmTextVerifyCallbackStruct     *call_data;
{

	char    *selected_string;


 selected_string = XmTextGetSelection(w);

 if(selected_string != NULL)/*added by AV*/
 { 
     XmTextFieldSetString(client_data->UxtextField1, selected_string);
     XtFree(selected_string);
 }


}



void popdown_patternNotFound_dialog(w, client_data, call_data)
	Widget                  w;
	_UxCmessageBox1         *client_data;
	XmAnyCallbackStruct     *call_data;
{

 XtPopdown(XtParent(client_data->UxmessageBox1));

}




/* ****************************************************************************

	popup_help_window()
		interface between help code and application code; callback
		function to pop-up the help window...

		The widget: 'HelpTopicsList' is global by necessity
			    (due to use of a GUI builder).

   **************************************************************************** */

void popup_help_window(w, HelpFileName, call_data)
	Widget                  w;
	char                    *HelpFileName;
	XmAnyCallbackStruct     *call_data;
{

	XWindowAttributes       attributes;
	int                     selection;


 if(popupHelp_shell == NULL)
	{
	puts("'popupHelp_shell' is NULL, return...");
	return;
	}

 
 if(XtIsRealized(popupHelp_shell))
	{
	XGetWindowAttributes(XtDisplay(w), XtWindowOfObject(popupHelp_shell), &attributes);
	if(attributes.map_state == IsUnmapped) XtPopup(popupHelp_shell, XtGrabNone);
	}
 else XtPopup(popupHelp_shell, XtGrabNone);

 if(selection = HelpListItemPosition(HelpFileName)) XmListSelectPos(HelpTopicsList, selection, TRUE);

}


/* ****************************************************************************

	int HelpListItemPosition()

   **************************************************************************** */

int HelpListItemPosition(HelpFileName)
	char    *HelpFileName;
{

	int     i;

 for(i = 0; i < NumberOfHelpItems; i++)
	{
	if(strcmp(HelpFileName, help_names[i][1]) == 0) return(i + 1);
	}

 return(0);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/OnlineHelp/RCS/OnlineHelp_callbacks.c,v $";
 static char rcs_id2[] = "$Id: OnlineHelp_callbacks.c,v 1.3 2006/04/19 21:16:12 aivo Exp $";}
/*  ===================================================  */

}
