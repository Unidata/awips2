

#include "applicationShell1.h"
#include "online_help.h"
#include "callbacks.h"
#include "search_list.h"
#include "messageBox1.h"
#include "global_defs.h"
#include "help.h"





void do_app_specific_stuff(UxContext, shell, help_path)
	_UxCapplicationShell1   *UxContext;
	Widget                  shell;
	char                    *help_path;
{

	Widget          sw1_verticalScrollBar;
	Widget          help_dialogShell;
	int             value_return, sliderSize_return, increment_return, pageIncrement_return;
	int             backgroundColor;
	help_struct     *help_data;   /* Structure for passing data for on-line help... */



 /* This global variable sets the location of the help files for the application...     */

 HELP_DIRECTORY_PATH = (char *) malloc((strlen(help_path) + 1) * sizeof(char));
 memset(HELP_DIRECTORY_PATH, '\0', strlen(help_path) + 1);
 strcpy(HELP_DIRECTORY_PATH, help_path);
 
 /* Make the these widgets available universally...     */
 HelptextField = UxContext->UxtextField1;
 HelpscrolledText = UxContext->UxscrolledText1;
 HelpTopicsList = UxContext->UxscrolledList1;


/* -------------------------------------------------------------------------------------------- */
/* -------------- Read in the Help names from a file & display them in a List widget ---------- */

 read_help_names();
 make_xmstr_listItem_array();

 XtVaSetValues(UxContext->UxscrolledList1,
	       XmNitemCount,         NumberOfHelpItems,
	       XmNvisibleItemCount,  NumberOfHelpItems,
	       XmNitems,             xmstr_helpItems,
	       NULL
	       );
/* -------------------------------------------------------------------------------------------- */



 /* T E M P O R A R Y  D E F A U L T . . .  show Help for the 1st item in the Help list...      */
 /*     In each implementation of this help system, the default should be to show help          */
 /*     relevant to the window that initiated the Help callback...                              */
 /*  XmListSelectPos(UxContext->UxscrolledList1, 1, TRUE);                                      */



/* -------------------------------------------------------------------------------------------- */
/* -------------- Make sure the Help text is positioned at the top of the window -------------- */

 XtVaGetValues(UxContext->UxscrolledWindow1,
	       XmNverticalScrollBar, &sw1_verticalScrollBar,
	       NULL);
 XmScrollBarGetValues(sw1_verticalScrollBar, &value_return, &sliderSize_return,
			 &increment_return, &pageIncrement_return);

 value_return = 0;       /* This sets the scrollBar position to the Top...       */
 XmScrollBarSetValues(sw1_verticalScrollBar, value_return, sliderSize_return,
			 increment_return, pageIncrement_return, TRUE);
/* -------------------------------------------------------------------------------------------- */


 help_data = (help_struct *) malloc(sizeof(help_struct));
 help_data->parent = UxContext->UxapplicationShell1;
 help_data->message_widget_name = "Help_button";
 help_dialogShell = (Widget)create_helpDialog(help_data);
 XtAddCallback(UxContext->UxpushButton3, XmNactivateCallback, map_HelpDialog, help_dialogShell);

 XtVaGetValues(UxContext->Uxform1, XmNbackground, &backgroundColor, NULL);
 XtVaSetValues(UxContext->UxpushButton1, XmNborderColor, backgroundColor, NULL);
 XtVaSetValues(UxContext->UxpushButton3, XmNborderColor, backgroundColor, NULL);

 /* Create an ErrorDialog which will let the user know that a search pattern was not found      */
 /* in any of the help files corresponding to the Help Topics...                                */
 patternNotFound_ErrorDialog_widget = XtParent((Widget)create_messageBox1(shell));

}




int find_selection_locations(selection)
	char    *selection;
{

	int     size;

	char    *string;
	char    *search_TempFile;
	char    home_dir[100];
	char    *remove_temp;
	char    *quote = "\"";
	char    *searchPath = HELP_DIRECTORY_PATH;      /* Set the SEARCH DIRECTORY ... */
	char    *fileName = "helpSearch_file";




 strcpy(home_dir, (char *)getenv("HOME"));

 /* Set the TEMP file used in the search...     */
 size = strlen(home_dir) + strlen(SEARCH_FILE_PATH) + strlen(fileName) + 1;
 search_TempFile = (char *) malloc(sizeof(char) * size);

 memset(search_TempFile,'\0', size);
 strcpy(search_TempFile, home_dir);
 strcat(search_TempFile, SEARCH_FILE_PATH);
 strcat(search_TempFile, fileName);

 /* --------------------------------------------------------
    Construct 'string' so that it looks like:

    'grep "selection" * > search_TempFile',

    where, 'selection' is the search pattern & 'search_TempFile'
    is a hidden temp file in the user's HOME directory...
    -------------------------------------------------------- */

 string = (char *) malloc(strlen(selection) + strlen(searchPath) + strlen(search_TempFile) + 13);
 strcpy(string, "grep ");
 strcat(string, quote);
 strcat(string, selection);
 strcat(string, quote);
 strcat(string, " ");
 strcat(string, searchPath);
 strcat(string, "*");
 strcat(string, " > ");
 strcat(string, search_TempFile);

 /* printf("search_TempFile = %s\n", search_TempFile);  */

 system(string);
 if(!parse_search_file(search_TempFile)) return(FALSE);

 remove_temp = (char *) malloc(strlen(search_TempFile) + 4);
 strcpy(remove_temp, "rm ");
 strcat(remove_temp, search_TempFile);
 system(remove_temp);

 return(TRUE);

}


/* ************************************************************************************

	parse_search_file()

		'buffer' is parsed by assuming that the file is structured:

		line1:  FileName1: ...ignore everything else...
		line2:  FileName2: ...ignore everything else...
		  *
		  *
		  *
		lineN:  FileNameN: ...ignore everything else...

		where N is unknown

		NOTE:   we are interested only in the file names, which appear before
			the colans, ':'

   ************************************************************************************ */

int parse_search_file(filePathName)
	char    *filePathName;
{

	char    *buffer;
	char    *string;
	char    *colan;
	char    *endline;
	char    *last_slash;
	char    temp[100];
	int     i = 0;
	int     j;
	int     FileFound;
	int     size;



 buffer = GetSource(filePathName);
 if(buffer == NULL)
	{
	/* printf("%s was not found!\n", filePathName); */
	return(FALSE);
	}

 /* printf("%s", buffer);       */

 while(strlen(buffer))
	{
	memset(temp, '\0', 100);

	endline = strstr(buffer, "\n");
	size = endline - buffer + 1;            /* The space calculated is the entire line + space for '\0' in  */
						/* place of '\n', to terminate the string...                    */

	string = (char *) malloc(sizeof(char) * 200);
	memset(string, '\0', 200);
	strncpy(string, buffer, size - 1);      /* So, we have: FileName: ...un-wanted stuff...                 */

	buffer += size;                         /* Advance 'buffer' to the beginning of the next line...        */

	colan = strchr(string, ':');
	size = colan - string;

	/* Get the Filename...                  */
	strncpy(temp, string, size);
	last_slash = strrchr(temp, '/');        /* We want just the file name, not its full path...     */
	last_slash++;

	FileFound = FALSE;
	for(j = 0; j < i; j++)
		{
		if(strcmp(helpFile_name[j], last_slash) == 0)
			{       /* The HelpFile name is in the array already, & we don't need to find           */
				/* anymore occurences - so, break...                                            */
			FileFound = TRUE;
			break;
			}
		}

	if(!FileFound)
		{       /* We're not repeating a HelpFile name, it's a new entry to the array...                */
		helpFile_name[i] = (char *) malloc(size + 1);
		memset(helpFile_name[i], '\0', size + 1);

		strcpy(helpFile_name[i], last_slash);
/*                printf("%s\n", helpFile_name[i]);             */
		i++;
		}
	}

 numberOfHelpOccurrences = i;
 return(TRUE);
}



void fill_searchList(searchList)
	Widget  searchList;
{

	XmString        *xmstr_items;
	int             i;
	int             j;



 foundCategoriesList_widget = searchList;
 xmstr_items = (XmString *) XtMalloc(sizeof(XmString) * numberOfHelpOccurrences);

 for(i = 0; i < numberOfHelpOccurrences; i++)
	{
	for(j = 0; j < NumberOfHelpItems; j++)
		if(strcmp(helpFile_name[i], help_names[j][1]) == 0) break;
	xmstr_items[i] = XmStringCreate(help_names[j][0], XmSTRING_DEFAULT_CHARSET);

	}

 XtVaSetValues(searchList,
	       XmNitems,                xmstr_items,
	       XmNitemCount,            numberOfHelpOccurrences,
	       XmNvisibleItemCount,     numberOfHelpOccurrences,
	       NULL);
 XtFree((String )xmstr_items);

}



void  highlight_search_string(scrolledText, search_string)
	Widget          scrolledText;
	char            *search_string;
{

	int     length;
	int     string_begin;
	int     string_end;

	char    *string;
	char    *temp;
	char    *next;


 length = strlen(search_string);
 string = (char *) malloc(length + 1);
 strcpy(string, search_string);

 temp = XmTextGetString(scrolledText);
 next = strstr(temp, search_string);
 if(next != NULL)
	{
	/*
	XmTextSetHighlight(scrolledText,
			   (XmTextPosition) 0,
			   (XmTextPosition) length, XmHIGHLIGHT_SELECTED);
	*/

	string_begin = next - temp;
	string_end = string_begin + length;

	XmTextSetHighlight(scrolledText, (XmTextPosition) string_begin,
				(XmTextPosition) string_end, XmHIGHLIGHT_SELECTED);
	}

 if(string != NULL) free(string);/*added by AV*/
}



void do_errorDialog_things(UxContext)
	_UxCmessageBox1   *UxContext;
{

	Widget  ok_button;
	Widget  help_button;



 ok_button = XmMessageBoxGetChild(UxContext->UxmessageBox1, XmDIALOG_OK_BUTTON);
 help_button = XmMessageBoxGetChild(UxContext->UxmessageBox1, XmDIALOG_HELP_BUTTON);

 XtUnmanageChild(ok_button);
 XtUnmanageChild(help_button);

}



int search_string_is_keyword(search_string)
	char    *search_string;
{

	int     length;
	char    *string;
	char    *filePathName;
	char    *quote = "\"";
	char    *searchDir = HELP_DIRECTORY_PATH;
	char    *fileName = "helpKeyword_file";
	char    *buffer;
	char    *new_line;
	char    *keyword;
	char    *end_of_line;
	char    *end_of_keyword;



 /* printf("Inside 'search_string_is_keyword'...\n");   */

 filePathName = (char *) malloc(strlen(searchDir) + strlen(fileName) + 1);
 strcpy(filePathName, searchDir);
 strcat(filePathName, fileName);


 buffer = GetSource(filePathName);
 if(buffer == NULL)
	{
	printf("%s was not found!\n", filePathName);
	return(FALSE);
	}

 /* printf("%s", buffer);       */

 while(strlen(buffer)){
 if((string = strstr(buffer, search_string)) == NULL) return(FALSE);
 else   {       /* 'search_string' was found in 'buffer'; Check that 'buffer' does not have     */
		/* more characters to the left of 'search_string'...                            */

	if((end_of_line = strstr(string, "\n")) == NULL) return(FALSE);
	else if((string == buffer) || (*(string - 1) == '\n'))
		{
		length = end_of_line - string;
		new_line = (char *) malloc(sizeof(char) * length + 1);
		memset(new_line, '\0', length + 1);
		strncpy(new_line, string, length);

		end_of_keyword = strstr(new_line, " ");
		length = end_of_keyword - new_line;
		keyword = (char *) malloc(sizeof(char) * length + 1);
		memset(keyword, '\0', length + 1);
		strncpy(keyword, new_line, length);

		/* Make sure 'keyword' & 'search_string' are exact matches...                   */
		if(strcmp(keyword, search_string) == 0)
			{ /* Get the filename for the Keyword, skipping over any white space... */
			while(isspace(*end_of_keyword)) end_of_keyword++;
			strcpy(KeyWord_filename, end_of_keyword);
			end_of_line = strstr(KeyWord_filename, "\n");
			if(end_of_line != NULL) *end_of_line = '\0';

			return(TRUE);
			}
		}
	else buffer = end_of_line + 1;
	}
    } /* End of while loop...                   */

 return(FALSE);

}


void clear_text_from_widget(w)
	Widget          w;
{

	XmTextPosition  left;
	XmTextPosition  right;
	char            *prev_text;


 prev_text = XmTextGetString(w);
 if(prev_text != NULL)
	{       /* There is old HelpText in the Text widget, so unhighlight any selected text                   */
		/* and re-initialize the widget's text-value to NULL...                                         */

	/* If there is highlighted text, set the selection to 'XmHIGHLIGHT_NORMAL', which unhighlights it...    */
	if(XmTextGetSelectionPosition(w, &left, &right))
		XmTextSetHighlight(w, left, right, XmHIGHLIGHT_NORMAL);

	/* This clears-out the text in the widget...            */
	XmTextReplace(w,
		      (XmTextPosition) 0,
		      (XmTextPosition) strlen(prev_text),
		      (char *) NULL);
	free(prev_text);/* modified by AV*/
	}


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/OnlineHelp/RCS/aux_funcs.c,v $";
 static char rcs_id2[] = "$Id: aux_funcs.c,v 1.3 2006/04/19 21:16:14 aivo Exp $";}
/*  ===================================================  */

}
