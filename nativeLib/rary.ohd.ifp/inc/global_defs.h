
/* ******************************************************************************************************

   File:                global_defs.h
   Purpose:


   Coded by:            Tom Adams
   Affiliation:         NOAA/NWS/Office of Hydrology/HRL

   Date:                05/22/92
   Last changed:        07/01/92

   ****************************************************************************************************** */

#ifndef global_defs_h
#define global_defs_h

#ifndef _MAX_HELP_TOPICS_DEFINED
#define _MAX_HELP_TOPICS_DEFINED

#define MAX_HELP_TOPICS         100

#endif  /* _MAX_HELP_TOPICS_DEFINED */

Widget  HelpTopicsList;
Widget  HelptextField;
Widget  HelpscrolledText;
Widget  foundCategoriesList_widget;
Widget  patternNotFound_ErrorDialog_widget;


int     NumberOfHelpItems;
int     helpItemPositionSelected;


#ifndef _SEARCH_FILE_PATH_DEFINED
#define _SEARCH_FILE_PATH_DEFINED

 /* Specifies where, in the user's HOME          */
 /*  directory, a temp file will be written...   */
static char    *SEARCH_FILE_PATH = "/.ifp_files/local/";
							
#endif  /* _SEARCH_FILE_PATH_DEFINED    */


char    *HELP_DIRECTORY_PATH;
char    KeyWord_filename[51];
char    *KeyWord;
char    *help_names[MAX_HELP_TOPICS][2];        /* 1st item:    Topic Item (appears in the Topic List widget)   */
						/* 2nd item:    File Name for help text (read in when the       */
						/*              Topic Item is selected...                       */


/*      Data structure passed by Callback functions for 'Help' buttons...       */
typedef struct _help_struct
	{
	char            *message_widget_name;
	Widget          parent;
	}       help_struct;




int     find_selection_locations();
int     parse_search_file();
void    fill_searchList();
int     search_string_is_keyword();
char    *get_Keyword_text();

void    do_app_specific_stuff();
void    highlight_search_string();
void    do_errorDialog_things();

void    create_help_Dialog();
void    map_HelpDialog();
void    unmap_HelpDialog();

#endif
