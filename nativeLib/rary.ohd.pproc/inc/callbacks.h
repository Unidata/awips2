
/* ******************************************************************************************************

   File:                callbacks.h
   Purpose:


   Coded by:            Tom Adams
   Affiliation:         NOAA/NWS/Office of Hydrology/HRL

   Date:                05/15/92
   Last changed:        06/03/92

   ****************************************************************************************************** */

#ifndef callbacks_h
#define callbacks_h


#ifndef _MAX_HELP_TOPICS_DEFINED
#define _MAX_HELP_TOPICS_DEFINED

#define MAX_HELP_TOPICS         100

#endif  /* _MAX_HELP_TOPICS_DEFINED */

char    *helpFile_name[MAX_HELP_TOPICS];

int     numberOfHelpOccurrences;
Widget  searchList_popup;

extern void listItem_selected();
extern void find_selected();
extern void close_selected();
extern void help_selected();

extern void set_helpItem_selected();
extern void show_helpItem_selected();
extern void show_pattern_selected();
extern void close_search_popup();
extern void set_selected_text();

extern void create_help_Dialog();
extern void popdown_patternNotFound_dialog();

#endif   /* callbacks_h */
