
/* ******************************************************************************************************

   File:                online_help.h
   Purpose:             Include file for online_help.c; provides online help through a Motif PushButton Callback...
			a help window "pops-up", allowing the user to select help from a list of help topics.

   Coded by:            Tom Adams
   Affiliation:         NOAA/NWS/Office of Hydrology/HRL

   Date:                05/07/92
   Last changed:        05/07/92

   ****************************************************************************************************** */

#ifndef online_help_h
#define online_help_h

#define MAX_PATHNAME_LENGTH     100

XmString        *xmstr_helpItems;

extern char *help_names_file;
extern char *help_directory;            /* Set the path to where you want the help files to     */
					/* reside so that they will be accessible by all...     */

void    read_help_names();
void    parse_help_names();
char    *get_help_text();
void    make_xmstr_listItem_array();

char    *GetSource();

#endif
