

/* ******************************************************************************************************

   File:                online_help.c
   Purpose:             provides online help through a Motif PushButton Callback... a help window "pops-up",
			allowing the user to select help from a list of help topics...

   Coded by:            Tom Adams
   Affiliation:         NOAA/NWS/Office of Hydrology/HRL

   Date:                05/07/92
   Modified:            05/07/92, 07/08/92

   ****************************************************************************************************** */


#include "applicationShell1.h"
#include "global_defs.h"
#include "online_help.h"

char *help_names_file = "help_topics";

void read_help_names()
{

	char    *filePathName;
	char    *buffer;
	int     filePathName_length;


 filePathName_length = strlen(HELP_DIRECTORY_PATH) + strlen(help_names_file);
 filePathName = (char *) malloc((filePathName_length + 1) * sizeof(char));

 memset(filePathName, '\0', filePathName_length + 1);

 strcpy(filePathName, HELP_DIRECTORY_PATH);
 strcat(filePathName, help_names_file);
 
 buffer = GetSource(filePathName);

 parse_help_names(buffer);
 


}


/* ************************************************************************************

	parse_help_names()

		'buffer' is parsed by assuming that the file is structured:

		line1:  "TopicItem1" ...some white space... FileName1
		line2:  "TopicItem2" ...some white space... FileName2
		  *
		  *
		  *
		lineN:  "TopicItemN" ...some white space... FileNameN

		where N <= MAX_HELP_TOPICS and the TopicItems are contained within
		double quotes ("...").

		NOTE:   'line1:', 'line1:', etc., does not appear in the file!
			'...some white space...' denotes any number of spaces, tabs, etc.

   ************************************************************************************ */

void parse_help_names(buffer)
	char    *buffer;
{

	char    *string;
	char    *quote;
	char    *endline;
	int     i = 0;
	int     size;




 while(strlen(buffer))
	{
	endline = strstr(buffer, "\n");
	size = endline - buffer + 1;            /* The space calculated is the entire line + space for '\0' in  */
						/* place of '\n', to terminate the string...                    */

	string = (char *) malloc(sizeof(char) * 100);
	memset(string, '\0', 100);
	strncpy(string, buffer, size - 1);      /* So, we have: "TopicItem"...white space...FileName\0          */

	buffer += size;                         /* Advance 'buffer' to the beginning of the next line...        */
	string++;                               /* Advance 'string' past the first quotation mark...            */

	quote = strchr(string, '"');
	size = quote - string;
	help_names[i][0] = (char *) malloc(size + 1);
	memset(help_names[i][0], '\0', size + 1);

	/* Get the TopicItem...                 */
	strncpy(help_names[i][0], string, size);
  /*printf("%s\n", help_names[i][0]);  */ 

	string += (size + 1);                   /* Advance 'string' past the second quotation mark...           */
	while(isspace(*string)) string++;       /* Advance 'string' past any white space...                     */

	size = strlen(string);
	help_names[i][1] = (char *) malloc(size + 1);

	/* Get the FileName...                  */
	strcpy(help_names[i][1], string);
 /*printf("%s\n", help_names[i][1]);   */

	i++;
	}

 NumberOfHelpItems = i;

}


char *get_help_text(selected_item)
	int     selected_item;
{

	char    *filePathName;
	char    *buffer;
	int     filePathName_length;


 filePathName_length = strlen(HELP_DIRECTORY_PATH) + strlen(help_names[selected_item - 1][1]);
 filePathName = (char *) malloc((filePathName_length + 1) * sizeof(char));

 memset(filePathName, '\0', filePathName_length + 1);

 strcpy(filePathName, HELP_DIRECTORY_PATH);
 strcat(filePathName, help_names[selected_item - 1][1]);
 
 buffer = GetSource(filePathName);
 
 if(filePathName != NULL) free(filePathName); /*added by AV */

 return(buffer);

}


void make_xmstr_listItem_array()
{

	int     i;


 xmstr_helpItems = (XmString *) XtMalloc(sizeof(XmString) * NumberOfHelpItems);

 for(i = 0; i < NumberOfHelpItems; i++)
		xmstr_helpItems[i] = XmStringCreate(help_names[i][0], XmSTRING_DEFAULT_CHARSET);

}




char *get_Keyword_text()
{

	char    *filePathName;
	char    *keyword_subdirectory = "keyword_files/";
	char    *buffer;
	char    *string;
	int     filePathName_length;


 filePathName_length = strlen(HELP_DIRECTORY_PATH) + strlen(keyword_subdirectory) + strlen(KeyWord_filename);
 filePathName = (char *) malloc((filePathName_length + 1) * sizeof(char));

 memset(filePathName, '\0', filePathName_length + 1);


 strcpy(filePathName, HELP_DIRECTORY_PATH);
 strcat(filePathName, keyword_subdirectory);
 strcat(filePathName, KeyWord_filename);

 buffer = GetSource(filePathName);

 if(buffer == NULL)
	{
	string = (char *) malloc(sizeof(char) * 101);
	strcpy(string, "Sorry, no keyword file was found for: ");
	strcat(string, KeyWord_filename);
	return(string);
	}
 else return(buffer);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/OnlineHelp/RCS/online_help.c,v $";
 static char rcs_id2[] = "$Id: online_help.c,v 1.2 2002/02/11 19:11:49 dws Exp $";}
/*  ===================================================  */

}


