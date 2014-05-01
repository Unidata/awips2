/************************************************************************
   temp_info_struct.h
   
   PURPOSE
   Include file for structure on ALL the information associated with a
   a specified template.
		
   NOTES
  ***********************************************************************/


#ifndef TEMP_INFO_STRUCT_H
#define TEMP_INFO_STRUCT_H

#include "temp_item_struct.h"
#include "temp_records_struct.h"

/* this structure contains all the information specified for a template, 
   including info for the phrases and any associated conditions.
   specifically, the template name, the number of conditions (which will
   be non-zero and equal to the number of phrases if conditions are 
   supported), the size of the stack which holds all the tokens in the
   conditional expression, a pointer to the bottom of this stack,
   the number of phrases, the length of each phrase, and the phrases
   themselves.  Also included is the result (i.e., true or false)
   of a conditional expression after it is evaluated */
  
typedef struct
{
   char				name[MAXLEN_TEMPLATENAME];
   int				num_conditions;
   int				stacksize[MAX_NUMPHRASES];
   template_item_struct		*bottom_condition_stack[MAX_NUMPHRASES];
   int				num_phrases;
   int				phraselen[MAX_NUMPHRASES];	
   char				*phrase[MAX_NUMPHRASES];
   int				include_phrase[MAX_NUMPHRASES];
   int                          bulletstr_flag[MAX_NUMPHRASES];
   int                          indentstr_flag[MAX_NUMPHRASES];
   format_struct		format;
   variable_struct		variable;
   spectime_struct		spectime;
} template_info_struct;



#endif
