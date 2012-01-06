/************************************************************************
   temp_item_struct.h
   
   PURPOSE
   Include file for structure on the items in the conditional
   expression.  This structure contains specific information for
   each instance of a token or variable allowed in the templates.
   
   By definition, templates contain conditional expressions and
   template phrases; conditional expressions contain items, which
   include variables and tokens, as defined below.  Phrases contain
   verbatim text and variables.
		
   NOTES
   See comments below on each structure.
   
  ***********************************************************************/


#ifndef TEMP_ITEM_STRUCT_H
#define TEMP_ITEM_STRUCT_H

#include <time.h>

#include "template_defs.h"	/* definitions */
#include "DbmsDefs.h"
#include "temp_varinfo_struct.h"

/*-----------------------------------------------------------------*/
/* define structure for the information on each  
   allowable token in the template; this does not include any
   information on the variables in the templates. */

typedef struct
{
   char		name[MAXLEN_TOKENNAME];
   itemtypes	type;
} template_token_struct;

/*--------------------------------------------------------------------*/
/* define structure for the information on each allowable
   variable that can appear in the templates.  This structure
   contains descriptive information only on the variable and does not
   contain the value of the variable; that information is contained in 
   a separate structure */

typedef struct
{
   char			name[MAXLEN_VARNAME];
   itemtypes		type;
   structnames		source_struct;
   char			accesslist[MAXLEN_CODES];
   unsigned char 	access_condition;
   float                regular_factor;
   float                flow_factor;
} template_variable_struct;


/*--------------------------------------------------------------------*/
/* allow storage for the different types of values for the 
   conditional expression types */

typedef union 
{
   int		i;
   float	f;
   char		s[MAXLEN_VALUESTR];
   time_t       t;
   char 	d[DATE_LEN + 1];
} values;

/*--------------------------------------------------------------------*/
/* by definition, template items refer to either template
   variables or tokens.  tokens can appear only in a conditional
   expression while variables can appear in expressions or in the
   template phrase. 
   for each item in the expression, maintain the following info:
   (1) the type of item;
   (2) if the item is a constant or variable, the value of the
       constant or variable;
   (3) if the item is a variable, an index to the variable. */

typedef struct
{
   itemtypes	   type;
   values	   value;
   varinfo_struct  varinfo[MAX_FORMAT_ITEMS];
   		
} template_item_struct;

#endif
