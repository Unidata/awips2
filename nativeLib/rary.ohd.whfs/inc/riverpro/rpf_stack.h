/*********************************************************************
   rpf_stack.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef RPF_STACK_H
#define RPF_STACK_H

#include "rpf_logs.h"                   /* protos */

#include "template_err_defs.h"          /* definitions */
#include "template_defs.h"
#include "rpf_err_defs.h"

#include "temp_item_struct.h"           /* structures */
#include "temp_info_struct.h"


/*void expr_pushstack(const 	itemtypes		type,
		    const	values 			value,
		    const	int			varindex,
		    const	int			slot,
		                template_item_struct 	*template_item);
*/
void expr_pushstack(const 	itemtypes		type,
		    const	values 			value,
		    varinfo_struct  		        varinfo,
		    const	int			slot,
		                template_item_struct 	*template_item);

void expr_popstack(const int			slot,
		   const int 			num_to_pop,
		   const int			stacksize,
			 template_item_struct 	*template_item);

			 
void free_template_memory(template_info_struct *template_info);

#endif
