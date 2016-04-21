/*********************************************************************
   evalk_condit.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef EVAL_CONDIT_H
#define EVAL_CONDIT_H

#include "template_err_defs.h"      /* definitions */
#include "rpf_general_defs.h"
#include "function_defs.h"
 
#include "temp_info_struct.h"       /* structures */
#include "temp_item_struct.h"

#include "rpf_converts.h"           /* protos */
#include "rpf_logs.h"
#include "rpf_stack.h"


void eval_condit(const	int			stacknum,
			template_info_struct 	*template_info);

int evaluate_rel_expr(const	template_item_struct	*template_item,
		      const	int 			index);

int evaluate_log_expr(const	template_item_struct	*template_item,
		      const	int 			index);


#endif
