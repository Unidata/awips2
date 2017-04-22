/*********************************************************************
   buf_template.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef BUF_TEMPLATE_H
#define BUF_TEMPLATE_H

#include <stdio.h>

#include "template_err_defs.h"        /* definitions */
#include "template_defs.h"
#include "rpf_general_defs.h"
#include "cat_and_product_defs.h"
#include "function_defs.h"

#include "temp_info_struct.h"         /* structures */
#include "temp_item_struct.h"
#include "temp_records_struct.h"
#include "time_format_struct.h"

#include "rpf_converts.h"             /* other functions protos */
#include "rpf_logs.h"

#include "rpf_stack.h"
#include "read_lines.h"               /* for: get_continuation_line()
				         for: determine_template_record_type() */
#include "buf_pe_var.h"               /* for: check_if_PEvar() */



void buf_template(const char 			template_file[],
		  const char 			template_chosen[],
		  const int			template_type,
		  const time_t			system_time,
		       	template_info_struct	*template_info);

void read_condition(char			*file_line,
		    FILE			*file_ptr,
		    template_info_struct	*template_info);

void read_phrase(char 			*file_line,
		 FILE			*file_ptr,
		 template_info_struct	*template_info,
		 int                     record_type);


void read_format(char		*fileline, 
		 format_struct	*format);

void grab_format_item(char *fileline,
		      char *format_item,
		      char *format_literal);

void read_varlist(char 			*fileline,
		  variable_struct	*variable);

void read_spectime(const time_t			system_time,
		    	 char			*fileline,
		    	 spectime_struct	*spectime);

void read_grpname(char 	*fileline,
		  int 	*skip_grpline,
		  int	*include_grpname);

void read_msgdata(char 	*fileline,
		  int 	*skip_msgdata,
		  int   *skip_allmsg,
		  char	*missing_str);

int get_num_items(char	file_line[]);

void verify_condition_syntax(template_info_struct	*template_info);

void get_item_info(char			*item,
		   itemtypes		*type,
		   values		*value,
		   varinfo_struct	*varinfo);

void check_if_variable(char		*item,
		       itemtypes	*type,
		       varinfo_struct	*varinfo);

int check_var_access(const char	*accesslist,
		     const int	product_section);

void check_if_token(const char      *token,
			  itemtypes *type);

void check_if_number(const char       *token, 
		           itemtypes  *type, 
		           values     *value);

void check_if_string(const char      *token, 
		           itemtypes *type, 
		           values    *value);

void check_if_missval(const char      *token, 
		            itemtypes *type, 
		            values    *value);

void check_if_boolean(const char      *token, 
		            itemtypes *type, 
		            values    *value);

int check_var_access(const char	*accesslist,
		     const int	product_section);

int check_if_metric(char            *item,
                    varinfo_struct   *varinfo,
		    itemtypes        *type);
#endif
