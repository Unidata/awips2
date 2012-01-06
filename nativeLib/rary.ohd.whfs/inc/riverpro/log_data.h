/*********************************************************************
   log_data.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef LOG_DATA_H
#define LOG_DATA_H

#include <stdio.h>

#include "rpf_general_defs.h"
#include "rpf_err_defs.h"            /* definitions */
#include "rpf_file_defs.h"
#include "function_defs.h"

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"     
#include "pccnames_struct.h"
#include "temp_item_struct.h"     				  
#include "temp_info_struct.h"
#include "temp_names_struct.h"
#include "temp_records_struct.h"  

#include "rpf_converts.h"            /* other functions protos */
#include "time_convert.h"

#include "DbmsDefs.h"


void log_fp_grp(const int 		numfps,
		const fp_struct 	*fp,
		const int		numgrps,
		const grp_struct	*grp);

void log_templatenames(const templatenames_struct *templatenames);

void log_tempnames(const temp_name_struct 	*tempnames,
		         FILE			*file_ptr);

void log_pccnames(pccnames_struct 	*pccnames);

void log_template_info(const template_info_struct *template_info);

void log_template_buffers(const format_struct	*format,
		          const variable_struct	*variable,
			  const spectime_struct *spectime);
 
#endif
