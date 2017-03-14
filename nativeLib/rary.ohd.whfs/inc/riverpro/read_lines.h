/*********************************************************************
   read_lines.h
   
   PURPOSE
   Include file for function prototypes.
   
   ******************************************************************/

#ifndef READ_LINES_H
#define READ_LINES_H

#include "template_err_defs.h"     /* definitions */
#include "rpf_general_defs.h"      
#include "rpf_file_defs.h" 

#include "temp_name_struct.h"      /* structures */

#include "rpf_converts.h"          /* other functions protos */
#include "rpf_logs.h"


void read_generic_templates(char 		*file_suffix,
			    char		*selected_office,
			    int			section_index,
			    temp_name_struct	*templatename);


void get_continuation_lines(char	*fileline,
		    	    FILE	*file_ptr,
			    int		*linenum,
			    char	outline[]);

void determine_template_record_type(char *file_line,
				    int  section_index,
				    int  linenum,
				    int  *record_type);
				    
int name_compare(const void *name1, 
		 const void *name2);				    

#endif
