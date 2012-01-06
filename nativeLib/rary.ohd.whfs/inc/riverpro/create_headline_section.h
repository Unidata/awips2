/*********************************************************************
   create_headline_section.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef CREATE_HEADLINE_SECTION_H
#define CREATE_HEADLINE_SECTION_H

#include <stdio.h>


#include "rpf_err_defs.h"           /* definitions */
#include "rpf_general_defs.h"      
#include "rpf_file_defs.h"
#include "function_defs.h"
#include "template_defs.h"
#include "temp_info_struct.h"

#include "fp_struct.h"              /* structures */
#include "grp_struct.h" 
#include "county_struct.h"
#include "temp_records_struct.h"
#include "pcc_struct.h" 
#include "misc_struct.h"

#include "check_conditions.h"       /* riverpro functions protos */
#include "create_product.h"
#include "define_product_content.h"
#include "load_variable_value.h"
#include "rpf_converts.h"
#include "rpf_logs.h"
#include "load_pe_value.h"

#include "format_vals.h"            /* template function protos */
#include "buf_template.h"
#include "read_lines.h"
#include "rpf_stack.h"


void create_headline_section(const int		 numfps,
			    fp_struct		*fp,
			    const int		  numgrps,
			    grp_struct		 *grp,
			    const int              numcnty,
			    county_struct        *cnty,
			    pcc_struct		 *pcc, 
			    misc_struct		 *misc,
			    vtecinfo_struct      *vtecinfo,
			    template_info_struct *template_info,
			    FILE 		 *outfile_ptr); 


void write_phrase_bullindent_text(template_info_struct    *template_info,
                                    pcc_struct            *pcc,
				    char                       *newphrase,
				    FILE                      *outfile_ptr,
				    const int                   phrasenum,
				     int                       bullet_1st,
			             int                       indent_1st);
				     
				     
void process_event_block(const int                fpindex,
                    const int                    product_section,
		         fp_struct             *fp, 
			 grp_struct            *grp,
                    const int  	                 numcnty,
		         county_struct         *cnty,
		         misc_struct	        *misc,
		         vtecinfo_struct       *vtecinfo,
		         template_info_struct	*template_info,
		    const char                   template_name[],
		         pcc_struct            *pcc,
		         FILE                  *file_ptr,
		         FILE                  *outfile_ptr,
			 int                    *line_written,
			 long int                begin_pos); 
			 


void buf_event_info(template_info_struct    *template_info,
                   const char                template_name[],
                   FILE                    *file_ptr, 
		   const int                 product_section,
		   long int                  begin_pos);

#endif
