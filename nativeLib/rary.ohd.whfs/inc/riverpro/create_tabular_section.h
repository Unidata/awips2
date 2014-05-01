/*********************************************************************
   create_tabular_section.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef CREATE_TABULAR_SECTION_H
#define CREATE_TABULAR_SECTION_H

#include <stdio.h>


#include "rpf_err_defs.h"           /* definitions */
#include "rpf_general_defs.h"      
#include "rpf_file_defs.h"
#include "function_defs.h"
#include "template_defs.h"

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


void create_tabular_section(int			numfps,
			    fp_struct		*fp,
			    int			numgrps,
			    grp_struct		*grp,
			    int                 numcnty,
			    county_struct       *cnty,
			    pcc_struct		*pcc, 
			    misc_struct		*misc,
			    vtecinfo_struct     *vtecinfo,
			    FILE 		*outfile_ptr); 

void load_tabular_values(fp_struct		*fp,
			 grp_struct		*grps,
			 int                    numcnty,
			 county_struct          *cnty,
			 pcc_struct		*pcc, 
			 misc_struct		*misc,
			 vtecinfo_struct        *vtecinfo,
			 int			fp_index,
			 char			*lid,
			 format_struct		*tabular_format,
			 variable_struct	*tabular_variable,
			 spectime_struct	*tabular_spectime,
			 int			skip_msgdata,
			 int                    skip_allmsg,
			 char			missing_str[],
			 int			*skip_output,
			 char 			outputline[]);

void write_leftover_fps(int			numfps,
			fp_struct		*fp,
			int			numgrps,
			grp_struct		*grp,
			int                     numcnty,
			county_struct           *cnty,
			pcc_struct		*pcc, 
			misc_struct		*misc,
			vtecinfo_struct         *vtecinfo,
			format_struct		*tabular_format,
			variable_struct	 	*tabular_variable,
			spectime_struct		*tabular_spectime,
			int 			add_grpname,
			int			skip_grpline,
			int			skip_msgdata,
			int                     skip_allmsg,
			char			missing_str[],
			int			*fps_written,
			FILE			*outfile_ptr);

#endif
