/*********************************************************************
   check_conditions.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef CHECK_CONDITIONS_H
#define CHECK_CONDITIONS_H

#include "rpf_err_defs.h"           /* definitions */
#include "template_err_defs.h"
#include "rpf_general_defs.h"
#include "function_defs.h"

#include "fp_struct.h"              /* structures */
#include "grp_struct.h"
#include "county_struct.h"
#include "misc_struct.h"
#include "pcc_struct.h"
#include "temp_info_struct.h"
#include "temp_item_struct.h"
#include "vtecinfo_struct.h"

#include "load_variable_value.h"     /* protos */

#include "eval_condit.h"

#include "rpf_converts.h"
#include "rpf_logs.h"



void check_conditions(const	int			id_index,
		           	fp_struct		*fp,
		          	grp_struct		*grp,
		      const     int                     numcnty,
		                county_struct           *cnty,
		                misc_struct		*misc,
		                vtecinfo_struct         *vtecinfo,
		                pcc_struct		*pcc,
		      const	int			product_section,
		     		template_info_struct	*template_info);

void load_condition_data(const int			id_index,
			       fp_struct		*fp,
			       grp_struct		*grp,
			 const int                      numcnty,
			       county_struct            *cnty,
			       misc_struct		*misc,
			       vtecinfo_struct          *vtecinfo,
			       pcc_struct		*pcc,
			 const int			stacknum,
			 const int			product_section,
			       template_info_struct	*template_info);


#endif
