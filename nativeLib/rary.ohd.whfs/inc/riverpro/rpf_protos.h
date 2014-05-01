/*****************************************************************
   rpf_protos.h
   
   PURPOSE
   Include file for function prototypes for those functions
   that do not have their own include file. -or-
   those functions called by main rpf_batch program.
   Also included are the structure definitions for those
   structures contained in the prototypes.
   
   ***************************************************************/

#ifndef RPF_PROTOS_H
#define RPF_PROTOS_H

#include "cat_and_product_defs.h"    /* definitions */
#include "rpf_general_defs.h"     
#include "rpf_err_defs.h"       
#include "rpf_file_defs.h" 

#include "fp_struct.h"               /* structures */
#include "grp_struct.h" 
#include "county_struct.h"
#include "pcc_struct.h" 
#include "pccnames_struct.h"
#include "temp_names_struct.h"    
#include "temp_item_struct.h"     
#include "misc_struct.h"
#include "fcsttrend_info_struct.h"
#include "vtecinfo_struct.h"

#include "check_conditions.h"         /* other functions protos */
#include "compute_stage_info.h"
#include "create_product.h"
#include "create_tabular_section.h"
#include "define_product_content.h"
#include "get_fp_grp_county.h"
#include "get_stages.h"
#include "load_offices.h"
#include "load_variable_value.h"
#include "malloc_misc.h"
#include "read_pcc.h"
#include "read_names.h"
#include "select_pid_and_fps.h"
#include "process_vtecinfo.h"
#include "read_trendphrase.h"

#include "rpf_util.h"
#include "rpf_converts.h"

#include "RpfParams.h"                 /* database */
#include "Admin.h"  


void rpf_batch_get_startup_options(const	int		argc,
			 	char		**argv,
			 	char		startup_pccfile[],
				char		user_suffix[]);


void get_misc(misc_struct *misc);



#endif
