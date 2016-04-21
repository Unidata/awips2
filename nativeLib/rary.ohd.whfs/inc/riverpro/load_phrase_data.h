/*********************************************************************
   load_phrase_data.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef LOAD_PHRASE_DATA_H
#define LOAD_PHRASE_DATA_H

#include <stdio.h>

#include "rpf_err_defs.h"            /* definitions */
#include "rpf_general_defs.h"
#include "template_defs.h"
#include "template_err_defs.h"

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"
#include "county_struct.h"
#include "misc_struct.h"
#include "pcc_struct.h"
#include "temp_info_struct.h"
#include "temp_item_struct.h"
#include "vtecinfo_struct.h"

#include "build_ugc.h"		    /* protos */
#include "select_pid_and_fps.h"

#include "format_vals.h"
#include "buf_template.h"
#include "rpf_stack.h"

#include "load_variable_value.h"

#include "rpf_converts.h"
#include "rpf_logs.h"



void load_phrase_data(const int				fpindex,
		      const int				product_section,
		            fp_struct			*fp,
		            grp_struct			*grp,
		      const int                         numcnty,
		            county_struct               *cnty,
		            misc_struct			*misc,
		            vtecinfo_struct             *vtecinfo,
		      const int				phrasenum,
		      const template_info_struct 	*template_info,
		            pcc_struct			*pcc,
		      	    char			**newphrase);

#endif
