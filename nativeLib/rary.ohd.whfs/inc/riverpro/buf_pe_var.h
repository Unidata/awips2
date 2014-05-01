/*********************************************************************
   buf_pe_var.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef BUF_PEVAR_H
#define BUF_PEVAR_H

#include <stdio.h>

#include "template_err_defs.h"              /* definitions */
#include "template_defs.h"
#include "rpf_general_defs.h"
#include "cat_and_product_defs.h"
#include "function_defs.h"

#include "temp_info_struct.h"          /* structures */
#include "temp_item_struct.h"
#include "temp_records_struct.h"
#include "time_format_struct.h"

#include "rpf_converts.h"          /* other functions protos */
#include "rpf_logs.h"

int check_if_PEvar(char 		*fullfield,
		   varinfo_struct	*varinfo);

int read_petime(time_t		curtime,
		char 		*fileline,
		petime_struct	*petime);

int check_if_timeflag(char 		*fullfield);

int check_if_derived(char 		*fullfield,
		     varinfo_struct	*varinfo);


#endif
