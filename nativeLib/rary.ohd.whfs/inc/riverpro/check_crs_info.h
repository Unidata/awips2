/*********************************************************************
   check_crs_info.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef CHECK_CRS_INFO_H
#define CHECK_CRS_INFO_H

#include <stdio.h>

#include "rpf_err_defs.h"            /* definitions */
#include "rpf_general_defs.h"
#include "rpf_file_defs.h"
#include "template_defs.h"

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"
#include "misc_struct.h"
#include "pcc_struct.h"
#include "temp_info_struct.h"
#include "temp_item_struct.h"
		     
#include "check_conditions.h"        /* protos */  
#include "create_tabular_section.h"
#include "load_variable_value.h"
#include "order_fps.h"

#include "format_vals.h"
#include "buf_template.h"

#include "rpf_converts.h"
#include "rpf_logs.h"

#include "NWRTransmitter.h"          /* database tables */
#include "LocTransmit.h"

#include "LoadUnique.h"


int check_loc_in_tower(const char 	*lid,
		       NWRTransmitter	*ntransPtr,
		       LocTransmit	*loctransPtr);

int check_grp_in_tower(const grp_struct		*grp,
		       const int		grpindex,
		       const fp_struct		*fp,
		             misc_struct	*misc);

int check_numloc_in_tower(const 	int		numfps,
		          const 	fp_struct	*fp,
		    	  		misc_struct	*misc,
					NWRTransmitter	*ntransPtr, 
					LocTransmit	*loctransPtr,
					FILE		*outfilePtr);

int check_locs_covered(const 	int		numfps,
		       const 	fp_struct	*fp,
		    	  	misc_struct	*misc,
				NWRTransmitter	*nwrtransPtr, 
				LocTransmit	*loctransPtr,
				FILE		*outfile_ptr);

int count_unique_prodcodes(char *where);

#endif
