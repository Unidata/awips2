/*****************************************************************
   select_pid_and_fps.h
   
   PURPOSE
   Include file for function prototypes.
  
   NOTES
   
   ***************************************************************/

#ifndef SELECT_PID_AND_FPS_H
#define SELECT_PID_AND_FPS_H


#include "cat_and_product_defs.h"    /* definitions */
#include "rpf_general_defs.h"     
#include "rpf_err_defs.h" 

#include "fp_struct.h"               /* structures */
#include "grp_struct.h" 
#include "county_struct.h"
#include "misc_struct.h"
#include "pcc_struct.h"
#include "vtecinfo_struct.h"

#include "PrevProd.h"                /* database */

#include "compute_stage_info.h"      /* protos */
#include "process_vtecinfo.h"
#include "rpf_converts.h"
#include "rpf_logs.h"
#include "rpf_util.h"

#include "time_convert.h"            /* utility */


void select_pid_and_fps(int		numfps,
			fp_struct	*fp,
			int		numgrps,
			grp_struct	*grp,
			pcc_struct	*pcc,
			misc_struct	*misc,
			vtecinfo_struct *vtecinfo);


void determine_issuance_number(int		numfps,
			       fp_struct	*fp,
			       int		prod_categ_index,
			       misc_struct	*misc);

void determine_expiretime(int		prod_categ_index,
			  misc_struct	*misc,
			  pcc_struct     *pcc);

void round_expiretime(misc_struct 	*misc);

		       
void check_cnty_included(county_struct         *cnty,
                         int                   numcnty,
			 misc_struct           *misc);	

		  			 
			 			      
#endif
