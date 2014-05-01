/*********************************************************************
   read_pcc.h
   
   PURPOSE
   Include file for function prototypes.
   
   ******************************************************************/

#ifndef READ_PCC_H
#define READ_PCC_H

#include <stdio.h>

#include "rpf_err_defs.h"           /* definitions */
#include "pcc_keyvalue_defs.h"      
#include "rpf_file_defs.h"
#include "template_defs.h"

#include "pcc_struct.h"             /* structures */ 
#include "fp_struct.h"
#include "grp_struct.h"

#include "rpf_converts.h"           /* protos */        
#include "rpf_logs.h"
#include "rpf_util.h"


void read_pcc(int		numfps,
	      fp_struct		*fp,
	      int		numgrps,
	      grp_struct 	*grp,
	      char		pcc_file[],
	      pcc_struct 	*pcc);

void init_pcc(int 		numfps,
	      int 		numgrps,
	      pcc_struct 	*pcc);

void read_pcc_product_info(int		numfps,
			   fp_struct	*fp,
			   FILE		*file_ptr,
			   pcc_struct	*pcc);

void read_pcc_header_info(FILE		*file_ptr,
			  pcc_struct	*pcc);
			  
void read_pcc_headline_info(FILE        *file_ptr,
                           pcc_struct  *pcc);			  

void read_pcc_summary_info(int		numgrps,
			   grp_struct	*grp,
			   FILE		*file_ptr,
			   pcc_struct  	*pcc);

void read_pcc_basis_info(FILE		*file_ptr,
			 pcc_struct  	*pcc);

void read_pcc_tabular_info(int		numfps,
			   fp_struct	*fp,
			   FILE		*file_ptr,
			   pcc_struct  	*pcc);

void read_pcc_cta_info(FILE		*file_ptr,
		       pcc_struct	*pcc);


void read_pcc_roundup_info(int		numfps,
			   fp_struct 	*fp,
			   FILE		*file_ptr,
			   pcc_struct	*pcc);

void read_pcc_impact_info(int		numfps,
			  fp_struct 	*fp,
			  FILE		*file_ptr,
			  pcc_struct	*pcc);

void read_pcc_comparison_info(int		numfps,
			      fp_struct	*fp,
			      FILE	*file_ptr,
			      pcc_struct 	*pcc);

void get_include_points(int		numfps,
			fp_struct	*fp,
			char 		*fileline,
			FILE		*file_ptr,
			int		include_fp[]);

void get_include_ctas(char		fileline[],
		      pcc_struct	*pcc);


void set_includeflags(pcc_struct *pcc);

void check_crestdate(char dait[]);

#endif
