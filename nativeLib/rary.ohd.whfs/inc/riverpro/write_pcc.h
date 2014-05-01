#ifndef WRITE_PCC_H
#define WRITE_PCC_H

#include <stdio.h>

#include "pcc_struct.h"       /* structures */
#include "grp_struct.h"
#include "fp_struct.h"
#include "misc_struct.h"

#include "cat_and_product_defs.h"   /* definitions */
#include "pcc_keyvalue_defs.h"
#include "rpf_converts.h"


/* prototypes */

void write_pcc_product(FILE 		*file_ptr,
		       pcc_struct 	*pcc,
		       int		numfps,
		       fp_struct	*fp,
		       misc_struct	*misc,
		       int		fps_switch);

void write_pcc_header(FILE 		*file_ptr,
		      pcc_struct 	*pcc);
		      
void write_pcc_headline(FILE 		*file_ptr,
		      pcc_struct 	*pcc);		      

void write_pcc_summary(FILE 		*file_ptr,
		       pcc_struct 	*pcc,
		       int		numgrps,
		       grp_struct	*grp,
		       misc_struct 	*misc);

void write_pcc_basis(FILE 		*file_ptr,
		     pcc_struct 	*pcc);

void write_pcc_tabular(FILE 		*file_ptr,
		       pcc_struct 	*pcc, 
		       int		numfps,
		       fp_struct	*fp,
		       misc_struct	*misc);

void write_pcc_roundup(FILE 		*file_ptr,
		       pcc_struct 	*pcc,
		       int		numfps,
		       fp_struct	*fp,
		       misc_struct	*misc);

void write_pcc_impact(FILE 		*file_ptr,
		      pcc_struct 	*pcc,
		      int		numfps,
		      fp_struct		*fp,
		      misc_struct	*misc);

void write_pcc_comparison(FILE 		*file_ptr,
			  pcc_struct 	*pcc,
			  int		numfps,
			  fp_struct	*fp,
			  misc_struct	*misc);

void write_pcc_cta(FILE 	*file_ptr,
		   pcc_struct 	*pcc);
#endif
