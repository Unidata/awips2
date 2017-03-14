/*****************************************************************
  malloc_misc.h
   
   PURPOSE
   Include file for function prototypes for those functions
   that do not have their own include file.
   Also included are the structure definitions for those
   structures contained in the prototypes.
  
   
   ***************************************************************/

#ifndef MALLOC_MISC_H
#define MALLOC_MISC_H

#include "cat_and_product_defs.h"    /* definitions */
#include "rpf_general_defs.h"     
#include "rpf_err_defs.h" 
#include "template_defs.h"

#include "fp_struct.h"               /* structures */
#include "grp_struct.h" 
#include "county_struct.h"
#include "pcc_struct.h"
#include "pccnames_struct.h"
#include "temp_names_struct.h"    
#include "temp_item_struct.h"     
#include "misc_struct.h"
#include "vtecinfo_struct.h"

#include "rpf_converts.h"	    /* protos */
#include "rpf_logs.h"


void malloc_miscmain(misc_struct 	**misc);

void malloc_miscsubs(int		numfps,
		     int		numgrps,
		     int                numcnty, 
		     misc_struct 	*misc);

void malloc_vtecinfo(int		numfps,
		     vtecinfo_struct	**vtecinfo);

void malloc_pcc(int		numfps,
		int		numgrps, 
		pcc_struct 	**pcc);

void malloc_pccsubs(int		numfps,
		    int		numgrps, 
		    		pcc_struct 	*pcc);

void malloc_templatenames(templatenames_struct **template_names);


/* ----------------------------------------------- */

void free_memory(int			numfps,
		 fp_struct		*fp,
		 int			numgrps,
		 grp_struct		*grp,
		 int                    numcnty,
		 county_struct          *cnty,
		 vtecinfo_struct        *vtecinfo,
		 misc_struct		*misc,
		 pcc_struct 		*pcc,
		 templatenames_struct	*templatenames);

void free_fpgrpcnty(int			numfps, 
		    fp_struct		*fp,
		    int			numgrps,
		    grp_struct		*grp,
		    int			numcnty,
		    county_struct 	*cnty);

void free_miscsubs(int 		numfps, 
		   int 		numgrps,
		   int          numcnty,
		   misc_struct 	*misc);

void free_templatenames(templatenames_struct	*templatenames);

void free_vtecinfo(vtecinfo_struct   *vtecinfo);

void free_pcc(int 		numfps,
	      int		numgrps,
	      pcc_struct 	*pcc);

void free_pccnames(pccnames_struct *pccnames);


#endif
