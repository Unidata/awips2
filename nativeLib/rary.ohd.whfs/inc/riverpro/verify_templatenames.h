/*****************************************************************
   verify_templatenames.h
   
   PURPOSE
   Include file for function prototypes.
 
   NOTES
   
   ***************************************************************/

#ifndef VERIFY_TEMPLATENAMES_H
#define VERIFY_TEMPLATENAMES_H

#include "rpf_general_defs.h"        /* definitions */
#include "rpf_err_defs.h"       

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"
#include "pcc_struct.h"      
#include "temp_names_struct.h"    

#include "rpf_converts.h"           /* other functions protos */
#include "rpf_logs.h"

void verify_templatenames(const int			numfps,
			  const fp_struct		*fp,
			  const	int			numgrps,
			  const grp_struct		*grp,
			  const templatenames_struct	*templatenames,
			        pcc_struct		*pcc);

void verify_hdrtemplate(const	templatenames_struct	*templatenames,
		        	pcc_struct 		*pcc);
   
void verify_bastemplate(const 	templatenames_struct 	*templatenames,
		      	    	pcc_struct 		*pcc);
				
void verify_headltemplate(const templatenames_struct 	*templatenames,
		      	    	pcc_struct 		*pcc);
								
   
void verify_sumtemplate(const	int			numgrps,
			const	grp_struct 		*grp,
			const	templatenames_struct 	*templatenames,
		      	    	pcc_struct 		*pcc);
   
void verify_tabtemplate(const	templatenames_struct 	*templatenames,
		    		pcc_struct 		*pcc);
   
void verify_rndtemplate(const	int 		 	numfps,
			const	fp_struct		*fp,
			const	templatenames_struct 	*templatenames,
		      	     	pcc_struct 		*pcc);
   
void verify_imptemplate(const	templatenames_struct 	*templatenames,
		            	pcc_struct 	 	*pcc);
   
void verify_cmptemplate(const	templatenames_struct 	*templatenames,
		      	   	pcc_struct 		*pcc);
   
void verify_ctatemplate(const	templatenames_struct 	*templatenames,
		           	pcc_struct  	 	*pcc);

#endif
