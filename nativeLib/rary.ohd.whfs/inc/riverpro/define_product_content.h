/*****************************************************************
   define_product_content.h
   
   PURPOSE
   Include file for function prototypes.
 
  ***************************************************************/

#ifndef DEFINE_PRODUCT_CONTENT_H
#define DEFINE_PRODUCT_CONTENT_H

#include <time.h>


#include "rpf_general_defs.h"        /* definitions */
#include "rpf_file_defs.h"        
#include "rpf_err_defs.h"       
#include "cat_and_product_defs.h" 
#include "pcc_keyvalue_defs.h"
#include "function_defs.h"

#include "fp_struct.h"               /* structures */
#include "grp_struct.h"
#include "county_struct.h"
#include "vtecinfo_struct.h"
#include "misc_struct.h"     
#include "pcc_struct.h"      
#include "temp_names_struct.h"    

#include "malloc_misc.h"             /* protos */
#include "read_pcc.h"
#include "read_names.h"
//#include "rec_prod.h"
#include "verify_templatenames.h"
#include "process_vtecinfo.h"

#include "rpf_converts.h"
#include "rpf_logs.h"
#include "rpf_util.h"

#include "Floodstmt.h"                 /* database tables*/
#include "Crest.h"


void define_product_content(int				numfps,
			    fp_struct			*fp,
			    int				numgrps,
			    grp_struct			*grp,
			    int				numcnty,
			    county_struct		*cnty,
			    templatenames_struct	*templatenames,
			    misc_struct			*misc,
			    pcc_struct			*pcc,
			    vtecinfo_struct		*vtecinfo);

void load_product_content(int			numfps,
			  fp_struct		*fp,
			  int			numgrps,
			  grp_struct		*grp,
			  int			numcnty,
			  county_struct		*cnty,
			  char 			pcc_file[],
			  templatenames_struct	*templatenames,
			  misc_struct		*misc,
			  pcc_struct		*pcc,
			  vtecinfo_struct	*vtecinfo);

void set_prodfps(pcc_struct		*pcc,
		 int			numfps,
		 fp_struct		*fp,
		 int			numgrps,
		 grp_struct		*grp,
		 misc_struct		*misc);

void set_pccfilename(misc_struct	*misc,
		     int		prod_categ_index,
		     char 		pcc_file[]);

void fill_pcc_impact_info(int		numfps,
			  fp_struct	*fp,
			  misc_struct	*misc,
			  pcc_struct	*pcc);

void load_impact(int		fpindex,
		 float		refstage,
		 char		systemdate[],
		 Floodstmt	*FloodstmtPtr,
		 pcc_struct	*pcc,
		 fp_struct      *fp);

void fill_pcc_comparison_info(int		numfps,
			      fp_struct		*fp,
			      misc_struct	*misc,
			      pcc_struct		*pcc);

void load_crest(int 		fpindex,
		float 		refstage,
		char		begin_time[],
		Crest		*crestPtr,
		pcc_struct	*pcc,
		fp_struct       *fp);

int check_impact_daterange(char system_time[],
			   char start[],
			   char end[]);

int compare_date(char	date1[],
		 char	date2[]);


#endif
