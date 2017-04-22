/*********************************************************************
   get_fp_grp_county.h
   
   PURPOSE
   Include file for function prototypes.
   
   NOTES
   
   ******************************************************************/

#ifndef GET_FP_GRP_H
#define GET_FP_GRP_H


#include "rpf_err_defs.h"              /* definitions */
#include "rpf_general_defs.h"         
#include "cat_and_product_defs.h"     

#include "fp_struct.h"                 /* structures */
#include "grp_struct.h"
#include "misc_struct.h"
#include "county_struct.h"
#include "vtecinfo_struct.h"

#include "malloc_misc.h"	       /* riverpro protos */
#include "get_stages.h"

#include "time_convert.h"              /* utility */

#include "RpfFcstGroup.h"              /* database tables */
#include "Crest.h"
#include "Floodcat.h"
#include "FpInfo.h"
#include "FpPrevProd.h"
#include "FpPrevProdPractice.h"
#include "PrevProd.h"
#include "Countynum.h"
#include "Counties.h"
#include "LoadUnique.h"


#include "DbmsUtils.h"		       /* database defs */
#include "DbmsDefs.h"


void get_fp_grp_county(char		selected_office[],
		       int		*numfps,
		       fp_struct	**fp,
		       int		*numgrps,
		       grp_struct	**grp,
		       int              *numcnty,
		       county_struct    **cnty);   

void load_fpdata(const		int		numfps, 
		 		FpInfo		*fpinfoPtr,
		 		fp_struct	*fp);

void load_grpdata(const	int		numfps,
		  const	fp_struct	*fp,
		  const	int		numgrps,
		  const	int		*num_in_grps,
	     		RpfFcstGroup	*rpffcstptPtr,
	     		grp_struct	*grp);

int count_grp_fps(const	char		*grpid,
		      	FpInfo		*fpinfoPtr);

int load_cntydata(char                selected_office[],
                  UniqueList          *ulHead,
                  county_struct       *cnty,
    	          const fp_struct     *fp,
		  const int           numfps);

void load_fpprev_data(int 		numfps,
		      fp_struct		*fp,
		      vtecinfo_struct	*vtecinfo,
		      misc_struct       *misc);

#endif
