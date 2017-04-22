/********************************************************************
   save_carryover.h
   
   PURPOSE
   Include file for save_carryover.c file.
   
   NOTES
   
********************************************************************/

#ifndef SAVE_CARRYOVER_H
#define SAVE_CARRYOVER_H

#include "PrevProd.h"
#include "FpPrevProd.h"
#include "DbmsDefs.h"
#include "PurgeProduct.h"


#include "rpf_general_defs.h"   /* definitions */

#include "fp_struct.h"          /* structures */
#include "grp_struct.h"
#include "pcc_struct.h"
#include "misc_struct.h"
#include "vtecinfo_struct.h"

#include "rpf_converts.h"       /* prototypes */
#include "get_fp_grp_county.h"
#include "get_stages.h"
#include "process_vtecinfo.h"

#include "time_convert.h"
#include "TextProduct.h"

#include "WordWrap.h"

int save_carryover(int			numfps,
		    fp_struct		*fp,
		    int			numgrps,
		    grp_struct		*grp,
		    int			numcnty,
		    county_struct	*cnty,
		    pcc_struct		*pcc,
		    misc_struct		*misc,
		    vtecinfo_struct	*vtecinfo,
		    char		product_class,
		    char		*filename);

int save_product(misc_struct	*misc,
		  char		*product_cnx,
		  char		product_class,
		  char		*prod_categ,
		  char		*filename);

void store_purgeinfo(char 	*prodid,
		     time_t	product_timet,
		     time_t	post_timet);    	  

void save_fpprevprod(misc_struct	*misc,
		     int		numfps,
		     fp_struct		*fp,
		     char		*product_id,
		     char		*prod_categ,
		     vtecinfo_struct	*vtecinfo);
   

#endif
