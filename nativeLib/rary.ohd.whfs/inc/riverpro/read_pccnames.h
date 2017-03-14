/*****************************************************************
   rpf_protos.h
   
   PURPOSE
   Include file for function prototypes for those functions
   that do not have their own include file.
   Also included are the structure definitions for those
   structures contained in the prototypes.
  
   NOTES
   
   ***************************************************************/

#ifndef READ_PCCNAMES_H
#define READ_PCCNAMES_H

#include "rpf_general_defs.h"    /* definitions */ 
#include "rpf_err_defs.h"       
#include "rpf_file_defs.h"
#include "cat_and_product_defs.h" 

#include "pccnames_struct.h"      /* structures */

#include "rpf_logs.h"	          /* protos */
#include "malloc_misc.h"


pccnames_struct * read_pccnames(misc_struct *misc);


void sort_pccproduct(pccnames_struct 	*tempnames, 
		     char		*prod_categ,
		     int		nwr_flag,
		     int		cnt,
		     int		*curout, 
		     int		*order_index);

int check_if_sorted(int 	*order_index,
		    int		cnt,
		    int 	indexnum);

#endif
