/************************************************************************
   pp_list_struct.h
   
   PURPOSE
   Include file for structure on the previous product issuances.
		
   NOTES
   
  ***********************************************************************/


#ifndef PP_LIST_STRUCT_H
#define PP_LIST_STRUCT_H

#include <time.h>

#include "rpf_general_defs.h"

/* this structure contains the list of previously issued products */

typedef struct
{
   time_t	prod_timet;
   char		prodid[PRODUCT_LEN + 1];
   int 		issnum;
} pp_struct;
 


#endif
