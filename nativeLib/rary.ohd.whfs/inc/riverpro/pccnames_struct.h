/************************************************************************
   pccnames_struct.h
   
   PURPOSE
   Include file for structures on the product content control information.
   
   NOTES
   Some of the product level information is contained in the following
   other variables: product_chosen, product_reason, fps_included
   
   ***********************************************************************/


#ifndef PCCNAMES_STRUCT_H
#define PCCNAMES_STRUCT_H

#include "List.h"
#include "DbmsDefs.h"

#include "rpf_general_defs.h"   /* general rpf program definitions */

typedef struct
{
   Node	node;
   
   char filename[MAXLEN_FILENAME];
   char product_cnx[PRODUCT_LEN + 1];
   char	prod_categ[PROD_CATEG_LEN + 1];
   int  nwr_flag;
   char descr[MAXLEN_STRING + MAXLEN_STRING];
   int	sequence;
   
   List list;
} pccnames_struct;

#endif
