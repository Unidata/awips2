/*********************************************************************
   load_offices.h
   
   PURPOSE
   Include file for function prototypes.
   
   ******************************************************************/

#ifndef LOAD_OFFICES_H
#define LOAD_OFFICES_H

#include "misc_struct.h"             /* riverpro structures */

#include "rpf_err_defs.h"            /* riverpro definitions */
#include "rpf_file_defs.h"

#include "rpf_logs.h"                /* protos */ 

#include "LoadUnique.h"              /* database access */

#include "DbmsDefs.h"		     /* other definitions */


/* prototypes */

void load_offices(misc_struct *misc);

int check_office_files(char		*office,
		       int		*num_tpl_files,
		       int		*num_default_pcc_files,
		       int		*num_total_pcc_files);



#endif
