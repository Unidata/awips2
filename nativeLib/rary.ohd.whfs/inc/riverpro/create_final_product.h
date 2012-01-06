/*********************************************************************
   create_final_product.h
   
   PURPOSE
   
   ******************************************************************/

#ifndef CREATE_FINAL_PRODUCT_H
#define CREATE_FINAL_PRODUCT_H

#include <stdio.h>

#include "rpf_err_defs.h"            /* definitions */
#include "rpf_general_defs.h"
#include "rpf_file_defs.h"

#include "vtecinfo_struct.h"         /* structures */
#include "fp_struct.h"               
#include "misc_struct.h"
#include "pcc_struct.h"
		     
#include "rpf_converts.h"            /* protos */
#include "rpf_logs.h"
#include "DbmsUtils.h"
#include "process_vtecinfo.h"

#include "VTECevent.h"               /* database tables */
#include "VTECpractice.h"

/* constants */


/* prototypes */


void  create_final_product(FILE 	*infile_ptr, 
		           FILE 	*outfile_ptr,
			   misc_struct  *misc,
			   int		max_buflen,
			   char		*returned_msg,
			   int		numfps,
			   fp_struct	*fp,
			   vtecinfo_struct *vtecinfo,
			   char		*event_signif,
			   pcc_struct   *pcc);	


void translate_timecode(FILE 		*outfile_ptr,
                        char		*fileline, 
			misc_struct	*misc,
	                int		*timecode_index, 
			time_t		*actual_time);

#endif
